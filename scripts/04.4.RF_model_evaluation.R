# Evaluate model framework, leaving out 1 country and compare the predictions with interpolation
# ------------------------------------------------------------------------------

test_tps <- function(d) {
# Fit a TPS model
	if (!("fields" %in% installed.packages()[,1])) install.packages("fields")

	# with X and Y only
	# cty_fit0 <- fields::Tps(cbind(d$x, d$y), d$farm_area_ha, lon.lat = TRUE)

	Zvars <- c("cropland", "cattle", "pop", "cropland_per_capita", "sand", "slope", "temperature", "rainfall", "market", "maizeyield")
	Z = as.matrix(d[, Zvars])
	
	tps_model <- fields::Tps(
		x = as.matrix(d[, c("x", "y")]),
		d$farm_area_ha, Z=Z, lon.lat = TRUE
	)
	# predict the TPS on the coordinates of observed data
	prediction <- predict(tps_model, d[, c("x", "y")], Z=Z)[,1]
	rsq <- cor(d$farm_area_ha, prediction)^2 # Get the r2
	list(prediction=prediction, results=data.frame(rsq=rsq))
}	


test_rf <- function(d_train, d_test) {
# Random forest with my_country (only the covariates). This serves as reference
	rf_model <- caret::train(
		farm_area_ha ~ .,
		data = d_train |> dplyr::select(!c(x, y)),
		method = "ranger",
		# preProcess = c("center", "scale", "spatialSign"),
		# trControl = ctrl,
		metric = "Rsquared"
	)
	print(rf_model)
	
	prediction <- predict(rf_model, d_test) |> as.numeric()
	
### cv <- rf_model$results |> as.data.frame() |> dplyr::select(Rsquared) |> dplyr::pull() |> mean() 
#	cv <- mean(rf_model$results$Rsquared)
#	rsq <- cor(d$farm_area_ha, prediction)^2

	list(prediction=prediction, results=rf_model$results)
	
}


# Using a training set (all other countries) and a test set (country of interest) to evaluate model performance
leave_one_country_models <- function(the_country, the_code, model, means, test, sample_size=NA){

	stopifnot(model %in% c("TPS", "RF"))

	input_path <- "data/processed"
	output_path <- "output/leave_one"
	dir.create(output_path, FALSE, TRUE)

	print(paste0("--------------- Model evaluation in ", the_country, " (point-based) -------------"))
	fname <- file.path(output_path, paste0("loc_", the_code, "_", model, "_",  c("all", "means")[means+1], "_", c("train", "test")[test+1], ".rds"))
	if (file.exists(fname)) {
		return(fname)
	}
	print(basename(fname))


	
	lsms_spatial <- readRDS(file.path(input_path, "lsms_trimmed_95th_africa.rds"))
	lsms_spatial <- lsms_spatial |> dplyr::select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>  na.omit() 

	set.seed(2024) # for reproducibility!

    # caret control parms
	ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = FALSE)
  
  # subsetting df: training - test split (point-based)
	training_set <- lsms_spatial|>  dplyr::filter(country != the_country) |>  dplyr::select(!country) |>  na.omit()
	test_set <- lsms_spatial |> dplyr::filter(country == the_country) |>  dplyr::select(!country) |>  na.omit()


	if (means) {
	  #training - test split (consolidated mean-based => exclude all points with less than 10 records)
		training_set_mean <- training_set |> dplyr::group_by(x, y) |>
			dplyr::summarize(across(where(is.numeric), \(x) mean(x, na.rm = T)), n_obs = dplyr::n()) |>
			dplyr::filter(n_obs > 9) |>	dplyr::select(!n_obs) |> dplyr::ungroup()
	  
		test_set_mean <- test_set |> dplyr::group_by(x, y) |>
			dplyr::summarize(across(where(is.numeric), \(x) mean(x, na.rm = T)), n_obs = dplyr::n()) |>
			dplyr::filter(n_obs > 9) |> dplyr::select(!n_obs) |> dplyr::ungroup()


		if (model == "TPS") {
			out <- test_tps(test_set_mean)
		} else {
			if (test) {
		# Random forest with the_country (only the covariates). This serves as reference
				out <- test_rf(test_set_mean, test_set_mean)
			} else {
		# Random forest with other countries (only the covariates)
				out <- test_rf(training_set_mean, test_set_mean)
			}
		}
	} else {

		if (!is.na(sample_size)) {
			training_set <- training_set[sample(min(nrow(training_set), sample_size)), ]
			test_set <- test_set[sample(min(nrow(test_set), 2*sample_size)), ]
		}


		if (model == "TPS") {
			out <- test_tps(test_set)
		} else {
		## Random forest with the_country (only the covariates). This serves as reference
			if (test) {
				out <- test_rf(test_set, test_set)
		# Random forest with other countries (only the covariates)
			} else {
				out <- test_rf(training_set, test_set)
			}
		}
	}
	
	out$results <- data.frame(
		country = the_country,
		code = the_code,
		model = model,
		means = means,
		test = test,
		out$results
	)

	saveRDS(out, fname)
	fname
}


summarize <- function() {
	frf <- list.files("output/leave_one", "RF.*\\.rds", full.names=TRUE)
	x <- do.call(rbind, lapply(frf, readRDS)$results)
	saveRDS(x, "output/leave_one_RF.rds")

	ftps <- list.files("output/leave_one", "TPS.*\\.rds", full.names=TRUE)
	y <- do.call(rbind, lapply(ftps, readRDS)$results)
	saveRDS(y, "output/leave_one_TPS.rds")

	# compare TPS predictions (focal country data seen) with RF predictions (focal country data not seen)
	ftp <- list.files("output/leave_one", "TPS_all", full.names=TRUE)
	frf <- list.files("output/leave_one", "RF_all_test", full.names=TRUE)
	out1 <- data.frame(code=country_codes, means=FALSE)
	out1$cor <- sapply(country_codes, 
		function(code) {
			tp <- readRDS(grep(code, ftp, value=TRUE))
			rf <- readRDS(grep(code, frf, value=TRUE))
			cor(tp$prediction, rf$prediction, use="pairwise.complete.obs")
		}
	)
	# using mean values
	ftp <- list.files("oldout/leave_one", "TPS_means", full.names=TRUE)
	frf <- list.files("oldout/leave_one", "RF_means_test", full.names=TRUE)
	out2 <- data.frame(code=country_codes, means=TRUE)
	out2$cor <- sapply(country_codes, 
		function(code) {
			if (code == "TZA") return(NA)
			tp <- readRDS(grep(code, ftp, value=TRUE))
			rf <- readRDS(grep(code, frf, value=TRUE))
			cor(tp$prediction, rf$prediction, use="pairwise.complete.obs")
		}
	)
	out <- rbind(out1, out2)
	
	saveRDS(out, "output/leave_one_cor.rds")
}


countries <- c("Benin", "Burkina", "Cote_d_Ivoire", "Ethiopia", "Ghana", "Guinea_Bissau", "Malawi", "Mali", "Niger", "Nigeria", "Rwanda", "Senegal", "Tanzania", "Togo", "Uganda", "Zambia")
country_codes <- c("BEN", "BFA", "CIV", "ETH", "GHA", "GNB", "MWI", "MLI", "NER", "NGA", "RWA", "SEN", "TZA", "TGO", "UGA", "ZMB")

trts <- expand.grid(country=1:length(countries), model=c("RF", "TPS"), means=c(TRUE, FALSE), test=c(TRUE, FALSE))
trts <- trts[!((trts$model=="TPS") & (!trts$test)), ]


### sequential with sampling
seqfun <- function() {
	for (i in 1:nrow(trts)) { 
		leave_one_country_models(countries[trts$country[i]], country_codes[trts$country[i]], trts$model[i], trts$means[i], trts$test[i], sample_size=100)
	}
}


### parallel
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (i <= nrow(trts)) {
	leave_one_country_models(countries[trts$country[i]], country_codes[trts$country[i]], trts$model[i], trts$means[i], trts$test[i])
	print("OK")
} else if (i == nrow(trts)) {
	summarize()
} else {
	print("done (i > nrow(trts))")
}


# slurm options
#sbatch --array=1-97 -p bmh --time=600 --mem=16G --job-name=farms ~/farm/clusterR.sh scripts/04.4.RF_model_evaluation.R



# Evaluate model framework, leaving out 1 country and compare the predictions with interpolation
# ------------------------------------------------------------------------------

# the original function. Does not look right because test data is not used
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
	rsq <- round(cor(d$farm_area_ha, prediction)^2, 4) # Get the r2
	list(prediction=prediction, rsq_cv=NA, rsq=rsq)
}	

### fixed TPS function?
### if so, same should be applied to test_rf
test_tps_fixed <- function(train, test) {
# Fit a TPS model
	if (!("fields" %in% installed.packages()[,1])) install.packages("fields")
	# with X and Y only
	## cty_fit0 <- fields::Tps(cbind(train$x, train$y), train$farm_area_ha, lon.lat = TRUE)
	
	train <- as.matrix(train)
	Zvars <- c("cropland", "cattle", "pop", "cropland_per_capita", "sand", "slope", "temperature", "rainfall", "market", "maizeyield")
	tps_model <- fields::Tps(train[, c("x", "y")], train[, "farm_area_ha"], Z = train[,Zvars], lon.lat = TRUE)
	
	# predict the TPS on the coordinates of observed data
	test <- as.matrix(test)
	prediction <- predict(tps_model, test[, c("x", "y")], Z=test[,Zvars])
	rsq <- round(cor(test[, "farm_area_ha"], prediction)^2, 4) # Get the r2
	list(prediction=as.numeric(prediction), rsq_cv=NA, rsq=rsq)
}	


test_rf <- function(d) {
# Random forest with my_country (only the covariates). This serves as reference
	rf_model <- caret::train(
		farm_area_ha ~ .,
		data = d |> dplyr::select(!c(x, y)),
		method = "ranger",
		# preProcess = c("center", "scale", "spatialSign"),
		# trControl = ctrl,
		metric = "Rsquared"
	)
	print(rf_model)
	
	prediction <- predict(rf_model, d) |> as.numeric()
	cv <- rf_model$results |> as.data.frame() |> dplyr::select(Rsquared) |> dplyr::pull() |> mean() |> round(4)
	rsq <- round(cor(d$farm_area_ha, prediction)^2, 4)
	list(prediction=prediction, rsq_cv=cv, rsq=rsq)
}


# Using a training set (all other countries) and a test set (country of interest) to evaluate model performance
leave_one_country_models <- function(country, model, means, test){

	stopifnot(model %in% c("TPS", "RF"))

	set.seed(2024) # just for reproducibility!

	input_path <- "data"
	output_path <- "output/leave_one"
	dir.create(output_path, FALSE, TRUE)

	countries <- c("Benin", "Burkina", "Cote_d_Ivoire", "Ethiopia", "Guinea_Bissau", "Malawi", "Mali", "Niger", "Nigeria", "Senegal", "Tanzania", "Togo", "Uganda", "Zambia")
	country_codes <- c("BEN", "BFA", "CIV", "ETH", "GNB", "MWI", "MLI", "NER", "NGA", "SEN", "TZA", "TGO", "UGA", "ZMB")
	my_country <- countries[country]
	my_code <- country_codes[country]

	c("all", "means")
	fname <- file.path(output_path, paste0("loc_", my_code, "_", model, "_",  c("all", "means")[means+1], "_", c("train", "test")[test+1], ".Rds"))

	print(paste0("--------------- Model evaluation in ", my_country, " (point-based) -------------"))


	lsms_spatial <- readRDS(file.path(input_path, "lsms_trimmed_95th_africa.Rds"))
	lsms_spatial <- lsms_spatial |> dplyr::select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>  na.omit() 

    # caret control parms
	ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = FALSE)
  
  # subsetting df: training - test split (point-based)
	training_set <- lsms_spatial|>  dplyr::filter(country != my_country) |>  dplyr::select(!country) |>  na.omit()
	test_set <- lsms_spatial |> dplyr::filter(country == my_country) |>  dplyr::select(!country) |>  na.omit()

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
		# Random forest with my_country (only the covariates). This serves as reference
				out <- test_rf(test_set_mean)
			} else {
		# Random forest with other countries (only the covariates)
				out <- test_rf(training_set_mean)
			}
		}
	} else {
		if (model == "TPS") {
			out <- test_tps(test_set)
		} else {
		## Random forest with my_country (only the covariates). This serves as reference
			if (test) {
				out <- test_rf(test_set)
		# Random forest with other countries (only the covariates)
			} else {
				out <- test_rf(training_set)
			}
		}
	}
	
	out$result <- data.frame(
		country = country,
		model = model,
		means = means,
		test = test,
		rsq = out$rsq,
		rsq_cv = out$rsq_cv
	)
	out$rsq <- out$rsq_cv <- NULL
	
	saveRDS(out, fname)
	fname
}


trts <- expand.grid(country=1:14, model=c("RF", "TPS"), means=c(TRUE, FALSE), test=c(TRUE, FALSE))
trts <- trts[!((trts$model=="TPS") & (!trts$test)), ]

# parallel
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (i <= 84) {
	leave_one_country_models(trts$country[i], trts$model[i], trts$means[i], trts$test[i])
	print("OK")
} else {
	print("done (i > 84)")
}


# slurm options
#sbatch --array=1-84 -p bmh --time=600 --mem=32G --job-name=farms ~/farm/clusterR.sh scripts/04.4.RF_model_evaluation.R


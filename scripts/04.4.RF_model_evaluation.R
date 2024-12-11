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
	list(prediction=prediction, rsq=rsq)
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
	list(prediction=as.numeric(prediction), rsq=rsq)
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
leave_one_country_models <- function(i){

	set.seed(2024) # just for reproducibility!

	input_path <- "data"
	output_path <- "output"

	fourteen_countries <- c("Benin", "Burkina", "Cote_d_Ivoire", "Ethiopia", "Guinea_Bissau", "Malawi", "Mali", "Niger", "Nigeria", "Senegal", "Tanzania", "Togo", "Uganda", "Zambia")
	fourteen_country_codes <- c("BEN", "BFA", "CIV", "ETH", "GNB", "MWI", "MLI", "NER", "NGA", "SEN", "TZA", "TGO", "UGA", "ZMB")

	my_country <- fourteen_countries[i]

	lsms_spatial <- readRDS(file.path(input_path, "lsms_trimmed_95th_africa.Rds"))
	lsms_spatial <- lsms_spatial |> dplyr::select(x, y, country, farm_area_ha, cropland, cattle, pop, cropland_per_capita,
         sand, slope, temperature, rainfall, maizeyield, market) |>  na.omit() 

    # caret control parms
	ctrl <- caret::trainControl(method = "cv", number = 10, verboseIter = FALSE)
  
  # subsetting df: training - test split (point-based)
	training_set <- lsms_spatial|>  dplyr::filter(country != my_country) |>  dplyr::select(!country) |>  na.omit()
	test_set <- lsms_spatial |> dplyr::filter(country == my_country) |>  dplyr::select(!country) |>  na.omit()
  
  #training - test split (consolidated mean-based => exclude all points with less than 10 records)
	training_set_mean <- training_set |> dplyr::group_by(x, y) |>
		dplyr::summarize(across(where(is.numeric), \(x) mean(x, na.rm = T)), n_obs = dplyr::n()) |>
		dplyr::filter(n_obs > 9) |>	dplyr::select(!n_obs) |> dplyr::ungroup()
  
	test_set_mean <- test_set |> dplyr::group_by(x, y) |>
		dplyr::summarize(across(where(is.numeric), \(x) mean(x, na.rm = T)), n_obs = dplyr::n()) |>
		dplyr::filter(n_obs > 9) |> dplyr::select(!n_obs) |> dplyr::ungroup()
	
	print(paste0("--------------- Model evaluation in ", my_country, " (point-based) -------------"))

## TPS
	pred_tps <- test_tps(test_set)
	pt_rsq_tps <- pred_tps$rsq
	print(paste0("TPS_calculated_pt_rsq_with_covariate = ", pt_rsq_tps))

## Random forest with my_country (only the covariates). This serves as reference
	rf_my <- test_rf(test_set)
	pt_rsq_reference <- rf_my$rsq
	pt_rsq_ref_cv = rf_my$rsq_cv
	print(paste0("pt_rsq_reference = ", pt_rsq_reference))
	print(paste0("pt_rsq_reference_cv = ", pt_rsq_ref_cv))

# Random forest with other countries (only the covariates)
	rf_other <- test_rf(training_set)
	pt_rsq_other <- rf_other$rsq
	pt_rsq_other_cv <- rf_other$rsq_cv
	print(paste0("pt_rsq_other = ", pt_rsq_other))
	print(paste0("pt_rsq_other_cv = ", pt_rsq_other_cv))
	
	# calculate Rsquare showing agreement between predictions from in-country TPS and RF_model from other countries
	thecor <- cor(pred_tps$prediction, rf_other$prediction, use = "complete.obs")
	pt_cor_other_vs_tps <- round(thecor, 4)	  # Get the cor coef
	pt_rsq_other_vs_tps <- round(thecor^2, 4) # Get the r2
	print(paste0("pt_rsq_TPS_vs_other_countries = ", pt_rsq_other_vs_tps))
	
	print(paste0("--------------- Model evaluation in ", my_country, " (consolidated means) -------------"))
	#-------- Thin plate spline (only the coordinates)-----------

	pred_tps_mean <- test_tps(test_set_mean)
	mn_rsq_tps <- pred_tps_mean$rsq
	print(paste0("TPS_calculated_pt_rsq_with_covariate = ", pt_rsq_tps))
	print(paste0("TPS_calculated_mn_rsq_with_covariate = ", mn_rsq_tps))
	
# Random forest with my_country (only the covariates). This serves as reference
	rf_my_mean <- test_rf(test_set_mean)
	mn_rsq_reference <- 
	mn_rsq_ref_cv <- 
	print(paste0("mn_rsq_reference = ", round(mn_rsq_reference, 2)))
	print(paste0("mn_rsq_reference_cv = ", mn_rsq_ref_cv))

# Random forest with other countries (only the covariates)
	rf_country_mean <- test_rf(training_set_mean)
	mn_rsq_other <- 
	print(paste0("mn_rsq_other_countries = ", round(mn_rsq_other, 2)))
	mn_rsq_other_cv <- 
	print(paste0("mn_rsq_other_cv = ", mn_rsq_other_cv))

	# calculate Rsquare showing agreement between predictions from in-country TPS and RF_model from other countries
	thecor <- cor(pred_tps_mean$predictions, rf_country_mean$predictions, use = "complete.obs")
	mn_cor_other_vs_tps <- round(thecor, 4)	 # Get the cor coef
	mn_rsq_other_vs_tps <- round(thecor^2, 4)	 # Get the r2
	print(paste0("mn_rsq_TPS_vs_other_countries = ", mn_rsq_other_vs_tps))
		
	# compile results
	all_rsq <- data.frame(
		country = my_country, 
		# point-basedmodel
		pt_rsq_tps = pt_rsq_tps, 
		pt_rsq_reference = pt_rsq_reference,		# RF trained and tested on my_country, calculated R2_oob
		pt_rsq_ref_cv = pt_rsq_ref_cv,				# RF trained and tested on my_country, cross-validated R2
		pt_rsq_other = pt_rsq_other,				# RF trained on other countries and tested on my_country, R2_oob
		pt_rsq_other_cv = pt_rsq_other_cv,			# RF trained on other countries and tested on my_country, R2_cv
		pt_rsq_other_vs_tps	= pt_rsq_other_vs_tps,	# R2 for predictions from Tps vs RF trained on other countries, R2_oob
		pt_cor_other_vs_tps = pt_cor_other_vs_tps,	# correlation coef for predictions from RF vs Tps, cor coef
		
		# consolidated means-based model
		mn_rsq_tps = mn_rsq_tps, 
		mn_rsq_reference = mn_rsq_reference,		# RF trained and tested on my_country, calculated R2_oob
		mn_rsq_ref_cv = mn_rsq_ref_cv,				# RF trained and tested on my_country, cross-validated R2
		mn_rsq_other = mn_rsq_other,				# RF trained on other countries and tested on my_country, R2_oob
		mn_rsq_other_cv = mn_rsq_other_cv,			# RF trained on other countries and tested on my_country, R2_cv
		mn_rsq_other_vs_tps	= mn_rsq_other_vs_tps, 	# R2 for predictions from Tps vs RF trained on other countries, R2_oob
		mn_cor_other_vs_tps = mn_cor_other_vs_tps	# correlation coef for predictions from RF vs Tps, cor coef
	)

	write.csv(mult_rsq, file = paste0("output/model_eval_", my_country, ".csv"), row.names = FALSE)

## can be added again. Need to make the model functions return these. 	
#	results <- list(mult_rsq, cty_fit0, cty_fit1, rf_country_ref, rf_country_model)
#	save(results, file = paste0("../output/results_", my_country, ".rdata"))

	return(mult_rsq)
}


# sequential 
# for (i in 1:14) leave_one_country_models(i)

## note 
## this can be further parallelized (14 countries * 4 models = 48 runs)

# parallel
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (i <= 14) {
	leave_one_country_models(i)
} else {
	print("done (i > 14")
}

# slurm options
#sbatch --array=1-14 -p bmh --time=1200 --mem=32G --job-name=farms ~/farm/clusterR.sh scripts/04.4.RF_model_evaluation.R


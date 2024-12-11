# Random forest optimization
# ------------------------------------------------------------------------------

rf_optim <- function(x) {

	# input and ouptut foders and files
	input_path <- "data"
	output_path <- "output"


	treatment <- paste0(x[,1:3], collapse="-")
	outfile <- paste0("RFoptim_", treatment, "_mbucket-", x$mbuck, ".Rds") 
	print(paste("-------- run =", outfile, "----------"))
	dir.create(output_path, FALSE, FALSE) 
	outfile <- file.path(output_path, outfile)
	# if (file.exists(output_file)) return("file existed")

	lsms_spatial <- readRDS(file.path(input_path, "lsms_trimmed_95th_africa.Rds"))

	# keep only variables needed in the model
	lsms_spatial <- lsms_spatial |>  dplyr::select(farm_area_ha, cropland, cattle, pop, cropland_per_capita,
			 sand, slope, temperature, rainfall, maizeyield, market) |> na.omit() 

	# ------------------------------------------------------------------------------
	# set parms
	train_control <- caret::trainControl(method = "cv", number = 10, savePredictions = "all", seeds = 2024)


	rf_full_model <- caret::train(
		farm_area_ha ~ .,
		data = lsms_spatial,
		method = "ranger",
		preProcess = c("center", "scale", "spatialSign"),
 #	 trControl = train_control,
		keep.inbag = TRUE,
		tuneGrid = x[, 1:3],
		importance	= "permutation",
		metric = "RMSE",
		min.bucket = x$mbuck,
		num.trees = 500
	)
	
	saveRDS(rf_full_model, file = outfile)
	rf_full_model$results
}


# input combinations
tune_grid <- expand.grid(
	mtry = 3:10,										 
	splitrule = c("variance", "extratrees"),											 
	min.node.size = c(5, 40:60, 100, 200),
	mbuck = c(1, 5, seq(10, 100, 10), 200)
)
# for the output filename
tune_grid$splitrule <- as.character(tune_grid$splitrule)


# sequential
# for (i in 1:nrow(tune_grid)) (rf_optim(tune_grid[i,]))

# parallel
i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (i <= nrow(tune_grid)) {
	r <- rf_optim(tune_grid[i,])
	print(r)
} else {
	print("done (i > nrow(tune_grid)")
}

# slurm options
#sbatch --array=1-4992 -p bmh --time=600 --mem=16G --job-name=farms ~/farm/clusterR.sh scripts/05.1.RF_model_optim.R

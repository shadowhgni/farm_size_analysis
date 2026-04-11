# ==============================================================================
# Script: 00.3_synthetic_data.R
# Project: Farm Size Prediction Across Sub-Saharan Africa
# Purpose: Generate ALL synthetic files needed for full CI pipeline testing
#
#   TRUE STUBS (raw data that cannot be produced in CI):
#     stacked_rasters_africa.tif   requires downloaded satellite layers
#     mask_forest/dryland_ssa.tif  requires downloaded land-cover data
#     rain_YYYY/avg/cv.tif         requires CHIRPS download (01.1/01.2 skipped)
#     AEZ raster                   requires IFPRI dataset
#     GADM per-country .rds        requires geodata download
#     all_cropland_mask.tif        requires satellite land-use data
#     SPAM crop rasters            requires SPAM download
#     lsms_and_zambia.csv/.rds     requires raw survey data (02.1 skipped)
#     Country-year raw CSVs        requires raw survey data (02.1 skipped)
#     Sarah Lowder xlsx files      requires downloaded supplementary tables
#     rf/qrf prediction rasters    Python scripts not tested in CI
#     gadm_1_cross_validation.csv  04.4 skipped (XNomial loop > 600s)
#     RF_optim_summarized_table    05.2 skipped (SLURM array job)
#     summarized_farm_area_*sarah  08.3 Sarah section (census data required)
#     CHINA croplands rds          external dataset
#     figure stubs                 F02/F03 inputs from processed rasters
#     lsms_oob.rds                 produced by Python 06.1 (not tested)
#     leave_one per-country files  04.5 inputs from SLURM array outputs
#
#   NOT STUBBED (pipeline scripts produce these in CI):
#     03.1 -> lsms_untrimmed/trimmed rds, lsms_spatial csv/rds
#     04.2 -> comparison_ML_models, country_variable_importance
#     04.3 -> country_auto/pairwise evaluation
#     04.5 -> leave_one_RF/TPS/cor.rds, cross_validation_graphs.rds
#     06.1.py -> etr_variable_importance.csv
#     08.2 -> nb_farms_per_grid_cell.tif
#     08.3 -> fsize_distribution_resample_long.rds, farm_size_distribution_parms.tif
#             back_transf_trunc_adj_*.tif, gini_raster.tif
#     10.1 -> cropland_stats_per_aez.rds
#     S06  -> Suppl.Fig06_divergence_table.rds
#
# Authors: Deo, Joao, Robert, Fred
# Code documentation: Claude (Anthropic) - April 2026
# ==============================================================================

message("=== FULL Synthetic Data Generation for CI ===\n")
set.seed(42)

# Package loading
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra",
    repos = c("https://packagemanager.posit.co/cran/__linux__/jammy/latest",
              "https://cloud.r-project.org"),
    dependencies = TRUE, quiet = FALSE)
}
terra_ok <- tryCatch({ library(terra); TRUE }, error = function(e) {
  install.packages("terra", type = "source",
    repos = "https://cloud.r-project.org", dependencies = TRUE, quiet = FALSE)
  tryCatch({ library(terra); TRUE }, error = function(e2) stop("Cannot load terra: ", conditionMessage(e2)))
})
suppressPackageStartupMessages(library(dplyr))
message("terra: OK  dplyr: OK")

# Paths
processed_path <- "../data/processed"
raw_spatial    <- "../data/raw/spatial"
output_path    <- "../output"

for (d in c(
  processed_path, raw_spatial,
  file.path(raw_spatial, "gadm"),
  file.path(raw_spatial, "landuse/landuse"),
  file.path(raw_spatial, "rainfall", "rainfall_yearly"),
  file.path(raw_spatial, "AEZ_SSA_IFPRI"),
  file.path(output_path, "other_illustr/graphs"),
  file.path(output_path, "other_illustr/maps"),
  file.path(output_path, "other_illustr/tables"),
  file.path(output_path, "plot_data"),
  file.path(output_path, "reports"),
  file.path(output_path, "leave_one")
)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Config
ssa_ext  <- terra::ext(-18, 52, -35, 15)
res      <- 0.5
res_pred <- 5.0

sixteen_countries     <- c("Benin","Burkina","Cote_d_Ivoire","Ethiopia","Ghana",
                            "Guinea_Bissau","Malawi","Mali","Niger","Nigeria",
                            "Rwanda","Senegal","Tanzania","Togo","Uganda","Zambia")
sixteen_country_codes <- c("BEN","BFA","CIV","ETH","GHA","GNB","MWI","MLI",
                            "NER","NGA","RWA","SEN","TZA","TGO","UGA","ZMB")
country_lon <- c( 2, -1.5, -5, 38, -1,-15, 34, -4,  8,  8, 30,-14, 35, 1, 32, 28)
country_lat <- c( 9, 12,    7,  9,  8, 12,-13, 17, 17,  9, -2, 14, -6, 8,  1,-15)
n_per_country <- 500L

make_rast <- function(name, mn, sd, positive = TRUE, clamp = NULL, r_res = res) {
  r  <- terra::rast(ssa_ext, res = r_res, crs = "EPSG:4326")
  nc <- terra::ncell(r)
  xy <- terra::xyFromCell(r, seq_len(nc))
  z_lat <- (xy[,2] - mean(xy[,2])) / sd(xy[,2])
  z_lon <- (xy[,1] - mean(xy[,1])) / sd(xy[,1])
  v <- rnorm(nc, mn, sd) + z_lat * sd * 0.3 + z_lon * sd * 0.1
  if (positive) v <- pmax(v, 0)
  if (!is.null(clamp)) v <- pmin(pmax(v, clamp[1]), clamp[2])
  terra::values(r) <- v; names(r) <- name; r
}

# ==============================================================================
# 1. RASTER STUBS
# ==============================================================================
message("1. Creating synthetic rasters...")

cropland   <- make_rast("cropland",    500, 300)
cattle     <- make_rast("cattle",       50,  40)
pop        <- make_rast("pop",         100, 150)
sand       <- make_rast("sand",         40,  20, clamp = c(0,100))
elevation  <- make_rast("elevation",   800, 500)
slope      <- make_rast("slope",      0.05, 0.03)
temperature<- make_rast("temperature",  25,   5, positive = FALSE, clamp = c(10,35))
rainfall   <- make_rast("rainfall",   1000, 500)
market     <- make_rast("market",      120,  80)
maizeyield <- make_rast("maizeyield", 5000,2000)
cropland_per_capita <- cropland / pop
cropland_per_capita[is.infinite(cropland_per_capita)] <- NA
names(cropland_per_capita) <- "cropland_per_capita"

all_predictors <- c(cropland, cattle, pop, cropland_per_capita,
                    sand, elevation, slope, temperature, rainfall, market, maizeyield)
names(all_predictors) <- c("cropland","cattle","pop","cropland_per_capita",
                           "sand","elevation","slope","temperature","rainfall","market","maizeyield")
terra::writeRaster(all_predictors,
  file.path(processed_path, "all_predictors.tif"), overwrite = TRUE)

stacked <- c(cropland, cattle, pop, cropland_per_capita,
             sand, slope, temperature, rainfall, maizeyield, market)
names(stacked) <- c("cropland","cattle","pop","cropland_per_capita",
                    "sand","slope","temperature","rainfall","maizeyield","market")
terra::writeRaster(stacked,
  file.path(processed_path, "stacked_rasters_africa.tif"), overwrite = TRUE)
saveRDS(stacked, file.path(processed_path, "stacked_africa.Rds"))

# Masks (true stubs - require real land-cover data)
terra::writeRaster(make_rast("forest",  0.3, 0.3, clamp = c(0,1), r_res = res_pred),
  file.path(processed_path, "mask_forest_ssa.tif"),   overwrite = TRUE)
terra::writeRaster(make_rast("dryland", 0.4, 0.3, clamp = c(0,1), r_res = res_pred),
  file.path(processed_path, "mask_drylands_ssa.tif"), overwrite = TRUE)

# Cropland mask (true stub)
band_nms <- c("SPAM 2010","SPAM 2017","SPAM 2020","ESA 2020","GLAD 2019","GEOSURVEY 2015")
all_cropland <- terra::rast(lapply(band_nms,
  function(nm) make_rast(nm, 0.5, 0.3, clamp = c(0,1), r_res = res)))
names(all_cropland) <- band_nms
terra::writeRaster(all_cropland,
  file.path(raw_spatial, "landuse/landuse/all_cropland_mask.tif"), overwrite = TRUE)

# Python prediction outputs (true stubs - Python not tested)
rf_pred <- make_rast("rf_mean", 2, 0.8, r_res = res_pred)
terra::writeRaster(rf_pred, file.path(processed_path, "rf_model_predictions_SSA.tif"), overwrite = TRUE)
terra::writeRaster(rf_pred, file.path(processed_path, "rf_predictions_africa.tif"),    overwrite = TRUE)

# QRF 100-quantile stack (monotone, strictly positive)
qrf_layers <- lapply(1:100, function(i) {
  r <- rf_pred * (0.5 + i/100 * 1.5)
  names(r) <- paste0("qrf_q", sprintf("%03d", i)); r
})
qrf_pred <- terra::rast(qrf_layers)
terra::writeRaster(qrf_pred,
  file.path(processed_path, "qrf_100quantiles_predictions_africa.tif"), overwrite = TRUE)

# Python cropland-source prediction variants (06.4 / F02)
for (nm in c("Python_SPAM2010_rf_predictions_africa","Python_SPAM2017_rf_predictions_africa",
             "Python_SPAM2020_rf_predictions_africa","Python_Geosurvey2015_rf_predictions_africa",
             "Python_potapov_rf_predictions_africa","Python_ESA2021_rf_predictions_africa"))
  terra::writeRaster(rf_pred, file.path(processed_path, paste0(nm,".tif")), overwrite = TRUE)

message("   Rasters done.")

# CHIRPS yearly rainfall stubs (01.3/01.4)
rain_yearly_dir <- file.path(raw_spatial, "rainfall", "rainfall_yearly")
for (yr in c(2010,2015,2018,2020,2022))
  terra::writeRaster(make_rast(paste0("rain_",yr), 1000, 300),
    file.path(rain_yearly_dir, paste0("chirps-yearly-rainfall-",yr,".tif")), overwrite = TRUE)
terra::writeRaster(make_rast("rain_avg", 1000, 200),
  file.path(rain_yearly_dir, "#_long_term_rainfall_avg.tif"), overwrite = TRUE)
terra::writeRaster(make_rast("rain_cv", 0.15, 0.05, clamp = c(0,1)),
  file.path(rain_yearly_dir, "#_long_term_rainfall_cv.tif"), overwrite = TRUE)
message("   Yearly rainfall stubs done.")

# AEZ raster (IFPRI dataset - true stub)
{
  aez_dir <- file.path(raw_spatial, "AEZ_SSA_IFPRI")
  aez_r   <- terra::rast(ssa_ext, res = 1.0, crs = "EPSG:4326")
  xy_aez  <- terra::xyFromCell(aez_r, seq_len(terra::ncell(aez_r)))
  lat <- xy_aez[,2]; lon <- xy_aez[,1]
  cls <- ifelse(lat >  8 & lat <= 15 & lon < 20, 3L,
         ifelse(lat >  3 & lat <=  8,             2L,
         ifelse(lat >= -5 & lat <=  3,            0L,
         ifelse(lat < -5  & lat >= -15 & lon > 25, 4L,
         ifelse(lat < -15,                         5L, 1L)))))
  cls[sample(length(cls), round(0.10*length(cls)))] <- sample(0:5, round(0.10*length(cls)), replace=TRUE)
  terra::values(aez_r) <- as.integer(cls); names(aez_r) <- "aez_class"
  terra::writeRaster(aez_r, file.path(aez_dir, "AEZ5_CLAS--SSA.tif"),
    datatype = "INT1U", overwrite = TRUE)
  message("   AEZ stub written.")
}

# SPAM crop stubs (10.1/T02)
all_crops_spam <- c("MAIZ","SOYB","RICE","WHEA","SORG","PMIL","SMIL","CASS",
                    "GROU","BEAN","CHIC","COWP","PIGE","LENT","OPUL",
                    "YAMS","POTA","SWPO","SUGC","SUGB","COTT","ACOF","RCOF",
                    "OFIB","COCO","TEAS","TOBA","BARL","OCER")
for (spam_yr in c("spam2010","spam2017")) {
  spam_dir <- file.path(raw_spatial, "spam", spam_yr)
  dir.create(spam_dir, recursive = TRUE, showWarnings = FALSE)
  for (vcode in c("H","P","V"))
    for (crop in all_crops_spam)
      terra::writeRaster(make_rast(paste0(crop,vcode), 500, 300),
        file.path(spam_dir, paste0("spam",spam_yr,"V2r0_SSA_",vcode,"_",crop,"_A.tif")),
        overwrite = TRUE)
}
message("   SPAM stubs written.")

# ==============================================================================
# 2. LSMS SURVEY DATA (02.1 skipped - true stub)
# ==============================================================================
message("2. Creating synthetic LSMS survey data...")

make_country_farms <- function(cty, lon_c, lat_c, n) {
  x   <- round(rnorm(n, lon_c, 1.5) / 0.5) * 0.5
  y   <- round(rnorm(n, lat_c, 1.5) / 0.5) * 0.5
  lat_signal <- (lat_c - mean(country_lat)) / (sd(country_lat) + 1e-6) * 0.25
  farm_area_ha <- pmin(rlnorm(n, 0.3 + lat_signal + runif(n,-0.1,0.1), 0.6), 50)
  data.frame(x=x, y=y, country=cty,
    year     = sample(c(2010,2012,2014,2016,2018,2020), n, replace=TRUE),
    farm_id  = paste0(cty,"_",sprintf("%05d",seq_len(n))),
    hh_size  = rpois(n,5)+1,
    farm_area_ha          = round(farm_area_ha, 4),
    reported_area_ha      = round(farm_area_ha*runif(n,0.8,1.3), 4),
    measured_plot_area_ha = ifelse(runif(n)>0.3, round(farm_area_ha,4), NA_real_),
    ea_id    = paste0(cty,"_EA_",sample(1:50,n,replace=TRUE)),
    field_id = paste0(cty,"_",sprintf("%05d",seq_len(n)),"_F1"),
    plot_id  = paste0(cty,"_",sprintf("%05d",seq_len(n)),"_F1_P1"),
    stringsAsFactors = FALSE)
}
lsms_list <- mapply(make_country_farms,
  sixteen_countries, country_lon, country_lat, n_per_country, SIMPLIFY = FALSE)
lsms_raw  <- do.call(rbind, lsms_list)
lsms_raw  <- lsms_raw[lsms_raw$x >= -18 & lsms_raw$x <= 52 &
                      lsms_raw$y >= -35 & lsms_raw$y <= 15, ]
lsms_raw$farm_area_ha[lsms_raw$farm_area_ha <= 0] <- 0.01

write.csv(lsms_raw, file.path(processed_path, "lsms_and_zambia.csv"), row.names = FALSE)
saveRDS(list(
  all_lsms_raw_data = lsms_raw,
  lsms_farm_size    = lsms_raw[,c("x","y","country","year","farm_id","hh_size","farm_area_ha")]
), file.path(processed_path, "lsms_and_zambia.rds"))
message("   LSMS CSV + RDS done  (", nrow(lsms_raw), " farms).")

# ==============================================================================
# 3. PREDICTOR EXTRACTION (feeds 03.1 which produces the analysis datasets)
# ==============================================================================
message("3. Extracting predictors at farm locations...")

lons <- seq(-18+res/2, 52-res/2, by=res)
lats <- seq(-35+res/2, 15-res/2, by=res)
ix   <- pmax(1L, pmin(length(lons), round((lsms_raw$x-lons[1])/res)+1L))
iy   <- pmax(1L, pmin(length(lats), round((lsms_raw$y-lats[1])/res)+1L))
stacked_df <- as.data.frame(stacked)
pred_cols  <- names(stacked_df)
extracted  <- stacked_df[(iy-1L)*length(lons)+ix, , drop=FALSE]; rownames(extracted) <- NULL

lsms_spatial <- cbind(lsms_raw, extracted)
lsms_spatial$gadm_0 <- sixteen_country_codes[match(lsms_spatial$country, sixteen_countries)]
lsms_spatial$gadm_1 <- paste0(lsms_spatial$country, "_Region1")
lsms_spatial$gadm_2 <- paste0(lsms_spatial$country, "_District1")
lsms_spatial$gadm_3 <- NA_character_
lsms_spatial$gadm_4 <- NA_character_
key_cols     <- c("x","y","farm_area_ha", pred_cols)
lsms_spatial <- lsms_spatial[complete.cases(lsms_spatial[, key_cols]), ]

trim95 <- do.call(rbind, lapply(split(lsms_spatial, lsms_spatial$country), function(d)
  d[d$farm_area_ha <= quantile(d$farm_area_ha, 0.95), ]))
ml_cols <- c("x","y","farm_area_ha",pred_cols,"country","gadm_0","gadm_1","gadm_2","gadm_3","gadm_4","year","farm_id","hh_size")
lsms_ml <- trim95[, intersect(ml_cols, names(trim95))]
lsms_ml <- lsms_ml[complete.cases(lsms_ml[, key_cols]), ]
message("   Predictor extraction done  (", nrow(lsms_ml), " farms in 95th trim).")

# ==============================================================================
# 4. GADM BOUNDARIES (requires geodata - true stub)
# ==============================================================================
message("4. Creating synthetic GADM boundaries...")

make_gadm_vect <- function(cty, code, lon_c, lat_c) {
  polys <- lapply(1:4, function(i) {
    cx <- lon_c + (i-1)%%2*1.2-0.6; cy <- lat_c + (i-1)%/%2*1.2-0.6
    terra::vect(matrix(c(cx-.6,cy-.6,cx+.6,cy-.6,cx+.6,cy+.6,cx-.6,cy+.6,cx-.6,cy-.6),
                       ncol=2,byrow=TRUE), type="polygons", crs="EPSG:4326")
  })
  v <- Reduce(rbind, polys)
  terra::values(v) <- data.frame(GID_0=code, NAME_0=cty,
    GID_1=paste0(code,".",1:4), NAME_1=paste0(cty,"_Reg",1:4),
    GID_2=paste0(code,".",1:4,".1"), NAME_2=paste0(cty,"_Dist",1:4),
    GID_3=paste0(code,".",1:4,".1.1"), NAME_3=paste0(cty,"_Sub",1:4),
    GID_4=paste0(code,".",1:4,".1.1.1"), NAME_4=paste0(cty,"_Sub4_",1:4),
    stringsAsFactors=FALSE)
  v
}
gadm_list <- mapply(make_gadm_vect, sixteen_countries, sixteen_country_codes,
                    country_lon, country_lat, SIMPLIFY=FALSE)

for (i in seq_along(sixteen_countries)) {
  fname <- paste0("gadm41_", sixteen_country_codes[i], "_2_pk.rds")
  for (d in c(file.path(raw_spatial,"gadm",sixteen_countries[i]),
              file.path(raw_spatial,"gadm",sixteen_countries[i],"gadm"))) {
    dir.create(d, recursive=TRUE, showWarnings=FALSE)
    saveRDS(gadm_list[[i]], file.path(d, fname))
  }
}
message("   GADM boundaries done.")

# ==============================================================================
# 5. OUTPUT TABLE STUBS (skipped scripts only: 04.4, 05.2)
# ==============================================================================
message("5. Creating output table stubs (skipped scripts only)...")

# 04.4 skipped (XNomial MC loop > 600s)
gadm_rsq <- data.frame(
  country=rep(sixteen_countries,each=4), gadm_1=paste0(rep(sixteen_countries,each=4),"_Reg",1:4),
  rf_cv_rsq=round(runif(64,.2,.6),2), rf_cv_rsq_sd=round(runif(64,.02,.1),3),
  gadm_test_rf_rsq=round(runif(64,.15,.55),2), n_obs=sample(50:200,64,replace=TRUE))
write.csv(gadm_rsq,
  file.path(output_path,"other_illustr/tables/gadm_1__point_based_cross_validation.csv"),
  row.names=FALSE)

# 05.2 skipped (SLURM array job)
rf_optim <- data.frame(
  filename=paste0("rf-",1:20,"-1.rds"),
  Rsquared=round(runif(20,.3,.7),3), RMSE=round(runif(20,.5,1.5),3),
  MAE=round(runif(20,.3,1.0),3), mtry=sample(2:6,20,replace=TRUE),
  min.node.size=sample(3:10,20,replace=TRUE),
  splitrule=sample(c("variance","extratrees"),20,replace=TRUE), mbucket=1:20)
saveRDS(rf_optim, file.path(output_path,"other_illustr/tables/RF_optim_summarized_table.rds"))
write.csv(rf_optim, file.path(output_path,"other_illustr/tables/RF_optim_summarized_table.csv"), row.names=FALSE)

# S03 plot stub
saveRDS(list(
  pred_cpland_df  = data.frame(x=lsms_ml$x[1:50], y=lsms_ml$y[1:50],
                               pred_farm_area_ha=lsms_ml$farm_area_ha[1:50]),
  ssa_cropland    = data.frame(source=c("spam_2017","spam_2010","esa_2021","geosurvey_2015"),
                               total=round(runif(4,1e8,5e8))),
  nb_farms_summarized = data.frame(source=c("spam_2017","spam_2010","esa_2021","geosurvey_2015"),
                                   nb_farms=round(runif(4,1e7,5e7)), nb_rounded=round(runif(4,10,50),0))
), file.path(output_path,"plot_data","plot_suppl_01_effect_of_source_of_cropland_masks.rds"))

message("   Output stubs done.")

# ==============================================================================
# 6. SARAH LOWDER XLSX STUBS (census data - true stub)
# ==============================================================================
message("6. Creating Sarah Lowder xlsx stubs...")
sarah_dir <- file.path("../data/raw","web_scrapped/sarah_lowder")
dir.create(sarah_dir, recursive=TRUE, showWarnings=FALSE)

lowder_countries <- c("Benin","Burkina Faso","Côte d'Ivoire","Ethiopia","Ghana",
  "Guinea-Bissau","Malawi","Mali","Niger","Nigeria","Rwanda","Senegal","Tanzania",
  "Togo","Uganda","Zambia","Kenya","Mozambique","Madagascar","Zimbabwe","Cameroon","Sudan")
n_cty       <- length(lowder_countries)
total_farms <- round(c(3,2.5,2,13,2.3,.2,1.8,2.1,2.4,14,2,.7,5,.8,3.8,1.3,6,3.5,3,1.2,2.8,5.5)*1e6)
census_years<- c("2015/16","2018/19","2014","2013/14","2015/16","2015","2018/19","2016/17",
                 "2012","2015","2015","2013/14","2017/18","2015/16","2019/20","2020",
                 "2018/19","2014/15","2010/11","2012","2015/16","2014/15")

mk_xlsx <- function(df, path) {
  if (requireNamespace("writexl",   quietly=TRUE)) writexl::write_xlsx(df, path)
  else if (requireNamespace("openxlsx", quietly=TRUE)) {
    wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb,"Sheet1")
    openxlsx::writeData(wb,"Sheet1",df); openxlsx::saveWorkbook(wb,path,overwrite=TRUE)
  } else stop("Need writexl or openxlsx")
}

# mmc3
mk_xlsx(data.frame(country=lowder_countries, census_year=census_years, nb_farms=total_farms,
  source="Agricultural Census", gadm_1=lowder_countries, income_group="Low income",
  stringsAsFactors=FALSE),
  file.path(sarah_dir,"1-s2.0-S0305750X2100067X-mmc3.xlsx"))

# mmc5
sz <- c("fsize0_1ha","fsize1_2ha","fsize2_5ha","fsize5_10ha","fsize10_20ha","fsize20_50ha",
        "fsize50_100ha","fsize100_200ha","fsize200_500ha","fsize500_1000ha","fsize1000ha_above")
pnb <- c(.45,.22,.16,.08,.04,.03,.01,.004,.002,.001,.001)
pha <- c(.10,.12,.18,.15,.12,.13,.08,.050,.040,.020,.010)
make_row <- function(cty,yr,tf,rt) {
  if (rt=="F") { p <- pnb+runif(11,-.03,.03); p <- pmax(p,.001)/sum(pmax(p,.001)); vals <- round(tf*p) }
  else { p <- pha+runif(11,-.02,.02); p <- pmax(p,.001)/sum(pmax(p,.001)); vals <- round(tf*runif(1,.9,1.4)*p) }
  row <- as.data.frame(t(vals)); names(row) <- sz
  cbind(data.frame(NAME_0=cty,year=yr,nb_farms_or_area=rt,
    total=if(rt=="F")tf else sum(vals),stringsAsFactors=FALSE),row,
    data.frame(source_code="AC",income_group="Low income",stringsAsFactors=FALSE))
}
mmc5_rows <- vector("list", n_cty*2)
for (i in seq_len(n_cty)) {
  yr <- as.integer(substr(census_years[i], nchar(census_years[i])-3, nchar(census_years[i])))
  mmc5_rows[[2*i-1]] <- make_row(lowder_countries[i],yr,total_farms[i],"F")
  mmc5_rows[[2*i]]   <- make_row(lowder_countries[i],yr,total_farms[i],"A")
}
mk_xlsx(do.call(rbind,mmc5_rows), file.path(sarah_dir,"1-s2.0-S0305750X2100067X-mmc5.xlsx"))

# mmc7
mmc7 <- do.call(rbind, lapply(seq_len(n_cty), function(i) {
  b <- runif(1,1.2,3.5); tr <- runif(1,-.05,-.01)
  data.frame(country=lowder_countries[i], census_year=c(1990,2000,2010,2020),
    avg_farm_size_ha=round(pmax(.3,b+tr*(c(1990,2000,2010,2020)-1990)/10),2),
    nb_farms_total=round(total_farms[i]*c(.6,.75,.9,1)),stringsAsFactors=FALSE)
}))
mk_xlsx(mmc7, file.path(sarah_dir,"1-s2.0-S0305750X2100067X-mmc7.xlsx"))
message("   Sarah Lowder xlsx stubs done.")

# ==============================================================================
# 7. FIGURE STUBS
# ==============================================================================
message("7. Creating figure stubs...")

# fig2c: farm_size grouped by avg_size class — need enough points per class for ECDF
# PRODUCTION: theor_farms_application joined with rf_model_predictions
set.seed(42)
n_each <- 300L
fig2c_small  <- data.frame(farm_size=pmax(.01,rlnorm(n_each,-0.3,0.5)), avg_size=runif(n_each,0.1,0.49), country=sample(sixteen_countries,n_each,TRUE))
fig2c_medium <- data.frame(farm_size=pmax(.01,rlnorm(n_each, 0.3,0.6)), avg_size=runif(n_each,1.0,1.99), country=sample(sixteen_countries,n_each,TRUE))
fig2c_large  <- data.frame(farm_size=pmax(.01,rlnorm(n_each, 1.5,0.8)), avg_size=runif(n_each,5.1,10.0), country=sample(sixteen_countries,n_each,TRUE))
saveRDS(rbind(fig2c_small,fig2c_medium,fig2c_large), "fig2c.rds")

qrf_q010_r <- make_rast("qrf_q010", 1.5, 0.8, r_res=res_pred)
qrf_q090_r <- make_rast("qrf_q090", 4.0, 1.5, r_res=res_pred)
terra::writeRaster(qrf_q010_r, "fig.2a_quantile_10_fsizes.tif",    overwrite=TRUE)
terra::writeRaster(qrf_q090_r, "fig.2b_quantile_90_fsizes.tif",    overwrite=TRUE)
terra::writeRaster(qrf_q010_r, "../fig.2a_quantile_10_fsizes.tif", overwrite=TRUE)
terra::writeRaster(qrf_q090_r, "../fig.2b_quantile_90_fsizes.tif", overwrite=TRUE)

saveRDS(list(
  predicted_avg_vs_gini=data.frame(avg=pmax(.1,rnorm(500,2,1.5)), gini=runif(500,.2,.7)),
  observed_avg_vs_gini=data.frame(mean=pmax(.1,rnorm(200,2,1.5)), gini=runif(200,.2,.7))
), "fig.2d_mean_fsize_gini_coefs.rds")

fig1a_spam    <- make_rast("spam_2017",         50000, 20000, r_res=res_pred)
fig1a_predfarm<- make_rast("pred_farm_area_ha",   2.0,   0.8, r_res=res_pred)
terra::writeRaster(c(fig1a_spam,fig1a_predfarm), "../fig.1a_nb_of_farm_per_grid_cell.tif", overwrite=TRUE)

saveRDS(list(
  comp_nb_farms=data.frame(country=c(sixteen_countries,sample(sixteen_countries,20,TRUE)),
    census_year=sample(1970:2020,36,replace=TRUE), nb_farms=round(runif(36,5e5,5e6)),
    estim_nb_farms=round(runif(36,4e5,6e6))),
  r2_sarah=round(runif(1,.4,.8),2)),
  "../fig.1c_comparison_with_sarah_lowder.rds")

saveRDS(list(lsms_spatial=data.frame(farm_area_ha=pmax(.01,rlnorm(500,.3,.8)),
  pred_oob=pmax(.01,rlnorm(500,.3,.8)))), "../fig.1d_reported_vs_predicted_fsize.rds")

# China cropland (external dataset - true stub)
# value must be CUMULATIVE (0→1 as pred_farm_area_ha increases) for line plots in F03
{
  fa_seq <- seq(0.2, 8, by=0.2)
  aez_lvls <- c("tropical highlands","humid","sub-humid","semi-arid","all_aez")
  prod_lvls <- c("all_crops","cattle","maize","sorghum","millet","cassava","legumes","non_food")
  set.seed(42)
  china_rows <- vector("list", length(aez_lvls)*length(prod_lvls))
  k <- 1L
  for (az in aez_lvls) for (pr in prod_lvls) {
    # Sigmoid-shaped cumulative curve with slight per-group variation
    midpoint <- runif(1, 1.5, 4.0); slope <- runif(1, 0.8, 2.5)
    val <- 1 / (1 + exp(-slope * (fa_seq - midpoint)))
    val <- (val - min(val)) / (max(val) - min(val))  # rescale to 0-1
    china_rows[[k]] <- data.frame(pred_farm_area_ha=fa_seq, aez=az, product=pr,
                                  value=round(val,4), stringsAsFactors=FALSE)
    k <- k + 1L
  }
  china_long <- do.call(rbind, china_rows)
  saveRDS(list(df_rel_long=china_long), "2026-01-24.CHINA_croplands_per_crop_per_aez.rds")
}

# summarized_farm_area_ha_per_class_vs_sarah (08.3 Sarah section - true stub)
sz_cls <- c(1,2,5,10,20,50)
s07 <- expand.grid(NAME_0=sixteen_countries, farm_class=sz_cls, stringsAsFactors=FALSE)
s07$GID_0         <- sixteen_country_codes[match(s07$NAME_0,sixteen_countries)]
s07$nb_farms      <- round(runif(nrow(s07),1e4,5e5))
s07$pred_nb_farms <- round(s07$nb_farms*runif(nrow(s07),.7,1.4))
s07$cropland_ha   <- round(runif(nrow(s07),1e4,1e6))
s07$pred_cropland_ha <- round(s07$cropland_ha*runif(nrow(s07),.8,1.2))
saveRDS(list(comp_fsize_classes_nb=s07, comp_fsize_classes_ha=s07),
  file.path(processed_path,"summarized_farm_area_ha_per_class_vs_sarah.rds"))

# Suppl.Fig06_divergence_table.rds — one row per country, read by S06 and S07
# In production this is computed by S06 from the sarah comparison data
div_table <- data.frame(
  NAME_0        = sixteen_countries,
  GID_0         = sixteen_country_codes,
  divergence_nb = round(runif(16, 0.05, 0.60), 2),
  divergence_ha = round(runif(16, 0.05, 0.55), 2),
  divergence    = round(runif(16, 0.05, 0.58), 2),
  stringsAsFactors = FALSE
)
saveRDS(div_table, "Suppl.Fig06_divergence_table.rds")

# ── F01 stubs: fig.1a raster, fig.1c Lowder, fig.1d OOB ─────────────────────
# fig.1a: 2-layer SpatRaster (spam_2017 cropland ha, pred_farm_area_ha)
ssa_ext <- terra::ext(-18, 52, -35, 15)
r_stub  <- terra::rast(ssa_ext, res = 1, crs = "EPSG:4326")
fig1a_stub <- c(
  setNames(terra::init(r_stub, function(n) runif(n, 100, 5000)), "spam_2017"),
  setNames(terra::init(r_stub, function(n) runif(n, 0.3, 8)),    "pred_farm_area_ha")
)
terra::writeRaster(fig1a_stub, "../data/processed/fig1a_stub.tif", overwrite = TRUE)

# fig.1c: list with comp_nb_farms and r2_sarah
ssa_countries <- c("Angola","Benin","Botswana","Ethiopia","Kenya","Malawi",
                   "Niger","Nigeria","Rwanda","Tanzania","Uganda","Zambia")
comp_nb_farms <- data.frame(
  country        = ssa_countries,
  nb_farms       = round(runif(12, 1e4, 5e6)),
  estim_nb_farms = round(runif(12, 1e4, 5e6)),
  census_year    = sample(c(1975,1985,1995,2005,2015), 12, replace = TRUE),
  stringsAsFactors = FALSE
)
saveRDS(list(comp_nb_farms = comp_nb_farms, r2_sarah = 0.72),
        "../data/processed/fig1c_stub.rds")

# fig.1d: list with lsms_spatial (farm_area_ha, pred_oob)
n_obs <- 500
saveRDS(list(lsms_spatial = data.frame(
  farm_area_ha = pmin(3, abs(rnorm(n_obs, 1.2, 0.8))),
  pred_oob     = pmin(3, abs(rnorm(n_obs, 1.2, 0.9)))
)), "../data/processed/fig1d_stub.rds")

message("   Figure stubs done.")

# ==============================================================================
# 8. LEAVE-ONE STUBS (04.5 inputs from SLURM array - true stubs)
# ==============================================================================
message("8. Creating leave-one stubs...")
leave_one_dir <- file.path(output_path, "leave_one")
for (i in seq_along(sixteen_countries)) {
  cty <- sixteen_countries[i]; code <- sixteen_country_codes[i]
  mk_s <- function(model, mv, tv, rc="rsq") {
    suf <- paste0("loc_",code,"_",model,"_",ifelse(mv=="TRUE","means","all"),"_",
                  ifelse(tv=="TRUE","test","train"),".rds")
    df  <- data.frame(country=cty,code=code,model=model,means=mv,test=tv,stringsAsFactors=FALSE)
    df[[rc]] <- round(runif(1,.2,.65),3)
    saveRDS(list(prediction=rnorm(60L),results=df), file.path(leave_one_dir,suf))
  }
  mk_s("RF","FALSE","FALSE","Rsquared"); mk_s("RF","FALSE","TRUE","Rsquared")
  mk_s("RF","TRUE", "FALSE","Rsquared"); mk_s("RF","TRUE", "TRUE","Rsquared")
  mk_s("TPS","FALSE","TRUE"); mk_s("TPS","TRUE","TRUE")
}
message("   Leave-one stubs done.")

# ==============================================================================
# 9. COUNTRY-YEAR RAW FILES (02.1 skipped - true stubs)
# ==============================================================================
message("9. Creating country-year raw files...")
for (cty in sixteen_countries)
  for (yr in c(2010,2012,2014,2016,2018,2020)) {
    d <- lsms_raw[lsms_raw$country==cty & lsms_raw$year==yr, ]
    if (nrow(d) > 0)
      write.csv(d, file.path(processed_path,paste0(cty,"_",yr,"_raw.csv")), row.names=FALSE)
  }

# lsms_oob.rds (Python 06.1 output - true stub)
lsms_oob <- lsms_ml[,c("x","y","country","farm_area_ha","gadm_0","gadm_1","gadm_2")]
lsms_oob$gadm_3 <- NA_character_; lsms_oob$gadm_4 <- NA_character_
lsms_oob$oob_pred      <- pmax(.01, lsms_oob$farm_area_ha*runif(nrow(lsms_oob),.6,1.4))
lsms_oob$in_sample_pred <- pmax(.01, lsms_oob$farm_area_ha*runif(nrow(lsms_oob),.8,1.2))
saveRDS(lsms_oob, file.path(processed_path,"lsms_oob.rds"))

# fsize_distribution_resample_long.rds — stub needed because 08.2 does NOT produce it in CI
# (08.3 reads it; on local this comes from the real 08.3 computation)
n_theor <- nrow(lsms_ml); fa <- pmax(0.01, lsms_ml$farm_area_ha)
pred_farm_sizes_list <- lapply(fa, function(mu) {
  v <- sort(qlnorm(seq(0.01,0.99,length.out=100), meanlog=log(mu), sdlog=0.8))
  setNames(v, paste0("qrf_q", sprintf("%03d",1:100)))
})
fitted_trunc_logn_list <- lapply(fa, function(mu)
  sort(pmax(0.01, rlnorm(9, meanlog=log(mu), sdlog=0.6))))
theor_farms <- data.frame(
  x=lsms_ml$x, y=lsms_ml$y, cell=seq_len(n_theor), nb_farms=9L,
  country=lsms_ml$country, farm_area_ha=fa,
  skew=runif(n_theor,.5,4), kurt=runif(n_theor,2,8), gini=runif(n_theor,.3,.6),
  ks_trunc_D=runif(n_theor,.05,.4), ks_trunc_pval=runif(n_theor,.01,.99),
  ks_D=runif(n_theor,.05,.4), ks_pval=runif(n_theor,.01,.99),
  adjusted_logn_mean=log(fa/sqrt(1.5)), adjusted_logn_sd=sqrt(log(1.5)),
  logn_mean=log(fa), logn_sd=0.8,
  sample_mean=fa*runif(n_theor,.8,1.2), sample_sd=fa*runif(n_theor,.2,.5),
  sd_sample_mean=runif(n_theor,.05,.3), sd_sample_sd=runif(n_theor,.02,.2),
  stringsAsFactors=FALSE)
theor_farms$pred_farm_sizes        <- pred_farm_sizes_list
theor_farms$fitted_trunc_logn      <- fitted_trunc_logn_list
theor_farms$fitted_logn            <- fitted_trunc_logn_list
theor_farms$virt_farms             <- fitted_trunc_logn_list
theor_farms$virt_farms_fixed       <- fitted_trunc_logn_list
theor_farms$virt_farms_f_max_trunc <- fitted_trunc_logn_list
theor_farms_application <- data.frame(
  x=lsms_ml$x, y=lsms_ml$y, cell=seq_len(n_theor), nb_farms=9L,
  country=lsms_ml$country,
  linear_farm_size_ha    = pmax(0.01,rlnorm(n_theor,log(fa),0.6)),
  trunc_log_farm_size_ha = pmax(0.01,rlnorm(n_theor,log(fa),0.5)),
  stringsAsFactors=FALSE)
saveRDS(list(theor_farms=theor_farms, theor_farms_application=theor_farms_application),
        file.path(processed_path, "fsize_distribution_resample_long.rds"))
message("   fsize_distribution_resample_long.rds stub written.")

# farm_size_distribution_parms.tif — rasterised theor_farms params, read by 08.3
# Produced by 08.3 on local from theor_farms; in CI 08.3 reads it as input
{
  farm_dist_parms <- terra::rast(lapply(
    c("adjusted_logn_mean","adjusted_logn_sd","ks_trunc_D","ks_trunc_pval","logn_mean"),
    function(nm) make_rast(nm, 1, 0.5, r_res = res_pred)))
  names(farm_dist_parms) <- c("adjusted_logn_mean","adjusted_logn_sd",
                               "ks_trunc_D","ks_trunc_pval","logn_mean")
  terra::writeRaster(farm_dist_parms,
    file.path(processed_path, "farm_size_distribution_parms.tif"), overwrite = TRUE)
}

# gini_raster.tif — computed by 08.3; read by S08
{
  gini_r  <- terra::rast(ssa_ext, res = res_pred, crs = "EPSG:4326")
  xy_g    <- terra::xyFromCell(gini_r, seq_len(terra::ncell(gini_r)))
  gini_v  <- pmin(pmax(0.35 + 0.20*(xy_g[,2]+10)/25 + rnorm(nrow(xy_g),0,.04), .2), .8)
  terra::values(gini_r) <- gini_v; names(gini_r) <- "gini"
  terra::writeRaster(gini_r, file.path(processed_path,"gini_raster.tif"), overwrite=TRUE)
}

# back_transf_trunc_adj_mean/sd.tif — read by S07
{
  r_base <- terra::rast(ssa_ext, res = res_pred, crs = "EPSG:4326")
  n_cells <- terra::ncell(r_base)
  back_avg_r <- r_base; terra::values(back_avg_r) <- pmax(0.3, rnorm(n_cells, 1.5, 0.8))
  names(back_avg_r) <- "adjusted_logn_mean"
  terra::writeRaster(back_avg_r, file.path(processed_path,"back_transf_trunc_adj_mean.tif"), overwrite=TRUE)
  back_sd_r <- r_base; terra::values(back_sd_r) <- pmax(0.5, rnorm(n_cells, 2.5, 0.6))
  names(back_sd_r) <- "adjusted_logn_sd"
  terra::writeRaster(back_sd_r, file.path(processed_path,"back_transf_trunc_adj_sd.tif"), overwrite=TRUE)
  message("   back_transf raster stubs done.")
}

# RF model stub (pre-trained, used by some scripts before 06.1 runs)
if (requireNamespace("ranger", quietly=TRUE)) {
  mini <- lsms_ml[sample(nrow(lsms_ml),min(200,nrow(lsms_ml))),]
  rf_full_model <- ranger::ranger(
    farm_area_ha ~ cropland+cattle+pop+cropland_per_capita+sand+slope+temperature+rainfall+maizeyield+market,
    data=mini, num.trees=10, keep.inbag=TRUE)
  save(rf_full_model, file=file.path(processed_path,"rf_full_model_with_95th_trimmed_data.rdata"))
  message("   RF model stub done.")
}

dir.create("../validation", showWarnings=FALSE)

# point_and_unconsolidated_means (cross-country scripts)
pw <- expand.grid(train_country=sixteen_countries, test_country=sixteen_countries, stringsAsFactors=FALSE)
pw$target <- pw$test_country; pw$rsq_tps <- round(runif(nrow(pw),.1,.6),2)
write.csv(pw, file.path(processed_path,
  "point_and_unconsolidated_means_models_TPS_RF_leave_one_out.csv"), row.names=FALSE)

# ==============================================================================
# SUMMARY
# ==============================================================================
message("\n", paste(rep("=",70),collapse=""))
message("SYNTHETIC DATA GENERATION COMPLETE")
message(paste(rep("=",70),collapse=""))
message("  Farms generated:   ", nrow(lsms_raw))
message("  After 95th trim:   ", nrow(lsms_ml))
message("  Countries:         ", length(sixteen_countries))
message("  Raster layers:     ", terra::nlyr(all_predictors))
message("  Training res:      ", res, "deg  Prediction res: ", res_pred, "deg")
message("")
message("  NOT STUBBED (pipeline scripts will produce):")
message("    03.1 -> lsms_trimmed rds + lsms_spatial csv")
message("    04.2 -> comparison_ML_models, country_variable_importance")
message("    04.3 -> country_auto/pairwise evaluation")
message("    04.5 -> leave_one_RF/TPS/cor.rds, cross_validation_graphs.rds")
message("    06.1.py -> etr_variable_importance.csv")
message("    08.2 -> nb_farms_per_grid_cell.tif")
message("    08.3 -> fsize_distribution_resample_long.rds, farm_size_distribution_parms.tif")
message("    10.1 -> cropland_stats_per_aez.rds")
message("    S06  -> Suppl.Fig06_divergence_table.rds")

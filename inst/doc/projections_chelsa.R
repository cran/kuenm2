## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 6
)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
# Load packages
library(kuenm2)
library(terra)

# Current directory
getwd()

# Define new directory
#setwd("YOUR/DIRECTORY")  # uncomment and modify if setting a new directory

## ----Download CHELSA past, eval=FALSE-----------------------------------------
# # Define variables to download
# var_to_use <- c("BIO_01", "BIO_07", "BIO_12", "BIO_15")
# 
# # Define GCMs
# gcms <- c("CCSM4", "CNRM-CM5", "FGOALS-g2", "IPSL-CM5A-LR", "MIROC-ESM",
#           "MPI-ESM-P", "MRI-CGCM3")
# 
# # Create a grid combining variables and GCMs
# g <- expand.grid("gcm" = gcms, "var" = var_to_use)
# 
# # Create links to download
# l <- do.call(paste, c(g, sep = "_"))
# l <- paste0("https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_",
#             l, ".tif")
# 
# # See links
# head(l)
# #> [1] "https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_CCSM4_BIO_01.tif"
# #> [2] "https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_CNRM-CM5_BIO_01.tif"
# #> [3] "https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_FGOALS-g2_BIO_01.tif"
# #> [4] "https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_IPSL-CM5A-LR_BIO_01.tif"
# #> [5] "https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_MIROC-ESM_BIO_01.tif"
# #> [6] "https://os.zhdk.cloud.switch.ch/chelsav1/pmip3/bioclim/CHELSA_PMIP_MPI-ESM-P_BIO_01.tif"
# 
# # Create a directory to save the raw variables
# raw_past_chelsa <- file.path(tempdir(), "Raw_past")  # Here, in a temporary directory
# dir.create(raw_past_chelsa)
# 
# # Download files and save in the Raw_past directory
# options(timeout = 300)  # To avoid errors with timeout
# sapply(l, function(i) {
#   downfile <- file.path(raw_past_chelsa, basename(i))
# 
#   # Download only if the file has not been downloaded yet
#   if(!file.exists(downfile)) {
#       download.file(url = i, destfile = downfile, method = "curl")
#   }
# })  # It will take a while
# 
# #Check the files in the Raw_past
# list.files(raw_past_chelsa)
# #> [1] "CHELSA_PMIP_CCSM4_BIO_01.tif"        "CHELSA_PMIP_CCSM4_BIO_07.tif"
# #> [3] "CHELSA_PMIP_CCSM4_BIO_12.tif"        "CHELSA_PMIP_CCSM4_BIO_15.tif"
# #> [5] "CHELSA_PMIP_CNRM-CM5_BIO_01.tif"     "CHELSA_PMIP_CNRM-CM5_BIO_07.tif"
# #> [7] "CHELSA_PMIP_CNRM-CM5_BIO_12.tif"     "CHELSA_PMIP_CNRM-CM5_BIO_15.tif"
# #> [9] "CHELSA_PMIP_FGOALS-g2_BIO_01.tif"    "CHELSA_PMIP_FGOALS-g2_BIO_07.tif"
# #> [11] "CHELSA_PMIP_FGOALS-g2_BIO_12.tif"    "CHELSA_PMIP_FGOALS-g2_BIO_15.tif"
# #> [13] "CHELSA_PMIP_IPSL-CM5A-LR_BIO_01.tif" "CHELSA_PMIP_IPSL-CM5A-LR_BIO_07.tif"
# #> [15] "CHELSA_PMIP_IPSL-CM5A-LR_BIO_12.tif" "CHELSA_PMIP_IPSL-CM5A-LR_BIO_15.tif"
# #> [17] "CHELSA_PMIP_MIROC-ESM_BIO_01.tif"    "CHELSA_PMIP_MIROC-ESM_BIO_07.tif"
# #> [19] "CHELSA_PMIP_MIROC-ESM_BIO_12.tif"    "CHELSA_PMIP_MIROC-ESM_BIO_15.tif"
# #> [21] "CHELSA_PMIP_MPI-ESM-P_BIO_01.tif"    "CHELSA_PMIP_MPI-ESM-P_BIO_07.tif"
# #> [23] "CHELSA_PMIP_MPI-ESM-P_BIO_12.tif"    "CHELSA_PMIP_MPI-ESM-P_BIO_15.tif"
# #> [25] "CHELSA_PMIP_MRI-CGCM3_BIO_01.tif"    "CHELSA_PMIP_MRI-CGCM3_BIO_07.tif"
# #> [27] "CHELSA_PMIP_MRI-CGCM3_BIO_12.tif"    "CHELSA_PMIP_MRI-CGCM3_BIO_15.tif"

## ----download CHELSA present, eval=FALSE--------------------------------------
# # Create directory to save the variables
# present_dir <- file.path(tempdir(), "Present_raw")
# dir.create(present_dir)
# 
# # Define variables to download
# var_present <-  c("bio1", "bio7", "bio12", "bio15")
# 
# # Create links, download and save in the Present_raw directory
# l_present <- sapply(var_present, function(i){
#   # Create link to download
#   l_present_i <- paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",
#                         i, "_1981-2010_V.2.1.tif")
# 
#   # Donwload only if the file has not been downloaded yet
#   if(!file.exists(file.path(present_dir, basename(l_present_i))))
#   download.file(url = l_present_i,
#                 destfile = file.path(present_dir, basename(l_present_i)),
#                 method = "curl")
# })  # This might take a while
# 
# # Check the files in the directory
# list.files(present_dir)
# #> [1] "CHELSA_bio1_1981-2010_V.2.1.tif"     "CHELSA_bio12_1981-2010_V.2.1.tif"
# #> [3] "CHELSA_bio15_1981-2010_V.2.1.tif"    "CHELSA_bio7_1981-2010_V.2.1.tif"

## ----prepare present, eval=FALSE----------------------------------------------
# # Import are for model calibration (M)
# m <- vect(system.file("extdata", "m.gpkg",
#                         package = "kuenm2"))
# 
# # Import present variables
# present_files <- list.files(present_dir, full.names = TRUE)  # List files
# present_var <- rast(present_files)
# 
# # Mask variables using the calibration area (m)
# present_m <- crop(present_var, m, mask = TRUE)
# 
# # Check variables names
# names(present_m)
# #> [1] "CHELSA_bio1_1981-2010_V.2.1"  "CHELSA_bio12_1981-2010_V.2.1"
# #> [3] "CHELSA_bio15_1981-2010_V.2.1" "CHELSA_bio7_1981-2010_V.2.1"
# 
# # Rename variables
# names(present_m) <- c("bio1", "bio12", "bio15", "bio7")
# names(present_m)
# #> [1] "bio1"  "bio12" "bio15" "bio7"
# 
# # Check current resolution (30arc-sec)
# res(present_m)
# #> [1] 0.008333333 0.008333333
# 
# # Decrease resolution to 10arc-min
# present_chelsa <- aggregate(present_m, fact = 20, fun = "mean")
# 
# # Save processed raster
# dir_current <- file.path(tempdir(), "Current_CHELSA")
# dir.create(dir_current)
# 
# writeRaster(present_chelsa,
#             filename = file.path(dir_current, "Current_CHELSA.tif"))

## ----mask future, eval=FALSE--------------------------------------------------
# # Import LGM variables
# lgm_files <- list.files(raw_past_chelsa, full.names = TRUE)  # List files
# lgm_var <- rast(lgm_files)
# 
# # Mask variables using the calibration area (m)
# lgm_m <- crop(lgm_var, m, mask = TRUE)
# 
# # Decrease resolution to 10arc-min
# lgm_chelsa <- aggregate(lgm_m, fact = 20, fun = "mean")
# 
# # Check variables names
# names(lgm_chelsa)
# #>  [1] "CHELSA_PMIP_CCSM4_BIO_01"        "CHELSA_PMIP_CCSM4_BIO_07"
# #>  [3] "CHELSA_PMIP_CCSM4_BIO_12"        "CHELSA_PMIP_CCSM4_BIO_15"
# #>  [5] "CHELSA_PMIP_CNRM-CM5_BIO_01"     "CHELSA_PMIP_CNRM-CM5_BIO_07"
# #>  [7] "CHELSA_PMIP_CNRM-CM5_BIO_12"     "CHELSA_PMIP_CNRM-CM5_BIO_15"
# #>  [9] "CHELSA_PMIP_FGOALS-g2_BIO_01"    "CHELSA_PMIP_FGOALS-g2_BIO_07"
# #> [11] "CHELSA_PMIP_FGOALS-g2_BIO_12"    "CHELSA_PMIP_FGOALS-g2_BIO_15"
# #> [13] "CHELSA_PMIP_IPSL-CM5A-LR_BIO_01" "CHELSA_PMIP_IPSL-CM5A-LR_BIO_07"
# #> [15] "CHELSA_PMIP_IPSL-CM5A-LR_BIO_12" "CHELSA_PMIP_IPSL-CM5A-LR_BIO_15"
# #> [17] "CHELSA_PMIP_MIROC-ESM_BIO_01"    "CHELSA_PMIP_MIROC-ESM_BIO_07"
# #> [19] "CHELSA_PMIP_MIROC-ESM_BIO_12"    "CHELSA_PMIP_MIROC-ESM_BIO_15"
# #> [21] "CHELSA_PMIP_MPI-ESM-P_BIO_01"    "CHELSA_PMIP_MPI-ESM-P_BIO_07"
# #> [23] "CHELSA_PMIP_MPI-ESM-P_BIO_12"    "CHELSA_PMIP_MPI-ESM-P_BIO_15"
# #> [25] "CHELSA_PMIP_MRI-CGCM3_BIO_01"    "CHELSA_PMIP_MRI-CGCM3_BIO_07"
# #> [27] "CHELSA_PMIP_MRI-CGCM3_BIO_12"    "CHELSA_PMIP_MRI-CGCM3_BIO_15"

## ----group future by gcm, eval=FALSE------------------------------------------
# # In each iteration, 'i' is a GCM
# lgm_by_gcm <- lapply(gcms, function(i){
#   # Subset variables that belong to GCM i
#   lgm_gcm_i <- lgm_chelsa[[grepl(i, names(lgm_chelsa))]]
# 
#   # Rename variables
#   names(lgm_gcm_i) <- c("bio1", "bio7", "bio12", "bio15")
#   return(lgm_gcm_i)
# })
# 
# names(lgm_by_gcm) <- gcms

## ----check units, eval = FALSE------------------------------------------------
# # Check values of variables in present
# #> minmax(present_chelsa[[c("bio1", "bio7", "bio12", "bio15")]])
# #>         bio1     bio7    bio12    bio15
# #> min 12.87700 10.11300 1211.605 10.32925
# #> max 24.70025 21.06125 3063.049 70.45125
# 
# # Check values of variables in LGM (CCSM4)
# minmax(lgm_by_gcm$CCSM4)
# #>         bio1     bio7    bio12    bio15
# #> min 2822.425 173.5125 10710.62  86.9125
# #> max 2946.758 219.7700 23159.20 659.0025

## ----convert units, eval=FALSE------------------------------------------------
# # Fixing units in loop
# lgm_fixed_units <- lapply(lgm_by_gcm, function(x) {
#   x$bio1 <- (x$bio1 / 10) - 273  # Divide by 10 and subtracts -273
#   x$bio7 <- x$bio7 / 10  # Divide by 10
#   x$bio12 <- x$bio12 / 10  # Divide by 10
#   x$bio15 <- x$bio15 / 10  # Divide by 10
#   return(x)
# })
# 
# # Check units
# minmax(lgm_fixed_units$CCSM4)
# #>         bio1     bio7    bio12    bio15
# #> min  9.24250 17.35125 1071.062  8.69125
# #> max 21.67575 21.97700 2315.920 65.90025

## ----Save lgm, eval=FALSE-----------------------------------------------------
# # Create directory to save processed lgm variables
# dir_lgm <- file.path(tempdir(), "LGM_CHELSA")
# dir.create(dir_lgm)
# 
# # Processing in loop
# lapply(names(lgm_fixed_units), function(i) {
#   # Subset SpatRaster from GCM i
#   r_i <- lgm_fixed_units[[i]]
# 
#   # Name the file with the Period (LGM) and GCM (i)
#   filename_i <- paste0("CHELSA_LGM_", i, ".tif")
# 
#   # Write Raster
#   writeRaster(r_i, filename = file.path(dir_lgm, filename_i))
# })

## ----set directory with examples do not show, include=FALSE-------------------
dir_current <- system.file("extdata", package = "kuenm2")
dir_lgm <- system.file("extdata", package = "kuenm2")

## ----list files---------------------------------------------------------------
# Listing files in directories
present_list <- list.files(path = dir_current, pattern = "Current_CHELSA",
                           full.names = TRUE)

lgm_list <- list.files(path = dir_lgm, pattern = "LGM", full.names = TRUE)

## ----check files, eval=FALSE--------------------------------------------------
# # Check list of files with full paths
# ## Present
# present_list  # Paths can be different in distinct computers
# #> "Local\\Temp\\Current_CHELSA.tif"
# 
# # Past
# lgm_list  # Paths can be different in distinct computers
# #> [1] "Local\\Temp\\CHELSA_LGM_CCSM4.tif"
# #> [2] "Local\\Temp\\CHELSA_LGM_CNRM-CM5.tif"
# #> [3] "Local\\Temp\\CHELSA_LGM_FGOALS-g2.tif"
# #> [4] "Local\\Temp\\CHELSA_LGM_IPSL-CM5A-LR.tif"
# #> [5] "Local\\Temp\\CHELSA_LGM_MIROC-ESM.tif"
# #> [6] "Local\\Temp\\CHELSA_LGM_MPI-ESM-P.tif"
# #> [7] "Local\\Temp\\CHELSA_LGM_MRI-CGCM3.tif"

## ----organize files, results='hide'-------------------------------------------
# Create a directory to save the variables
# Here, in a temporary directory. Change to your work directory in your computer
out_dir <- file.path(tempdir(), "Projection_variables")

# Organizing files the way it is needed
organize_for_projection(output_dir = out_dir, 
                        variable_names = c("bio1", "bio7", "bio12", "bio15"), 
                        present_file = present_list, 
                        past_files = lgm_list, 
                        past_period = "LGM", 
                        past_gcm = c("CCSM4", "CNRM-CM5", "FGOALS-g2", 
                                     "IPSL-CM5A-LR", "MIROC-ESM", "MPI-ESM-P",
                                     "MRI-CGCM3"),
                        resample_to_present = TRUE,
                        overwrite = TRUE)

## ----fs, eval = FALSE---------------------------------------------------------
# # Install package if necessary
# if(!require("fs")) {
#   install.packages("fs")
# }
# 
# fs::dir_tree(out_dir)
# #> Local\Temp\Projection_variables
# #> ├── Past
# #> │   └── LGM
# #> │       ├── CCSM4
# #> │       │   └── Variables.tif
# #> │       ├── CNRM-CM5
# #> │       │   └── Variables.tif
# #> │       ├── FGOALS-g2
# #> │       │   └── Variables.tif
# #> │       ├── IPSL-CM5A-LR
# #> │       │   └── Variables.tif
# #> │       ├── MIROC-ESM
# #> │       │   └── Variables.tif
# #> │       ├── MPI-ESM-P
# #> │       │   └── Variables.tif
# #> │       └── MRI-CGCM3
# #> │           └── Variables.tif
# #> └── Present
# #>     └── Variables.tif

## ----create fitted models with CHELSA, eval=FALSE, include=FALSE--------------
# d_chelsa <- prepare_data(algorithm = "maxnet", occ = occ_data, x = "x", y = "y",
#                          raster_variables = present_chelsa,
#                          species = "Myrcia hatschbachii", partition_method = "kfolds",
#                          n_partitions = 4,
#                          n_background = 1000,
#                          features = c("l", "q", "lq", "lqp"),
#                          r_multiplier = c(0.1, 1, 2))
# 
# m_chelsa <- calibration(data = d_chelsa,
#                         error_considered = c(5, 10),
#                         omission_rate = 10,
#                         parallel = TRUE,  # Set TRUE to run in parallel
#                         ncores = 8)
# 
# fitted_model_chelsa <- fit_selected(m_chelsa, n_replicates = 4)

## ----import fitted_model_chelsa, include=TRUE---------------------------------
# Load the object with fitted models
data(fitted_model_chelsa, package = "kuenm2")

## ----prepare projections------------------------------------------------------
# Define present_dir and past_dir
in_dir_present <- file.path(out_dir, "Present")
in_dir_past <- file.path(out_dir, "Past")

# Prepare projections using fitted models to check variables
pr <- prepare_projection(models = fitted_model_chelsa,
                         present_dir = in_dir_present,  # Directory with present variables
                         past_dir = in_dir_past, 
                         past_period = "LGM", 
                         past_gcm = c("CCSM4", "CNRM-CM5", "FGOALS-g2", 
                                      "IPSL-CM5A-LR", "MIROC-ESM", "MPI-ESM-P",
                                      "MRI-CGCM3"), 
                         future_dir = NULL,  # NULL because we won't project to the Future
                         future_period = NULL, 
                         future_pscen = NULL,  
                         future_gcm = NULL)  

## ----print pr, eval=FALSE-----------------------------------------------------
# pr
# #> projection_data object summary
# #> ==============================
# #> Variables prepared to project models for Present and Past
# #> Past projections contain the following periods and GCMs:
# #>   - Periods: LGM
# #>   - GCMs: CCSM4 | CNRM-CM5 | FGOALS-g2 | IPSL-CM5A-LR | MIROC-ESM | MPI-ESM-P | MRI-CGCM3
# #> All variables are located in the following root directory:
# #> Local/Temp/Projection_variables

## ----project selected---------------------------------------------------------
# Create a folder to save projection results
# Here, in a temporary directory
out_dir_projections <- file.path(tempdir(), "Projection_results/chelsa")
dir.create(out_dir_projections, recursive = TRUE)

# Project selected models to multiple scenarios
p <- project_selected(models = fitted_model_chelsa, 
                      projection_data = pr,
                      out_dir = out_dir_projections, 
                      write_replicates = TRUE,
                      progress_bar = FALSE,  # Do not print progress bar
                      overwrite = TRUE)

# Import mean of each projected scenario
p_mean <- import_results(projection = p, consensus = "mean")

# Plot all scenarios
terra::plot(p_mean, cex.main = 0.8)

## ----changes------------------------------------------------------------------
# Run function to detect changes
changes <- projection_changes(model_projections = p, consensus = "mean",
                              output_dir = out_dir_projections, 
                              write_bin_models = TRUE,  # Write individual binary results
                              return_raster = TRUE, overwrite = TRUE)

# Set colors
summary_with_colors <- colors_for_changes(changes_projections = changes)

# Plot
terra::plot(summary_with_colors$Summary_changes)


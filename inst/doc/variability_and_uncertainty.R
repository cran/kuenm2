## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 6,
  dpi=100,
  out.width = "95%"
)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
# Load packages
library(kuenm2)
library(terra)

# Current directory
getwd()

# Define new directory
#setwd("YOUR/DIRECTORY")  # uncomment and modify if setting a new directory

# Saving original plotting parameters
original_par <- par(no.readonly = TRUE)

## ----create model_projections, warning=FALSE, results='hide'------------------
# Import calib_results_maxnet
data("fitted_model_maxnet", package = "kuenm2")

# Import path to raster files with future variables provided as example
# The data is located in the "inst/extdata" folder.
in_dir <- system.file("extdata", package = "kuenm2")

# Import raster layers (same used to calibrate and fit final models)
var <- rast(system.file("extdata", "Current_variables.tif", package = "kuenm2"))

# Get soilType
soiltype <- var$SoilType

# Organize and structure WorldClim files
# Create folder to save structured files
out_dir_future <- file.path(tempdir(), "Future_raw")  # Here, in a temporary directory

# Organize
organize_future_worldclim(input_dir = in_dir,  # Path to the raw variables from WorldClim
                          output_dir = out_dir_future, 
                          name_format = "bio_",  # Name format
                          static_variables = var$SoilType,  # Static variables
                          progress_bar = FALSE, overwrite = TRUE)

# Create a "Current_raw" folder in a temporary directory
# and copy the rawvariables there.
out_dir_current <- file.path(tempdir(), "Current_raw")
dir.create(out_dir_current, recursive = TRUE)

# Save current variables in temporary directory
terra::writeRaster(var, file.path(out_dir_current, "Variables.tif"), 
                   overwrite = TRUE)

# Prepare projections using fitted models to check variables
pr <- prepare_projection(models = fitted_model_maxnet,
                         present_dir = out_dir_current,  # Directory with present-day variables
                         future_dir = out_dir_future,  # Directory with future variables
                         future_period = c("2041-2060", "2081-2100"),
                         future_pscen = c("ssp126", "ssp585"),
                         future_gcm = c("ACCESS-CM2", "MIROC6"))

# Project selected models to multiple scenarios
## Create a folder to save projection results
# Here, in a temporary directory
out_dir <- file.path(tempdir(), "Projection_results/maxnet")
dir.create(out_dir, recursive = TRUE)

## Project selected models to multiple scenarios
p <- project_selected(models = fitted_model_maxnet, 
                      projection_data = pr,
                      out_dir = out_dir,
                      write_replicates = TRUE,
                      progress_bar = FALSE,  # Do not print progress bar
                      overwrite = TRUE)

## ----compute variance---------------------------------------------------------
# Create a directory to save results
v <- projection_variability(model_projections = p, write_files = TRUE,
                            output_dir = out_dir,
                            verbose = FALSE, overwrite = TRUE)

## ----plot variability present, fig.width = 7, fig.height = 4------------------
# Variability for the present
terra::plot(v$Present, range = c(0, 0.15))

## ----plot variability future--------------------------------------------------
# Variability for Future_2081-2100_ssp585 
terra::plot(v$`Future_2081-2100_ssp585`, range = c(0, 0.1))

## ----root_directory, eval = FALSE---------------------------------------------
# # See the folder where the results were saved
# v$root_directory
# #> [1] "Temp/Projection_results/maxnet/variance"

## ----import variability_projections-------------------------------------------
# Importing results
v_2041_2060_ssp126 <- import_results(projection = v, 
                                     present = FALSE,  # Do not import results from the present time
                                     future_period = "2041-2060", 
                                     future_pscen = "ssp126")

# Plot
terra::plot(v_2041_2060_ssp126, range = c(0, 0.1))

## ----save raster variability, eval = FALSE------------------------------------
# writeRaster(v$`Future_2081-2100_ssp585`,
#             file.path(out_dir, "Future_2081-2100_ssp585.tif"))

## ----read variability projection----------------------------------------------
v <- readRDS(file.path(out_dir, "variance/variability_projections.rds"))

## ----see maximum temp---------------------------------------------------------
max(fitted_model_maxnet$calibration_data$bio_1)

## ----Highlight non-analogous--------------------------------------------------
# Import variables from the 2081-2100 period (SSP585, GCM MIROC6)
future_ACCESS_CM2 <- rast(file.path(out_dir_future,
                                "2081-2100/ssp585/ACCESS-CM2/Variables.tif"))

# Range of values in bio_1
terra::minmax(future_ACCESS_CM2$bio_1)

# Plot
terra::plot(future_ACCESS_CM2$bio_1, 
            breaks = c(-Inf, 22.7, Inf))  # Highlight regions with values above 22.7ºC

## ----perform mop--------------------------------------------------------------
# Create a folder to save MOP results
out_dir_mop <- file.path(tempdir(), "MOPresults")
dir.create(out_dir_mop, recursive = TRUE)

# Run MOP
kmop <- projection_mop(data = fitted_model_maxnet, projection_data = pr, 
                       subset_variables = TRUE,
                       calculate_distance = TRUE,
                       out_dir = out_dir_mop, 
                       type = "detailed", 
                       overwrite = TRUE, progress_bar = FALSE)

## ----import mop, warning=FALSE------------------------------------------------
# Import results from MOP runs
mop_ssp585_2100 <- import_results(projection = kmop,
                                  future_period = "2081-2100", 
                                  future_pscen = "ssp585")

# See types of results
names(mop_ssp585_2100)

## ----plot distance------------------------------------------------------------
terra::plot(mop_ssp585_2100$distances)

## ----plot basic---------------------------------------------------------------
terra::plot(mop_ssp585_2100$basic)

## ----plot simple--------------------------------------------------------------
terra::plot(mop_ssp585_2100$simple)

## ----plot towards end---------------------------------------------------------
# Non-analogous conditions towards high values in the ACCESS-CM2 scenario
terra::plot(mop_ssp585_2100$towards_high_end$`Future_2081-2100_ssp585_ACCESS-CM2`)

# Non-analogous conditions towards low values in the MIROC6 scenario
terra::plot(mop_ssp585_2100$towards_low_end$`Future_2081-2100_ssp585_ACCESS-CM2`,
     main = names(mop_ssp585_2100$towards_low_end$`Future_2081-2100_ssp585_ACCESS-CM2`))

## ----plot towars combined-----------------------------------------------------
# Variables with conditions above training range
terra::plot(mop_ssp585_2100$towards_high_combined)

# Variables with conditions below training range
terra::plot(mop_ssp585_2100$towards_low_combined)

## ----mop zero-----------------------------------------------------------------
# Create a folder to save MOP results, now assigning 0 to cells within the range
out_dir_mop_zero <- file.path(tempdir(), "MOPresults_zero")
dir.create(out_dir_mop_zero, recursive = TRUE)

# Run MOP
kmop_zero <- projection_mop(data = fitted_model_maxnet, projection_data = pr, 
                            subset_variables = TRUE, 
                            na_in_range = FALSE,  # Assign 0 to cells within range
                            calculate_distance = TRUE,
                            out_dir = out_dir_mop_zero, 
                            type = "detailed", 
                            overwrite = TRUE, progress_bar = FALSE)

## ----check difference zero mop, fig.width = 7, fig.height = 4-----------------
# Import MOP for the scenario ssp585 in 2081-2100
mop_ssp585_2100_zero <- import_results(projection = kmop_zero,
                                       future_period = "2081-2100", 
                                       future_pscen = "ssp585")

# Compare with the MOP that assigns NA to cells within the calibration range
# Simple MOP
terra::plot(c(mop_ssp585_2100$simple$`Future_2081-2100_ssp585_ACCESS-CM2`, 
              mop_ssp585_2100_zero$simple$`Future_2081-2100_ssp585_ACCESS-CM2`),
            main = c("Within range as NA", "Within range as 0"))

# Detailed MOP
terra::plot(c(mop_ssp585_2100$towards_high_combined$`Future_2081-2100_ssp585_ACCESS-CM2`, 
              mop_ssp585_2100_zero$towards_high_combined$`Future_2081-2100_ssp585_ACCESS-CM2`),
            main = c("Within range as NA", "Within range as 0"),
            plg=list(cex=0.6))


## ----plot towards high values-------------------------------------------------
# Non-analogous conditions towards high values in the ACCESS-CM2 scenario
terra::plot(mop_ssp585_2100$towards_high_combined$`Future_2081-2100_ssp585_ACCESS-CM2`)

## ----response curves towards high, fig.height=3.8-----------------------------
# Plotting response curves to check extrapolation towards the high end of variables
par(mfrow = c(1,3))  # Set plot grid
response_curve(models = fitted_model_maxnet, variable = "bio_1", 
               new_data = future_ACCESS_CM2)
response_curve(models = fitted_model_maxnet, variable = "bio_12", 
               new_data = future_ACCESS_CM2)
response_curve(models = fitted_model_maxnet, variable = "bio_15", 
               new_data = future_ACCESS_CM2)

## ----plot towards low values, fig.width = 7, fig.height = 4-------------------
# Non-analogous conditions towards low values in the ACCESS-CM2 scenario
par(mfrow = c(1, 2))  # Set grid
terra::plot(mop_ssp585_2100$towards_low_combined$`Future_2081-2100_ssp585_ACCESS-CM2`)

## It's bio 7. Plot response curve:
response_curve(models = fitted_model_maxnet, variable = "bio_7", 
               new_data = future_ACCESS_CM2)

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 

## ----save mop rds-------------------------------------------------------------
# Check for RDS files in the directory where we saved the MOP results
list.files(path = out_dir_mop, pattern = "rds")

# Import the mop_projections file
kmop <- readRDS(file.path(out_dir_mop, "mop_projections.rds"))


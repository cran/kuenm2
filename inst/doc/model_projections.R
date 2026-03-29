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

# Saving original plotting parameters
original_par <- par(no.readonly = TRUE)

## ----import maxnet calib, warning=FALSE---------------------------------------
# Import fitted_model_maxnet
data("fitted_model_maxnet", package = "kuenm2")

# Print fitted models
fitted_model_maxnet

## -----------------------------------------------------------------------------
# See raster files with future variables provided as example
# The data is located in the "inst/extdata" folder.
in_dir <- system.file("extdata", package = "kuenm2")
list.files(in_dir)

## ----geodata, eval=FALSE------------------------------------------------------
# # Install geodata if necessary
# if (!require("geodata")) {
#   install.packages("geodata")
# }
# 
# # Load geodata
# library(geodata)
# 
# # Create folder to save the raster files
# # Here, in a temporary directory
# geodata_dir <- file.path(tempdir(), "Future_worldclim")
# dir.create(geodata_dir)
# 
# # Define GCMs, SSPs and time periods
# gcms <- c("ACCESS-CM2", "MIROC6")
# ssps <- c("126", "585")
# periods <- c("2041-2060", "2061-2080")
# 
# # Create a grid of combination of periods, ssps and gcms
# g <- expand.grid("period" = periods, "ssps" = ssps, "gcms" = gcms)
# g  # Each line is a specific scenario for future
# 
# # Loop to download variables for each scenario (It can take a while)
# lapply(1:nrow(g), function(i) {
#   cmip6_world(model = g$gcms[i],
#               ssp = g$ssps[i],
#               time = g$period[i],
#               var = "bioc",
#               res = 10, path = geodata_dir)
# })

## ----check geodata folder, eval=FALSE-----------------------------------------
# list.files(geodata_dir, recursive = TRUE)
# #> [1] "climate/wc2.1_10m/wc2.1_10m_bioc_ACCESS-CM2_ssp126_2041-2060.tif"
# #> [2] "climate/wc2.1_10m/wc2.1_10m_bioc_ACCESS-CM2_ssp126_2061-2080.tif"
# #> [3] "climate/wc2.1_10m/wc2.1_10m_bioc_ACCESS-CM2_ssp585_2041-2060.tif"
# #> [4] "climate/wc2.1_10m/wc2.1_10m_bioc_ACCESS-CM2_ssp585_2061-2080.tif"
# #> [5] "climate/wc2.1_10m/wc2.1_10m_bioc_MIROC6_ssp126_2041-2060.tif"
# #> [6] "climate/wc2.1_10m/wc2.1_10m_bioc_MIROC6_ssp126_2061-2080.tif"
# #> [7] "climate/wc2.1_10m/wc2.1_10m_bioc_MIROC6_ssp585_2041-2060.tif"
# #> [8] "climate/wc2.1_10m/wc2.1_10m_bioc_MIROC6_ssp585_2061-2080.tif"
# #>
# #> #Set climate as input directory
# #> in_dir <- file.path(geodata_dir, "climate")

## ----names in fitted----------------------------------------------------------
fitted_model_maxnet$continuous_variables

## ----static soil--------------------------------------------------------------
# Import raster layers (same used to calibrate and fit final models)
var <- rast(system.file("extdata", "Current_variables.tif", package = "kuenm2"))

# Get soilType
soiltype <- var$SoilType

## ----organize_future_worldclim------------------------------------------------
# Create folder to save structured files
out_dir_future <- file.path(tempdir(), "Future_raw")  # a temporary directory

# Organize
organize_future_worldclim(input_dir = in_dir,  # Path to variables from WorldClim
                          output_dir = out_dir_future, 
                          name_format = "bio_",  # Name format
                          static_variables = var$SoilType)  # Static variables

# Check files organized
dir(out_dir_future, recursive = TRUE)

## ----fs, eval = FALSE---------------------------------------------------------
# # Install package if necessary
# if (!require("fs")) {
#   install.packages("fs")
# }
# 
# dir_tree(out_dir_future)
# #> Temp\RtmpkhmGWN/Future_raw
# #> ├── 2041-2060
# #> │   ├── ssp126
# #> │   │   ├── ACCESS-CM2
# #> │   │   │   └── Variables.tif
# #> │   │   └── MIROC6
# #> │   │       └── Variables.tif
# #> │   └── ssp585
# #> │       ├── ACCESS-CM2
# #> │       │   └── Variables.tif
# #> │       └── MIROC6
# #> │           └── Variables.tif
# #> └── 2081-2100
# #>     ├── ssp126
# #>     │   ├── ACCESS-CM2
# #>     │   │   └── Variables.tif
# #>     │   └── MIROC6
# #>     │       └── Variables.tif
# #>     └── ssp585
# #>         ├── ACCESS-CM2
# #>         │   └── Variables.tif
# #>         └── MIROC6
# #>             └── Variables.tif

## ----copy present-------------------------------------------------------------
# Create a "Current_raw" folder in a temporary directory
# and copy the rawvariables there.
out_dir_current <- file.path(tempdir(), "Current_raw")
dir.create(out_dir_current, recursive = TRUE)

# Save current variables in temporary directory
terra::writeRaster(var, file.path(out_dir_current, "Variables.tif"))

# Check folder
list.files(out_dir_current)

## ----prepare projections------------------------------------------------------
# Prepare projections using fitted models to check variables
pr <- prepare_projection(models = fitted_model_maxnet,
                         present_dir = out_dir_current,  # Directory with present-day variables
                         past_dir = NULL,  # NULL because we won't project to the past
                         past_period = NULL,  # NULL because we won't project to the past
                         past_gcm = NULL,  # NULL because we won't project to the past
                         future_dir = out_dir_future,  # Directory with future variables
                         future_period = c("2041-2060", "2081-2100"),
                         future_pscen = c("ssp126", "ssp585"),
                         future_gcm = c("ACCESS-CM2", "MIROC6"))

## ----print pr-----------------------------------------------------------------
pr

## ----str pr, eval = FALSE-----------------------------------------------------
# # Open prepared_projection in a new window
# View(pr)

## ----project selected---------------------------------------------------------
# Create a folder to save projection results
# Here, in a temporary directory
out_dir <- file.path(tempdir(), "Projection_results/maxnet")
dir.create(out_dir, recursive = TRUE)

# Project selected models to multiple scenarios
p <- project_selected(models = fitted_model_maxnet, 
                      projection_data = pr,
                      out_dir = out_dir, 
                      write_replicates = TRUE,
                      progress_bar = FALSE)  # Do not print progress bar

## ----print model_projections--------------------------------------------------
print(p)

## ----print results, eval=FALSE------------------------------------------------
# dir_tree(out_dir)
# #> Temp\Projection_results/maxnet
# #> ├── Future
# #> │   ├── 2041-2060
# #> │   │   ├── ssp126
# #> │   │   │   ├── ACCESS-CM2
# #> │   │   │   │   ├── General_consensus.tif
# #> │   │   │   │   ├── Model_192_consensus.tif
# #> │   │   │   │   ├── Model_192_replicates.tif
# #> │   │   │   │   ├── Model_219_consensus.tif
# #> │   │   │   │   └── Model_219_replicates.tif
# #> │   │   │   └── MIROC6
# #> │   │   │       ├── General_consensus.tif
# #> │   │   │       ├── Model_192_consensus.tif
# #> │   │   │       ├── Model_192_replicates.tif
# #> │   │   │       ├── Model_219_consensus.tif
# #> │   │   │       └── Model_219_replicates.tif
# #> │   │   └── ssp585
# #> │   │       ├── ACCESS-CM2
# #> │   │       │   ├── General_consensus.tif
# #> │   │       │   ├── Model_192_consensus.tif
# #> │   │       │   ├── Model_192_replicates.tif
# #> │   │       │   ├── Model_219_consensus.tif
# #> │   │       │   └── Model_219_replicates.tif
# #> │   │       └── MIROC6
# #> │   │           ├── General_consensus.tif
# #> │   │           ├── Model_192_consensus.tif
# #> │   │           ├── Model_192_replicates.tif
# #> │   │           ├── Model_219_consensus.tif
# #> │   │           └── Model_219_replicates.tif
# #> │   └── 2081-2100
# #> │       ├── ssp126
# #> │       │   ├── ACCESS-CM2
# #> │       │   │   ├── General_consensus.tif
# #> │       │   │   ├── Model_192_consensus.tif
# #> │       │   │   ├── Model_192_replicates.tif
# #> │       │   │   ├── Model_219_consensus.tif
# #> │       │   │   └── Model_219_replicates.tif
# #> │       │   └── MIROC6
# #> │       │       ├── General_consensus.tif
# #> │       │       ├── Model_192_consensus.tif
# #> │       │       ├── Model_192_replicates.tif
# #> │       │       ├── Model_219_consensus.tif
# #> │       │       └── Model_219_replicates.tif
# #> │       └── ssp585
# #> │           ├── ACCESS-CM2
# #> │           │   ├── General_consensus.tif
# #> │           │   ├── Model_192_consensus.tif
# #> │           │   ├── Model_192_replicates.tif
# #> │           │   ├── Model_219_consensus.tif
# #> │           │   └── Model_219_replicates.tif
# #> │           └── MIROC6
# #> │               ├── General_consensus.tif
# #> │               ├── Model_192_consensus.tif
# #> │               ├── Model_192_replicates.tif
# #> │               ├── Model_219_consensus.tif
# #> │               └── Model_219_replicates.tif
# #> ├── Present
# #> │   └── Present
# #> │       ├── General_consensus.tif
# #> │       ├── Model_192_consensus.tif
# #> │       ├── Model_192_replicates.tif
# #> │       ├── Model_219_consensus.tif
# #> │       └── Model_219_replicates.tif
# #> └── Projection_paths.RDS

## ----import mean--------------------------------------------------------------
# Import mean of each projected scenario
p_mean <- import_results(projection = p, consensus = "mean")

# Plot all scenarios
terra::plot(p_mean, cex.main = 0.8)

## ----specific scen, fig.width = 7, fig.height = 4-----------------------------
# Importing
p_2060_ssp126 <- import_results(projection = p, consensus = "mean", 
                                present = FALSE,  # Do not import present projections
                                future_period = "2041-2060",
                                future_pscen = "ssp126")

# Plot all scenarios
terra::plot(p_2060_ssp126, cex.main = 0.8)

## ----projection_changes-------------------------------------------------------
# Run analysis to detect changes in suitable areas
changes <- projection_changes(model_projections = p, 
                              output_dir = out_dir, 
                              write_bin_models = TRUE,  # Write individual binary results
                              return_raster = TRUE)

## ----set colors for change----------------------------------------------------
# Set colors for change maps
changes_col <- colors_for_changes(changes)

## ----binarized----------------------------------------------------------------
terra::plot(changes_col$Binarized, cex.main = 0.8)

## ----results by gcm-----------------------------------------------------------
terra::plot(changes_col$Results_by_gcm, cex.main = 0.8)

## ----Results_by_change--------------------------------------------------------
# Results by change for the scenario of 2041-2060 (ssp126)
terra::plot(changes_col$Results_by_change$`Future_2041-2060_ssp126`)

## ----summary changes----------------------------------------------------------
terra::plot(changes_col$Summary_changes, plg = list(cex = 0.75))  # Decrease size of legend text

## ----import general summary---------------------------------------------------
# Import changes detected for 2041–2060 SSP5-8.5 
general_changes <- import_results(projection = changes, 
                                  future_period = "2041-2060", 
                                  future_pscen = "ssp585",
                                  change_types = "summary")

# Set colors
general_changes <- colors_for_changes(general_changes)

# Plot
terra::plot(general_changes$Summary, main = names(general_changes$Summary),
            plg = list(cex = 0.75))  # Decrease size of legend text

## ----save raster changes, eval = FALSE----------------------------------------
# writeRaster(changes$Summary_changes,
#             file.path(out_dir, "Summary_changes.tif"))

## ----read changes projection--------------------------------------------------
changes <- readRDS(file.path(out_dir, "Projection_changes/changes_projections.rds"))

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 


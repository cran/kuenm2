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

## ----import glm calib, warning=FALSE------------------------------------------
# Import fitted_model_glm
data("fitted_model_glm", package = "kuenm2")

# Print fitted models
fitted_model_glm

## ----Import raster layers-----------------------------------------------------
# Import raster layers
var <- rast(system.file("extdata", "Current_variables.tif", package = "kuenm2"))

# Plot raster layers
terra::plot(var)

## ----check variables----------------------------------------------------------
# Variables used to calibrate maxnet models
colnames(fitted_model_maxnet$calibration_data)

# Variables used to calibrate glms
colnames(fitted_model_glm$calibration_data)

## ----predict maxnet-----------------------------------------------------------
p_maxnet <- predict_selected(models = fitted_model_maxnet, new_variables = var,
                             progress_bar = FALSE)

## ----check maxnet predictions-------------------------------------------------
# See objects in the output of predict_selected
names(p_maxnet)

## ----plot maxnet general------------------------------------------------------
terra::plot(p_maxnet$General_consensus)

## ----plot models maxnet-------------------------------------------------------
# Predictions for each replicate from model 192
terra::plot(p_maxnet$Model_192$Replicates)

# Consensus across each replicate from model 192
terra::plot(p_maxnet$Model_192$Model_consensus)

## ----compare, fig.width = 7, fig.height = 4-----------------------------------
# Predict glm 
p_glm <- predict_selected(models = fitted_model_glm, 
                          new_variables = var,
                          progress_bar = FALSE)
# See selected models that were predicted
names(p_glm)

# Compare general consensus (mean) between maxnet and glm
par(mfrow = c(1, 2))  # Set grid to plot
terra::plot(p_maxnet$General_consensus$mean, main = "Maxnet")
terra::plot(p_glm$General_consensus, main = "GLM")


## ----var to df----------------------------------------------------------------
var_df <- as.data.frame(var)
head(var_df)

## ----predict to df------------------------------------------------------------
p_df <- predict_selected(models = fitted_model_maxnet, 
                         new_variables = var_df,  # Now, a data.frame
                         progress_bar = FALSE) 

## ----predict df results-------------------------------------------------------
# Results by replicate of the model 192
head(p_df$Model_192$Replicates)

# Consensus across replicates of the model 192
head(p_df$Model_192$Model_consensus)

# General consensus across all models
head(p_df$General_consensus)

## ----output types-------------------------------------------------------------
p_cloglog <- predict_selected(models = fitted_model_maxnet, new_variables = var, 
                              type = "cloglog", progress_bar = FALSE)
p_logistic <- predict_selected(models = fitted_model_maxnet, new_variables = var, 
                              type = "logistic", progress_bar = FALSE)
p_cumulative <- predict_selected(models = fitted_model_maxnet, new_variables = var, 
                                 type = "cumulative", progress_bar = FALSE)
p_raw <- predict_selected(models = fitted_model_maxnet, new_variables = var, 
                          type = "raw", progress_bar = FALSE)

# Plot the differences
par(mfrow = c(2, 2))
terra::plot(p_cloglog$General_consensus$mean, main = "Cloglog (Default)",
            zlim = c(0, 1))
terra::plot(p_logistic$General_consensus$mean, main = "Logistic", 
            zlim = c(0, 1))
terra::plot(p_cumulative$General_consensus$mean, main = "Cumulative",
            zlim = c(0, 1))
terra::plot(p_raw$General_consensus$mean, main = "Raw", zlim = c(0, 1))

## ----response bio_7-----------------------------------------------------------
response_curve(models = fitted_model_maxnet, variable = "bio_7", 
               extrapolation_factor = 1)

## ----check lower and upper limits---------------------------------------------
range(fitted_model_maxnet$calibration_data$bio_7)

## ----create scenario, fig.width = 7, fig.height = 4---------------------------
# From bio_7, reduce values
new_bio7 <- var$bio_7 - 3

# Create new scenario
new_var <- var

# Replace bio_7 with new_bio7 in this scenario
new_var$bio_7 <- new_bio7

# Plot the differences
par(mfrow = c(1, 2))
terra::plot(var$bio_7, main = "Original bio_7", range = c(5, 25))
terra::plot(new_var$bio_7, main = "New bio_7", range = c(5, 25))

## -----------------------------------------------------------------------------
# Predict to hypothetical scenario with free extrapolation
p_free_extrapolation <- predict_selected(models = fitted_model_maxnet, 
                                         new_variables = new_var,  # New scenario
                                         consensus = "mean",
                                         extrapolation_type = "E",  # Free extrapolation (Default)
                                         progress_bar = FALSE)

# Predict to hypothetical scenario with clamping
p_clamping <- predict_selected(models = fitted_model_maxnet, 
                               new_variables = new_var,  # New scenario
                               consensus = "mean",
                               extrapolation_type = "EC",  # Extrapolation with clamping
                               progress_bar = FALSE)

# Get and see differences
p_difference <- p_free_extrapolation$General_consensus$mean - p_clamping$General_consensus$mean

# Plot the differences
par(mfrow = c(2, 2))
terra::plot(p_free_extrapolation$General_consensus$mean, 
            main = "Free extrapolation", zlim = c(0, 1))
terra::plot(p_clamping$General_consensus$mean, main = "Clamping",
             zlim = c(0, 1))
terra::plot(p_difference, main = "Difference")
terra::plot(new_bio7, main = "Hypothetical bio_7", type = "interval")

## ----no extrapolation---------------------------------------------------------
# Predict to hypothetical scenario with no extrapolation
p_no_extrapolation <- predict_selected(models = fitted_model_maxnet, 
                                       new_variables = new_var,  # New scenario
                                       consensus = "mean",
                                       extrapolation_type = "NE",  # No extrapolation
                                       progress_bar = FALSE)
# Plot the differences
par(mfrow = c(2, 2))
terra::plot(p_free_extrapolation$General_consensus$mean, main = "Free extrapolation",
            zlim = c(0, 1))
terra::plot(p_clamping$General_consensus$mean, main = "Clamping",
            zlim = c(0, 1))
terra::plot(p_no_extrapolation$General_consensus$mean, main = "No extrapolation",
            zlim = c(0, 1))
terra::plot(new_bio7, main = "Hypothetical bio_7", type = "interval")

## ----omission error-----------------------------------------------------------
# Get omission error used to select models and calculate the thesholds
## For maxnet model
fitted_model_maxnet$omission_rate

## For glm
fitted_model_glm$omission_rate

## ----thresholds---------------------------------------------------------------
# For maxnet
fitted_model_maxnet$thresholds$consensus

# For glm
fitted_model_glm$thresholds$consensus

## ----binarize models, fig.width = 7, fig.height = 4---------------------------
# Get the threshold values for models (general consensus)
thr_mean_maxnet <- fitted_model_maxnet$thresholds$consensus$mean  # Maxnet
thr_mean_glm <- fitted_model_glm$thresholds$consensus$mean  # glm

# Binarize models
mean_maxnet_bin <- (p_maxnet$General_consensus$mean >= thr_mean_maxnet) * 1
mean_glm_bin <- (p_glm$General_consensus >= thr_mean_glm) * 1

# Compare results
par(mfrow = c(1, 2))  # Set grid to plot
terra::plot(mean_maxnet_bin, main = "Maxnet")
terra::plot(mean_glm_bin, main = "GLM")

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 

## ----save, eval=FALSE---------------------------------------------------------
# p_save <- predict_selected(models = fitted_model_maxnet,
#                            new_variables = var,
#                            write_files = TRUE,  # To save to the disk
#                            write_replicates = TRUE,  # To save predictions for each replicate
#                            out_dir = tempdir(),  # Directory to save the results (temporary directory)
#                            progress_bar = FALSE)

## ----writeRaster, eval = FALSE------------------------------------------------
# terra::writeRaster(p_maxnet$General_consensus$mean,
#                    filename = file.path(tempdir(), "Mean_consensus.tif"))

## ----import var future--------------------------------------------------------
# Read layers representing future conditions
future_var <- terra::rast(system.file("extdata",
                                      "wc2.1_10m_bioc_ACCESS-CM2_ssp585_2081-2100.tif",
                                      package = "kuenm2"))

# Plot future layers
terra::plot(future_var)

## -----------------------------------------------------------------------------
# renaming layers to match names of variables used to fit the model
names(future_var) <- sub("bio0", "bio", names(future_var))
names(future_var) <- sub("bio", "bio_", names(future_var))
names(var)
names(future_var)

# Adding soil layer to future variable set
future_var <- c(future_var, var$SoilType)

## -----------------------------------------------------------------------------
# Predict
p_future <- predict_selected(models = fitted_model_maxnet,
                             new_variables = future_var, 
                             progress_bar = FALSE)

# Plot consensus (mean)
terra::plot(c(p_maxnet$General_consensus$mean,
              p_future$General_consensus$mean),
            main = c("Present", "Future (SSP 585, 2081-2100)"))

## ----detect changes-----------------------------------------------------------
# Compute changes between scenarios
p_changes <- prediction_changes(current_predictions = p_maxnet$General_consensus$mean,
                                new_predictions = p_future$General_consensus$mean,
                                fitted_models = fitted_model_maxnet,
                                predicted_to = "future")

# Plot result
terra::plot(p_changes)


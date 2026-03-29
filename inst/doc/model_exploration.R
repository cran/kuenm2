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

## ----import calibration results, warning=FALSE--------------------------------
# Import an example of calibration results 
data("calib_results_maxnet", package = "kuenm2")

# Print calibration result
calib_results_maxnet

## ----import glm---------------------------------------------------------------
# Import calib_results_glm
data("calib_results_glm", package = "kuenm2")

# Print calibration result
calib_results_glm

## ----fit_selected not echo, warning=FALSE, echo=FALSE-------------------------
# Fit models using calibration results
fm <- fit_selected(calibration_results = calib_results_maxnet, 
                   replicate_method = "kfolds", n_replicates = 4, 
                   verbose = FALSE, progress_bar = FALSE)

## ----fit_selected not eval, warning=FALSE, eval=FALSE-------------------------
# # Fit selected models using calibration results
# fm <- fit_selected(calibration_results = calib_results_maxnet,
#                    replicate_method = "kfolds", n_replicates = 4)
# # Fitting replicates...
# #   |========================================================================| 100%
# # Fitting full models...
# #   |========================================================================| 100%

## ----explore  fitted----------------------------------------------------------
# See names of selected models
names(fm$Models)

# See models inside Model 192
names(fm$Models$Model_192)

## ----omission error-----------------------------------------------------------
# Get omission error used to select models and calculate the theshold values
fm$omission_rate

## ----thresholds---------------------------------------------------------------
fm$thresholds

## ----all var resps------------------------------------------------------------
# Produce response curves for all variables in all fitted models
all_response_curves(fm)

## ----all var resps1-----------------------------------------------------------
# All response curves showing each model as a different line
all_response_curves(fm, show_lines = TRUE)

## ----all var resps2-----------------------------------------------------------
# All response curves for model 219
all_response_curves(fm, modelID = "Model_219")

## ----all var resps3-----------------------------------------------------------
# All response curves for model 219 (GAM for variability)
all_response_curves(fm, modelID = "Model_219", show_variability = TRUE)

# All response curves for model 219 (each curve is a replicate)
all_response_curves(fm, modelID = "Model_219", show_variability = TRUE, 
                    show_lines = TRUE)

## ----Get variables------------------------------------------------------------
# Get variables with non-zero coefficients in the models
fm$Models[[1]]$Full_model$betas  # From the first model selected
fm$Models[[2]]$Full_model$betas  # From the second model selected

## ----response curve-----------------------------------------------------------
par(mfrow = c(2, 2), mar = c(4, 4, 1, 0.5))  # Set grid of plot
response_curve(models = fm, variable = "bio_1", las = 1)
response_curve(models = fm, variable = "bio_7", ylab = "", las = 1)
response_curve(models = fm, variable = "bio_12", las = 1)
response_curve(models = fm, variable = "bio_15", ylab = "", las = 1)

## ----response ID--------------------------------------------------------------
par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 0.5))  # Set grid of plot
response_curve(models = fm, variable = "bio_1", modelID = "Model_192", 
               main = "Model_192", las = 1)
response_curve(models = fm, variable = "bio_1", modelID = "Model_219", 
               main = "Model_219", ylab = "", las = 1)
response_curve(models = fm, variable = "bio_7", modelID = "Model_192", 
               main = "Model_192", las = 1)
response_curve(models = fm, variable = "bio_7", modelID = "Model_219", 
               main = "Model_219", ylab = "", las = 1)

## ----extrapolation factor-----------------------------------------------------
par(mfrow = c(2, 2), mar = c(4, 4, 1, 0.5))  # Set grid of plot
response_curve(models = fm, variable = "bio_1", extrapolation_factor = 2, 
               las = 1)
response_curve(models = fm, variable = "bio_7", extrapolation_factor = 2,
               ylab = "", las = 1)
response_curve(models = fm, variable = "bio_12", extrapolation_factor = 2,
               las = 1)
response_curve(models = fm, variable = "bio_15", extrapolation_factor = 2,
               ylab = "", las = 1)

## ----lower limit--------------------------------------------------------------
response_curve(models = fm, variable = "bio_12", extrapolation_factor = 0.1, 
               l_limit = 0)

## ----GAM vs lines-------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(4, 4, 1, 0.5))  # Set grid of plot
response_curve(models = fm, variable = "bio_1", extrapolation_factor = 0.5, 
               las = 1)
response_curve(models = fm, variable = "bio_1", extrapolation_factor = 0.5, 
               show_lines = TRUE, ylab = "", las = 1)

## ----add points---------------------------------------------------------------
response_curve(models = fm, variable = "bio_1", show_lines = TRUE,
               add_points = TRUE)

## ----bivariate----------------------------------------------------------------
# First, let's check the terms in the models fitted
## Model 192
fm$Models$Model_192$Full_model$betas

## Model 219
fm$Models$Model_219$Full_model$betas

# Example of a bivariate response plot 
bivariate_response(models = fm, variable1 = "bio_1", variable2 = "bio_15", 
                   modelID = "Model_192")

## ----bivariate1---------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 0.5))

# Bivariate response model 192 
bivariate_response(models = fm, variable1 = "bio_1", variable2 = "bio_15", 
                   modelID = "Model_192", add_bar = FALSE, main = "Model 192")

# Bivariate response model 219 
bivariate_response(models = fm, variable1 = "bio_1", variable2 = "bio_15", 
                   modelID = "Model_219", add_bar = FALSE, main = "Model 219", 
                   ylab = "")

## ----var importance not echo, echo=FALSE--------------------------------------
# Calculate variable importance
imp <- variable_importance(models = fm, progress_bar = FALSE, verbose = FALSE)

## ----var importance not eval, eval=FALSE--------------------------------------
# # Calculate variable importance
# imp <- variable_importance(models = fm)
# 
# # Calculating variable contribution for model 1 of 2
# #   |======================================================================| 100%
# # Calculating variable contribution for model 2 of 2
# #   |======================================================================| 100%

## ----print importance---------------------------------------------------------
imp

## ----plot importance----------------------------------------------------------
plot_importance(imp)

## ----one model importance-----------------------------------------------------
# Calculate variable importance for a specific selected Model
imp_192 <- variable_importance(models = fm, modelID = "Model_192", 
                               progress_bar = FALSE)

# Plot variable contribution for model 192
plot_importance(imp_192, main = "Variable importance: Model 192")

## ----term importance----------------------------------------------------------
# Calculate variable importance for a specific selected Model
imp_terms <- variable_importance(models = fm, by_terms = TRUE, 
                                 progress_bar = FALSE)

# Check results
imp_terms

# Plot variable contribution for model 192
par(cex = 0.7, mar = c(3, 4, 2.5, 0.5))  # Set grid of plot
plot_importance(imp_terms, main = "Importance of variable terms")

## ----import new occ-----------------------------------------------------------
# Import independent records
data("new_occ", package = "kuenm2")

# See data structure
str(new_occ)

## ----extract var--------------------------------------------------------------
# Import raster layers
var <- terra::rast(system.file("extdata", "Current_variables.tif",
                               package = "kuenm2"))

# Extract variables to occurrences
new_data <- extract_occurrence_variables(occ = new_occ, x = "x", y = "y",
                                         raster_variables = var)

# See data structure
str(new_data)

## ----add fake data------------------------------------------------------------
# Add some fake data beyond the limits of calibration ranges
fake_data <- data.frame("pr_bg" = c(1, 1, 1),
                        "x" = c(NA, NA, NA),
                        "y" = c(NA, NA, NA),
                        "bio_1" = c(10, 15, 23),
                        "bio_7" = c(12, 16, 20),
                        "bio_12" = c(2300, 2000, 1000),
                        "bio_15" = c(30, 40, 50),
                        "SoilType" = c(1, 1, 1))

# Bind data
new_data <- rbind(new_data, fake_data)

## -----------------------------------------------------------------------------
# Evaluate models with independent data
res_ind <- independent_evaluation(fitted_models = fm, new_data = new_data)

## -----------------------------------------------------------------------------
res_ind$evaluation

## -----------------------------------------------------------------------------
# Show the mop results for the last 5 independent records
res_ind$predictions$continuous[81:85 , c("mop_distance", "inside_range", 
                                         "n_var_out", "towards_low", 
                                         "towards_high")]

## -----------------------------------------------------------------------------
# Show the continuous predictions for the last 5 independent records
# Round to two decimal places
round(res_ind$predictions$continuous[81:85, 1:6], 2)

# Show the binary predictions for the last 5 independent records
res_ind$predictions$binary[81:85, 1:6]

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 

## ----save data, eval=FALSE----------------------------------------------------
# # Set directory to save (here, in a temporary directory)
# dir_to_save <- file.path(tempdir())
# 
# # Save the data:
# saveRDS(fm, file.path(dir_to_save, "fitted_models.rds"))
# 
# # Import data
# fm <- readRDS(file.path(dir_to_save, "fitted_models.rds"))


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

## ----Create d_maxnet again, warning=FALSE-------------------------------------
# Import occurrences
data(occ_data, package = "kuenm2")

# Import raster layers
var <- rast(system.file("extdata", "Current_variables.tif", package = "kuenm2"))

# Prepare data for maxnet model
d_maxnet <- prepare_data(algorithm = "maxnet",
                         occ = occ_data,
                         x = "x", y = "y",
                         raster_variables = var,
                         species = "Myrcia hatschbachii",
                         categorical_variables = "SoilType", 
                         partition_method = "kfolds", 
                         n_partitions = 4,
                         n_background = 1000,
                         features = c("l", "q", "lq", "lqp"),
                         r_multiplier = c(0.1, 1, 2))

# Prepare data for glm model
d_glm <- prepare_data(algorithm = "glm",
                      occ = occ_data,
                      x = "x", y = "y",
                      raster_variables = var,
                      species = "Myrcia hatschbachii",
                      categorical_variables = "SoilType", 
                      partition_method = "bootstrap", 
                      n_partitions = 10, 
                      train_proportion = 0.7,
                      n_background = 300,
                      features = c("l", "q", "p", "lq", "lqp"),
                      r_multiplier = NULL)  # Not necessary with glms

## ----import calib_results_maxnet, echo=FALSE----------------------------------
m_maxnet <- kuenm2::calib_results_maxnet

## ----calibrate maxnet, eval=FALSE---------------------------------------------
# #Calibrate maxnet models
# m_maxnet <- calibration(data = d_maxnet,
#                         error_considered = c(5, 10),
#                         omission_rate = 10,
#                         parallel = FALSE,  # Set TRUE to run in parallel
#                         ncores = 1)  # Define number of cores to run in parallel
# # Task 1/1: fitting and evaluating models:
# #   |=====================================================================| 100%
# #
# # Model selection step:
# # Selecting best among 300 models.
# # Calculating pROC...
# #
# # Filtering 300 models.
# # Removing 0 model(s) because they failed to fit.
# # 135 models were selected with omission rate below 10%.
# # Selecting 2 final model(s) with delta AIC <2.
# # Validating pROC of selected models...
# #   |=====================================================================| 100%
# # All selected models have significant pROC values.

## ----explore calibration------------------------------------------------------
# See first rows of the summary of calibration results
head(m_maxnet$calibration_results$Summary[, c("ID", "Omission_rate_at_10.mean", 
                                              "AICc", "Is_concave")])

## ----explore selected---------------------------------------------------------
# See first rows of the summary of calibration results
m_maxnet$selected_models[, c("ID", "Formulas", "R_multiplier",
                             "Omission_rate_at_10.mean", "AICc", "Is_concave")]

## -----------------------------------------------------------------------------
print(m_maxnet)

## ----import calib_results_glm, echo=FALSE-------------------------------------
m_glm <- kuenm2::calib_results_glm

## ----calibrate glm, eval=FALSE, warning=FALSE---------------------------------
# #Calibrate maxnet models
# m_glm <- calibration(data = d_glm,
#                      error_considered = c(5, 10),
#                      omission_rate = 10,
#                      parallel = FALSE,  # Set TRUE to run in parallel
#                      ncores = 1)  # Define number of cores to run in parallel
# # Task 1/1: fitting and evaluating models:
# #   |=====================================================================| 100%
# # Model selection step:
# # Selecting best among 122 models.
# # Calculating pROC...
# #
# # Filtering 122 models.
# # Removing 0 model(s) because they failed to fit.
# # 21 models were selected with omission rate below 10%.
# # Selecting 1 final model(s) with delta AIC <2.
# # Validating pROC of selected models...
# #   |=====================================================================| 100%
# # All selected models have significant pROC values.

## ----selected models in glm---------------------------------------------------
m_glm

## ----figure of concave curves, out.width="82%", echo=FALSE, fig.align='center', fig.cap="Figure 1. Representation of convex (left) and concave (right) response curves. Dashed lines indicate limits of environmental conditions for model training."----
knitr::include_graphics("vignettes_img/concave_curves.png")

## ----has concave--------------------------------------------------------------
#Selected maxnet models
m_maxnet$selected_models[, c("ID", "Formulas", "Is_concave")]

#Selected glm models
m_glm$selected_models[, c("ID", "Formulas", "Is_concave")]

## ----remove_concave, eval = FALSE---------------------------------------------
# m_unimodal <- calibration(data = d_maxnet,
#                           remove_concave = TRUE,  # Ensures concave models are not selected
#                           error_considered = c(5, 10),
#                           omission_rate = 10)
# # Task 1/2: checking for concave responses in models:
# #   |=====================================================================| 100%
# #
# # Task 2/2: fitting and evaluating models with no concave responses:
# #   |=====================================================================| 100%
# #
# # Model selection step:
# # Selecting best among 300 models.
# # Calculating pROC...
# #
# # Filtering 300 models.
# # Removing 0 model(s) because they failed to fit.
# # Removing 39 model(s) with concave curves.
# # 110 models were selected with omission rate below 10%.
# # Selecting 2 final model(s) with delta AIC <2.
# # Validating pROC of selected models...
# #   |=====================================================================| 100%
# # All selected models have significant pROC values.

## ----pROC NA------------------------------------------------------------------
# See first rows of the summary of calibration results (pROC values)
head(m_maxnet$calibration_results$Summary[, c("ID", "Mean_AUC_ratio_at_10.mean",
                                              "pval_pROC_at_10.mean")])
# See pROC values of selected models
m_maxnet$selected_models[, c("ID", "Mean_AUC_ratio_at_10.mean", 
                             "pval_pROC_at_10.mean")]

## -----------------------------------------------------------------------------
# Re-select maxnet models
new_m_maxnet <- select_models(calibration_results = m_maxnet, 
                              compute_proc = TRUE, 
                              omission_rate = 5)  # New omission rate

print(new_m_maxnet)

## ----selected models or5------------------------------------------------------
new_m_maxnet$selected_models[,c("ID", "Formulas", "R_multiplier", 
                                "Omission_rate_at_5.mean", 
                                "Mean_AUC_ratio_at_5.mean",
                                "AICc", "Is_concave")]

## ----select models dataframe--------------------------------------------------
#Re-select models using data.frame
new_summary <- select_models(candidate_models = m_maxnet$calibration_results$Summary,
                             data = d_maxnet,  # Needed to compute pROC
                             compute_proc = TRUE, 
                             omission_rate = 5)

#Get class of object
class(new_summary)

#See selected models
new_summary$selected_models[, c("ID", "Formulas", "R_multiplier", 
                                "Omission_rate_at_5.mean", 
                                "Mean_AUC_ratio_at_5.mean",
                                "AICc", "Is_concave")]

## ----extrap_partitions, eval=TRUE---------------------------------------------
# ID of models that were selected
m_maxnet$selected_models$ID

# Response curves for model 192
partition_response_curves(calibration_results = m_maxnet, modelID = 192)

## ----extrap_partitions1, eval=TRUE--------------------------------------------
# Response curves for model 219
partition_response_curves(calibration_results = m_maxnet, modelID = 219)

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 

## ----save data, eval=FALSE----------------------------------------------------
# # Set directory to save (here, in a temporary directory)
# dir_to_save <- file.path(tempdir())
# 
# # Save the data
# saveRDS(m_maxnet, file.path(dir_to_save, "Candidates_maxnet.rds"))
# 
# # Import data
# m_maxnet <- readRDS(file.path(dir_to_save, "Candidates_maxnet.rds"))


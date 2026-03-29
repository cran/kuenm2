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

## ----Import occurrence data---------------------------------------------------
# Import occurrences
data(occ_data, package = "kuenm2")

# Check data structure
str(occ_data)

## ----Load variables, dpi = 80, out.width = "70%"------------------------------
# Import raster layers
var <- terra::rast(system.file("extdata", "Current_variables.tif", 
                               package = "kuenm2"))

# Check variables
terra::plot(var)

## ----dpi = 80, out.width = "70%"----------------------------------------------
# Visualize occurrences on one variable
terra::plot(var[["bio_1"]], main = "Bio 1")

points(occ_data[, c("x", "y")], col = "black")

## ----simple prepare data------------------------------------------------------
# Prepare data for maxnet model
d <- prepare_data(algorithm = "maxnet",
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

## ----print prepared data------------------------------------------------------
print(d)

## ----explore some data--------------------------------------------------------
# Check the algorithm selected
d$algorithm

# See first rows of calibration data
head(d$calibration_data)

# See first rows of formula grid
head(d$formula_grid)

## ----prepare glm--------------------------------------------------------------
# Prepare data selecting GLM as the algorithm
d_glm <- prepare_data(algorithm = "glm",
                      occ = occ_data,
                      x = "x", y = "y",
                      raster_variables = var,
                      species = "Myrcia hatschbachii",
                      categorical_variables = "SoilType", 
                      partition_method = "subsample", 
                      n_partitions = 10, 
                      train_proportion = 0.7,
                      n_background = 300,
                      features = c("l", "q", "p", "lq", "lqp"),
                      r_multiplier = NULL)  # Not necessary with GLMs

# Print object
d_glm

## ----import user data---------------------------------------------------------
data("user_data", package = "kuenm2")

head(user_data)

## ----prepare user data--------------------------------------------------------
# Prepare data for maxnet model
data_user <- prepare_user_data(algorithm = "maxnet",
                               user_data = user_data,  # user-prepared data.frame
                               pr_bg = "pr_bg",
                               species = "Myrcia hatschbachii",
                               categorical_variables = "SoilType",
                               partition_method = "bootstrap",
                               features = c("l", "q", "p", "lq", "lqp"),
                               r_multiplier = c(0.1, 1, 2, 3, 5))

data_user 

## ----explore histogram, out.width = "70%", fig.width = 7, fig.height = 4.5----
# Prepare histogram data
calib_hist <- explore_calibration_hist(data = d, raster_variables = var,
                                       include_m = TRUE)

# Plot histograms
plot_calibration_hist(explore_calibration = calib_hist)

## ----explore spatial, out.width = "75%"---------------------------------------
# Explore spatial distribution
pbg <- explore_partition_geo(data = d, raster_variables = var[[1]])

# Plot exploration results in geography
terra::plot(pbg)

## -----------------------------------------------------------------------------
# Run extrapolation risk analysis
mop_partition <- explore_partition_extrapolation(data = d, 
                                                 include_test_background = TRUE)

## ----mop_results--------------------------------------------------------------
# Check some of the results
head(mop_partition$Mop_results)

# Number of testing records with values outside training ranges
nrow(mop_partition$Mop_results[mop_partition$Mop_results$n_var_out > 1, ])

## ----fig.width = 8, fig.height = 3.5, out.width = "75%"-----------------------
# Simple plot in geographic space
plot_explore_partition(explore_partition = mop_partition, space = "G", 
                       type_of_plot = "simple")

# Distance plot in geographic space
plot_explore_partition(explore_partition = mop_partition, space = "G", 
                       type_of_plot = "distance",
                       lwd_legend = 4)

# Simple plot in environmental space
plot_explore_partition(explore_partition = mop_partition, space = "E", 
                       type_of_plot = "simple", 
                       variables = c("bio_7", "bio_15"))

# Distance plot in environmental space
plot_explore_partition(explore_partition = mop_partition, space = "E", 
                       type_of_plot = "distance",
                       variables = c("bio_7", "bio_15"), 
                       lwd_legend = 4)


## ----formula example----------------------------------------------------------
"~ bio_1 + bio_12 + I(bio_1^2) + I(bio_12^2)"

## ----user_formula-------------------------------------------------------------
# Set custom formulas
my_formulas <- c("~ bio_1 + bio_12 + I(bio_1^2) + I(bio_12^2)",
                 "~ bio_1 + bio_12 + I(bio_1^2)",
                 "~ bio_1 + bio_12 + I(bio_12^2)",
                 "~ bio_1 + I(bio_1^2) + I(bio_12^2)")

# Prepare data using custom formulas
d_custom_formula <- prepare_data(algorithm = "maxnet",
                                 occ = occ_data,
                                 x = "x", y = "y",
                                 raster_variables = var,
                                 species = "Myrcia hatschbachii",
                                 categorical_variables = "SoilType",
                                 partition_method = "kfolds",
                                 n_partitions = 4,
                                 n_background = 1000,
                                 user_formulas = my_formulas,  # Custom formulas
                                 r_multiplier = c(0.1, 1, 2))

# Check formula grid
d_custom_formula$formula_grid

## ----import bias file, out.width = "70%"--------------------------------------
# Import a bias file
bias <- terra::rast(system.file("extdata", "bias_file.tif", package = "kuenm2"))

# Check the bias values
terra::plot(bias)

## ----bias data, results='hide'------------------------------------------------
# Using a direct bias effect in sampling
d_bias_direct <- prepare_data(algorithm = "maxnet",
                              occ = occ_data,
                              x = "x", y = "y",
                              raster_variables = var,
                              species = "Myrcia hatschbachii",
                              categorical_variables = "SoilType",
                              n_background = 1000, 
                              partition_method = "kfolds",
                              bias_file = bias, bias_effect = "direct",  # bias parameters
                              features = c("l", "q", "p", "lq", "lqp"),
                              r_multiplier = c(0.1, 1, 2, 3, 5))

# Using an indirect bias effect 
d_bias_inverse <- prepare_data(algorithm = "maxnet",
                               occ = occ_data,
                               x = "x", y = "y",
                               raster_variables = var,
                               species = "Myrcia hatschbachii",
                               categorical_variables = "SoilType",
                               n_background = 1000,
                               partition_method = "kfolds",
                               bias_file = bias, bias_effect = "inverse",  # bias parameters
                               features = c("l", "q", "p", "lq", "lqp"),
                               r_multiplier = c(0.1, 1, 2, 3, 5))

## ----exp_part_geobias---------------------------------------------------------
# Explore spatial distribution of points
## No bias
geo_dist <- explore_partition_geo(data = d, raster_variables = var)

## Direct bias effect
geo_dist_bias <- explore_partition_geo(data = d_bias_direct,
                                       raster_variables = var)

## Inverse bias effect
geo_dist_bias_inv <- explore_partition_geo(data = d_bias_inverse,
                                           raster_variables = var)

## Adjusting plotting grid
par(mfrow = c(2, 2))  

## The plots to show sampling bias effects
terra::plot(bias, main = "Bias file")
terra::plot(geo_dist$Background, main = "Random Background", 
            plg = list(cex = 0.75))  # Decrease size of legend text)
terra::plot(geo_dist_bias$Background, main = "Direct Bias Effect", 
            plg = list(cex = 0.75))  # Decrease size of legend text)
terra::plot(geo_dist_bias_inv$Background, main = "Inverse Bias Effect", 
            plg = list(cex = 0.75))  # Decrease size of legend text)

## ----prepare data pca---------------------------------------------------------
# Prepare data for maxnet models using PCA parameters
d_pca <- prepare_data(algorithm = "maxnet",
                      occ = occ_data,
                      x = "x", y = "y",
                      raster_variables = var, 
                      do_pca = TRUE, center = TRUE, scale = TRUE,  # PCA parameters
                      species = "Myrcia hatschbachii",
                      categorical_variables = "SoilType",
                      n_background = 1000,
                      partition_method = "kfolds",
                      features = c("l", "q", "p", "lq", "lqp"),
                      r_multiplier = c(0.1, 1, 2, 3, 5))

print(d_pca)

## ----explore hist, out.width = "70%", fig.width = 7, fig.height = 4.5---------
# Check calibration data
head(d_pca$calibration_data)

# Check formula grid
head(d_pca$formula_grid)

# Explore variables distribution
calib_hist_pca <- explore_calibration_hist(data = d_pca, raster_variables = var,
                                           include_m = TRUE, breaks = 7)

plot_calibration_hist(explore_calibration = calib_hist_pca)

## ----do PCA, out.width = "70%"------------------------------------------------
# PCA with raw raster variables
pca_var <- perform_pca(raster_variables = var, exclude_from_pca = "SoilType",
                       center = TRUE, scale = TRUE)

# Plot
terra::plot(pca_var$env)

## ----prepare data pca external------------------------------------------------
# Prepare data for maxnet model using PCA variables
d_pca_extern <- prepare_data(algorithm = "maxnet",
                             occ = occ_data,
                             x = "x", y = "y",
                             raster_variables = pca_var$env,  # Output of perform_pca()
                             do_pca = FALSE,  # Set to FALSE because variables are PCs
                             species = "Myrcia hatschbachii",
                             categorical_variables = "SoilType", 
                             n_background = 1000, 
                             partition_method = "kfolds",
                             features = c("l", "q", "p", "lq", "lqp"),
                             r_multiplier = c(0.1, 1, 2, 3, 5))

# Check the object
print(d_pca_extern)

# Check formula grid
head(d_pca_extern$formula_grid)

## ----import enmeval_block, echo=FALSE-----------------------------------------
data(enmeval_block, package = "kuenm2")

## ----spatial block enmeval, eval=FALSE----------------------------------------
# # Install ENMeval if not already installed
# if(!require("ENMeval")){
#   install.packages("ENMeval")
# }
# 
# # Extract calibration data from the prepared_data object and
# # separate presence and background records
# calib_occ <- d$data_xy[d$calibration_data$pr_bg == 1, ]  # Presences
# calib_bg <- d$data_xy[d$calibration_data$pr_bg == 0, ]  # Background
# 
# # Apply spatial block partitioning using ENMeval
# enmeval_block <- ENMeval::get.block(occs = calib_occ, bg = calib_bg)
# 
# # Inspect the structure of the output
# str(enmeval_block)
# #> List of 2
# #>  $ occs.grp: num [1:51] 1 1 2 2 1 4 2 2 4 2 ...
# #>  $ bg.grp  : num [1:957] 2 4 4 1 3 3 3 1 1 3 ...

## ----index part kuenm2--------------------------------------------------------
str(d$part_data)

## ----transform enmeval_block--------------------------------------------------
# Identify unique spatial blocks
id_blocks <- sort(unique(unlist(enmeval_block)))

# Create a list of test indices for each spatial block
new_part_data <- lapply(id_blocks, function(i) {
  # Indices of occurrence records in group i
  rep_i_presence <- which(enmeval_block$occs.grp == i)
  
  # Indices of background records in group i
  rep_i_bg <- which(enmeval_block$bg.grp == i)
  
  # To get the right indices for background, 
  # we need to sum the total number of records to background indices
  rep_i_bg <- rep_i_bg + nrow(occ_data)
  
  # Combine presence and background indices for the test set
  c(rep_i_presence, rep_i_bg)
})

# Assign names to each partition
names(new_part_data) <- paste0("Partition_", id_blocks)

# Inspect the structure of the new partitioned data
str(new_part_data)

## ----replace part_data--------------------------------------------------------
# Replace the original partition data with the new spatial blocks
d_spatial_block <- d
d_spatial_block$part_data <- new_part_data

# Update the partitioning method to reflect the new approach
d_spatial_block$partition_method <- "Spatial block (ENMeval)"  # Any name works

# Print the updated prepared_data object
print(d_spatial_block)

## ----plot spatial block, fig.width = 9, fig.height = 4------------------------
# Explore data partitioning in geography
geo_block <- explore_partition_geo(d_spatial_block, raster_variables = var[[1]])

# Plot data partition in geography
terra::plot(geo_block[[c("Presence", "Background")]])

## ----fig.width = 8, fig.height = 3.5, out.width = "75%"-----------------------
# Run extrapolation risk analysis
mop_blocks <- explore_partition_extrapolation(data = d_spatial_block, 
                                              include_test_background = TRUE)

# Check some testing records with values outside training ranges
head(mop_blocks$Mop_results[mop_blocks$Mop_results$n_var_out > 1, ])

# Check simple extrapolation in geographic space
plot_explore_partition(explore_partition = mop_blocks, space = "G", 
                       type_of_plot = "simple")

# Now in environmental space
plot_explore_partition(explore_partition = mop_blocks, space = "E", 
                       type_of_plot = "simple", 
                       variables = c("bio_7", "bio_15"))

## ----import flexsdm_block, echo=FALSE-----------------------------------------
data(flexsdm_block, package = "kuenm2")

## ----spatial block flexsdm, eval=FALSE----------------------------------------
# # Install flexsdm if not already installed
# if (!require("flexsdm")) {
#   if (!require("remotes")) {
#     install.packages("remotes")
#   }
# 
#   remotes::install_github("sjevelazco/flexsdm")  # needs compilation tools to work
# }
# 
# # Combine calibration data with spatial coordinates
# calib_data <- cbind(d$data_xy, d$calibration_data)
# 
# # Split data in test and train using flexsdm
# flexsdm_block <- flexsdm::part_sblock(env_layer = var, data = calib_data,
#                                       x = "x", y = "y", pr_ab = "pr_bg",
#                                       min_res_mult = 1, max_res_mult = 500,
#                                       num_grids = 30, n_part = 4,
#                                       prop = 0.5)
# #> The following grid cell sizes will be tested:
# #> 0.17 | 3.03 | 5.9 | 8.77 | 11.64 | 14.51 | 17.37 | 20.24 | 23.11 | 25.98 |
# #> 28.84 | 31.71 | 34.58 | 37.45 | 40.32 | 43.18 | 46.05 | 48.92 | 51.79 |
# #> 54.66 | 57.52 | 60.39 | 63.26 | 66.13 | 68.99 | 71.86 | 74.73 | 77.6 |
# #> 80.47 | 83.33
# #>
# #> Creating basic raster mask...
# #>
# #> Searching for the optimal grid size...
# 
# # See the structure of the object
# str(flexsdm_block$part)
# #> Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1008 obs. of  4 variables:
# #>  $ x    : num  -51.3 -50.6 -49.3 -49.8 -50.2 ...
# #>  $ y    : num  -29 -27.6 -27.8 -26.9 -28.2 ...
# #>  $ pr_ab: num  1 1 1 1 1 1 1 1 1 1 ...
# #>  $ .part: int  3 1 3 2 3 3 1 4 4 3 ...

## ----transform flexsdm_block--------------------------------------------------
# Identify unique spatial blocks from flexsdm output
id_blocks <- sort(unique(flexsdm_block$part$.part))

# Create a list of test indices for each spatial block
new_part_data_flexsdm <- lapply(id_blocks, function(i) {
  # Indices of occurrences and background points in group i
  which(flexsdm_block$part$.part == i)
})

# Assign names to each partition partition
names(new_part_data_flexsdm) <- paste0("Partition_", id_blocks)

# Inspect the structure of the new partitioned data
str(new_part_data_flexsdm)

# Replace the partition data in the prepared_data object
d_block_flexsdm <- d
d_block_flexsdm$part_data <- new_part_data_flexsdm

# Update the partition method description
d_block_flexsdm$partition_method <- "Spatial block (flexsdm)"  # any name works

# Display the updated prepared_data object
print(d_block_flexsdm)

## ----plot spatial block flexsdm, fig.width = 9, fig.height = 4----------------
# Explore data partitioning in geography
geo_block_flexsdm <- explore_partition_geo(d_block_flexsdm, 
                                           raster_variables = var[[1]])

# Plot data partition in geography
terra::plot(geo_block_flexsdm[[c("Presence", "Background")]])

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 

## ----save data, eval=FALSE----------------------------------------------------
# # Set directory to save (here, in a temporary directory)
# dir_to_save <- file.path(tempdir())
# 
# # Save the data
# saveRDS(d, file.path(dir_to_save, "Data.rds"))
# 
# # Import data
# d <- readRDS(file.path(dir_to_save, "Data.rds"))


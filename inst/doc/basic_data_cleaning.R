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

## ----Import occurrence data---------------------------------------------------
# Import occurrences
data(occ_data_noclean, package = "kuenm2")

# Check data structure
str(occ_data_noclean)

## ----Load variables-----------------------------------------------------------
# Import raster layers
var <- terra::rast(system.file("extdata", "Current_variables.tif", 
                               package = "kuenm2"))

# Keep only one layer
var <- var$bio_1

# Check variable
terra::plot(var)

## ----visualize----------------------------------------------------------------
# Visualize occurrences on one variable
## Create an extent based on the layer and the records to see all errors
vext <- terra::ext(var)  # extent of layer
pext <- apply(occ_data_noclean[, 2:3], 2, range, na.rm = TRUE)  # extent of records

allext <- terra::ext(c(min(pext[1, 1], vext[1]), max(pext[2, 1], vext[2]), 
                       min(pext[1, 2], vext[3]), max(pext[2, 2], vext[4]))) + 1

# plotting records on the variable
terra::plot(var, ext = allext, main = "Bio 1")
points(occ_data_noclean[, c("x", "y")])

## ----remove missing-----------------------------------------------------------
# remove missing data
mis <- remove_missing(data = occ_data_noclean, columns = NULL, remove_na = TRUE,
                      remove_empty = TRUE)

# quick check
nrow(occ_data_noclean)
nrow(mis)

## ----remove duplicates--------------------------------------------------------
# remove exact duplicates
mis_dup <- remove_duplicates(data = mis, columns = NULL, keep_all_columns = TRUE)

# quick check
nrow(mis)
nrow(mis_dup)

## ----remove 00----------------------------------------------------------------
# remove records with 0 for x and y coordinates
mis_dup_00 <- remove_corrdinates_00(data = mis_dup, x = "x", y = "y")

# quick check
nrow(mis_dup)
nrow(mis_dup_00)

## ----filter decimal-----------------------------------------------------------
# remove coordinates with low decimal precision.
mis_dup_00_dec <- filter_decimal_precision(data = mis_dup_00, x = "x", y = "y", 
                                           decimal_precision = 2)

# quick check
nrow(mis_dup_00)
nrow(mis_dup_00_dec)

## ----all basic----------------------------------------------------------------
# all basic cleaning steps
clean_init <- initial_cleaning(data = occ_data_noclean, species = "species", 
                               x = "x", y = "y", remove_na = TRUE, 
                               remove_empty = TRUE, remove_duplicates = TRUE, 
                               by_decimal_precision = TRUE,
                               decimal_precision = 2)

# quick check
nrow(occ_data_noclean)  # original data
nrow(clean_init)  # data after all basic cleaning steps

# a final plot to check
par(mfrow = c(2, 2))

## initial data
terra::plot(var, ext = allext, main = "Initial data")
points(occ_data_noclean[, c("x", "y")])

## data after basic cleaning steps
terra::plot(var, ext = allext, main = "After basic cleaning")
points(clean_init[, c("x", "y")])

terra::plot(var, main = "After basic cleaning (zoom)")
points(clean_init[, c("x", "y")])

## ----cell duplicates----------------------------------------------------------
# exclude duplicates based on raster cell (pixel)
celldup <- remove_cell_duplicates(data = clean_init, x = "x", y = "y",
                                  raster_layer = var)

# quick check
nrow(clean_init)  # data after all basic cleaning steps
nrow(celldup)  # plus removing cell duplicates

## ----move records-------------------------------------------------------------
# move records to valid pixels
moved <- move_2closest_cell(data = celldup, x = "x", y = "y", 
                            raster_layer = var, move_limit_distance = 10)

# quick check
nrow(celldup)  # basic cleaning and no cell duplicates
nrow(moved[moved$condition != "Not_moved", ])  # plus moved to valid cells

## ----all advanced-------------------------------------------------------------
# move records to valid pixels
clean_data <- advanced_cleaning(data = clean_init, x = "x", y = "y", 
                                raster_layer = var, cell_duplicates = TRUE,
                                move_points_inside = TRUE, 
                                move_limit_distance = 10)

# exclude points not moved
clean_data <- clean_data[clean_data$condition != "Not_moved", 1:3]

# quick check
nrow(occ_data_noclean)  # original data
nrow(clean_init)  # data after all basic cleaning steps
nrow(clean_data)  # data after all basic cleaning steps

# a final plot to check
par(mfrow = c(3, 2))

## initial data
terra::plot(var, ext = allext, main = "Initial")
points(occ_data_noclean[, c("x", "y")])

## data after basic cleaning steps
terra::plot(var, ext = allext, main = "Basic cleaning")
points(clean_init[, c("x", "y")])

terra::plot(var, main = "Basic cleaning (zoom)")
points(clean_init[, c("x", "y")])

## data after basic cleaning steps
terra::plot(var, main = "Final data")
points(clean_data[, c("x", "y")])

## zoom to a particular area, initial data
terra::plot(var, xlim = c(-48, -50), ylim = c(-26, -25),  main = "Initial (zoom +)")
points(occ_data_noclean[, c("x", "y")])

## zoom to a particular area, final data
terra::plot(var, xlim = c(-48, -50), ylim = c(-26, -25),  main = "Final (zoom +)")
points(clean_data[, c("x", "y")])

## ----par_reset----------------------------------------------------------------
# Reset plotting parameters
par(original_par) 

## ----save data, eval=FALSE----------------------------------------------------
# # Save as CSV
# write.csv(clean_data, file = "Clean_data.csv", row.names = FALSE)
# 
# # Save as RDS
# saveRDS(clean_data, file = "Clean_data.rds")
# 


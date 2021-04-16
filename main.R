# Niek Koelewijn
# Registration number: 1039084
# Wageningen University and Research
# Advanced Earth Observation
# GRS-32306
# Comparison of tree metrics derived from MLS and UAV data
# Licence: GNU General Public Licence v3.0

# Install required packages
if(!"rgl" %in% rownames(installed.packages())){install.packages("rgl")}
if(!"circular" %in% rownames(installed.packages())){install.packages("circular")}
if(!"sp" %in% rownames(installed.packages())){install.packages("sp")}
if(!"raster" %in% rownames(installed.packages())){install.packages("raster")}
if(!"colorRamps" %in% rownames(installed.packages())){install.packages("colorRamps")}
if(!"lidR" %in% rownames(installed.packages())){install.packages("lidR")}

# Load required packages
library(rgl)
library(circular)
library(raster)
library(sp)
library(colorRamps)
library(lidR)

# Create input directory
if(!dir.exists(path = "input")) {
  dir.create(path = "input")
}

# Load trees
UAVTree1 <- readLAS("input/UAV_tree1.las")

# Set correct espg
epsg(UAVTree1) <- 28992

# Select only those points that are classified as medium vegetation
UAVTree1 <- filter_poi(UAVTree1, Classification == 4)

# Create table from X, Y & Z values of trees


## Estimate DBH ##

# DBH standard in the Netherlands is 1.3 meter
DBH_H <- 1.3 

# Create a ring of 5 cm around DBH height
DBH_min <- DBH.H-0.025
DBH_max <- DBH.H+0.025

# Take a subset of the tree at the DBH range
UAVTree1DBHSubset <- subset(UAVTree1, UAVTree1@data[["Z"]] > min(UAVTree1@data[["Z"]]) + DBH_min 
                            & UAVTree1@data[["Z"]] < min(UAVTree1@data[["Z"]]) + DBH_max)






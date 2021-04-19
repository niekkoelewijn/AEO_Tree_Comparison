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
if(!"plotrix" %in% rownames(installed.packages())){install.packages("plotrix")}

# Load required packages
library(rgl)
library(circular)
library(raster)
library(sp)
library(colorRamps)
library(lidR)
library(plotrix)

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
UAVTree1DF <- data.frame(UAVTree1@data)

## Estimate DBH ##

# DBH standard in the Netherlands is 1.3 meter
DBH_H <- 1.3 

# Create a ring of 5 cm around DBH height
DBH_min <- DBH_H-0.025
DBH_max <- DBH_H+0.025

# Take a subset of the tree at the DBH range
UAVTree1DBHSubset <- subset(UAVTree1DF, UAVTree1DF$Z > min(UAVTree1DF$Z) + DBH_min 
                            & UAVTree1DF$Z < min(UAVTree1DF$Z) + DBH_max)

# Take a look at the DBH point subset
plot3d(x=UAVTree1DF$X,y=UAVTree1DF$Y,z=UAVTree1DF$Z,col="lightgrey",asp=1)
plot3d(x=UAVTree1DBHSubset$X,y=UAVTree1DBHSubset$Y,z=UAVTree1DBHSubset$Z,col="red",add=T,size=10)

# Estimate DBH with the lsfit.circle function from the circular package
UAVTree1Circle1 <- lsfit.circle(x = UAVTree1DBHSubset$X, y = UAVTree1DBHSubset$Y)

# Store coefficients in separate variable
UAVTree1Circle1 <- UAVTree1Circle1$coefficients

# Visualise DBH subset and circle
# plot(x=UAVTree1DBHSubset[,1],
#     y=UAVTree1DBHSubset[,2],
#     col="grey",xlab="X in m",ylab="Y in m",
#     main=paste("UAV tree 1 - DBH",sep=" "),
#     xlim=c(min(UAVTree1DBHSubset[,1]),
#            max(UAVTree1DBHSubset[,1])),
#     ylim=c(min(UAVTree1DBHSubset[,2]),
#            max(UAVTree1DBHSubset[,2])),
#     asp=1)
# draw.circle(x=UAVTree1Circle1[2],y=UAVTree1Circle1[3],radius=UAVTree1Circle1[1],
#            lty=2,lwd=4,col=NA,border="red")

# Store DBH in a variable and remove heading
UAVTree1DBH <- UAVTree1Circle1[1] * 2
names(UAVTree1DBH) <- NULL

## Estimate tree height ##

# Derive minimal and maximal Z value of tree 
UAVTree1DFminZ <- min(UAVTree1DF$Z)
UAVTree1DFmaxZ <- max(UAVTree1DF$Z)

# Calculate height by substracting maximum from minimum
UAVTree1Height <- UAVTree1DFmaxZ - UAVTree1DFminZ

# Visualize tree and height
# plot(x=UAVTree1DF[,1],
#     y=UAVTree1DF[,3],
#     col="grey",xlab="X in m",ylab="Z in m",
#     main=paste("UAV tree 1 - Height",sep=" "),
#     xlim=c(min(UAVTree1DF[,1])-0.1,
#            max(UAVTree1DF[,1])+0.1),
#     ylim=c(min(UAVTree1DF[,3])-0.1,
#            max(UAVTree1DF[,3])+0.1),
#     asp=1)
# arrows(x0=min(UAVTree1DF[,1]),y0=min(UAVTree1DF[,3]),
#       x1=min(UAVTree1DF[,1]),y1=min(UAVTree1DF[,3]+UAVTree1Height),
#       length = 0.25, angle = 30,code=3,
#       col="red",lwd=4)



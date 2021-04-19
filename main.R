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


## Load trees ##

# Tree 1
UAVTree1 <- readLAS("input/UAV_tree1.las")

MLSTree1 <- readLAS("input/MLS_tree1.las")

# Tree 2
UAVTree2 <- readLAS("input/UAV_tree2.las")

MLSTree2 <- readLAS("input/MLS_tree2.las")


## Set correct espg ##

# Tree 1
epsg(UAVTree1) <- 28992

epsg(MLSTree1) <- 28992

# Tree 2
epsg(UAVTree2) <- 28992

epsg(MLSTree2) <- 28992


## Select only those points that are classified as 'medium vegetation', UAV data only ##

# Tree 1
UAVTree1 <- filter_poi(UAVTree1, Classification == 4)

# Tree 2
UAVTree2 <- filter_poi(UAVTree2, Classification == 4)


## Create table from X, Y & Z values of trees ##

# Tree 1
UAVTree1DF <- data.frame(UAVTree1@data)

MLSTree1DF <- data.frame(MLSTree1@data)

# Tree 2
UAVTree2DF <- data.frame(UAVTree2@data)

MLSTree2DF <- data.frame(MLSTree2@data)


### Estimate DBH ###


## DBH standard in the Netherlands is 1.3 meter ##
DBH_H <- 1.3 


## Create a ring of 5 cm around DBH height ##
DBH_min <- DBH_H-0.025
DBH_max <- DBH_H+0.025


## Take a subset of the tree at the DBH range ##

# Tree 1
UAVTree1DBHSubset <- subset(UAVTree1DF, UAVTree1DF$Z > min(UAVTree1DF$Z) + DBH_min 
                            & UAVTree1DF$Z < min(UAVTree1DF$Z) + DBH_max)

MLSTree1DBHSubset <- subset(MLSTree1DF, MLSTree1DF$Z > min(MLSTree1DF$Z) + DBH_min 
                           & MLSTree1DF$Z < min(MLSTree1DF$Z) + DBH_max)

# Tree 2
UAVTree2DBHSubset <- subset(UAVTree2DF, UAVTree2DF$Z > min(UAVTree2DF$Z) + DBH_min 
                            & UAVTree2DF$Z < min(UAVTree2DF$Z) + DBH_max)

MLSTree2DBHSubset <- subset(MLSTree2DF, MLSTree2DF$Z > min(MLSTree2DF$Z) + DBH_min 
                            & MLSTree2DF$Z < min(MLSTree2DF$Z) + DBH_max)


## Take a look at the DBH point subset ##
#plot3d(x=MLSTree2DF$X,y=MLSTree2DF$Y,z=MLSTree2DF$Z,col="lightgrey",asp=1)
#plot3d(x=MLSTree2DBHSubset$X,y=MLSTree2DBHSubset$Y,z=MLSTree2DBHSubset$Z,col="red",add=T,size=10)


## Estimate DBH with the lsfit.circle function from the circular package ##
## Store onlt coefficients in variable ##

# Tree 1
UAVTree1Circle <- lsfit.circle(x = UAVTree1DBHSubset$X, y = UAVTree1DBHSubset$Y)
UAVTree1Circle <- UAVTree1Circle$coefficients

MLSTree1Circle <- lsfit.circle(x = MLSTree1DBHSubset$X, y = MLSTree1DBHSubset$Y)
MLSTree1Circle <- MLSTree1Circle$coefficients

# Tree 2
UAVTree2Circle <- lsfit.circle(x = UAVTree2DBHSubset$X, y = UAVTree2DBHSubset$Y)
UAVTree2Circle <- UAVTree2Circle$coefficients

MLSTree2Circle <- lsfit.circle(x = MLSTree2DBHSubset$X, y = MLSTree2DBHSubset$Y)
MLSTree2Circle <- MLSTree2Circle$coefficients


# Visualise DBH subset and circle
# plot(x=UAVTree2DBHSubset[,1],
#     y=UAVTree2DBHSubset[,2],
#     col="grey",xlab="X in m",ylab="Y in m",
#     main=paste("UAV tree 2 - DBH",sep=" "),
#     xlim=c(min(UAVTree2DBHSubset[,1]),
#            max(UAVTree2DBHSubset[,1])),
#     ylim=c(min(UAVTree2DBHSubset[,2]),
#            max(UAVTree2DBHSubset[,2])),
#     asp=1)
# draw.circle(x=UAVTree2Circle[2],y=UAVTree2Circle[3],radius=UAVTree2Circle[1],
#            lty=2,lwd=4,col=NA,border="red")


## Store DBH in a variable and remove heading ##

# Tree 1
UAVTree1DBH <- UAVTree1Circle[1] * 2
names(UAVTree1DBH) <- NULL

MLSTree1DBH <- MLSTree1Circle[1] * 2
names(MLSTree1DBH) <- NULL

# Tree 2
UAVTree2DBH <- UAVTree2Circle[1] * 2
names(UAVTree2DBH) <- NULL

MLSTree2DBH <- MLSTree2Circle[1] * 2
names(MLSTree2DBH) <- NULL

### Estimate tree height ###


## Derive minimal and maximal Z value of tree ##

# Tree 1
UAVTree1DFminZ <- min(UAVTree1DF$Z)
UAVTree1DFmaxZ <- max(UAVTree1DF$Z)

MLSTree1DFminZ <- min(MLSTree1DF$Z)
MLSTree1DFmaxZ <- max(MLSTree1DF$Z)

# Tree 2
UAVTree2DFminZ <- min(UAVTree2DF$Z)
UAVTree2DFmaxZ <- max(UAVTree2DF$Z)

MLSTree2DFminZ <- min(MLSTree2DF$Z)
MLSTree2DFmaxZ <- max(MLSTree2DF$Z)


## Calculate height by substracting maximum from minimum ##

# Tree 1
UAVTree1Height <- UAVTree1DFmaxZ - UAVTree1DFminZ

MLSTree1Height <- MLSTree1DFmaxZ - MLSTree1DFminZ

# Tree 2
UAVTree2Height <- UAVTree2DFmaxZ - UAVTree2DFminZ

MLSTree2Height <- MLSTree2DFmaxZ - MLSTree2DFminZ


# Visualize tree and height
 plot(x=MLSTree2DF[,1],
     y=MLSTree2DF[,3],
     col="grey",xlab="X in m",ylab="Z in m",
     main=paste("MLS tree 2 - Height",sep=" "),
     xlim=c(min(MLSTree2DF[,1])-0.1,
            max(MLSTree2DF[,1])+0.1),
     ylim=c(min(MLSTree2DF[,3])-0.1,
            max(MLSTree2DF[,3])+0.1),
     asp=1)
 arrows(x0=min(MLSTree2DF[,1]),y0=min(MLSTree2DF[,3]),
       x1=min(MLSTree2DF[,1]),y1=min(MLSTree2DF[,3]+MLSTree2Height),
       length = 0.25, angle = 30,code=3,
       col="red",lwd=4)


### Store values in a DF
UAVHeightVector <- c(UAVTree1Height, UAVTree2Height)
MLSHeightVector <- c(MLSTree1Height, MLSTree2Height)
UAVDHBVector <- c(UAVTree1DBH, UAVTree2DBH)
MLSDHBVector <- c(MLSTree1DBH, MLSTree2DBH)


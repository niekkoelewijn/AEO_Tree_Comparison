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

# Tree 3
UAVTree3 <- readLAS("input/UAV_tree3.las")

MLSTree3 <- readLAS("input/MLS_tree3.las")

# Tree 4
UAVTree4 <- readLAS("input/UAV_tree4.las")

MLSTree4 <- readLAS("input/MLS_tree4.las")

# Tree 5
UAVTree5 <- readLAS("input/UAV_tree5.las")

MLSTree5 <- readLAS("input/MLS_tree5.las")

# Tree 6
UAVTree6 <- readLAS("input/UAV_tree6.las")

MLSTree6 <- readLAS("input/MLS_tree6.las")

# Tree 7
UAVTree7 <- readLAS("input/UAV_tree7.las")

MLSTree7 <- readLAS("input/MLS_tree7.las")

# Tree 8
UAVTree8 <- readLAS("input/UAV_tree8.las")

MLSTree8 <- readLAS("input/MLS_tree8.las")

# Tree 9
UAVTree9 <- readLAS("input/UAV_tree9.las")

MLSTree9 <- readLAS("input/MLS_tree9.las")

# Tree 10
UAVTree10 <- readLAS("input/UAV_tree10.las")

MLSTree10 <- readLAS("input/MLS_tree10.las")

# Tree 11
UAVTree11 <- readLAS("input/UAV_tree11.las")

MLSTree11 <- readLAS("input/MLS_tree11.las")

# Tree 12
UAVTree12 <- readLAS("input/UAV_tree12.las")

MLSTree12 <- readLAS("input/MLS_tree12.las")


## Set correct espg ##

# Tree 1
epsg(UAVTree1) <- 28992

epsg(MLSTree1) <- 28992

# Tree 2
epsg(UAVTree2) <- 28992

epsg(MLSTree2) <- 28992

# Tree 3
epsg(UAVTree3) <- 28992

epsg(MLSTree3) <- 28992

# Tree 4
epsg(UAVTree4) <- 28992

epsg(MLSTree4) <- 28992

# Tree 5
epsg(UAVTree5) <- 28992

epsg(MLSTree5) <- 28992

# Tree 6
epsg(UAVTree6) <- 28992

epsg(MLSTree6) <- 28992

# Tree 7
epsg(UAVTree7) <- 28992

epsg(MLSTree7) <- 28992

# Tree 8
epsg(UAVTree8) <- 28992

epsg(MLSTree8) <- 28992

# Tree 9
epsg(UAVTree9) <- 28992

epsg(MLSTree9) <- 28992

# Tree 10
epsg(UAVTree10) <- 28992

epsg(MLSTree10) <- 28992

# Tree 11
epsg(UAVTree11) <- 28992

epsg(MLSTree11) <- 28992

# Tree 12
epsg(UAVTree12) <- 28992

epsg(MLSTree12) <- 28992



## Create table from X, Y & Z values of trees ##

# Tree 1
UAVTree1DF <- data.frame(UAVTree1@data)

MLSTree1DF <- data.frame(MLSTree1@data)

# Tree 2
UAVTree2DF <- data.frame(UAVTree2@data)

MLSTree2DF <- data.frame(MLSTree2@data)

# Tree 3
UAVTree3DF <- data.frame(UAVTree3@data)

MLSTree3DF <- data.frame(MLSTree3@data)

# Tree 4
UAVTree4DF <- data.frame(UAVTree4@data)

MLSTree4DF <- data.frame(MLSTree4@data)


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

# Tree 3
UAVTree3DBHSubset <- subset(UAVTree3DF, UAVTree3DF$Z > min(UAVTree3DF$Z) + DBH_min 
                            & UAVTree3DF$Z < min(UAVTree3DF$Z) + DBH_max
                            & UAVTree3DF$Y > 445686)

MLSTree3DBHSubset <- subset(MLSTree3DF, MLSTree3DF$Z > min(MLSTree3DF$Z) + DBH_min 
                            & MLSTree3DF$Z < min(MLSTree3DF$Z) + DBH_max)

# Tree 4
UAVTree4DBHSubset <- subset(UAVTree4DF, UAVTree4DF$Z > min(UAVTree4DF$Z) + DBH_min 
                            & UAVTree4DF$Z < min(UAVTree4DF$Z) + DBH_max)

MLSTree4DBHSubset <- subset(MLSTree4DF, MLSTree4DF$Z > min(MLSTree4DF$Z) + DBH_min 
                            & MLSTree4DF$Z < min(MLSTree4DF$Z) + DBH_max
                            & MLSTree4DF$X > 177546.6)


## Take a look at the DBH point subset ##
#plot3d(x=UAVTree2DF$X,y=UAVTree2DF$Y,z=UAVTree2DF$Z,col="lightgrey",asp=1)
#plot3d(x=UAVTree2DBHSubset$X,y=UAVTree2DBHSubset$Y,z=UAVTree2DBHSubset$Z,col="red",add=T,size=10)


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

# Tree 3
UAVTree3Circle <- lsfit.circle(x = UAVTree3DBHSubset$X, y = UAVTree3DBHSubset$Y)
UAVTree3Circle <- UAVTree3Circle$coefficients

MLSTree3Circle <- lsfit.circle(x = MLSTree3DBHSubset$X, y = MLSTree3DBHSubset$Y)
MLSTree3Circle <- MLSTree3Circle$coefficients

# Tree 4
UAVTree4Circle <- lsfit.circle(x = UAVTree4DBHSubset$X, y = UAVTree4DBHSubset$Y)
UAVTree4Circle <- UAVTree4Circle$coefficients

MLSTree4Circle <- lsfit.circle(x = MLSTree4DBHSubset$X, y = MLSTree4DBHSubset$Y)
MLSTree4Circle <- MLSTree4Circle$coefficients


# Visualise DBH subset and circle
 plot(x=UAVTree2DBHSubset[,1],
     y=UAVTree2DBHSubset[,2],
     col="grey",xlab="X in m",ylab="Y in m",
     main=paste("UAV tree 4 - DBH",sep=" "),
     xlim=c(min(UAVTree2DBHSubset[,1]),
            max(UAVTree2DBHSubset[,1])),
     ylim=c(min(UAVTree2DBHSubset[,2]),
            max(UAVTree2DBHSubset[,2])),
     asp=1)
 draw.circle(x=UAVTree2Circle[2],y=UAVTree2Circle[3],radius=UAVTree2Circle[1],
            lty=2,lwd=4,col=NA,border="red")


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

# Tree 3
UAVTree3DBH <- UAVTree3Circle[1] * 2
names(UAVTree3DBH) <- NULL

MLSTree3DBH <- MLSTree3Circle[1] * 2
names(MLSTree3DBH) <- NULL

# Tree 4
UAVTree4DBH <- UAVTree4Circle[1] * 2
names(UAVTree4DBH) <- NULL

MLSTree4DBH <- MLSTree4Circle[1] * 2
names(MLSTree4DBH) <- NULL


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

# Tree 3
UAVTree3DFminZ <- min(UAVTree3DF$Z)
UAVTree3DFmaxZ <- max(UAVTree3DF$Z)

MLSTree3DFminZ <- min(MLSTree3DF$Z)
MLSTree3DFmaxZ <- max(MLSTree3DF$Z)

# Tree 4
UAVTree4DFminZ <- min(UAVTree4DF$Z)
UAVTree4DFmaxZ <- max(UAVTree4DF$Z)

MLSTree4DFminZ <- min(MLSTree4DF$Z)
MLSTree4DFmaxZ <- max(MLSTree4DF$Z)


## Calculate height by substracting maximum from minimum ##

# Tree 1
UAVTree1Height <- UAVTree1DFmaxZ - UAVTree1DFminZ

MLSTree1Height <- MLSTree1DFmaxZ - MLSTree1DFminZ

# Tree 2
UAVTree2Height <- UAVTree2DFmaxZ - UAVTree2DFminZ

MLSTree2Height <- MLSTree2DFmaxZ - MLSTree2DFminZ

# Tree 3
UAVTree3Height <- UAVTree3DFmaxZ - UAVTree3DFminZ

MLSTree3Height <- MLSTree3DFmaxZ - MLSTree3DFminZ

# Tree 4
UAVTree4Height <- UAVTree4DFmaxZ - UAVTree4DFminZ

MLSTree4Height <- MLSTree4DFmaxZ - MLSTree4DFminZ


# Visualize tree and height
 plot(x=UAVTree4DF[,1],
     y=UAVTree4DF[,3],
     col="grey",xlab="X in m",ylab="Z in m",
     main=paste("UAV tree 4 - Height",sep=" "),
     xlim=c(min(UAVTree4DF[,1])-0.1,
            max(UAVTree4DF[,1])+0.1),
     ylim=c(min(UAVTree4DF[,3])-0.1,
            max(UAVTree4DF[,3])+0.1),
     asp=1)
 arrows(x0=min(UAVTree4DF[,1]),y0=min(UAVTree4DF[,3]),
       x1=min(UAVTree4DF[,1]),y1=min(UAVTree4DF[,3]+UAVTree4Height),
       length = 0.25, angle = 30,code=3,
       col="red",lwd=4)


### Store values in a DF
UAVHeightVector <- c(UAVTree1Height, UAVTree2Height, UAVTree3Height,
                     UAVTree4Height)
MLSHeightVector <- c(MLSTree1Height, MLSTree2Height, MLSTree3Height,
                     MLSTree4Height)
UAVDBHVector <- c(UAVTree1DBH, UAVTree2DBH, UAVTree3DBH,
                  UAVTree4DBH)
MLSDBHVector <- c(MLSTree1DBH, MLSTree2DBH, MLSTree3DBH,
                  MLSTree4DBH)


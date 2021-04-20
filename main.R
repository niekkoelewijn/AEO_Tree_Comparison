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

# Tree 5
UAVTree5DF <- data.frame(UAVTree5@data)

MLSTree5DF <- data.frame(MLSTree5@data)

# Tree 6
UAVTree6DF <- data.frame(UAVTree6@data)

MLSTree6DF <- data.frame(MLSTree6@data)

# Tree 7
UAVTree7DF <- data.frame(UAVTree7@data)

MLSTree7DF <- data.frame(MLSTree7@data)

# Tree 8
UAVTree8DF <- data.frame(UAVTree8@data)

MLSTree8DF <- data.frame(MLSTree8@data)

# Tree 9
UAVTree9DF <- data.frame(UAVTree9@data)

MLSTree9DF <- data.frame(MLSTree9@data)

# Tree 10
UAVTree10DF <- data.frame(UAVTree10@data)

MLSTree10DF <- data.frame(MLSTree10@data)

# Tree 11
UAVTree11DF <- data.frame(UAVTree11@data)

MLSTree11DF <- data.frame(MLSTree11@data)

# Tree 12
UAVTree12DF <- data.frame(UAVTree12@data)

MLSTree12DF <- data.frame(MLSTree12@data)


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

# Tree 5
UAVTree5DBHSubset <- subset(UAVTree5DF, UAVTree5DF$Z > min(UAVTree5DF$Z) + DBH_min 
                            & UAVTree5DF$Z < min(UAVTree5DF$Z) + DBH_max)

MLSTree5DBHSubset <- subset(MLSTree5DF, MLSTree5DF$Z > min(MLSTree5DF$Z) + DBH_min 
                            & MLSTree5DF$Z < min(MLSTree5DF$Z) + DBH_max)

# Tree 6
UAVTree6DBHSubset <- subset(UAVTree6DF, UAVTree6DF$Z > min(UAVTree6DF$Z) + DBH_min 
                            & UAVTree6DF$Z < min(UAVTree6DF$Z) + DBH_max)

MLSTree6DBHSubset <- subset(MLSTree6DF, MLSTree6DF$Z > min(MLSTree6DF$Z) + DBH_min 
                            & MLSTree6DF$Z < min(MLSTree6DF$Z) + DBH_max)

# Tree 7
UAVTree7DBHSubset <- subset(UAVTree7DF, UAVTree7DF$Z > min(UAVTree7DF$Z) + DBH_min 
                            & UAVTree7DF$Z < min(UAVTree7DF$Z) + DBH_max)

MLSTree7DBHSubset <- subset(MLSTree7DF, MLSTree7DF$Z > min(MLSTree7DF$Z) + DBH_min 
                            & MLSTree7DF$Z < min(MLSTree7DF$Z) + DBH_max)

# Tree 8
UAVTree8DBHSubset <- subset(UAVTree8DF, UAVTree8DF$Z > min(UAVTree8DF$Z) + DBH_min 
                            & UAVTree8DF$Z < min(UAVTree8DF$Z) + DBH_max)

MLSTree8DBHSubset <- subset(MLSTree8DF, MLSTree8DF$Z > min(MLSTree8DF$Z) + DBH_min 
                            & MLSTree8DF$Z < min(MLSTree8DF$Z) + DBH_max)

# Tree 9
UAVTree9DBHSubset <- subset(UAVTree9DF, UAVTree9DF$Z > min(UAVTree9DF$Z) + DBH_min 
                            & UAVTree9DF$Z < min(UAVTree9DF$Z) + DBH_max)

MLSTree9DBHSubset <- subset(MLSTree9DF, MLSTree9DF$Z > min(MLSTree9DF$Z) + DBH_min 
                            & MLSTree9DF$Z < min(MLSTree9DF$Z) + DBH_max)

# Tree 10
UAVTree10DBHSubset <- subset(UAVTree10DF, UAVTree10DF$Z > min(UAVTree10DF$Z) + DBH_min 
                             & UAVTree10DF$Z < min(UAVTree10DF$Z) + DBH_max)

MLSTree10DBHSubset <- subset(MLSTree10DF, MLSTree10DF$Z > min(MLSTree10DF$Z) + DBH_min 
                             & MLSTree10DF$Z < min(MLSTree10DF$Z) + DBH_max)

# Tree 11
UAVTree11DBHSubset <- subset(UAVTree11DF, UAVTree11DF$Z > min(UAVTree11DF$Z) + DBH_min 
                             & UAVTree11DF$Z < min(UAVTree11DF$Z) + DBH_max)

MLSTree11DBHSubset <- subset(MLSTree11DF, MLSTree11DF$Z > min(MLSTree11DF$Z) + DBH_min 
                             & MLSTree11DF$Z < min(MLSTree11DF$Z) + DBH_max)

# Tree 12
UAVTree12DBHSubset <- subset(UAVTree12DF, UAVTree12DF$Z > min(UAVTree12DF$Z) + DBH_min 
                             & UAVTree12DF$Z < min(UAVTree12DF$Z) + DBH_max)

MLSTree12DBHSubset <- subset(MLSTree12DF, MLSTree12DF$Z > min(MLSTree12DF$Z) + DBH_min 
                             & MLSTree12DF$Z < min(MLSTree12DF$Z) + DBH_max)


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

# Tree 5
UAVTree5Circle <- lsfit.circle(x = UAVTree5DBHSubset$X, y = UAVTree5DBHSubset$Y)
UAVTree5Circle <- UAVTree5Circle$coefficients

MLSTree5Circle <- lsfit.circle(x = MLSTree5DBHSubset$X, y = MLSTree5DBHSubset$Y)
MLSTree5Circle <- MLSTree5Circle$coefficients

# Tree 6
UAVTree6Circle <- lsfit.circle(x = UAVTree6DBHSubset$X, y = UAVTree6DBHSubset$Y)
UAVTree6Circle <- UAVTree6Circle$coefficients

MLSTree6Circle <- lsfit.circle(x = MLSTree6DBHSubset$X, y = MLSTree6DBHSubset$Y)
MLSTree6Circle <- MLSTree6Circle$coefficients

# Tree 7
UAVTree7Circle <- lsfit.circle(x = UAVTree7DBHSubset$X, y = UAVTree7DBHSubset$Y)
UAVTree7Circle <- UAVTree7Circle$coefficients

MLSTree7Circle <- lsfit.circle(x = MLSTree7DBHSubset$X, y = MLSTree7DBHSubset$Y)
MLSTree7Circle <- MLSTree7Circle$coefficients

# Tree 8
UAVTree8Circle <- lsfit.circle(x = UAVTree8DBHSubset$X, y = UAVTree8DBHSubset$Y)
UAVTree8Circle <- UAVTree8Circle$coefficients

MLSTree8Circle <- lsfit.circle(x = MLSTree8DBHSubset$X, y = MLSTree8DBHSubset$Y)
MLSTree8Circle <- MLSTree8Circle$coefficients

# Tree 9
UAVTree9Circle <- lsfit.circle(x = UAVTree9DBHSubset$X, y = UAVTree9DBHSubset$Y)
UAVTree9Circle <- UAVTree9Circle$coefficients

MLSTree9Circle <- lsfit.circle(x = MLSTree9DBHSubset$X, y = MLSTree9DBHSubset$Y)
MLSTree9Circle <- MLSTree9Circle$coefficients

# Tree 10
UAVTree10Circle <- lsfit.circle(x = UAVTree10DBHSubset$X, y = UAVTree10DBHSubset$Y)
UAVTree10Circle <- UAVTree10Circle$coefficients

MLSTree10Circle <- lsfit.circle(x = MLSTree10DBHSubset$X, y = MLSTree10DBHSubset$Y)
MLSTree10Circle <- MLSTree10Circle$coefficients

# Tree 11
UAVTree11Circle <- lsfit.circle(x = UAVTree11DBHSubset$X, y = UAVTree11DBHSubset$Y)
UAVTree11Circle <- UAVTree11Circle$coefficients

MLSTree11Circle <- lsfit.circle(x = MLSTree11DBHSubset$X, y = MLSTree11DBHSubset$Y)
MLSTree11Circle <- MLSTree11Circle$coefficients

# Tree 12
UAVTree12Circle <- lsfit.circle(x = UAVTree12DBHSubset$X, y = UAVTree12DBHSubset$Y)
UAVTree12Circle <- UAVTree12Circle$coefficients

MLSTree12Circle <- lsfit.circle(x = MLSTree12DBHSubset$X, y = MLSTree12DBHSubset$Y)
MLSTree12Circle <- MLSTree12Circle$coefficients

# Visualise DBH subset and circle
#plot(x=UAVTree10DBHSubset[,1],
#     y=UAVTree10DBHSubset[,10],
#     col="grey",xlab="X in m",ylab="Y in m",
#     main=paste("UAV tree 10 - DBH",sep=" "),
#     xlim=c(min(UAVTree10DBHSubset[,1]),
#            max(UAVTree10DBHSubset[,1])),
#     ylim=c(min(UAVTree10DBHSubset[,2]),
#            max(UAVTree10DBHSubset[,2])),
#     asp=1)
#draw.circle(x=UAVTree10Circle[10],y=UAVTree10Circle[3],radius=UAVTree10Circle[1],
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

# Tree 5
UAVTree5DBH <- UAVTree5Circle[1] * 2
names(UAVTree5DBH) <- NULL

MLSTree5DBH <- MLSTree5Circle[1] * 2
names(MLSTree5DBH) <- NULL

# Tree 6
UAVTree6DBH <- UAVTree6Circle[1] * 2
names(UAVTree6DBH) <- NULL

MLSTree6DBH <- MLSTree6Circle[1] * 2
names(MLSTree6DBH) <- NULL

# Tree 7
UAVTree7DBH <- UAVTree7Circle[1] * 2
names(UAVTree7DBH) <- NULL

MLSTree7DBH <- MLSTree7Circle[1] * 2
names(MLSTree7DBH) <- NULL

# Tree 8
UAVTree8DBH <- UAVTree8Circle[1] * 2
names(UAVTree8DBH) <- NULL

MLSTree8DBH <- MLSTree8Circle[1] * 2
names(MLSTree8DBH) <- NULL

# Tree 9
UAVTree9DBH <- UAVTree9Circle[1] * 2
names(UAVTree9DBH) <- NULL

MLSTree9DBH <- MLSTree9Circle[1] * 2
names(MLSTree9DBH) <- NULL

# Tree 10
UAVTree10DBH <- UAVTree10Circle[1] * 2
names(UAVTree10DBH) <- NULL

MLSTree10DBH <- MLSTree10Circle[1] * 2
names(MLSTree10DBH) <- NULL

# Tree 11
UAVTree11DBH <- UAVTree11Circle[1] * 2
names(UAVTree11DBH) <- NULL

MLSTree11DBH <- MLSTree11Circle[1] * 2
names(MLSTree11DBH) <- NULL

# Tree 12
UAVTree12DBH <- UAVTree12Circle[1] * 2
names(UAVTree12DBH) <- NULL

MLSTree12DBH <- MLSTree12Circle[1] * 2
names(MLSTree12DBH) <- NULL


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

# Tree 5
UAVTree5DFminZ <- min(UAVTree5DF$Z)
UAVTree5DFmaxZ <- max(UAVTree5DF$Z)

MLSTree5DFminZ <- min(MLSTree5DF$Z)
MLSTree5DFmaxZ <- max(MLSTree5DF$Z)

# Tree 6
UAVTree6DFminZ <- min(UAVTree6DF$Z)
UAVTree6DFmaxZ <- max(UAVTree6DF$Z)

MLSTree6DFminZ <- min(MLSTree6DF$Z)
MLSTree6DFmaxZ <- max(MLSTree6DF$Z)

# Tree 7
UAVTree7DFminZ <- min(UAVTree7DF$Z)
UAVTree7DFmaxZ <- max(UAVTree7DF$Z)

MLSTree7DFminZ <- min(MLSTree7DF$Z)
MLSTree7DFmaxZ <- max(MLSTree7DF$Z)

# Tree 8
UAVTree8DFminZ <- min(UAVTree8DF$Z)
UAVTree8DFmaxZ <- max(UAVTree8DF$Z)

MLSTree8DFminZ <- min(MLSTree8DF$Z)
MLSTree8DFmaxZ <- max(MLSTree8DF$Z)

# Tree 9
UAVTree9DFminZ <- min(UAVTree9DF$Z)
UAVTree9DFmaxZ <- max(UAVTree9DF$Z)

MLSTree9DFminZ <- min(MLSTree9DF$Z)
MLSTree9DFmaxZ <- max(MLSTree9DF$Z)

# Tree 10
UAVTree10DFminZ <- min(UAVTree10DF$Z)
UAVTree10DFmaxZ <- max(UAVTree10DF$Z)

MLSTree10DFminZ <- min(MLSTree10DF$Z)
MLSTree10DFmaxZ <- max(MLSTree10DF$Z)

# Tree 11
UAVTree11DFminZ <- min(UAVTree11DF$Z)
UAVTree11DFmaxZ <- max(UAVTree11DF$Z)

MLSTree11DFminZ <- min(MLSTree11DF$Z)
MLSTree11DFmaxZ <- max(MLSTree11DF$Z)

# Tree 12
UAVTree12DFminZ <- min(UAVTree12DF$Z)
UAVTree12DFmaxZ <- max(UAVTree12DF$Z)

MLSTree12DFminZ <- min(MLSTree12DF$Z)
MLSTree12DFmaxZ <- max(MLSTree12DF$Z)


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

# Tree 5
UAVTree5Height <- UAVTree5DFmaxZ - UAVTree5DFminZ

MLSTree5Height <- MLSTree5DFmaxZ - MLSTree5DFminZ

# Tree 6
UAVTree6Height <- UAVTree6DFmaxZ - UAVTree6DFminZ

MLSTree6Height <- MLSTree6DFmaxZ - MLSTree6DFminZ

# Tree 7
UAVTree7Height <- UAVTree7DFmaxZ - UAVTree7DFminZ

MLSTree7Height <- MLSTree7DFmaxZ - MLSTree7DFminZ

# Tree 8
UAVTree8Height <- UAVTree8DFmaxZ - UAVTree8DFminZ

MLSTree8Height <- MLSTree8DFmaxZ - MLSTree8DFminZ

# Tree 9
UAVTree9Height <- UAVTree9DFmaxZ - UAVTree9DFminZ

MLSTree9Height <- MLSTree9DFmaxZ - MLSTree9DFminZ

# Tree 10
UAVTree10Height <- UAVTree10DFmaxZ - UAVTree10DFminZ

MLSTree10Height <- MLSTree10DFmaxZ - MLSTree10DFminZ

# Tree 11
UAVTree11Height <- UAVTree11DFmaxZ - UAVTree11DFminZ

MLSTree11Height <- MLSTree11DFmaxZ - MLSTree11DFminZ

# Tree 12
UAVTree12Height <- UAVTree12DFmaxZ - UAVTree12DFminZ

MLSTree12Height <- MLSTree12DFmaxZ - MLSTree12DFminZ


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


### Store values in a DF ###
UAVHeightVector <- c(UAVTree1Height, UAVTree2Height, UAVTree3Height,
                     UAVTree4Height, UAVTree5Height, UAVTree6Height,
                     UAVTree7Height, UAVTree8Height, UAVTree9Height,
                     UAVTree10Height, UAVTree11Height, UAVTree12Height)
MLSHeightVector <- c(MLSTree1Height, MLSTree2Height, MLSTree3Height,
                     MLSTree4Height, MLSTree5Height, MLSTree6Height,
                     MLSTree7Height, MLSTree8Height, MLSTree9Height,
                     MLSTree10Height, MLSTree11Height, MLSTree12Height)
UAVDBHVector <- c(UAVTree1DBH, UAVTree2DBH, UAVTree3DBH,
                  UAVTree4DBH, UAVTree5DBH, UAVTree6DBH,
                  UAVTree7DBH, UAVTree8DBH, UAVTree9DBH,
                  UAVTree10DBH, UAVTree11DBH, UAVTree12DBH)
MLSDBHVector <- c(MLSTree1DBH, MLSTree2DBH, MLSTree3DBH,
                  MLSTree4DBH, MLSTree5DBH, MLSTree6DBH,
                  MLSTree7DBH, MLSTree8DBH, MLSTree9DBH,
                  MLSTree10DBH, MLSTree11DBH, MLSTree12DBH)


### Statistical Analysis

# 


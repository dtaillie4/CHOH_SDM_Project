############### Dylan Taillie ###############
############### generation of new raster for c&o canal ###############

### set working directory
setwd('/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/')
install.packages("leaflet")
### load libraries
library(raster)
library(sf)
library(ggplot2)
library(tidyverse)
library(rgdal)
library(leaflet)


### read in current ForestClassification raster 
forestClassOrig <- raster("forestClass.tif")

### or actually read it in as a shapefile?? 



### read in management option shapefile 
agshp <- st_read('layers_nad83/AG_Option_1.shp')
agshp2 <- st_read('layers_nad83/AG_Option_1_project.shp')
agField <- raster('layers_nad83/Ag_Option_30_ProjectRaster1.tif')
View(agshp)
### do they match up? plotting to check 

plot(forestClassOrig)
plot(agshp$geometry, add = TRUE, col = "red", legend = FALSE)
plot(agField, add = TRUE, col = "red", legend = FALSE)
crs(forestClassOrig)
crs(agField)

?spTransform
prj <- crs(forestClassOrig)

agField <- st_transform(agField, proj4string(forestClassOrig))
agshp <- st_transform(agshp$geometry, proj4string(forestClassOrig))

### checking the projection after transforming the CRS
crs(agshp)
projection(agshp)
### nothing shows up but the function worked so... 


agshp$value <- NA
View(agshp2)

### looks good. now to create a raster mask  
?mask

newforestClass <- mask(x = forestClassOrig, mask = agshp)
plot(masked, col = "red")
extent(masked)
extent(forestClassOrig)
View(masked)

### below is an example of using overlay to 

forestClass_mod <- overlay(forestClassOrig, masked, fun = function(x, y) {
  x[!is.na(y[])] <- 411
  return(x)
})

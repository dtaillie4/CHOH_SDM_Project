library(raster)
library(rgdal)
library(sp)

setwd("/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/")
chMD <- raster("layers_nad83/CanHeight_NAD83_April15_MD.tif")

#this a 30m raster of Canopy height in Maryland, already cropped to the
#counties bordering the C&O Canal

#remove(list=ls())

######### Forest Classiffiction for C&O counties in MD #########

###################### RECLASS CANOPY HEIGHT ###################################
# CH raster from UMD, height is meters*10
#already ingested above, just renaming to Matts convention of naming
canopyHeight <- chMD
?cellStats
# break canopyHeight into height classes following Dickinson et al. (2014)
#o to .3 is reclassed as "0", 0.3 to 9.1 reclassed as "1", and so on...
reClass <- c(0, 0.3, 0, 
             0.3, 9.1, 1, 
             9.1, 19.8, 2, 
             19.8, 24.4, 3, 
             24.4, cellStats(canopyHeight, max), 4)

reClass <- matrix(reClass, ncol=3, byrow=T)

# reclassify to height classes (div by 10 for correct units) & save

ch <- reclassify(canopyHeight/10, reClass, include.lowest=T)
writeRaster(ch, "/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/forest_classification_outputs/chClasses.tif",
            overwrite=T)

###################### RECLASS CANOPY HEIGHT ###################################


########################### RECLASS NLCD #####################################
# nlcd 2011 raster 
nlcd <- raster("layers_nad83/nlcd_2011_landcover_2011_nad83.tif")
# retain / modify vegetation class IDs, set all non-veg classes -> 0
reClass <- c(9, 32, 0, # water, developed, barren -> 0
             40.5, 41.5, 410, # deciduous forests -> 410
             41.6, 42.5, 420, # evergreen foresst -> 420
             42.6, 43.5, 430, # mixed forests -> 430
             50, 53, 500, # shrublands -> 500
             70, 75, 700, # grasslands -> 700
             80.5, 82.5, 800, # crops -> 800
             89.5, 95.5, 900) # wetlands -> 900
reClass <- matrix(reClass, ncol=3, byrow=T)
View(reClass)
vegClass <- reclassify(nlcd, reClass)
writeRaster(vegClass, "/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/forest_classification_outputs/vegClass.tif",
            overwrite=T)
plot(vegClass)
plot(ch)
########################### RECLASS NLCD #####################################


#################### Combine CH and veg rasters ##############################

#ch <- crop(ch, vegClass)
#tried cropping vegClass to ch above, but still getting the same error. crop function doesn't
#seem to change extents when looking at extent(ch) and extent(vegCLass) after cropping
# alignExtent(vegClass, ch) doesn't work
#show(vegClassRe)
#resampling the vegClass layer so that these two can be combined 
?resample
vegClassRe <- resample(vegClass, ch, method = "ngb")
plot(vegClassRe)
chPlusVeg <- vegClassRe+ch 
View(chPlusVeg)
writeRaster(chPlusVeg, "/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/forest_classification_outputs/chPlusVeg.tif", 
            overwrite=TRUE)

# resulting classes
classFreq <- table(chPlusVeg[])
print(data.frame(freq=classFreq, precent=round(as.numeric(classFreq)/ncell(chPlusVeg)*100, 2)))

# 411, 421, 431 -> good, forest w/ CH 0.3 - 9.1m 
# 412, 422, 432 -> good, forest w/ CH 9.1 - 19.8m
# 413, 423, 433 -> good, forest w/ CH 19.8 - 24.4m
# 414, 424, 434 -> good, forest w/ CH > 24.4m
# 501 -> good, shrublands w/ CH 0.3 - 9.1m
# 701 -> good, grasslands w/ CH 0.3 - 9.1m
# 801 -> good, crops w/ CH 0.3 - 9.1m
# 901 -> good, wetlands w/ CH 0.3 - 9.1m
# most other classes require modification as outlined below

#################### Combine CH and veg rasters ##############################

#######Dylan stopped here on June 13th##############################

################## Reclass to final forest classification ####################
#places NLCD suggested land cover was forest, but actually no canopy height showing up here
# reclass problematic CH-veg combos, using table 
# in Dickinson et al. (2014) as guide
# 100 class is for nlcd vegetation classes, but no CH returns > 0.3 m 
# In essence, class 100 might be considered managed vegetation.
reClass <- c(0, 4.5, 0, # non-veg classes (buildings, water, etc) across CH classes
             409, 410.5, 100, # 410 (nlcd deciduous), no returns > 0.3m -> 100
             419, 420.5, 100, # 420 (nlcd evergreen), no returns > 0.3m -> 100 
             429, 430.5, 100, # 430 (nlcd mixed), no returns > 0.3m -> 100
             499, 500.5, 100, # 500 (nlcd shrublands), no returns > 0.3m -> 100
             699, 700.5, 100, # 700 (nlcd grasslands), no returns > 0.3m -> 100
             799, 800.5, 100, # 800 (nlcd crops), no returns > 0.3m -> 100
             899, 900.5, 100, # 900 (nlcd wetlands), no returns > 0.3m -> 100
             
             # The remainder of these are non-forest nlcd classes
             # but with CH returns that imply
             # forest/vegetation. AJE and MCF decided to 
             # classify all these as mixed forest (430)
             # of the appropriate CH class (432, 433, or 434)
             
             # shrublands
             501.5, 502.5, 432, # 502 = nlcd shrublands with CH returns 9.1 - 19.8m 
             502.6, 503.5, 433, # 503 = nlcd shrublands with CH returns 19.8 - 24.4m
             503.6, 504.5, 434, # 504 = nlcd shrublands with CH returns > 24.4m
             
             # grasslands
             701.5, 702.5, 432, # 702 = nlcd grasslands with CH returns 9.1 - 19.8m
             702.6, 703.5, 433, # 703 = nlcd grasslands with CH returns 19.8 - 24.4m
             703.6, 704.5, 434, # 704 = nlcd grasslands with CH returns > 24.4m
             
             # crops
             801.5, 802.5, 432, # 802 = nlcd crops with CH returns 9.1 - 19.8m
             802.6, 803.5, 433, # 803 = nlcd crops with CH returns 19.8 -24.4m
             803.6, 804.5, 434, # 804 = nlcd crops with CH returns > 24.4m
             
             # wetlands
             901.5, 902.5, 432, # 902 = nlcd wetlands with CH returns 9.1 - 19.8m
             902.6, 903.5, 433, # 903 = nlcd wetlands with CH returns 19.8 - 24.4m
             903.6, 904.5, 434) # 904 = nlcd wetlands with CH returns > 24.4m

reClass <- matrix(reClass, ncol=3, byrow=T)
forestClass <- reclassify(chPlusVeg, reClass)

# review classification
classFreq <- table(forestClass[])
print(data.frame(freq=classFreq, precent=round(as.numeric(classFreq)/ncell(chPlusVeg)*100, 2)))

# write to file
writeRaster(forestClass, "/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/forest_classification_outputs/forestClass.tif",
            overwrite=T)
writeRaster(forestClass, "/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/forestClass.tif",
            overwrite=T)
plot(forestClass)
################## Reclass to final forest classification ####################


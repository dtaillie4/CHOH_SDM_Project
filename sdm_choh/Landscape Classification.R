library(raster)
library(rgdal)
library(party)
library(foreign)
library(car)
library(usdm)
library(sp)
library(dismo)
library(maptools)
library(plyr)
library(FedData)
library(svglite)
library(tidyr)
library(ggplot2)
install.packages("svglite")

#reading in TIF layers that I want to combine into one 'forested landscape' layer 
#no LANDFIRE data yet - might not use except for WV canopy height situation
setwd("/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project")
getwd()

#remove(list=ls())
#dev.off()

#read in tifs of env data as rasters 
chALLmd <- raster("layers_nad83/Maryland Canopy Height - Image Service.tiff")
plot(chALLmd)
chMD <- raster("layers_nad83/CanHeight_NAD83_April15_MD.tif")
bioMD <- raster("layers_nad83/biomd_west1.tif")
nlcdMD <- raster("layers_nad83/nlcd_2011_landcover_2011_nad83.tif")
forestClass <- raster("forest_classification_outputs/forestClass.tif")
plot(forestClass)
chMD <- chMD/10
plot(chMD)
plot(chMD, col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(50))


#trying to create more discrete forest patches using the standard deviation 
#of canopy heights within a 3x3 matrix of each cell to better get an idea of
#patch structure by height 
rm <- matrix(c(1,1,1,1,1,1,1,1,1), nrow = 3) 
CHinterp <- focal(chMD, w = rm, method = "sdev")
CHinterpM <- CHinterp/10
plot(CHinterp, col = colorRampPalette(c("white", "lightgreen", "darkgreen"))(50))
plot(CHinterpM)
# interesting - makes it look like there is a lot more forest than there
# actually is - just unifying the patches?
plot(chMD)


#understanding distribution of values in canopy height layer 
hist(chMD,
     main = "Distribution of canopy height values",
     xlab = "Canopy Height (meters)", ylab = "Frequency",
     col = "darkgreen")

hist(bioMD, 
     maxpixels = 10000000000,
     main = "Distribution of biomass values",
     xlab = "Biomass", ylab = "Frequency",
     col = "lightgreen")
?hist
ForestClassHist1 <- hist(forestClass,
     maxpixels = 10000000000,
     main = "Distribution of Forest Classes",
     xlab = "classification", ylab = "Frequency",
     xlim = c(409, 435),
     breaks = c(0,120,410,411,412,413,414,420,421,422,423,424,430,431,432,433,434,435,920),
     col = "darkgreen")
?ggsave

ggplot(forestClass) 

head(forestClass)
ggsave("ForestClassHist1.svg", device = "svg", width = 10, height = 8)


NLCD <- as.matrix(table(raster::values(nlcdMD)))
cols <- dplyr::filter(pal_nlcd(), code %in% row.names(NLCD))
barplot(NLCD, beside = FALSE, col = cols$color) 
legend("bottom", legend = cols$description, fill = cols$color, 
       ncol = 2, inset = c(0, -0.6))
plot(nlcdMD, col = cols$color)
View(cols)


brks <- seq(0,50,by=1)
nbrks <- length(brks)-1
wg <- colorRampPalette(c("white", "green", "darkgreen"),(nbrks))
?colorRampPalette

#reading in and plotting management layer 

management_albers <- raster("layers_nad83/Ag_Option_30_ProjectRaster1.tif")

plot(chMD)
plot(management_albers, add = TRUE, col = "purple")
manMask <- management_albers[1,] == 81 

management_albers[1,]
View(management_albers)



?mask
plot(management, col = "purple")
#resampling and stacking 
bioCO <- resample(bioMD, chMD, method = "bilinear")
nlcdCO <- resample(nlcdMD, chMD, method = "bilinear")
plot(nlcdCO)
plot(bioCO)

#classify forests 





library(auk)
library(raster)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(tigris)
library(viridisLite)
library(rgdal)

####### possibly use Chestnut Sided Warbler instead of GOWW #########################
################### Loading rasters I want to plot ebird data over ###########################
setwd("/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project")
getwd()

chMD <- raster("layers_nad83/CanHeight_NAD83_April15_MD.tif")
plot(chMD)
plot(chMD, col = colorRampPalette(c("white","lightgreen", "darkgreen"))) ((50))
remove(list=ls())
dev.off()
################### Loading rasters I want to plot ebird data over ###########################

################### importing golden wing, cerulean, and wood thrush #########################

### these are specific species I chose and asked ebird to extract for me. 
### may redo extraction with all eastern US ebird data, choose a new 
### species that may work as a proxy for golden wing warbler who isn't as rare

goww <- read_ebd("ebird/ebd_gowwar_relMar-2019/ebd_gowwar_relMar-2019.txt")
cerw <- read_ebd("ebird/ebd_cerwar_relMar-2019/ebd_cerwar_relMar-2019.txt")
woth <- read_ebd("ebird/ebd_woothr_relMar-2019/ebd_woothr_relMar-2019.txt")
allpa <- read.csv("ebird/rawBirdTable_ORIGINAL.csv", header = TRUE)
plot(chMD)


################# data from NPS inventory and monitoring network for birds ###################

choh_birds <- read.csv("choh/CHOH_BIRDS.csv", header = TRUE)
#View(choh_birds)
coordinates(choh_birds) <- c("long","lat")
proj4string(choh_birds) <- CRS("+proj=longlat +ellps=clrk66")
choh.utm <- spTransform(choh_birds, CRS(chMD))
points(choh.utm, pch = 19, col = "darkgreen")
points(choh_birds$lat, choh_birds$long, pch = 19)
choh_birds <- as.matrix(choh_birds)
?spTransform
choh_birds_transform <- spTransform(choh_birds, CRSobj = (proj4string(chMD)))
points(choh_birds, y = choh_birds$lat, x = choh_birds$long, pch = 19)
?points
points (allpa, x = "lat" , y ="long", pch = 19)
?read.csv


################### importing golden wing, cerulean, and wood thrush #########################

#changing the sheets (.txt) to spatial objects and reporjecting in UTM 
#golden wing warbler
coordinates(goww) <- c("longitude","latitude")
proj4string(goww) <- CRS("+proj=longlat +ellps=clrk66")
goww.utm <- spTransform(goww, CRS("+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9998983998 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
points(goww.utm, pch = 18, col = "orange")
#cerulean
coordinates(cerw) <- c("longitude","latitude")
proj4string(cerw) <- CRS("+proj=longlat +ellps=clrk66")
cerw.utm <- spTransform(cerw, CRS("+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9998983998 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
points(cerw.utm, pch = 18, col = "darkblue")
#wood thrush
coordinates(woth) <- c("longitude","latitude")
proj4string(woth) <- CRS("+proj=longlat +ellps=clrk66")
woth.utm <- spTransform(woth, CRS("+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9998983998 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
points(woth.utm, pch = 18, col = "brown")
?points

#reprojecting to the exact same current projection of chMD my canopy height layer 
woth.equal <- spTransform(woth, CRS("+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9998983998 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs +towgs84=0,0,0"))
points(woth.equal, pch = 18, col = "red")
plot(woth.utm)
View(chMD)

str(chMD)
#I don't think these are matching up, I think I must have the wrong CRS infromation for the
#spTransform function 
#look into how my canopy height layer is projected and reproject bird point 
#locations in the same way. Taking a break on this for a little while to work on 
#classifying my forest types 


crs(goww.utm) <- CRS("+init=espg:2248")
points(goww.utm)
proj4string(chMD) <- CRS(paste("+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9998983998 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
plot(goww.utm)
coordnames(goww.utm)
points(goww2 $latitude, goww2 $longitude)
points(goww.utm, coords)
glimpse(goww.utm)
?points
#plot(goww.utm)

#chunk below is just messing with raw EBird Data - not necessary for my uses right now 
#in the auk instructions (#1-2), they download overall data sets and then 
#perform an extraction for each bird type they want 
#since I pre-filtered my data via ebird to just the species I want/location I want, 
#I can start at #3 on the instructions 
#sampling event dat, needed in order to filter 
ebd_sampling <- ("ebd_sampling_relMar-2019/ebd_sampling_relMar-2019.txt")

#importing without species rollup - don't need to do this but just learning auk package.
goww_raw <- read_ebd("ebd_gowwar_relMar-2019/ebd_gowwar_relMar-2019.txt", unique = FALSE,
                     rollup = FALSE)
#looking at difference in datasets 
#goww$category
#goww_raw$category
#glimpse(goww)
#zero filling


?spTransform  



goww_sf <- goww %>% 
  select(common_name, latitude, longitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269)
plot(chMD)
plot(goww_sf, pch = 1, add = TRUE)
?sf

raster::plot(goww$longitude, goww$latitude, ext = newext, pch = 18, col = "gold")
newext <- drawExtent()
?plot

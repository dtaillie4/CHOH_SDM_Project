#matts code with MD data 


#landscape metrics code from Steven 
#takes a long time for all of PA 
#ask for a few cores to run mine - start with not all of the different radii, 
#what fraction of area within 200m of each cell is deciduous <9m? (% of landscape in class in distance)
#base class - control landscape 
#modifications - what IF NPS does *this*?
#what do these experimental landscapes look like and how do they influence bird distribution/habitat?
#need to re-run landscape metrics 
#don't need to be changes in the whole area - buffer this by the largest radius i run landscape metrics by
#each of test landscapes need ot be buffered by 5km if 5km is the largest radius done in landsacpe metrics

#impact of feasible changes - can you really stem the tide? 

#viualization of distribution of types of forest (as classified by us)
library(ggplot2)
library(raster)
library(rgdal)
library(tidyverse)
library(stringr)
library(tiff)
library(sf)


#run this first 
#setwd('/Users/dtaillie/Documents/Projects/C&O Canal/GitHubFolder/CHOH_SDM_Project/')

GDALinfo("forestClass.tif")
forestClass <- raster("forestClass.tif")
forestClass_df <- as.data.frame(forestClass, xy = TRUE)
summary(forestClass)


str(forestClass_df)
View(forestClass_df$forestClass)
freq(forestClass)
# crop designation divided by 414 
112675/1656285
#1%? This isn't right. Much more cropland in this area.Check ForestClassification
raster::unique(forestClass)

??frequency

ggplot() +
  geom_raster(data = forestClass_df , aes(x = x, y = y, fill = forestClass )) +
  scale_fill_viridis_c() +
  coord_quickmap()

ggplot()+
  geom_histogram(data  = forestClass_df, aes (forestClass), bins = 10, 
                 breaks = c(410,411,412,413,414,420,421,422,423,424, 430,431,432,433,434,
                            799, 801))
ggplot()+
  geom_histogram(data  = forestClass_df, aes (forestClass), bins = 10, 
                 breaks = c(410.5,411.5,412.5,413.5,414.5,421.5,422.5,423.5,424.5,
                            431.5,432.5,433.5,434.5))
?count
ggsave("histogram_forestclasses.svg")
?stat_bin


?as.data.frame
plot(forestClass@data[values])


ggplot(melt(forestClass))+
  geom_sf

#foresthist 




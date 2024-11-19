#### Spatial analysis in R
# using terra, sf, leaflet
# script by Han Olff, november 2023
# visualise and analyse causes of woody cover 

#### PREPARE THE WORKING ENVIRONMENT ####
# clear and read libraries
rm(list = ls())

library(tidyverse)   # you know me
library(leaflet)     # for interactive maps
library(renv)        # r environment control
library(mapproj)     # map projection
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(sf)          # for vector spatial data
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for arranging multiple plots on a page

# restore the libraries to the same version used to develop this script
renv::restore()

# set the working directory to the folder where you unzipped the GIS data
setwd("G:/Shared drives/Org OlffLab/Teaching/APCE/APCE2023/APCE2023GIS")

#### EXPLORE COLOR PALETTES ####
# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,50), col = grey.colors(50))
grey.colors(50)
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = terrain.colors(10))
pal_rgb<-c("red","green","blue")
pal_rgb
barplot(rep(1,3), col = pal_rgb)

# RColorBrewer
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))

# Viridis
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::turbo(10))
viridis::turbo(10)
# wesanderson
library(wesanderson)
pal_wesanderson<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
barplot(rep(1,10), col = pal_wesanderson)

# read the vector data for the whole ecosystem
sf::st_layers(dsn="./2022_protected_areas/protected_areas.gpkg")
boundaries<-sf::st_read(dsn="./2022_protected_areas/protected_areas.gpkg",
                        layer="protected_areas_2022")
rivers<-sf::st_read(dsn="./2022_rivers/rivers_hydrosheds.gpkg") |>
  dplyr::filter(UP_CELLS>5000)
lakes<-sf::st_read(dsn="./lakes/lakes.gpkg") 

# read set the extent to the study area
studyarea<-sf::st_read("./studyarea/my_studyarea.shp")
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
ext<-sf::st_as_sfc(sf::st_bbox(studyarea))
ext


# read the raster data
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36s.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
hillshade<-terra::rast("./elevation_hillshade/srtm3_500m_focalstatmean_hillshadeZ6.tif")
elevation<-terra::rast("./elevation/srtm3/srtm3.tif")

# crop the layers to the study area
woodybiom_sa<-crop(woodybiom,ext)
elevation_sa<-crop(elevation,ext)
hillshade_sa<-crop(hillshade,ext)
rainfall_sa<-crop(rainfall,ext)
boundaries_sa<-sf::st_intersection(boundaries,studyarea)
rivers_sa<-sf::st_intersection(rivers,studyarea)

# plot using terra for the study area
terra::zoom(rainfall,e=lakes) # zoom to the extent of lakes
terra::plot(st_geometry(lakes),col="lightblue",add=T)
terra::plot(st_geometry(boundaries),add=T,lwd=2)
terra::plot(st_geometry(rivers),col="blue",add=T,lwd=2)

# make the same map in ggplot
# set the extent that I want to plot for whole GSME
xlimits<-sf::st_bbox(boundaries)[c(1,3)]
ylimits<-sf::st_bbox(boundaries)[c(2,4)]
### Make a map with ggplot
map0<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colours=rev(terrain.colors(10)),
                       limits=c(500,1100),oob=squish) +
  geom_sf(data=sf::st_geometry(boundaries),fill=NA,linewidth=0.7) +
  geom_sf(data=st_geometry(studyarea), fill=NA,col="red",linewidth=0.7) +
  geom_sf(data=rivers, col="blue") +
  coord_sf(xlimits,ylimits,datum=sf::st_crs(32736))
map0


#### STUDY AREA: CREATE RANDOM POINTS  AND MAP PREDICTORS ####
# create 1000 random points in our study area


# make a plot with woody biomass 
map1<-ggplot() +
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(10)),
                       limits=c(0,6),oob=squish) +
  geom_sf(data=boundaries_sa,fill=NA,linewidth=1) +
  geom_sf(data=rivers_sa, col="blue",linewidth=1) +
  geom_sf(data=studyarea, fill=NA,col="red", linewidth=1) +
  ggtitle("Woody total basal area (m2/ha)") +
  labs(fill="TBA") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8)) 
map1

# make a plot with  your random points
map2<-ggplot() +
  geom_sf(data=boundaries_sa,fill=NA,linewidth=1) +
  geom_sf(data=rivers_sa, col="blue",linewidth=1) +
  geom_sf(data=studyarea, fill=NA,col="red", linewidth=1) +
  geom_sf(data=randompoints,size=0.2) +
  ggtitle("1000 random points ") +
  labs(fill="TBA") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8)) 
map2


map3<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=rev(topo.colors(10)),
                       limits=c(500,1000),oob=squish) +
  geom_sf(data=sf::st_geometry(boundaries_sa), fill=NA) +
  geom_sf(data=sf::st_geometry(rivers_sa), col="blue") +
  geom_sf(data=studyarea, fill=NA,col="red") +
  ggtitle("rainfall (mm/year") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8)) 

map3

map4<-ggplot() +
  tidyterra::geom_spatraster(data=hillshade_sa,
                             show.legend=F) +
  scale_fill_gradient(low="black",high="white") +
  ggnewscale::new_scale_fill() +   # allow for a new color fill scale
  tidyterra::geom_spatraster(data=elevation_sa,
                             alpha=0.5) +
  scale_fill_gradientn(colours = pal_wesanderson,
                       limits=c(1400,2000),
                       oob=squish) +
    geom_sf(data=boundaries_sa, fill=NA,linewidth=1) +
  geom_sf(data=studyarea, fill=NA,col="red",linewidth=1) +
  geom_sf(data=rivers_sa, col="blue") +
   ggtitle("elevation (m)") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8)) 
map4

all_maps1 <- (map1 + map2 + map3 + map4 + plot_layout(ncol = 2)) 
all_maps1
  
ggsave("./results/all_maps.png",plot=all_maps1, 
       width=1920,height=1068,units="px")

# make a distance to river and distrance to boundaries raster

map5<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=dist2rivers_sa,
                             show.legend=T) +
  scale_fill_gradientn(colours=topo.colors(10)) +
  geom_sf(data=boundaries_sa,color="black", size=4,fill=NA) +
  geom_sf(data=rivers_sa,size=1,color="purple") +
  ggtitle("distance to rivers") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8)) 
map5 

map6<- ggplot2::ggplot() +
  tidyterra::geom_spatraster(data=dist2boundaries_sa,
                             show.legend=T) +
  scale_fill_gradientn(colours=rev(terrain.colors(10))) +
  geom_sf(data=boundaries_sa,color="black", size=4,fill=NA) +
  geom_sf(data=rivers_sa,size=1,color="purple") +
  ggtitle("distance to rivers") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(size = 8),
        legend.text=element_text(size = 8),
        legend.title=element_text(size = 8)) 
map6 

### read the wildebeest tracking data, collected from 2010-2013 
# for dataset description see (remove hashtag at start of next line)
# browseURL("https://www.movebank.org/cms/webapp?gwt_fragment=page%3Dstudies%2Cpath%3Dstudy208413731")
# read the dataset (only the GSME wildebeest have been selected for this script)


# plot with ggplot just based on x,y coordinates (lat lon)
# wait for the plot - takes a few seconds

# show only the migrant individuals with animal_id 2838,2834,2845
# each in a separate graph





###METHOD 3 Making maps with leaflet
# read wildebeest tracking data
wb_tracks<-sf::st_read()

boundaries2<-terra::vect(boundaries)
wb_tracks2<-terra::vect(wb_tracks)
class(boundaries2)
pal = leaflet::colorFactor(palette=c("red","blue"),
                           domain=wb_tracks$season)

# be patient for the map to show
terra::plet(boundaries2,col="darkgreen", tiles=c("Esri.WorldImagery", "OpenTopoMap", 
                                                   "Esri.WorldTerrain", "Esri.WorldTopoMap")) |>
  terra::points(wb_tracks2 |> dplyr::filter(animal_id==2845),
                col=~pal(season), cex=3,alpha=0) |>
  addLegend("topleft", 
            colors = c("red",  "blue"),
            labels = c("dry season (jun-nov)", "wet season (dec-may)"),
            title = "wildebeest 2845, 2010-2013",
            opacity = 1)

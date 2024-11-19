# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Users/ebelt/Documents/APCE2024/apce2024gis")

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
mycolors<-c('red', 'white', 'blue')
mycolors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)

sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)



# plot the woody biomass map that you want to predict
woody_map <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77, 6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="woody biomass") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)

  
# plot the rainfall map
rainfall_map <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colors=pal_zissou1,
                       limits=c(350, 2100),
                       oob=squish,
                       name="mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="rainfall") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
rainfall_map
# plot the elevation map
elevation_map <- ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colors=terrain.colors(10),
                       limits=c(500, 2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="elevation") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)

elevation_map
# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png
woody_map + elevation_map + rainfall_map
all_maps<-woody_map+elevation_map +
  patchwork::plot_layout(ncol=1)
all_maps
ggsave("./figures/all_maps.png", width = 18, height=18, units="cm", dpi=300)
############################
### explore your study area
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)
saExt
# crop the woody biomass to the extent of the studyarea
woodybiom_sa<- terra::crop(woodybiom, saExt)

# plot the woody biomass
woody_map_sa <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77, 6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="woody biomass") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
woody_map_sa


# make maps also for the other layers that you found
rainfall_sa<- terra::crop(rainfall, saExt)

# first you need to increase the raster resolution to 30 m
# Define the extent and resolution for the new raster
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area
map_rainfall_sa <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=(pal_zissou1),
                       limits=c(600, 1000),
                       oob=squish,
                       name="mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="rainfall") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_rainfall_sa


elevation_sa<- terra::crop(elevation, saExt)
map_elevation_study <- ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colors=terrain.colors(6),
                       limits=c(1500, 2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="elevation") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_elevation_study


dist2river <- terra::rast("./MyData/DistanceToRiver.tif")
map_dist2river_sa <- ggplot() +
  tidyterra::geom_spatraster(data=dist2river/1000) +
  scale_fill_gradientn(colours=rev(pal_zissou2),
                       limits=c(0, 10),
                       oob=squish,
                       name="kilometers") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="distance to rivers") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_dist2river_sa


BurnFreq <- terra::rast("./MyData/BurnFreq.tif")
map_burnfreq_sa <- ggplot() +
  tidyterra::geom_spatraster(data=BurnFreq) +
  scale_fill_gradientn(colours=(pal_zissou2),
                       limits=c(0, 16),
                       oob=squish,
                       name="number of years burned") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="Burn frequency") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_burnfreq_sa


fertility <- terra::rast("./MyData/CEC_5_15cm.tif")
fertility_sa<- terra::crop(fertility, saExt)
map_fertility_sa <- ggplot() +
  tidyterra::geom_spatraster(data=fertility_sa) +
  scale_fill_gradientn(colours=(viridis::viridis(256)),
                       limits=c(152, 283),
                       oob=squish,
                       name="CEC") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="soil fertility") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_fertility_sa


# landform valleys and plains (CEC)
landform_sa<-terra::rast("./MyData/hills.tif")
landform_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_map_sa


r<-terra::rast("./MyData/CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa

Nitrogen <- terra::rast("./MyData/Nitrogen.tif")
map_nitrogen_sa <- ggplot() +
  tidyterra::geom_spatraster(data=Nitrogen) +
  scale_fill_gradientn(colours=rev(viridis::viridis(256)),
                       limits=c(61, 106),
                       oob=squish,
                       name="g/kg") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="Soil Nitrogen at 0-20 cm") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_nitrogen_sa



SandContent <- terra::rast("./MyData/SandContent_5_15cm.tif")
map_sandcontent_sa <- ggplot() +
  tidyterra::geom_spatraster(data=SandContent) +
  scale_fill_gradientn(colours=rev(viridis::viridis(256)),
                       limits=c(366, 636),
                       oob=squish,
                       name="g/kg") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="Sand particle proportion") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_sandcontent_sa


TotalCarbon <- terra::rast("./MyData/TotalCarbon.tif")
map_totalcarbon_sa <- ggplot() +
  tidyterra::geom_spatraster(data=TotalCarbon) +
  scale_fill_gradientn(colours=rev(viridis::viridis(256)),
                       limits=c(24, 35),
                       oob=squish,
                       name="g/kg") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="Carbon total at 0-20cm") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_totalcarbon_sa



pH <- terra::rast("./MyData/ph_5_15cm.tif")
map_ph_sa <- ggplot() +
  tidyterra::geom_spatraster(data=pH) +
  scale_fill_gradientn(colours=rev(viridis::viridis(256)),
                       limits=c(61, 106),
                       oob=squish,
                       name="pHx10") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="skyblue") +
  tidyterra::geom_spatvector(data=rivers,
                             colour="blue") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, colour="red") +
  labs(title="Soil pH at 5-15 cm") +
  coord_sf(xlimits,ylimits,datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
map_ph_sa
#combine maps study area
all_maps_sa<-woody_map_sa +map_dist2river_sa + map_elevation_study + map_rainfall_sa + map_burnfreq_sa + map_fertility_sa + 
  landform_map_sa + CoreProtectedAreas_map_sa + map_nitrogen_sa + map_sandcontent_sa +
  map_totalcarbon_sa + map_ph_sa
  patchwork::plot_layout(ncol=2)
all_maps_sa

ggsave("./figures/all_maps_sa.png", width = 18, height = 18, units = "cm",dpi=300)


# create 500 random points in our study area
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")

#plot them
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa

# and add them to the previous map

# make distance to river map



### put all maps together
all_maps_sa<-woody_map_sa +map_dist2river_sa + map_elevation_study + map_rainfall_sa + map_burnfreq_sa + map_fertility_sa + 
  landform_map_sa  + CoreProtectedAreas_map_sa + rpoints_map_sa + map_nitrogen_sa + map_sandcontent_sa +
  map_totalcarbon_sa + map_ph_sa
patchwork::plot_layout(ncol=4)
all_maps_sa
ggsave("./figures/all_maps_sa.png", width = 18, height = 18, units = "cm",dpi=300)

# extract your the values of the different raster layers to the points
woody_points <- terra::extract(woodybiom_sa, rpoints) |> 
  as_tibble() |>
 dplyr::rename(woody=TBA_gam_utm36s)
woody_points
dist2river_points <- terra::extract(dist2river, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points
elevation_points <- terra::extract(elevation_sa, rpoints) |> 
  as_tibble() 
elevation_points
CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points
rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points
cec_points <- terra::extract(fertility_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points
burnfreq_points <- terra::extract(BurnFreq, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points
landform_points <- terra::extract(landform_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(hills=remapped)
landform_points
nitrogen_points <- terra::extract(Nitrogen, rpoints) |> 
  as_tibble() |>
  dplyr::rename(nitrogen = mean_0_20)
nitrogen_points
sand_points <- terra::extract(SandContent, rpoints) |> 
  as_tibble() |>
  dplyr::rename(sandcontent = `sand_5-15cm_mean`)
sand_points
carbon_points <- terra::extract(TotalCarbon, rpoints) |> 
  as_tibble() |>
  dplyr::rename(carbon = mean_0_20)
  carbon_points
ph_points <- terra::extract(pH, rpoints) |> 
  as_tibble() |>
  dplyr::rename(pH = `phh2o_5-15cm_mean`)
  ph_points

# merge the different variable into a single table
# use woody biomass as the last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2], 
                 CorProtAr_points[,2], rainfall_points[,2], 
                 cec_points[,2],burnfreq_points[,2], nitrogen_points [,2], 
                 sand_points [,2], carbon_points [,2], ph_points [,2],
                 landform_points[,2],woody_points[,2]) |>
  as_tibble()
pointdata
pointdata <- pointdata[complete.cases(pointdata),]

getwd()
readr::write_csv(pointdata,"pointdata.csv")
# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)

# make long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:hills, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 

# plot how woody cover is predicted by different variables
# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot") 
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4") 




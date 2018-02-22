###################################################
## Landcover Classification with Sentinel-2 data ##
###################################################


# Sentinel-2: is a polar-orbiting, multispectral high-resolution imaging mission for land monitoring to provide, 
# for example, imagery of vegetation, soil and water cover, inland waterways and coastal areas (http://www.esa.int)

# Free data download is possible via: https://scihub.copernicus.eu/dhus/#/home
# --> registration is necessary
# --> choose extent via drawing Polygon or Box
# --> download data directly
# --> also atmospherical corrected data are accessable (Level 1C and Level 2)

# search criteria:
# - from 01/04/2017 to 01/09/17
# - cloudcover: 0 to 10 %
# - Product type: S2MSI2Ap

# choosen data:
# date 06.07.2017
# Sentinel-2
# level 2A
# cloud cover: 0.255488 %


setwd("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/Daten Skript")

library(rgdal)
library(RStoolbox)
library(grid)
library(raster)
library(ggplot2)
library(rgeos)


## 1) read in the data ####

# Sentinel data is delivered in .jp2 - files.
# There is the possibility to install the rgdal-package with the Driver for reading jpeg2000-files.
# Another easier possiblity is to pass the files to QGIS and safe them as Geotiff-files.

# color composition Sentinel2: red-B02, green-B04, blue-B03, NIR-B08

# read in the GeoTiff-files
sentinel_20170706_B02_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_B02_10m.tif")
sentinel_20170706_B03_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_B03_10m.tif")
sentinel_20170706_B04_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_B04_10m.tif")
sentinel_20170706_B08_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_B08_10m.tif")
sentinel_20170706_AOT_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_AOT_10m.tif")
sentinel_20170706_TCI_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_TCI_10m.tif")
sentinel_20170706_WVP_10m <- raster("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/S2A_20170706T102021_WVP_10m.tif")

# stack the single bands in same order as original data
sentinel_20170706_10m <- stack(sentinel_20170706_AOT_10m, sentinel_20170706_B02_10m, sentinel_20170706_B03_10m, sentinel_20170706_B04_10m, 
                               sentinel_20170706_B08_10m, sentinel_20170706_TCI_10m, sentinel_20170706_WVP_10m)

# rename layer to shorter names and safe the result
names(sentinel_20170706_10m) <- gsub(pattern="S2A_20170706T102021_", replace="", x=names(sentinel_20170706_10m))
sentinel_20170706_10m
writeRaster(sentinel_20170706_10m, "raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/sentinel_20170706_10m.grd")

# Sentinel Level2A data is corrected and it´s reflectance values already
# --> can directly be used for calculations.


# read in the studyarea, check for equal projection and transform studyarea to data.frame for plotting
studyarea1 = readOGR("Wuerzburg_bbox_points", "Wuerzburg_bbox_points") # load the vector data
identical(crs(studyarea1), crs(sentinel_20170706_10m))
studyarea1_utmN = spTransform(studyarea1, CRS = crs(sentinel_20170706_10m)) # Transform the projection of studyarea1 to the ones from the scenes
identicalCRS(studyarea1_utmN, sentinel_20170706_10m)
studyarea1_utmN.df <- as.data.frame(studyarea1_utmN) # transform to data.frame for plotting



# plot the whole image
ggRGB(sentinel_20170706_10m, r=2, g=4, b=3,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("True color Image with marked studyarea\n(Sentinel-Level2A data, 10m resolution))") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"), 
        legend.title=element_text(size=10, colour = "black", face="bold")) +
  geom_point(data=studyarea1_utmN.df,
             aes(x=studyarea1_utmN.df$coords.x1, 
                 y=studyarea1_utmN.df$coords.x2, 
                 col=studyarea1_utmN.df$Id),
             shape=1) +
  scale_colour_manual(name="Studyarea:", 
                      values="red",
                      labels=c("Corner"))
## saved as: sentinel_20170706_10m

# --> studarea is not completely covered 
# --> more data would be needed, but with littel cloud cover not available for roughly same data



## 2A) correct for clouds via supervised Classification ####
# RStoolbox::cloudMask() can´t be used, as no thermal infrared band (tir) is delivered in Sentinel-2 data.
# Alternative approach: run a supervised classification to classify cloud pixels.

# in QGIS: create vector data with polygons for 3 classes: cloud, shadow, land 
# ---> training- and validation data for a cloud classification

# read in the Satellite image with clouds
sentinel_20170706_10m <- brick("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/sentinel_20170706_10m.grd")

# read vector data in R
td_cmask_sentinel_20170706_10 <- readOGR(dsn="vector", layer="cmask_sentinel_20170706_10")
vd_cmask_sentinel_20170706_10 <- readOGR(dsn="vector", layer="vd_cmask_sentinel_20170706_10")




########## 1. run a supervised classification with only training data and plot the result
cmask_sentinel_20170706_10 <- superClass(img = sentinel_20170706_10m,
                                      model = "rf", 
                                      trainData = td_cmask_sentinel_20170706_10,
                                      trainPartition = 0.7, # 70% of training data are used for training, 40% are used for validation
                                      responseCol = "id")
# plot the resulting "map"
cols =c("1"="white", "2"="darkblue", "3"="khaki")
ggR(cmask_sentinel_20170706_10$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("cmask_sentinel_20170706_10\nSupervised classification\nmodel: rf, trainPartition: 0.7") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title= element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Clouds", "Class2: Shadows", "Class3: Land"), 
                    name = "Cloudmask\nClasses\n")
## --> water is classified as cloud shadows!

# check the accuracy and safe the result
cmask_sentinel_20170706_10$modelFit
cmask_sentinel_20170706_10$validation$performance # overall accuracy: 0.9893
writeRaster(cmask_sentinel_20170706_10$map, "results/Sentinel/cmask_sentinel_20170706_10.grd")




########## 2. run a supervised classification with training and validation data
cmask_2_sentinel_20170706_10 <- superClass(img = sentinel_20170706_10m,
                                         model = "rf", 
                                         trainData = td_cmask_sentinel_20170706_10,
                                         valData=vd_cmask_sentinel_20170706_10,
                                         responseCol = "id")

# plot the resulting "map"
cols =c("1"="white", "2"="darkblue", "3"="khaki")
ggR(cmask_2_sentinel_20170706_10$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("cmask_2_sentinel_20170706_10\nSupervised classification\nmodel: rf, training + validation data") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title= element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Clouds", "Class2: Shadows", "Class3: Land"), 
                    name = "Cloudmask\nClasses\n")

# check the accuracy and safe result
cmask_2_sentinel_20170706_10$modelFit
cmask_2_sentinel_20170706_10$validation$performance
writeRaster(cmask_2_sentinel_20170706_10$map, "results/Sentinel/cmask_2_sentinel_20170706_10.grd", overwrite=TRUE)





########## 3. run classification with other models and compare the accuracy (!!! not tried yet! used model so far: rf)

## --> run a loop to compare the results of 3 different models
#model_names <- c("svmRadial", "pls", "plsr")
#for (i in 1:length(model_names)){ # for the whole vector of models (model[1] to last model) ...
#  cmask_3_sentinel_20170706_10 <- superClass(sentinel_20170706_10m,
#                                             trainData=td_cmask_sentinel_20170706_10,
#                                             trainPartition=0.6,
#                                             responseCol = "id",
#                                             model=model_names[i]) # ... run the classification like so and use model[i]]
#  
#  names(cmask_3_sentinel_20170706_10$map) <- model_names[i] # directly rename the name of the layers (new name == name of used model)
#  
#  #plot each classifcation automatically within the loop
#  p <- ggR(cmask_3_sentinel_20170706_10$map, forceCat = TRUE, geom_raster = TRUE) +
#    ggtitle(paste("cmask_3_sentinel_20170706_10\nSupervised classification - model:", model_names[i])) +
#    theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
#          legend.title= element_text(size=11, colour="black", face="bold")) +
#    scale_fill_manual(values = cols, 
#                      c("Class1: Clouds", "Class2: Shadows", "Class3: Land"), 
#                      name = "Cloudmask\nClasses\n")
#  print(p) 
#  
#  
#  # for later comparison:
#  if (i==1){                              # in the first run: safe the sc-result as sc_stack
#    cmask_3_sentinel_20170706_10_stack <- cmask_3_sentinel_20170706_10$map
#    cmask_3_sentinel_20170706_10_modelFit <- cmask_3_sentinel_20170706_10$modelFit[[1]]
#  } else{                                 # later on: stack the actual sc-result with sc_stack 
#    cmask_3_sentinel_20170706_10_stack <- stack(cmask_3_sentinel_20170706_10_stack, cmask_3_sentinel_20170706_10$map)
#    cmask_3_sentinel_20170706_10_modelFit <- rbind(cmask_3_sentinel_20170706_10_modelFit, cmask_3_sentinel_20170706_10$modelFit[[1]]) 
#    # save the modelFit parameters to select model with best accuracy afterwards
#    
#  }}




########## 4. mask the cloud pixels from the raster image

# define the cloud pixels and safe the Raster
cloudQuery <- cmask_2_sentinel_20170706_10$map > 2 # the query excludes the values 1 and 2, which stand for clouds and their shadows
writeRaster(cloudQuery, "results/Sentinel/cloudQuery.grd", overwrite=TRUE) # save the result
cloudQuery <- raster("results/Sentinel/cloudQuery.grd") # reload the result (later use)
plot(cloudQuery)# plot the query Raster to check

# set cloud pixels to NA
sentinel_20170706_10_csmasked <- sentinel_20170706_10m # create a "copy" from the raster image --> this will have the same properties
sentinel_20170706_10_csmasked[cloudQuery == 0] <- NA # set all values which are covered by clouds to NA
plot(sentinel_20170706_10_csmasked) # plot to check the result

# adopt the band names and save the result
band_names_sentinel <- c("AOT_10m", "B02_10m", "B03_10m", "B04_10m", "B08_10m", "TCI_10m", "WVP_10m")
names(sentinel_20170706_10_csmasked) <- band_names_sentinel
writeRaster(sentinel_20170706_10_csmasked, "results/Sentinel/sentinel_20170706_10_csmasked.grd", overwrite =TRUE)


# plot the masked image to check
ggRGB(sentinel_20170706_10_csmasked, r=2, g=4, b=3,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("True color Image without clouds\n(Sentinel-2A data, 10m resolution))") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"), 
        legend.title=element_text(size=10, colour = "black", face="bold")) 
# saved as: sentinel_20170706_10_csmasked in results/Sentinel_results



## 3) calculate vegetation index for later use ####

# load the raster image without clouds
sentinel_20170706_10_csmasked <- brick("results/Sentinel/sentinel_20170706_10_csmasked.grd")

# calculate vegetation indices
sentinel_20170706_10m_ndvi_msavi2 <- spectralIndices(sentinel_20170706_10_csmasked, 
                                              red=2, # red --> B02_10m
                                              nir=5, # nir --> B08_10m
                                              indices = c("NDVI", "MSAVI2"))
# check the value distribution
boxplot(sentinel_20170706_10m_ndvi_msavi2$NDVI)
boxplot(sentinel_20170706_10m_ndvi_msavi2$MSAVI2) 
# --> some single values are out of the range [-1, 1] 
# --> only single values --> probably artefacts
# remove these values/ set to NA
sentinel_20170706_10m_ndvi_msavi2_new <- sentinel_20170706_10m_ndvi_msavi2 # create a "copy" of the raster with same properties
sentinel_20170706_10m_ndvi_msavi2_new[sentinel_20170706_10m_ndvi_msavi2_new < -1] <- NA # set values out of range to NA
names(sentinel_20170706_10m_ndvi_msavi2_new) <- c("NDVI", "MSAVI2") # adopt the layer names

# save the results
writeRaster(sentinel_20170706_10m_ndvi_msavi2, "results/Sentinel/sentinel_20170706_10m_ndvi_msavi2.grd")
writeRaster(sentinel_20170706_10m_ndvi_msavi2_new, "results/Sentinel/sentinel_20170706_10m_ndvi_msavi2_new.grd")



# plot the vegetation indices
ggR(sentinel_20170706_10m_ndvi_msavi2_new$NDVI, stretch="lin", geom_raster = TRUE) +
  scale_fill_gradient2(low="darkred", mid="orange", high="darkgreen", 
                       name="NDVI-values\n", 
                       midpoint=0, 
                       na.value="grey95", 
                       limits=c(-1, 1)) +
  ggtitle("NDVI\n(sentinel-2A data, 2017_07_06, csmasked)") +
  theme(legend.spacing = unit(2,"lines"), 
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(colour = "black", size = 11, face="bold"),
        legend.title = element_text(color="black", size=12, face="bold"))
# saved as: sentinel_20170706_10m_ndvi

ggR(sentinel_20170706_10m_ndvi_msavi2_new$MSAVI2, stretch="lin", geom_raster = TRUE) +
  scale_fill_gradient2(low="darkred", mid="orange", high="darkgreen", 
                       name="MSAVI2-values\n", 
                       midpoint=0, 
                       na.value="grey95",
                       limits=c(-1, 1)) +
  ggtitle("MSAVI2\n(sentinel-2A data, 2017_07_06, csmasked)") +
  theme(legend.spacing = unit(2,"lines"), 
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(colour = "black", size = 11, face="bold"),
        legend.title = element_text(color="black", size=12, face="bold"))
# saved as: sentinel_20170706_10m_msavi2




## 4) create an extract (buffer of certain radius around chosen coordinate) ####


########## 1.  define the gps-coordinates of points (could be coordinates of traps etc...)

# here as example: coordinates of cities of Bavaria: Wuerzburg, Karlstadt, Aschaffenburg, schweinfurt, Hammelburg, Schlüchtern)
gps_points <- data.frame(x=c(9.9533548, 9.7734603,  9.1355554,  10.2194228, 9.891789, 9.5255493), 
                         y=c(49.7913044, 49.9604785, 49.9806625, 50.0492047, 50.1185626, 50.3493544), 
                         point_names=c("Wuerzburg", "Karlstadt", "Aschaffenburg", "Schweinfurt", "Hammelburg", "Schlüchtern"), 
                         id=c(1,2,3,4,5,6))
coordinates(gps_points) <- c("x", "y")
proj4string(gps_points) <- CRS("+proj=longlat +datum=WGS84")
gps_points <- spTransform(gps_points, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
gps_points_df <- as.data.frame(gps_points) # transform back to dataframe for plotting

ggRGB(sentinel_20170706_10_csmasked, r=2, g=4, b=3,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("Sentinel Image with selected coordinates") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"), 
        legend.title=element_text(size=10, colour = "black", face="bold")) +
  geom_point(data=gps_points_df,
             aes(x=gps_points_df$x,y=gps_points_df$y), 
             colour="red", 
             shape=3, size=3, stroke=1.5)





########## 2. Define and plot the buffers around the points

# create a buffer around the coordinates (here: 40 km)
buf_25 <- gBuffer(gps_points, width=25000, byid=TRUE) # create multiple buffers for all of the polygons
buf_25_wurzburg <- buf_25[buf_25$id == "1",] # create the buffer around one of the coordinates

# prepare the polygons for plotting
buf_25_df <- fortify(buf_25, region="id") # transform the coordinates to dataframe
buf_25_df_final <- merge(buf_25_df, buf_25@data, by="id") # add the information
buf_25_wurzburg_df <- fortify(buf_25_wurzburg, region="id")
buf_25_wurzburg_df_final <- merge(buf_25_wurzburg_df, buf_25_wurzburg@data, by="id")

# plot the buffers on the Satellite image
ggRGB(sentinel_20170706_10_csmasked, r=2, g=4, b=3,
      stretch="lin", geom_raster=TRUE) +
  ggtitle("Sentinel Image with selected coordinates\n(5 towns closed to Wuerzburg with buffers of 25 km radius)") +
  theme(plot.title=element_text(size=12, colour="black", face="bold"), 
        legend.title=element_text(size=10, colour = "black", face="bold")) +
  geom_polygon(data=buf_25_df_final, 
               mapping=aes(x=buf_25_df_final$long, 
                           y=buf_25_df_final$lat,
                           group=group), 
               colour="orange", 
               fill="orange", 
               alpha=0.3) +
  geom_polygon(data=buf_25_wurzburg_df_final, 
               mapping=aes(x=buf_25_wurzburg_df_final$long, 
                           y=buf_25_wurzburg_df_final$lat,
                           group=group), 
               colour="green", 
               fill="green", 
               alpha=0.3) +
  geom_point(data=gps_points_df,
             aes(x=gps_points_df$x,y=gps_points_df$y), 
             colour="red", 
             shape=3, size=3, stroke=1.5)
# saved as: sentinel_20170706_10_csmasked_buffers25






########## 3. Extract and plot the satellite-data within 25 km around Würzburg




## 5) create + read in training data + validation data + vegetation indices ####

## 6) calculate + validate classification ####

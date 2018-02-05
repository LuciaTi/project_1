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



## 2) correct for clouds ####
# RStoolbox::cloudMask() can´t be used, as no thermal infrared band is delivered in Sentinel-2 data.
# Alternative approach: run a supervised classification to classify cloud pixels.

### in QGIS: create vector data with polygons for 3 classes: cloud, shadow, land ---> these are training data for a cloud classification

# read vector data in R
td_cmask_sentinel_20170706_10 <- readOGR(dsn="vector", layer="cmask_sentinel_20170706_10")

# run a supervised classification and plot the result
cmask_sentinel_20170706_10 <- superClass(img = sentinel_20170706_10m,
                                      model = "rf", 
                                      trainData = td_cmask_sentinel_20170706,
                                      trainPartition = 0.6, # 60% of training data are used for training, 40% are used for validation
                                      responseCol = "id")
# plot the resulting "map"
cols =c("1"="white", "2"="darkblue", "3"="khaki")
ggR(cmask_sentinel_20170706$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("cmask_sentinel_20170706_10\nSupervised classification - model: rf") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title= element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class1: Clouds", "Class2: Shadows", "Class3: Land"), 
                    name = "Cloudmask\nClasses\n")




# load the data
sentinel_20170706_10m <- brick("raster/S2A_MSIL2A_20170706T102021_N0205_R065_T32UNA_20170706T102301/S2A_MSIL2A_20170706_tiff/sentinel_20170706_10m.grd")

# choose an extent with clouds and plot it.
plotRGB(sentinel_20170706_10m, r=2, g=4, b=3, stretch="lin")
ex = drawExtent()
# chose extent:
#xmin        : 505072.8 
#xmax        : 518517.7 
#ymin        : 5490851 
#ymax        : 5496962 
sentinel_20170706_10_ex = crop(sentinel_20170706_10m, ex)
ggRGB(sentinel_20170706_10_ex, r=2, g=4, b=3, stretch = "lin")


cmask_test_sentinel_20170706_10 = cloudMask(sentinel_20170706_10_ex, blue = B03_10m) # calculate and plot the cloud mask
ggR(cmask_test_illu[[2]], geom_raster=TRUE) 
plot(cmask_test_illu) # plot both the mask itself and the spectral index usedfor the threshold
# choose the threshold for the cloudmask

# use the calculated threshold and additionally a buffer around the cloud pixels
# plot the scene with the clouds
cmask_final_illu = cloudMask(p194r25r26_2011_cdr_crop_new_illu, threshold = -0.95, buffer = 5, blue = 1, tir = 6)
ggRGB(p194r25r26_2011_cdr_crop_new_illu, stretch = "lin") +
  ggR(cmask_final_illu[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
  scale_fill_manual(values = "red", na.value = NA)


## 3) calculate Vegetaion index for later use ####

## 4) create  read in training data + validation data ####

## 5) calculate + validate classification ####

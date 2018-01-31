###################################################
## Landcover Classification with Sentinel-2 data ##
###################################################


# Sentinel-2: is a polar-orbiting, multispectral high-resolution imaging mission for land monitoring to provide, 
# for example, imagery of vegetation, soil and water cover, inland waterways and coastal areas (http://www.esa.int)

# Free data download is possible via: https://scihub.copernicus.eu/
# --> registration is necessary
# --> choose extent via drawing Polygon or Box
# --> download data directly

# choosen data:
# date 06.07.2017
# Sentinel-2
# level 2A
# cloud cover: 0.255488


setwd("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/Daten Skript")

library(rgdal)
library(gdal)
library(grid)
library(raster)
library(gdalUtils)
gdal_chooseInstallation('JP2OpenJPEG') # installation to allow reading of jpeg2000-files
gdalDrivers() # check if Driver for Jpeg2000 is available


## 1) read in the data ####
sentinel2_06_07_17_aot <- rgdal::readGDAL('raster/S2A_MS~1/S2A_MS~1/S2A_MS~1.SAF/GRANULE/L2A_T3~1/IMG_DATA/R10m/L2A_T32UNA_20170706T102021_AOT_10m.jpg')
test <- raster("raster/S2A_MS~1/S2A_MS~1/S2A_MS~1.SAF/GRANULE/L2A_T3~1/IMG_DATA/R10m/L2A_T32UNA_20170706T102021_AOT_10m.jp2")


## 2) correct for clouds ####

## 3) correct for topographic illumination ####

## 4) create  read in training data + validation data ####

## 5) calculate + validate classification ####

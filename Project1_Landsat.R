# Studysite: Würzburg

# used data: Landsat 4/5

# The following code is follows the Book "Remote Sensing and GIS for Ecologists". The used commands and most of the explantions 
# represent thus extracts from this Book. For further details see:
# Wegmann, M., Leutner, B., & Dech, S. (Eds.). (2016). Remote sensing and GIS for ecologists: using open source software. 
# Pelagic Publishing Ltd.



# set the workspace directory:
setwd("C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/Daten Skript")



## 1) read in the gps-points and find the fitting Satellite data ####

# load package raster - lots of tools for spatial analysis.
library(raster) 
library(rgdal)
library(wrspathrow)

## ensure that the file/ patch to the data exists or look up the filenames:
list.files('C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/Daten Skript/Wuerzburg_bbox_points/', pattern='\\.shp$')
file.exists('C:/Users/Ltischer/Documents/Studium/A Master/HIWI_GIS/Daten Skript/Wuerzburg_bbox_points/Wuerzburg_bbox_points.shp')

### 1. possibility:
# read in the information about the studyarea
studyarea1 <- readOGR("Wuerzburg_bbox_points", "Wuerzburg_bbox_points")

# get the Coordinates of the area:
coordinates(studyarea1) # --> these are in UTM-format

# transform these coordinates to LongLat Format and show the points (or use the summary function, there they also displayed in LongLat format)
studyarea1LongLat = spTransform(studyarea1,CRS("+proj=longlat"))
studyarea1LongLat

# get some information about your gps-points (for example coordinates in LongLat)
summary(studyarea1)


### 2. possibility:
# read in information about studyarea, but shapefile==polygon
studyarea1.2 <- readOGR("Wuerzburg_bbox_points", "studysite_polygon")

# get the numbers of paths and rows of LandSat data covered by the studyarea
# --> these can be used online on EarthExplorer to query coresponding data sets
pathrowSA <- pathrow_num(studyarea1.2)
pathrowSA


## 2) download the Satellite Image from https://earthexplorer.usgs.gov/####

## choose the data sets under Search Criteria:
       # The lonlat - coordinates can be used in EarthExlorer to get the 4 corner of the study site.
       # When displaying the overlap with the study site, one can also check for the needed path and row number.
       # If path/row is already known (see before) these numbers can be entered directly.
       # In this case: path 194, row 25 + row 26 is used

## pre-processed (atmospharically corrected) data must be ordered, therefore an account is necessary.
       # --> choose datasets -> Landsat Archieve -> Collection 1 Higher Level -> L4-5 TM
## the data can also be directly downloaded in tar.gz - format (not pre-processed).
       # --> choose datasets -> Landsat Archieve -> Collection 1 Level 1 -> L4-5 TM
## additional cirteria (cloud cover etc.) can be set under additional criteria

## once the scene is chosen, click download button.
   # data is downloaded as tar.gz - data
   # helpful tool to open: 7-zip
   # right-click on the downloaded data -> 7-zip -> open -> rightclick on the text line -> open internally -> unzip -> choose the folder
     # where to unzip the data.




## 3 A) read in the choosen data (without preprocessing) ####

library(ggplot2)
library(RStoolbox)


# here the data is from 15/10/2011 - Landsat 4 - 5.
# the studyarea covered parts of 2 scenes --> first load the sepparatly and connect them afterwards.

allBands1 = list.files("raster/LT05_L1TP_194025_20111015_20161005_01/", pattern = "TIF", full.names = TRUE) # load all the bands from the first scene
p194r25_2011 = stack(allBands1) # connect the bands from scene 1 to one RasterStack
plot(p194r25_2011, 5) # plots Band 5 from this data

allBands2 = list.files("raster/LT05_L1TP_194026_20111015_20161006_01_T1/", pattern = "TIF", full.names = TRUE) #same for the second scene
p194r26_2011 = stack(allBands2)
plot(p194r26_2011, 5)

# rename the band names to only B1, B2 etc ...
names(p194r25_2011) = gsub(pattern = "LT05_L1TP_194025_20111015_20161005_01_T1_", replace = "", x = names(p194r25_2011))
p194r25_2011 # check: all Bands are renamed correctly.
# same for the second scene:
names(p194r26_2011) = gsub(pattern = "LT05_L1TP_194026_20111015_20161006_01_T1_", replace = "", x = names(p194r26_2011))
p194r26_2011

# check the projection of the Satellite images.
projection(p194r25_2011) 
projection(p194r26_2011)
projection(studyarea1)  #--> differ from projection of studyarea1
studyarea1_utmN = spTransform(studyarea1, CRS = crs(p194r25_2011)) # Transform the projection of studyarea1 to the ones from the scenes
identicalCRS(p194r25_2011, studyarea1_utmN) # check again, if projections are the same now. --> Yes.

# set the extents of the two scenes to the same values
extend(p194r26_2011, p194r25_2011) 

# connect the data from the 2 scene to one dataset#
p194r25r26_2011 = mosaic(p194r25_2011, p194r26_2011, fun = max) # in case of overlay the maximum value will be kept (avoidance of 0-values of the background)
plot(p194r25r26_2011, 5) # plot band 5 of the connected scenes to check.
plot(studyarea1_utmN, add = TRUE, col = "red", pch = 1, lwd = 2) # --> only part of the Satellite image is needed.

# define the overlay of the Satellite data with the studyarea and crop it.
p194r25r26_2011_crop_studyarea1 = crop(p194r25r26_2011, studyarea1_utmN)
plot(p194r25r26_2011_crop_studyarea1, 5)
plot(studyarea1_utmN, add = TRUE, col = "red", pch = 1, lwd = 2)

# save the created extract as GeoTiff -  file and as Grid - file
writeRaster(p194r25r26_2011_crop_studyarea1, filename = "results/p194r25r26_2011_unprocessed_crop_studyarea.tif")
writeRaster(p194r25r26_2011_crop_studyarea1, filename = "results/p194r25r26_2011_unprocessed_crop_studyarea.grd")


## 3 B) read in the chosen data (with preprocessing) ####

library(RStoolbox)

## Adding the meta-data:

# meta-data for the imagery profides information about acquisition time, processing level, projection etc.
# --> can be read in R and labeled automatically


# read in the meta data
meta_p194r25_2011 = readMeta("raster/LT05_L1TP_194025_20111015_20161005_01/LT05_L1TP_194025_20111015_20161005_01_T1_MTL.txt") 

# get the information about the scene
summary(meta_p194r25_2011) # --> gives information about scene id, platform, sensor, date, path/row, projection, bands and calibration parameters

# import and label the rasta data of the scene automatically
# --> advantageous for pre-processing with RStoolbox, as the link between each band and its meta-data parameter is correct.
p194r25_2011 = stackMeta(meta_p194r25_2011)



## conversion to meaningfull units:

# actual radiation needs to be recalculated from the delivered digital numbers
# (digital numbers means, that pixel values are given as integers ranging from [0, 255] for landsat data due to the 8-bit
#  radiometric resolution (= 2^8 possible values per pixel))
# For the calculation, a multiplicativ term ("gain") and an additive term ("offset") are used.

# extracting the bandwise conversion parameters:
dn2rad_p194r25 = meta_p194r25_2011$CALRAD
dn2rad_p194r25

# application of the gain/offset information to all the single bands:
p194r25_2011_rad = p194r25_2011 * dn2rad_p194r25$gain + dn2rad_p194r25$offset

# the same can be done with RStoolbox::radCor()
p194r25_2011_rad = radCor(p194r25_2011, metaData = meta_p194r25_2011, method = "rad")

# the digital numbers which have been delivered have no been converted to physically meaningfull units
# (top-of-atmosphere-radiance)
# the data Type changed from integer to float:
dataType(p194r25_2011[[1]])
dataType(p194r25_2011_rad[[1]])

# have a look at the new value range for each band and safe the result
p194r25_2011_rad
writeRaster(p194r25_2011_rad, filename = "results/p194r25_2011_rad.grd")
writeRaster(p194r25_2011_rad, filename = "results/p194r25_2011_rad.tif")

# Furthermore, the top-of-atmosphere-reflectence can be calculated (minimum correction that should be done!)
# Therefore the radiance gets normalized to the solar irradiation at a given day of the year.
# The result is the apparent reflectence with values from [0,1].
# Additionally the thermal bands are converted in "brightness temperature" (the temp that a black body would have in the given 
# radiation), which is a proxi for surface temperature.
p194r25_2011_ref = radCor(p194r25_2011, metaData = meta_p194r25_2011, method = "apref")
writeRaster(p194r25_2011_ref, filename = "results/p194r25_2011_ref.grd", overwrite=TRUE)
writeRaster(p194r25_2011_ref, filename = "results/p194r25_2011_ref.tif", overwrite=TRUE)


# The radiation which is emmitted by the sun has to pass the atmosphere 2 times, before it reaches the sensor.
# The sensor also measures radiation which didn´t origin from the sun, but from other objects, this has to be removed.
# There should always be some pixels which are "dark" and which therefore don´t reflect any radiation.
# It´s assumed, that the reflectence from these objects is due to scattering in the atmosphere, it´s called "haze".

# Using the "dark object subtraction" the haze will be estimated and then be substracted from all pixels.
# (Here for the Bands 1 to 4). darProp is the expected proportion of dark pixels in the image.
haze_p194r25 = estimateHaze(p194r25_2011, darkProp = 0.01, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn"))

# Bands 1-4 are the corrected for haze (= atmospheric scattering).
p194r25_2011_sdos = radCor(p194r25_2011, metaData = meta_p194r25_2011, 
                           hazeValues = haze_p194r25, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn"), method = "sdos")
writeRaster(p194r25_2011_sdos, filename = "results/p194r25_2011_sdos.grd", overwrite=TRUE)
writeRaster(p194r25_2011_sdos, filename = "results/p194r25_2011_sdos.tif", overwrite=TRUE)

# Another DOS-method (see command afterwards) relies on the assumption, that scattering is the highest in the blue
# wavelengths, and that scattering in the other wavelengths can be derived by estimating the haze in the blue band.
p194r25_2011_dos = radCor(p194r25_2011, metaData = meta_p194r25_2011, darkProp = 0.01, method = "dos")
writeRaster(p194r25_2011_dos, filename = "results/p194r25_2011_dos.grd", overwrite=TRUE)
writeRaster(p194r25_2011_dos, filename = "results/p194r25_2011_dos.tif", overwrite=TRUE)

# Process the same commands for the second scene:
meta_p194r26_2011 = readMeta("raster/LT05_L1TP_194026_20111015_20161006_01_T1/LT05_L1TP_194026_20111015_20161006_01_T1_MTL.txt")
summary(meta_p194r26_2011)
p194r26_2011 = stackMeta(meta_p194r26_2011)
dn2rad_p194r26 = meta_p194r26_2011$CALRAD
dn2rad_p194r26
p194r26_2011_rad = p194r26_2011 * dn2rad_p194r26$gain + dn2rad_p194r26$offset
p194r26_2011_rad = radCor(p194r26_2011, metaData = meta_p194r26_2011, method = "rad")
dataType(p194r26_2011[[1]])
dataType(p194r26_2011_rad[[1]])
p194r26_2011_rad
p194r26_2011_ref = radCor(p194r26_2011, metaData = meta_p194r26_2011, method = "apref")
haze_p194r26 = estimateHaze(p194r26_2011, darkProp = 0.01, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn"))
p194r26_2011_sdos = radCor(p194r26_2011, metaData = meta_p194r26_2011, hazeValues = haze_p194r26, hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn"), method = "sdos")
p194r26_2011_dos = radCor(p194r26_2011, metaData = meta_p194r26_2011, darkProp = 0.01, method = "dos")

# also safe these results
writeRaster(p194r26_2011_rad, filename = "results/p194r26_2011_rad.grd")
writeRaster(p194r26_2011_rad, filename = "results/p194r26_2011_rad.tif")

writeRaster(p194r26_2011_ref, filename = "results/p194r26_2011_ref.grd")
writeRaster(p194r26_2011_ref, filename = "results/p194r26_2011_ref.tif")

writeRaster(p194r26_2011_sdos, filename = "results/p194r26_2011_sdos.grd")
writeRaster(p194r26_2011_sdos, filename = "results/p194r26_2011_sdos.tif")

writeRaster(p194r26_2011_dos, filename = "results/p194r26_2011_dos.grd")
writeRaster(p194r26_2011_dos, filename = "results/p194r26_2011_dos.tif")



# connect the corrected images as before and crop the result to the studyarea.
studyarea1 = readOGR("Wuerzburg_bbox_points", "Wuerzburg_bbox_points")
studyarea1_utmN = spTransform(studyarea1, CRS = crs(p194r25_2011)) # Transform the projection of studyarea1 to the ones from the scenes
identicalCRS(p194r25_2011sdos, studyarea1_utmN) # check again, if projections are the same now. --> Yes.

# check, if the extents of the two rasters are the same:
extent(p194r26_2011_dos)
extent(p194r25_2011_dos) 
extent(p194r26_2011_sdos)
extent(p194r25_2011_sdos) #--> p194r25_2011 has an bigger extent.

# the extends of the objects get adapted:
extend(p194r26_2011_dos, p194r25_2011_dos)   # p194r26_2011_dos gets extended on the size of p194r25_2011_dos
extend(p194r26_2011_sdos, p194r25_2011_sdos) # same for the other pairs of objects.
extend(p194r26_2011_rad, p194r25_2011_rad)
extend(p194r26_2011_ref, p194r25_2011_ref)

# each pair of raster objects now gets overlayed to produce one complete raster of the whole study area.
# plot the overlapt to check
# this raster gets reduced afterwards to show only the study area.
# Afterwards, save created rasters to workdirectory.
p194r25r26_2011_sdos = mosaic(p194r25_2011_sdos, p194r26_2011_sdos, fun = max)
ggRGB(p194r25r26_2011_sdos, stretch="lin")
p194r25r26_2011_sdos_crop_studyarea1 = crop(p194r25r26_2011_sdos, studyarea1_utmN)
writeRaster(p194r25r26_2011_sdos_crop_studyarea1, filename = "results/p194r25r26_2011_sdos_crop_studyarea.grd", overwrite=TRUE)
writeRaster(p194r25r26_2011_sdos_crop_studyarea1, filename = "results/p194r25r26_2011_sdos_crop_studyarea.tif", overwrite=TRUE)

p194r25r26_2011_dos = mosaic(p194r25_2011_dos, p194r26_2011_dos, fun = max)
ggRGB(p194r25r26_2011_dos, stretch="lin")
p194r25r26_2011_dos_crop_studyarea1 = crop(p194r25r26_2011_dos, studyarea1_utmN)
writeRaster(p194r25r26_2011_dos_crop_studyarea1, filename = "results/p194r25r26_2011_dos_crop_studyarea.grd", overwrite=TRUE)
writeRaster(p194r25r26_2011_dos_crop_studyarea1, filename = "results/p194r25r26_2011_dos_crop_studyarea.tif", overwrite=TRUE)

p194r25r26_2011_rad = mosaic(p194r25_2011_rad, p194r26_2011_rad, fun = max)
ggRGB(p194r25r26_2011_rad, stretch="lin")
p194r25r26_2011_rad_crop_studyarea1 = crop(p194r25r26_2011_rad, studyarea1_utmN)
writeRaster(p194r25r26_2011_rad_crop_studyarea1, filename = "results/p194r25r26_2011_rad_crop_studyarea.grd", overwrite=TRUE)
writeRaster(p194r25r26_2011_rad_crop_studyarea1, filename = "results/p194r25r26_2011_rad_crop_studyarea.tif", overwrite=TRUE)

p194r25r26_2011_ref = mosaic(p194r25_2011_ref, p194r26_2011_ref, fun = max)
ggRGB(p194r25r26_2011_ref, stretch="lin")
p194r25r26_2011_ref_crop_studyarea1 = crop(p194r25r26_2011_ref, studyarea1_utmN)
writeRaster(p194r25r26_2011_ref_crop_studyarea1, filename = "results/p194r25r26_2011_ref_crop_studyarea.grd", overwrite=TRUE)
writeRaster(p194r25r26_2011_ref_crop_studyarea1, filename = "results/p194r25r26_2011_ref_crop_studyarea.tif", overwrite=TRUE)



# look for differences (optical) between non-preporcessed and preprocessed images:
# creat smaller extend to look at an extract only/zooms in the centre of image - shrinks the image on 10 % of the whole.
ex_01 = extent(p194r25_2011) * 0.1

#par(mfrow = c(1,3))
unprocessed_01 = crop(p194r25r26_2011_crop_studyarea1, ex_01)
ggRGB(unprocessed_01, r = 3, g = 2, b = 1, stretch = "lin") 
#safed as: p194r25r26_2011_zoom_unprocessed

sdos_01 = crop(p194r25r26_2011_sdos_crop_studyarea1, ex_01)
ggRGB(sdos_01, r = 3, g = 2, b = 1, stretch = "lin")
#safed as: p194r25r26_2011_zoom_sdos

dos_01 = crop(p194r25r26_2011_dos_crop_studyarea1, ex_01)
ggRGB(unprocessed_01, r = 3, g = 2, b = 1, stretch = "lin")
#safed as: p194r25r26_2011_zoom_dos


## 4 A) Loading Atmospherically Corrected Data ####

### 1. read in the atmospherically corrected data
# --> using the pre-processed data from USGS (EarthExplorer), which is provided as XML-file within the data.
# These are already processed to at-surface-reflectance level
# Download the data as described earlier and read it in.

library(RStoolbox)
library(raster)
library(rgdal)
library(ggplot2)

# read in the Meta-Data - these are shipped as xml file.
meta_p194r25_cdr_2011 = readMeta("raster/Corr_LT05_194025_2011101501T1/LT05_L1TP_194025_20111015_20161005_01_T1.xml")
meta_p194r26_cdr_2011 = readMeta("raster/Corr_LT05_194026_2011101501T1/LT05_L1TP_194026_20111015_20161006_01_T1.xml")

# import all layers containing surface reflectance (sre) or brightness temperature (bt).
## !!! The newer products of LandSat data don´t contain brightness temperature any more.
##     If needed, these can be downloaded from https://espa.cr.usgs.gov/index (thermal data can improve the classifcation in some cases)
##     Here: working only with reflectance bands.
p194r25_2011_cdr <- stackMeta(meta_p194r25_cdr_2011, quantity = c("sre", "bt"))
p194r26_2011_cdr <- stackMeta(meta_p194r26_cdr_2011, quantity = c("sre", "bt"))

# The original reflectance data were scaled by a factor 10,000 to store them as integers.
# To rescale to original values they need to be multiplied with the scale_factor from the MetaData.
scaleF_194_25_2011 <- getMeta(p194r25_2011_cdr, meta_p194r25_cdr_2011, what="SCALE_FACTOR") # define the scaling factor
scaleF_194_25_2011 # check the scaling factor
scaleF_194_26_2011 <- getMeta(p194r26_2011_cdr, meta_p194r26_cdr_2011, what="SCALE_FACTOR")
scaleF_194_26_2011

# multiply with scaling factor --> data are in original format.
p194r25_2011_cdr <- p194r25_2011_cdr * scaleF_194_25_2011 
p194r26_2011_cdr <- p194r26_2011_cdr * scaleF_194_26_2011

# also check the data
p194r25_2011_cdr 
p194r26_2011_cdr
# --> data range is not in [0,1] as it should be

# load the study area and crop the data to this extent to save processing time
studyarea1 = readOGR("Wuerzburg_bbox_points", "Wuerzburg_bbox_points") # load the vector data
identical(crs(studyarea1), crs(p194r25_2011_cdr)) # check if the projection is the same
projection(studyarea1) # it is already projection --> projection needs to be changed
studyarea1_utmN = spTransform(studyarea1, CRS = crs(p194r25_2011_cdr)) # Transform the projection of studyarea1 to the ones from the scenes
identicalCRS(studyarea1_utmN, p194r25_2011_cdr) # check again, if projections are the same now. --> Yes.
identicalCRS(studyarea1_utmN, p194r26_2011_cdr)
studyarea1_utmN.df <- as.data.frame(studyarea1_utmN) # transform to data.frame for plotting

p194r25_2011_cdr_crop <- crop(p194r25_2011_cdr, studyarea1_utmN)
p194r26_2011_cdr_crop <- crop(p194r26_2011_cdr, studyarea1_utmN)

# check the distribution of the data points
par(mfrow=c(1,2))
boxplot(p194r25_2011_cdr_crop)
lines(x=c(0,7), y=c(0,0), col="red", lty="dashed")
lines(x=c(0,7), y=c(1,1), col="red", lty="dashed")
boxplot(p194r26_2011_cdr_crop)
lines(x=c(0,7), y=c(0,0), col="red", lty="dashed")
lines(x=c(0,7), y=c(1,1), col="red", lty="dashed")
summary(p194r25_2011_cdr_crop)
summary(p194r26_2011_cdr_crop)
# --> most of the points outside of [0,1] are outlayers/artefacts

# remove those data from the datasets
p194r25_2011_cdr_crop_new <- p194r25_2011_cdr_crop # first create copy of data
query_raster <- ((p194r25_2011_cdr_crop_new[[1:6]] < 0) | (p194r25_2011_cdr_crop_new[[1:6]] > 1)) # define the values which are to replace in all bands (1:6)
p194r25_2011_cdr_crop_new[query_raster] <- NA # set the defined values to NA

p194r26_2011_cdr_crop_new <- p194r26_2011_cdr_crop
query_raster.2 <- ((p194r26_2011_cdr_crop_new[[1:6]] < 0) | (p194r26_2011_cdr_crop_new[[1:6]] > 1))
p194r26_2011_cdr_crop_new[query_raster.2] <- NA

# check the range again
summary(p194r25_2011_cdr_crop_new)
summary(p194r26_2011_cdr_crop_new) 
# range is now in [0,1]


# rename the band names to shorter ones.
names(p194r25_2011_cdr_crop_new) <-  gsub(pattern = "_sre", replace = "", x = names(p194r25_2011_cdr_crop_new))
p194r25_2011_cdr_crop_new # check: all Bands are renamed correctly.
names(p194r26_2011_cdr_crop_new) <-  gsub(pattern = "_sre", replace = "", x = names(p194r26_2011_cdr_crop_new))
p194r26_2011_cdr_crop_new


# safe the single scenes for later use
writeRaster(p194r25_2011_cdr_crop_new, "results/p194r25_2011_cdr_crop_new.grd", overwrite=TRUE)
writeRaster(p194r26_2011_cdr_crop_new, "results/p194r26_2011_cdr_crop_new.grd", overwrite=TRUE)



# create a mosaic from the single scenes for plotting (if calculated earlier)
# therefore: load the scenes calculated before
p194r25_2011_cdr_crop_new <- brick("results/p194r25_2011_cdr_crop_new.grd")
p194r26_2011_cdr_crop_new <- brick("results/p194r26_2011_cdr_crop_new.grd")
p194r25r26_2011_cdr_crop_new <- mosaic(p194r25_2011_cdr_crop_new, p194r26_2011_cdr_crop_new, fun=max)

# plot the true color image
ggRGB(p194r25r26_2011_cdr_crop_new, stretch="lin", geom_raster=TRUE) +
  ggtitle("True color Image with marked studyarea\n(cropped CDR data without topogr. illumin. correction))") +
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
# saved as: p194r25r26_2011_cdr_crop_new

# save the created data for later use
writeRaster(p194r25r26_2011_cdr_crop_new, filename = "results/p194r25r26_2011_cdr_crop_new.grd")


## 4 B) Topographic Illumination Correction of Athmospherically corrected Data ####

library(RStoolbox)
library(raster)
library(rgdal)
library(ggplot2)

# load the single scenes needed for the study site and also the already croped image from before
p194r25_2011_cdr_crop_new <- brick("results/p194r25_2011_cdr_crop_new.grd")
p194r26_2011_cdr_crop_new <- brick("results/p194r26_2011_cdr_crop_new.grd")

# also load again the meta-data files
meta_p194r25_cdr_2011 = readMeta("raster/Corr_LT05_194025_2011101501T1/LT05_L1TP_194025_20111015_20161005_01_T1.xml")
meta_p194r26_cdr_2011 = readMeta("raster/Corr_LT05_194026_2011101501T1/LT05_L1TP_194026_20111015_20161006_01_T1.xml")


# download the elevation data for germany
x <- getData("ISO3") # store all the country codes as variable
x$ISO3[x$NAME == "Germany"] # query the country code for Germany
elev_germany <- getData('alt', country = "DEU") # download the elvation data

# check the projections of the two data sets
projection(elev_germany)
projection(p194r25_2011_cdr_crop_new) # they are different

# change projection of elevation data to image projection
# check if projections are identical now
elev_germany_proj <- projectRaster(elev_germany, crs=crs(p194r25_2011_cdr_crop_new))
identical(projection(elev_germany_proj), projection(p194r25_2011_cdr_crop)) # --> yes, they are


# crop the elevation data to extent of actuell scene
elev_p194r25_proj_crop <- crop(elev_germany_proj, p194r25_2011_cdr_crop_new)
elev_p194r26_proj_crop <- crop(elev_germany_proj, p194r26_2011_cdr_crop_new)

# elevation data has non-square resolution, scene data has square resolution
# --> resample the elevation data to fit the resolution and dimensions of the scene data
elev_p194r25_proj_crop_resampled <- resample(elev_p194r25_proj_crop, p194r25_2011_cdr_crop_new, method="bilinear") # as the two datasets don´t fit, elevation data gets resampled to fit the shape of the scene-image
elev_p194r26_proj_crop_resampled <- resample(elev_p194r26_proj_crop, p194r26_2011_cdr_crop_new, method="bilinear")

# compare if resolutions and dimensions fit now.
p194r25_2011_cdr_crop_new
elev_p194r25_proj_crop_resampled
p194r26_2011_cdr_crop_new
elev_p194r26_proj_crop_resampled # --> yes they do.

# safe the created elevation data
writeRaster(elev_p194r25_proj_crop_resampled, "results/elev_p194r25_proj_crop_resampled.grd")
writeRaster(elev_p194r26_proj_crop_resampled, "results/elev_p194r26_proj_crop_resampled.grd")


# plot the cropped elevation data
p <- ggR(elev_p194r25_proj_crop_resampled, stretch = "lin", geom_raster = TRUE) +
  scale_fill_gradientn(colours=rev(terrain.colors(10)), 
                      name = "Terrain Hight\n") +
  ggtitle("Elevation of p194r25_2011_cdr_crop_new") +
  xlab("x-values") +
  ylab("y-values") +
  theme(plot.title = element_text(color="black", size=15, face="bold.italic"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(size = 11, colour = "black"),
        legend.title = element_text(color="black", size=12, face="bold"),
        legend.justification = c("right", "bottom"),
        legend.direction = "vertical",
        legend.spacing = unit(2,"lines"),
        legend.key.width = unit(3, "lines"),
        legend.key.height = unit(2, "lines"))
p





# Apply the Topographic Illumination Correction (sepperate for each scene, as metadata is needed)
p194r25_2011_cdr_crop_new_illu <- RStoolbox::topCor(p194r25_2011_cdr_crop_new, dem=elev_p194r25_proj_crop_resampled, 
                                metaData=meta_p194r25_cdr_2011, method="C")

p194r26_2011_cdr_crop_new_illu <- RStoolbox::topCor(p194r26_2011_cdr_crop_new, dem=elev_p194r26_proj_crop_resampled,
                                                    metaData=meta_p194r26_cdr_2011, method="C")


# safe the created data
writeRaster(p194r25_2011_cdr_crop_new_illu, "results/p194r25_2011_cdr_crop_new_illu.grd")
writeRaster(p194r26_2011_cdr_crop_new_illu, "results/p194r26_2011_cdr_crop_new_illu.grd")


# connect the two corrected scene-parts to one file
p194r25r26_2011_cdr_crop_new_illu <- mosaic(p194r25_2011_cdr_crop_new_illu, p194r26_2011_cdr_crop_new_illu, fun=max)

# safe the created data
writeRaster(p194r25r26_2011_cdr_crop_new_illu, "results/p194r25r26_2011_cdr_crop_new_illu.grd")


# plot the true color image
studyarea1_utmN.df <- as.data.frame(studyarea1_utmN) # transform to data.frame for plotting
ggRGB(p194r25r26_2011_cdr_crop_new_illu, stretch="lin", geom_raster=TRUE) +
  ggtitle("True color Image with marked studyarea\n(cdr data: cropped, topografic illumination corrected))") +
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
# saved as: p194r25r26_2011_cdr_crop_new_illu




## 5 A) correction for clouds and cloud shadows - data corrected to "top-of-atmosphere radiance" ####

# clouds can be detected through their low thermal radiation and their high reflectance in the visible region of the spectrum.
# --> see ?cloudMask > Note

library(RStoolbox)
library(raster)
library(ggplot2)

# plot the whole area
#ggRGB(p194r25r26_2011_rad_crop_studyarea1, stretch = "lin")

# choose an extent with clouds to work on for getting the parameters (xmin, xmax, ymin, ymax) and plot this image.
#ex = c(538545,  558545, 5485000, 5505000)
#p194r25r26_2011_rad_crop_ex = crop(p194r25r26_2011_rad, ex)
#ggRGB(p194r25r26_2011_rad_crop_ex, stretch = "lin")

# get the parameters for the cloudMask
#cmask_test = cloudMask(p194r25r26_2011_rad_crop_ex, threshold = 0.03, buffer = 3, blue = 1, tir = 6)
#plot(cmask_test)
#ggRGB(p194r25r26_2011_rad_crop_ex, stretch = "lin") +
#  ggR(cmask_test[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
#  scale_fill_manual(values = "red", na.value = NA) 

# create the mask for the clouds
#p194r25r26_2011_cmask = cloudMask(p194r25r26_2011_rad_crop_studyarea1, threshold = 0.03, buffer = 3, blue = 1, tir = 6)# in the book: blue = "layer.1", tir = "layer.6"

# plot the mask
# first layer (CMASK) contains the mask, second layer contains the spectral index used for the threshold.
#plot(p194r25r26_2011_cmask)
# or
#ggRGB(p194r25r26_2011_rad_crop_studyarea1, stretch = "lin") +
#  ggR(p194r25r26_2011_cmask[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
#  scale_fill_manual(values = "red", na.value = NA) 
#saved as: p194r25r26_2011_crop_cmask

#create the cloud shadow 
# --> interactiv: nc is the number of control points (default = 5), choose alternatingpixels with cloud and with it´s shadow
# or  non interaktiv through shiftEstimate
#p194r25r26_2011_shadow = cloudShadowMask(p194r25r26_2011_rad_crop_studyarea1, p194r25r26_2011_cmask, shiftEstimate = c(-16, -6))
  

# merge cloud and shadow data
#p194r25r26_2011_rad_crop_studyarea1_csmask = raster::merge(p194r25r26_2011_cmask[[1]], p194r25r26_2011_shadow)
# plot the clouds with shadows
#ggRGB(p194r25r26_2011_rad_crop_studyarea1, stretch = "lin") +
#  ggR(p194r25r26_2011_rad_crop_studyarea1_csmask, ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
#  scale_fill_manual(values = c("blue", "yellow"), labels = c("shadow", "cloud"), na.value = NA) +
#  coord_equal()
# saved as: p194r25r26_2011_rad_crop_csmask

# save the cmask file
#writeRaster(p194r25r26_2011_rad_crop_studyarea1_csmask, filename = "results/p194r25r26_2011_rad_csmask.grd")
#writeRaster(p194r25r26_2011_rad_crop_studyarea1_csmask, filename = "results/p194r25r26_2011_rad_csmask.tif")



## 5 B) Correction for Clouds and Cloud Shadows - data corrected to "top-of-atmosphere reflectance" (!! minimal correction level that should be used) ####

library(RStoolbox)
library(raster)
library(ggplot2)
library(ggsn)

# load the calculated data of top-of-atmosphere reflectance correction
p194r25r26_2011_ref_crop_studyarea1 = brick("results/p194r25r26_2011_ref_crop_studyarea.grd")

# plot the whole area
ggRGB(p194r25r26_2011_ref_crop_studyarea1, stretch="lin")

# choose an extent with clouds to work on for getting the parameters (xmin, xmax, ymin, ymax) and plot this image.
ex = c(538545,  558545, 5485000, 5505000)
p194r25r26_2011_ref_crop_ex = crop(p194r25r26_2011_ref_crop_studyarea1, ex)
ggRGB(p194r25r26_2011_ref_crop_ex, stretch = "lin")


cmask_test = cloudMask(p194r25r26_2011_ref_crop_ex, blue = 1, tir = 6) # calculate and plot the cloud mask
ggR(cmask_test[[2]], geom_raster=TRUE) 
plot(cmask_test) # plot both the mask itself and the spectral index usedfor the threshold
# choose the threshold for the cloudmask

# use the calculated threshold and additionally a buffer around the cloud pixels
# plot the scene with the clouds
cmask_final = cloudMask(p194r25r26_2011_ref_crop_ex, threshold = 0.0, buffer = 5, blue = 1, tir = 6)
ggRGB(p194r25r26_2011_ref_crop_ex, stretch = "lin") +
  ggR(cmask_final[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
  scale_fill_manual(values = "red", na.value = NA) 

# create the mask for the whole image and safe the result
p194r25r26_2011_ref_cmask = cloudMask(p194r25r26_2011_ref_crop_studyarea1, threshold = 0.0, buffer = 5, blue = 1, tir = 6)# in the book: blue = "layer.1", tir = "layer.6"
writeRaster(p194r25r26_2011_ref_cmask, "results/p194r25r26_2011_ref_cmask.tif", overwrite=TRUE)
writeRaster(p194r25r26_2011_ref_cmask, "results/p194r25r26_2011_ref_cmask.grd", overwrite=TRUE)

# plot the mask
# first layer (CMASK) contains the mask, second layer contains the spectral index used for the threshold.
plot(p194r25r26_2011_ref_cmask)
# plot the scene with the cloud mask
ggRGB(p194r25r26_2011_ref_crop_studyarea1, stretch = "lin") +
  ggR(p194r25r26_2011_ref_cmask[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("True color plot with Cloud Mask\n(top-of-atmosphere reflectance data)") +
  scale_fill_manual(values = "red", na.value = NA,
                    labels=c("Clouds", "NAs"),
                    name="CloudMask")
#saved as: p194r25r26_2011_ref_crop_cmask

#create the cloud shadow 
# --> interactiv: nc is the number of control points (default = 5), choose alternatingpixels with cloud and with it´s shadow
# or  non interaktiv through shiftEstimate
p194r25r26_2011_ref_cshadow = cloudShadowMask(p194r25r26_2011_ref_crop_studyarea1, p194r25r26_2011_ref_cmask, shiftEstimate = c(-16, -6))
writeRaster(p194r25r26_2011_ref_cshadow, "results/p194r25r26_2011_ref_cshadow.tif", overwrite=TRUE)
writeRaster(p194r25r26_2011_ref_cshadow, "results/p194r25r26_2011_ref_cshadow.grd", overwrite=TRUE)

# merge cloud and shadow data
p194r25r26_2011_ref_crop_csmask = raster::merge(p194r25r26_2011_ref_cmask[[1]], p194r25r26_2011_ref_cshadow)

# plot the clouds with shadows
ggRGB(p194r25r26_2011_ref_crop_studyarea1, stretch = "lin") + 
  ggR(p194r25r26_2011_ref_crop_csmask, ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("True color plot with Cloud-/Shadow Mask\n(top-of-atmosphere reflectance data)") +
  scale_fill_manual(values = c("blue", "yellow"), 
                    labels = c("shadow", "cloud"), 
                    na.value = NA, 
                    name="CloudMask")
# saved as: p194r25r26_2011_ref_crop_csmask

# save the csmask
writeRaster(p194r25r26_2011_ref_crop_csmask, filename = "results/p194r25r26_2011_ref_csmask.grd", overwrite=TRUE)
writeRaster(p194r25r26_2011_ref_crop_csmask, filename = "results/p194r25r26_2011_ref_csmask.tif", overwrite=TRUE)


# Apply the created mask to the image.
p194r25r26_2011_ref_csmasked <- mask(p194r25r26_2011_ref_crop_studyarea1, 
                                     p194r25r26_2011_ref_crop_csmask,
                                     inverse = T, # like this the mask excludes the pixels covered with clouds and includes everythink else.
                                     maskvalue = NA)

# plot the created masked image (clouds are excluded)
ggRGB(p194r25r26_2011_ref_csmasked, stretch = "lin", geom_raster=TRUE) +
  ggtitle("True color plot - clouds are excluded\n(top-of-atmosphere reflectance data)") 
# saved as: p194r25r26_2011_ref_csmasked

# save the result.
writeRaster(p194r25r26_2011_ref_csmasked, filename = "results/p194r25r26_2011_ref_csmasked.grd", overwrite=TRUE)
writeRaster(p194r25r26_2011_ref_csmasked, filename = "results/p194r25r26_2011_ref_csmasked.tif", overwrite=TRUE)


## (!!! probably the cloud is "wrongly" classified as cloud, probably due to bad data quality
#       due to processing level
#   --> see 5 C) approach for cdr-data!)


## 5 C) Correction for Clouds and Cloud Shadows - data is atmospherically corrected (commanded CDR data + topographic illumination correction) ####

# load the topographic illumination corrected data
p194r25r26_2011_cdr_crop_new_illu <- brick("results/p194r25r26_2011_cdr_crop_new_illu.grd")

# choose an extent with clouds to work on for getting the parameters and plot this image.
ex = c(538545,  558545, 5485000, 5505000)
p194r25r26_2011_cdr_crop_new_illu_ex = crop(p194r25r26_2011_cdr_crop_new_illu, ex)
ggRGB(p194r25r26_2011_cdr_crop_new_illu_ex, stretch = "lin")


cmask_test_illu = cloudMask(p194r25r26_2011_cdr_crop_new_illu_ex, blue = 1, tir = 6) # calculate and plot the cloud mask
ggR(cmask_test_illu[[2]], geom_raster=TRUE) 
plot(cmask_test_illu) # plot both the mask itself and the spectral index usedfor the threshold
# choose the threshold for the cloudmask

# use the calculated threshold and additionally a buffer around the cloud pixels
# plot the scene with the clouds
cmask_final_illu = cloudMask(p194r25r26_2011_cdr_crop_new_illu, threshold = -0.95, buffer = 5, blue = 1, tir = 6)
ggRGB(p194r25r26_2011_cdr_crop_new_illu, stretch = "lin") +
  ggR(cmask_final_illu[[1]], ggLayer = TRUE, forceCat = TRUE, geom_raster = TRUE) +
  scale_fill_manual(values = "red", na.value = NA)

## --> even if really low threshold-values are chosen, only water gets marked
#  --> no clouds on the picture, scene/data is used without cloud correction further on.



## 6) plotting the Images ####

library(RStoolbox)
library(ggplot2)
par(mar = c(2,2,2,0))

# transform studyarea1_utmN into dataframe for plotting.
studyarea1_utmN_df = as.data.frame(studyarea1_utmN) 


# plot in real color composition, pixel number is not limited (not preprocessed)
par(mar = c(2,5,3,0))
plotRGB(p194r25r26_2011_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin", 
        axes = TRUE,
        main = "studyarea - true colour - not preprocessed")
plot(studyarea1_utmN, add = TRUE, col = "red", pch = 19, lwd = 2)
raster:scalebar(d = 10000, xy = click(), type = "bar", divs = 2, below = "Kilometer", label = c(0,5,10))

# other possibility (plot non-prepocessed:
ggRGB(p194r25r26_2011_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin")

# plot dos
par(mar = c(2,5,3,0))
plotRGB(p194r25r26_2011_dos_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin", 
        axes = TRUE,
        main = "studyarea - true colour - dos")
plot(studyarea1_utmN, add = TRUE, col = "red", pch = 19, lwd = 2)
raster:scalebar(d = 10000, xy = click(), type = "bar", divs = 2, below = "Kilometer", label = c(0,5,10))


# plot sdos
ggRGB(p194r25r26_2011_sdos_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin")


# more complete plot
plotRGB(p194r25r26_2011_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin")
box(which = "plot", lty = "solid")
axis(side = 1, tick = TRUE, xpd = TRUE)
axis(side = 2, tick = TRUE, xpd = TRUE)
raster:scalebar(d = 10000, xy = click(), type = "bar", divs = 2, below = "Kilometer", label = c(0,5,10))
plot(studyarea1_utmN, add = TRUE, col = "red", pch = 1, lwd = 2)


# other possibility to plot in real color composition, pixel number is not limited.
ggRGB(p194r25r26_2011_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin") +
  geom_point(data = studyarea1_utmN_df, aes(x = coords.x1, y = coords.x2), size = 5, colour = "red") +
  coord_equal()


# plot in real color composition, pixel number is limited.
ggRGB(p194r25r26_2011_crop_studyarea1, r = 3, g = 2, b = 1, stretch = "lin", maxpixels = 1e+05) +
  geom_point(data = studyarea1_utmN_df, aes(x = coords.x1, y = coords.x2), size = 5, colour = "red") +
  coord_equal()


# plot band 5 only in grey colours
plot(p194r25r26_2011_cropStudyarea1, 5, col = grey.colors(100)) 
plot(studyarea1_utmN, add = TRUE, col = "red", pch = 1, lwd = 2)


# plot in false color composition, pixel number is not limited.
ggRGB(p194r25r26_2011, r = 4, g = 3, b = 2, stretch = "lin") +
  geom_point(data = studyarea1_utmN_df, aes(x = coords.x1, y = coords.x2), size = 5, colour = "red") +
  coord_equal()

## 7) Calculation of Vegetation Indices ####
library(RStoolbox)
library(raster)
library(rgdal)
library(ggplot2)

# Vegetation Indices give information of the healthyness of the vegetation.
# Healthy vegetation shows very low reflectance in the red region and high reflectance in the NIR (near-wave infrared) region.
# --> decreasing health and thus decreasing photosynthetic activity causes decreasing differences between the red and the NIR reagion.

# Often used is the NDVI, which is linked to vegatiotion cover, biomass (thus primary productivity)
# It´s calculated as (NIR - red)/(NIR + red).
# This equals Landsat 5 (band4 - band3)/(band4 + band 3).
# Values range from -1 to 1, negativ: water, near 0: non-vegetated, higher: higher photosynthesis

# Here also used: DVI
# calculated as: (NIR - RED)
# one of the very first VIs

# Here also used: MSAVI2
# calculated as: (2*NIR + 1 - sqrt((2*NIR + 1)^2 - 8*(NIR - RED))) / 2
# results my be better if vegetation is really sparse or dense


# load the dataset that shell be used
# (top-of-atmosphere reflectance data, topographic illumination corrected cdr data)
p194r25r26_2011_ref_csmasked <- brick("results/p194r25r26_2011_ref_csmasked.grd")
p194r25r26_2011_cdr_crop_new_illu <- brick("results/p194r25r26_2011_cdr_crop_new_illu.grd")


########### 1. NDVI-calculation "by hand" 
p194r25r26_2011.ndvi_ref_csmasked = ((p194r25r26_2011_ref_csmasked$layer.4 - p194r25r26_2011_ref_csmasked$layer.3) / 
                              (p194r25r26_2011_ref_csmasked$layer.4 + p194r25r26_2011_ref_csmasked$layer.3))

p194r25r26_2011.ndvi_cdr_crop_new_illu = ((p194r25r26_2011_cdr_crop_new_illu$layer.4 - p194r25r26_2011_cdr_crop_new_illu$layer.3) / 
                              (p194r25r26_2011_cdr_crop_new_illu$layer.4 + p194r25r26_2011_cdr_crop_new_illu$layer.3))


# save the results for later use
writeRaster(p194r25r26_2011.ndvi_ref_csmasked, "results/p194r25r26_2011.ndvi_ref_csmasked.grd")
writeRaster(p194r25r26_2011.ndvi_cdr_crop_new_illu, "results/p194r25r26_2011.ndvi_cdr_crop_new_illu.grd")







######## 2. DVI-calculation by using a self-defined formula:

# define the fomula for dvi:
dvi_fomula <- function(nir, red) {  # nir and red are defined as the variables.
  (nir - red)                       # the function including these variables is defined
}

# calculate the dvi
p194r25r26_2011.dvi_cdr_crop_new_illu <- overlay(p194r25r26_2011_cdr_crop_new_illu$layer.4, 
                                                 p194r25r26_2011_cdr_crop_new_illu$layer.3, 
                                                 fun=dvi_fomula)
                                                  # add filename="..." to automatically store result in directory

# save the result for later use
writeRaster(p194r25r26_2011.dvi_cdr_crop_new_illu, "results/p194r25r26_2011.dvi.cdr_crop_new_illu.grd")







######### 3. Calculation of VIs by using RStoolbox::spectralIndices.

# calculate the MSAVI2
p194r25r26_2011.MSAVI2_cdr_crop_new_illu <- spectralIndices(p194r25r26_2011_cdr_crop_new_illu, 
                                                       red = "layer.3", 
                                                       nir = "layer.4", 
                                                       indices = "MSAVI2")
# save for later use
writeRaster(p194r25r26_2011.MSAVI2_cdr_crop_new_illu, "results/p194r25r26_2011.MSAVI2_cdr_crop_new_illu.grd")






######### 4. Plot the ndvi-data for the top-of-atmosphere reflectance data

# load the csmask
p194r25r26_2011_ref_crop_csmask <- raster("results/p194r25r26_2011_ref_csmask.grd")

# plot the NDVI - values 
p <- ggR(p194r25r26_2011.ndvi_ref_csmasked, stretch="lin", geom_raster = TRUE) +
  scale_fill_gradient2(low="darkred", mid="orange", high="darkgreen", 
                       name="NDVI-values\n", 
                       midpoint=0) +
  ggtitle("NDVI\n(top-of-atmosphere reflectance data)") +
  theme(legend.spacing = unit(2,"lines"), 
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(colour = "black", size = 11, face="bold"),
        legend.title = element_text(color="black", size=12, face="bold"))
p
# saved as: p194r25r26_2011.ndvi_ref_csmasked





########## 5. Plot the NDVI-/DVI-/MSAVI2- data for the cdr-data by using a loop

# define the names of VIs for plotting
VI_names <- c("NDVI", "DVI", "MSAVI2")

for (i in 1:length(VI_names)){ # for the whole vector of VIs (name[1] to last name) ...
  
  # query/ define which of the calculated VIs shell be plotted
  # i must be equal to position in names-vector of correct VI-name
  if (i==1){
           X <- p194r25r26_2011.ndvi_cdr_crop_new_illu
          }else{
                if (i==2){
                         X <- p194r25r26_2011.dvi_cdr_crop_new_illu
                        }else{
                              X <- p194r25r26_2011.MSAVI2_cdr_crop_new_illu
                             }
          }
  
  # plot each calculation automatically within the loop
  p <- ggR(X, geom_raster = TRUE) +
    ggtitle(paste("Vegetation Index:", VI_names[i])) +
    theme(legend.spacing = unit(2,"lines"), 
          plot.title = element_text(color="black", size=15, face="bold"),
          axis.title.x = element_text(color="black", size=12, face="bold"),
          axis.title.y = element_text(color="black", size=12, face="bold"), 
          legend.text = element_text(colour = "black", size = 11, face="bold"),
          legend.title = element_text(color="black", size=12, face="bold")) +
    scale_fill_gradient2(low="darkred", mid="white", high="darkgreen", 
                         name=paste(VI_names[i], "-values\n"))
  print(p)
}

#saved as:  p194r25r26_2011.ndvi_cdr_crop_new_illu
#           p194r25r26_2011.dvi_cdr_crop_new_illu
#           p194r25r26_2011.MSAVI2_cdr_crop_new_illu






####### 6. Compare Vegetation Indices (here calculated NDVI, DVI and MSAVI2 of cdr-data)

# A) via standard deviation between the vegetation indices:
# create a RasterBrick with the three Indices
p194r25r26_2011_cdr_crop_new_illu_viStack = spectralIndices(p194r25r26_2011_cdr_crop_new_illu, 
                                                            red = "layer.3", 
                                                            nir = "layer.4", 
                                                            indices = c("DVI", "NDVI", "MSAVI2"))


# calculate the standard deviation to show, where the VIs differ and plot it.
p194r25r26_2011_cdr_crop_new_illu_viStack_sd = calc(p194r25r26_2011_cdr_crop_new_illu_viStack, fun = sd)

p <- ggR(p194r25r26_2011_cdr_crop_new_illu_viStack_sd, geom_raster = TRUE) +
  ggtitle("Difference between VIs: Standard Deviation\n(Comparison of NDVI, DVI, MSAVI2 for p194r25r26_cdr)") +
  theme(legend.spacing = unit(2,"lines"), 
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(colour = "black", size = 11, face="bold"),
        legend.title = element_text(color="black", size=12, face="bold")) +
  scale_fill_gradient(low="white", high="darkred", 
                       name="Standard Deviation\n")
(p)

# saved as: p194r25r26_2011_cdr_crop_new_illu_viStack_sd

# safe result for later use
writeRaster(p194r25r26_2011_cdr_crop_new_illu_viStack_sd, "results/p194r25r26_2011_cdr_crop_new_illu_viStack_sd.grd")




# B) via spatial correlation between two indices (!!! proces cancelled - took too long - to much workspace needed??):
# --> "local correlation analysis", several pixels from one raster are compared with the corresponding pixels
# in the second raster with raster::corLocal (ngb=number of pixels to compare per neighbourhood)

# create an extract with smaller extent (otherwise: dataset to large)
plot(p194r25r26_2011_cdr_crop_new_illu_viStack$NDVI)
ex <- drawExtent()
# chosen extent:
#xmin        : 552959.8 
#xmax        : 573548.8 
#ymin        : 5504473 
#ymax        : 5521547

# crop the VI-stack to smaller extent
p194r25r26_2011_cdr_crop_new_illu_viStack_ex <- crop(p194r25r26_2011_cdr_crop_new_illu_viStack, ex)
p194r25r26_2011_cdr_crop_new_illu_ex <- crop(p194r25r26_2011_cdr_crop_new_illu, ex)

p194r25r26_2011_cdr_crop_new_illu_ex_NDVI_DVI_cor <- corLocal(p194r25r26_2011_cdr_crop_new_illu_viStack_ex$NDVI, 
                                                           p194r25r26_2011_cdr_crop_new_illu_viStack_ex$DVI, 
                                                           ngb = 11,              # size of the neighborhood.
                                                           method = "spearman")

p <- ggR(p194r25r26_2011_cdr_crop_new_illu_ex_NDVI_DVI_cor, geom_raster = TRUE) +
  ggtitle("Difference between VIs: Spatial Correlation\n(Comparison of NDVI and DVI for p194r25r26_cdr_ex)") +
  theme(legend.spacing = unit(2,"lines"), 
        plot.title = element_text(color="black", size=15, face="bold"),
        axis.title.x = element_text(color="black", size=12, face="bold"),
        axis.title.y = element_text(color="black", size=12, face="bold"), 
        legend.text = element_text(colour = "black", size = 11, face="bold"),
        legend.title = element_text(color="black", size=12, face="bold")) +
  scale_fill_gradientn(colours= rainbow(6), 
                    name="Correlation\n")
(p) # --> smaller correlation at edges
ggRGB(p194r25r26_2011_cdr_crop_new_illu_ex, stretch="lin")


# saved as: p194r25r26_2011_cdr_crop_new_illu_ex_NDVI_DVI_cor
writeRaster(p194r25r26_2011_cdr_crop_new_illu_ex_NDVI_DVI_cor, "results/p194r25r26_2011_cdr_crop_new_illu_ex_NDVI_DVI_cor.grd")


## 8 A) Unsupervised Land Cover classification ####

library(raster)
library(RStoolbox)
library(ggplot2)


###### 1. at-surface-reflectance data 




# load the raster data (calculated and saved earlier)
p194r25r26_2011_ref_csmasked <- brick("results/p194r25r26_2011_ref_csmasked.grd")

# initialize R´s random number generator --> random number will be the same each time one runs the code --> reproducible
set.seed(6)

# unsupervised clustering of Raster data using kmeans clustering:
# Based on the data set, clusters of data points are created.
# Kmeans algorithm tries to find clusters with minimal within-cluster variation.
# Iterative --> result depends on starting configuration!
# Best of several random starting configurations will be chosen.

p194r25r26_2011_ref_uc <- unsuperClass(p194r25r26_2011_ref_csmasked, 
                                       nClasses = 4, 
                                       nStarts = 50, # number of random starts for kmeans algorithms
                                       nSamples = 10000, # sample number to fit cluster map
                                       norm = TRUE) # data will be normalized, because variables at different scales are used (reflectance and brightness temperature)

# plot the uc_classification - with ggR()
cols <- c("1"="darkgreen", "2"="blue", "3"="darkred", "4"="yellow")
ggR(p194r25r26_2011_ref_uc$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification\n(p194r25r26_2011_ref)") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class 1", "Class 2" ,"Class 3", "Class 4"), 
                    name = "Classes\n")
# saved as: p194r25r26_2011_ref_uc

# save the result (only classification map)
writeRaster(p194r25r26_2011_ref_uc$map, "results/p194r25r26_2011_ref_uc.grd")



# create an extract from the classification to have a closer look
# plot the whole area to get the extent
plotRGB(p194r25r26_2011_ref_csmasked, stretch = "lin")
extract <- drawExtent() # draw the extent of the extract
# chosen extent:
#class       : Extent 
#xmin        : 552094.9 
#xmax        : 581346.8 
#ymin        : 5497375 
#ymax        : 5520540 

# crop the classification and the true colour image to the smaller extent
p194r25r26_2011_ref_uc_extract <- crop(p194r25r26_2011_ref_uc$map, extract)
p194r25r26_2011_ref_csmasked_extract <- crop(p194r25r26_2011_ref_csmasked, extract)


# plot the extract - true colour and classified
ggRGB(p194r25r26_2011_ref_csmasked_extract, stretch="lin") +
  ggtitle("Extract - True colour image\n(p194r25r26_2011_ref_extract)")

cols <- c("1"="darkgreen", "2"="blue", "3"="darkred", "4"="yellow")
ggR(p194r25r26_2011_ref_uc_extract, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Extract - Unsupervised classification\n(p194r25r26_2011_ref)") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class 1", "Class 2" ,"Class 3", "Class 4"), 
                    name = "Classes\n")
#saved as: p194r25r26_2011_ref_uc_extract



## 8 B) Unsupervised Classification - manually done ####
## manual approach only works for small rasters, because all needs to be loaded into memory.
library(RStoolbox)

# subset the raster to only the first four bands
p194r25r26_2011_ref_csmasked_extract_sub <- p194r25r26_2011_ref_csmasked_extract[[c(1:4)]]

# run the actual clustering
p194r25r26_2011_ref_csmasked_kmeans <- kmeans(na.omit(p194r25r26_2011_ref_csmasked_extract_sub[]), # raster will be read as matrix. kmeans can´t deal with NAs --> omit them
                                              centers = 5, # five cluster centers are chosen
                                              iter.max = 100, # max. 100 iterations per run
                                              nstart = 10) # 10 random starting configurations

# created object is not spatial --> needs to be transformed to a raster object
# therefore: create an empty raster object with same projection, extent, resolution and number of rows/
# columns as the original raster
p194r25r26_2011_ref_csmasked_uc2 <- raster(p194r25r26_2011_ref_csmasked_extract)

# the empty raster can then be filled up with the actual cluster values
p194r25r26_2011_ref_csmasked_uc2[] <- p194r25r26_2011_ref_csmasked_kmeans$cluster

# write the raster to hardisk for later use
writeRaster(p194r25r26_2011_ref_csmasked_uc2, filename = "results/p194r25r26_2011_ref_csmasked_uc2", 
            dataType = "INT1U", # store as unsigned integer format --> reduced file size
            overwrite = TRUE)
writeRaster(p194r25r26_2011_ref_csmasked_uc2, filename = "results/p194r25r26_2011_ref_csmasked_uc2", 
            dataType = "INT1U",
            overwrite = TRUE)




####### 2. cdr-data (topographic illumination corrected)

# load the raster data (calculated and saved earlier)
p194r25r26_2011_cdr_crop_new_illu <- brick("results/p194r25r26_2011_cdr_crop_new_illu.grd")

# initialize R´s random number generator --> random number will be the same each time one runs the code --> reproducible
set.seed(6)

# run the classification
p194r25r26_2011_cdr_crop_new_illu_uc <- unsuperClass(p194r25r26_2011_cdr_crop_new_illu, 
                                                     nClasses = 4, 
                                                     nStarts = 50, # number of random starts for kmeans algorithms
                                                     nSamples = 10000, # sample number to fit cluster map
                                                     norm = TRUE) # data will be normalized, because variables at different scales are used (reflectance and brightness temperature)

cols <- c("1"="darkgreen", "2"="blue", "3"="darkred", "4"="yellow")
ggR(p194r25r26_2011_cdr_crop_new_illu_uc$map, forceCat = TRUE, geom_raster = TRUE) +
  ggtitle("Unsupervised classification\n(p194r25r26_2011_cdr_crop_new_illu)") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size=11, colour="black", face="bold")) +
  scale_fill_manual(values = cols, 
                    labels=c("Class 1", "Class 2" ,"Class 3", "Class 4"), 
                    name = "Classes\n")

# saved as: p194r25r26_2011_cdr_crop_new_illu_uc
writeRaster(p194r25r26_2011_cdr_crop_new_illu_uc$map, "results/p194r25r26_2011_cdr_crop_new_illu_uc.grd") # (only classification map is safed)




## 9) Supervised Land Cover Classification ####

### Training data needs to be collected.
# Field data and the imagery for classification should be aquired as close to the same date if possible.
# Training data should be collected from homogenous areas but contain all intra-class variation at the same time.
# Training data is collected by on-screen digitizing in QGIS:
  # - create a new shapefile layer
  # - make it editable
  # - add new features
  # - create polygon outlines (left mouse button)
  # - close polygons (right mouse button)
  # - add an ID and description in the pop-up menu

### Also Validation data needs to be collected.
# V. data is needed for assessing the quality of the classification result.
# V. data must be independent from training data (no overlapping polygons).
# V. data should hold the same classes.
## --> load the study region into QGIS (here used: p194r25r26_2011_ref_csmasked.tif) 
##     and digitize the different surfaces


library(rgdal)
library(RStoolbox)
library(raster)
library(ggplot2)
library(randomForest)
library(maptools)


# read in the image for plotting the study area
#p194r25r26_2011_ref_csmasked <- brick("results/p194r25r26_2011_ref_csmasked.grd")
p194r25r26_2011_cdr_crop_new_illu <- brick("results/p194r25r26_2011_cdr_crop_new_illu.grd")


# read in the training- and validation data (defined in QGIS), or other layers that can be usefull for the classification:
td_p194r25r26_2011_2 <- readOGR(dsn="vector", layer="td_sc_p194r25r26_2011_2")
vd_p194r25r26_2011_2 <- readOGR("vector", "vd_sc_p194r25r26_2011_2")
p194r25r26_2011.ndvi_cdr_crop_new_illu <- raster("results/p194r25r26_2011.ndvi_cdr_crop_new_illu.grd")

# check if projections match
identical(projection(td_p194r25r26_2011_2), projection(p194r25r26_2011_cdr_crop_new_illu))

# prepare the data for plotting
td_p194r25r26_2011_2_df <- fortify(td_p194r25r26_2011_2, region="id") # transformation in dataframe
td_p194r25r26_2011_2_df_final <- merge(td_p194r25r26_2011_2_df, td_p194r25r26_2011_2@data, by="id") # add Spatial Information


# check that the two layers overlay without reprojecting:
# -->plot raster together with the shapefiles to check overlay
# (same can be done with each combination of traning-/validation and raster data)
ggRGB(p194r25r26_2011_cdr_crop_new_illu, stretch="lin") +
  geom_polygon(data=td_p194r25r26_2011_2_df_final, 
               mapping=aes(x=long, 
                           y=lat, 
                           group=group, 
                           fill=name)) +
  ggtitle("True colour scene with training data\np194r25r26_2011 ref-data") +
  theme(plot.title=element_text(size=12, color="black", face="bold")) +
  guides(fill=guide_legend(title="Class names"))





########## 1. Parts of the training-data are used to classify the image

# run the classification
p194r25r26_2011_cdr_crop_new_illu_sc_1 <- superClass(img = p194r25r26_2011_cdr_crop_new_illu, 
                                       model = "rf", 
                                       trainData = td_p194r25r26_2011_2,
                                       trainPartition = 0.6, # 60% of training data are used for training, 40% are used for validation
                                       responseCol = "id")

# have a look on the classifcation
p194r25r26_2011_cdr_crop_new_illu_sc_1
p194r25r26_2011_cdr_crop_new_illu_sc_1$map







########## 2. Training data and validation data are available as input

# run the classification
p194r25r26_2011_cdr_crop_new_illu_sc_2 <- superClass(img = p194r25r26_2011_cdr_crop_new_illu, 
                                                     model = "rf", 
                                                     trainData = td_p194r25r26_2011_2, 
                                                     valData = vd_p194r25r26_2011_2,
                                                     responseCol = "id")





########## 3. Adding additional layers can improve the classification (here NDVI)

# load the NDVI layer calculated earlier
p194r25r26_2011.ndvi_cdr_crop_new_illu <- raster("results/p194r25r26_2011.ndvi_cdr_crop_new_illu.grd")

#stack the ndvi layer with the raster layer
p194r25r26_2011_cdr_crop_new_illu_ndvi_stack <- stack(p194r25r26_2011_cdr_crop_new_illu, p194r25r26_2011.ndvi_cdr_crop_new_illu)


# run the classification
p194r25r26_2011_cdr_crop_new_illu_sc_3 <- superClass(img = p194r25r26_2011_cdr_crop_new_illu_ndvi_stack, 
                                                     model = "rf", 
                                                     trainData = td_p194r25r26_2011_2, 
                                                     valData = vd_p194r25r26_2011_2,
                                                     responseCol = "id")


# compare accuracy of this classification with equivalent classification without NDVI
p194r25r26_2011_cdr_crop_new_illu_sc_2$modelFit # overall accuracy: 0.0.9189712
p194r25r26_2011_cdr_crop_new_illu_sc_3$modelFit # overall accuracy: 0.9262914 (NDVI included)





########## 4. Different models for the classification can be compared

# possible models can be found with:
names(getModelInfo())

# store the names of models which are to use in a vector
model_names <- c("rf", "svmRadial", "pls")
#model_names <- c("rf", "svmRadial", "pls", "extraTrees", "RRFglobal")
#model_names <- names(getModelInfo())

# define the colors used for plotting
cols <- c("1"="seagreen3", "2"="orange", "3"="khaki1", "4"="blue")

# run a loop which calculates all models from the vector
# (and plots them automatically if you like)

# define a progress bar, to make processing progress visible (opens in new window)
#progression <- winProgressBar(title = "Progress bar", min = 0, max = length(model_names), width = 300)

for (i in 1:length(model_names)){ # for the whole vector of models (model[1] to last model) ...
  sc_4 <- superClass(p194r25r26_2011_cdr_crop_new_illu, 
                     trainData=td_p194r25r26_2011_2, 
                     responseCol = "id",
                     valData=vd_p194r25r26_2011_2,
                     model=model_names[i]) # ... run the classification like so and use model[i]]
  
   names(sc_4$map) <- model_names[i] # directly rename the name of the layers (new name == name of used model)
  
   #plot each classifcation automatically within the loop
    p <- ggR(sc_4$map, forceCat = TRUE, geom_raster = TRUE) +
      ggtitle(paste("p194r25r26_2011_crop_new_illu\nSupervised classification - model:", model_names[i])) +
      theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
            legend.title= element_text(size=11, colour="black", face="bold")) +
      scale_fill_manual(values = cols, 
                        labels=c("Class1: Forest", "Class2: Urban", "Class3: Arable", "Class4: Water"), 
                       name = "Landcover\nClasses\n")
    print(p) 
  
  
  # for later comparison:
  if (i==1){                              # in the first run: safe the sc-result as sc_stack
    sc_4_stack <- sc_4$map
    sc_4_modelFit <- sc_4$modelFit[[1]]
  } else{                                 # later on: stack the actual sc-result with sc_stack 
    sc_4_stack <- stack(sc_4_stack, sc_4$map)
    sc_4_modelFit <- rbind(sc_4_modelFit, sc_4$modelFit[[1]]) # save the modelFit parameters to select model with best accuracy afterwards
    
  }
  
  
  # add a progression bar to show the progress of calculation
  #progress <- i
  #setWinProgressBar(progression, 
  #                  progress, 
  #                  title=paste(round(progress/length(model_names))*100,"% done"))
  
}


# plots saved as: p194r25r26_2011_crop_new_illu_sc_4_"modelname"





########## 6. Accuracy Assessment

# query information about the accuracy
p194r25r26_2011_cdr_crop_new_illu_sc_4$validation$performance 
p194r25r26_2011_cdr_crop_new_illu_sc_4$modelFit

# directly query the overall accuracy of a classification
p194r25r26_2011_cdr_crop_new_illu_sc_3$modelFit[[1]]["TrainAccuracy"]

# validate the classification with validateMap()
p194r25r26_2011_cdr_crop_new_illu_sc_3 <- validateMap(map=p194r25r26_2011_cdr_crop_new_illu_sc_3$map, 
                                                      valData=vd_p194r25r26_2011_2, 
                                                      responseCol="id", 
                                                      nSamples=1000, 
                                                      mode="classification")

# check the Entropy between different approaches
p194r25r26_2011_crio_new_illu_sc_stack <- stack(p194r25r26_2011_cdr_crop_new_illu_sc_1$map, 
                                                p194r25r26_2011_cdr_crop_new_illu_sc_2$map)
p194r25r26_2011_crio_new_illu_sc_entropy <- rasterEntropy(p194r25r26_2011_crio_new_illu_sc_stack) # calculate the Entropy between single classifications

# plot also the entropy of different classifications
p <- ggR(p194r25r26_2011_crio_new_illu_sc_entropy, geom_raster=TRUE) +
  ggtitle("Entropy between Classifications\np194r25r26_2011") +
  theme(plot.title = element_text(size = 12, colour = "black", face="bold"), 
        legend.title=element_text(size = 1, colour = "black", face="bold")) +
  scale_fill_gradientn(colours=rev(rainbow(4)), 
                       name="Entropy\n")
p















## RESTE #####
# find matching surface between p194r25_2011 and the studysite:
p194r25_2011_maskStudyarea1 = mask(p194r25_2011, studyarea1_utmN) # umriss der Fläche, die mit studyarea1 übereinstimmt.
p194r25_2011_cropStudyarea1 = crop(p194r25_2011, studyarea1_utmN) # Fläche, die mit Studyarea1 übereinstimmt --> gewünschter Plot.
plot(p194r25_2011_maskStudyarea1)
plot(p194r25_2011_cropStudyarea1) # check, if the write part was selected.

# plot matching surface of the Satellite data, which is interesting.
ggRGB(p194r25_2011_cropStudyarea1, r = 4, g = 3, b = 2, stretch = "lin") +
  geom_point(data = studyarea1_utmN_df, aes(x = coords.x1, y = coords.x2), size = 5, colour = "red") +
  coord_equal()
ggRGB(p194r25_2011_cropStudyarea1, r = 3, g = 2, b = 1, stretch = "lin") +
  geom_point(data = studyarea1_utmN_df, aes(x = coords.x1, y = coords.x2), size = 5, colour = "red") +
  coord_equal()


# write a multiband GeoTiff:
writeRaster(p194r25_2011, filename = "LT05_L1TP_194025_20111015_20161005_01/LT0519402520111015.tif")

# write also a grd file (same data as GeoTiff)
writeRaster(p194r25_2011, filename = "LT05_L1TP_194025_20111015_20161005_01/LT0519402520111015.grd")

# to import all Bands from the GeoTiff data later on, the current scene gets processed and stored as .grd file:
#p194r25_2011 = brick("LT05_L1TP_194025_20111015_20161005_01/LT0519402520111015.grd")

inMemory(p194r25_2011)

# converting the data into a SpatialPixelsDataFrame and back into a Rasterbrick     !!! didn´t work. computer broke down.
#p194r25_2011_sp = as(p194r25_2011, "SpatialPixelsDataFrame")
#class(p194r25_2011_sp)
#p194r25_2011_ras = as(p194r25_2011_sp, "RasterBrick")
# --> through the conversion from sp to rasterBrick, the data stays in Memory
# --> can be deleted or written as file:
writeRaster(p194r25_2011, filename = "LT05_L1TP_194025_20111015_20161005_01/raster_data/p194r25.tif")









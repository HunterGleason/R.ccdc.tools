# R.ccdc.tools
Simple R tools for working with Continuous Change Detection and Classification (Zhu &amp; Woodcock 2014) images. 

# Installation
````
remotes::install_github('HunterGleason/R.ccdc.tools')

````

# Example
````

#Imports
library(R.ccdc.tools)
library(stars)
library(sf)
library(terra)

#Read in a CDCC image example data (see https://github.com/parevalo/gee-ccdc-tools) as a stars object
ccdc_img = exampleData

#Example coordinates (EPSG:3005) within the image bounds
x_coord<-1794708.42
y_coord<-611511.36
epsg<-3005

#Extract the model coeffcients at the point from the CCDC image
DF <- data.frame(x = c(x_coord), y = c(y_coord))
DF_sf = st_as_sf(DF, coords = c("x", "y"), 
                 crs = st_crs(epsg), agr = "constant")
extracted <- st_as_sf(st_extract(ccdc_img, DF_sf))

#View the coeffcients for each segment, these are used in 'get_ccdc_ts' to generate the time series data
ccdc_coefs <- as.data.frame(extracted)
ccdc_coefs

#Get the green surface reflectance time series (ts) for up to 4-segments at the point.
ts_green<-get_ccdc_ts(ccdc_img,x_coord,y_coord,epsg,'green',4)

#Get the near-infrared (nir) surface reflectance time series (ts) for up to 4-segments at the point.
ts_nir<-get_ccdc_ts(ccdc_img,x_coord,y_coord,epsg,'nir',4)

#Plot the time series 
plot(ts_green[,'jdoy_vec'],ts_green[,'ts'],type='l',xlab='Julian Date',ylab='Green Band Surface Reflectance')
plot(ts_nir[,'jdoy_vec'],ts_nir[,'ts'],type='l',xlab='Julian Date',ylab='NIR Band Surface Reflectance')

latest<-gen_latest_ccdc_rast(ccdc_img,4)
plot(latest)

````

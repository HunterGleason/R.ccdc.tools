# R.ccdc.tools
Simple R tools for working with Continious Change Detection and Classification (Zhu &amp; Woodcock 2014) images. 

# Installation
````
remotes::install_github('HunterGleason/R.ccdc.tools')
````

# Example
````
#Imports
library(R.ccdc.tools)
library(stars)

#Read in a CDCC image example data (see https://github.com/parevalo/gee-ccdc-tools) as a stars object
ccdc_img = exampleData


#Get the green surface reflectance time series (ts) for up to 4-segments at the point 1794708.42,611511.36 (EPSG:3005)
ts_green<-get_ccdc_ts(ccdc_img,1794708.42,611511.36,3005,'green',4)

#Get the near-infrared (nir) surface reflectance time series (ts) for up to 4-segments at the point 1794708.42,611511.36 (EPSG:3005)
ts_nir<-get_ccdc_ts(ccdc_img,1794708.42,611511.36,3005,'nir',4)

#Plot the time series 
plot(ts_green[,'jdoy_vec'],ts_green[,'ts'],type='l',xlab='Julian Date',ylab='Green Band Surface Reflectance')
plot(ts_nir[,'jdoy_vec'],ts_nir[,'ts'],type='l',xlab='Julian Date',ylab='NIR Band Surface Reflectance')
````

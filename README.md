# R.ccdc.tools
Simple R tools for working with Continious Change Detection and Classification (Zhu &amp; Woodcock 2014) images. 

# Installation
````
remotes::install_github('HunterGleason/R.ccdc.tools')
````

# Example
````
library(R.ccdc.tools)
library(stars)

#Read in a CDCC image (see https://github.com/parevalo/gee-ccdc-tools) as a stars object
setwd("~/CDCC")
ccdc_img = stars::read_stars("koot_ts_seg4_lasso_pnt01.tif",n_proxy=10^10)

#Get the green surface reflectance time series (ts) for up to 4-segments at the point 1788456.97,633915.52 (EPSG:3005)
ts<-get_ccdc_ts(ccdc_img,1788456.97,633915.52,3005,'green',4)

#Plot the time series 
plot(ts[,1],ts[,2],type='l',xlab='Julian Date',ylab='Green Band Surface Reflectance')
````

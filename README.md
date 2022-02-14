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
library(bcdata)

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
plot(jday_to_date(ts_green[,'jdoy_vec']),ts_green[,'ts'],type='l',xlab='Julian Date',ylab='Green Band Surface Reflectance')
plot(jday_to_date(ts_nir[,'jdoy_vec']),ts_nir[,'ts'],type='l',xlab='Julian Date',ylab='NIR Band Surface Reflectance')

#Get a CCDC image with coeffecents for segments occuring during July 15, 2021 (i.e., near present day landscape conditions)
latest<-gen_ccdc_at_jdoy(ccdc_img,as.Date('2021-07-15'))
plot(latest)

#Load a example catagorical layer (e.g., landcover polygons)
landcover <- landcover
landcover <- vect(landcover)

#Get freshwater wetlands within AOI, merge with landcover layer 
wetlands<-bcdc_query_geodata('93b413d8-1840-4770-9629-641d74bd1cc6') %>%
  filter(INTERSECTS(st_bbox(st_as_stars(latest)))) %>%
  collect()

wetlands<-wetlands[,'id']
wetlands$landcover<-'Wetland'


landcover <- rbind(landcover,vect(wetlands)) 

#Plot layers
plot(latest,maxnl=1)
plot(landcover,add=T,col=as.factor(landcover$landcover))

#Plot CCDC 60 sample time series by polygon type (i.e., landcover) for the near infarred band ('nir') over a 720 day period 
plot_ccdc_ts_bypoly(latest,landcover,'landcover',720,'nir',60)

#Same plot as above, only showing quartiles
plot_ccdc_ts_quartiles(latest,landcover,'landcover',720,'nir')

#Plot CCDC 60 sample time series by polygon type (i.e., landcover) for the shortwave infrared band 2 ('swir2') over a 720 day period 
plot_ccdc_ts_bypoly(latest,landcover,'landcover',720,'swir2',60)

#Plot CCDC 60 sample time series by polygon type (i.e., landcover) for the shortwave infrared band 2 ('swir2') over a 720 day period 
plot_ccdc_ts_quartiles(latest,landcover,'landcover',720,'swir2')

````


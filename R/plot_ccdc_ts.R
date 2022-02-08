
#' Plot CCDC  (Zhu & Woodcock 2014) time series sample by polygon.  
#'
#' Provided a CCDC image and a polygon layer with some category field, this function plots a sample
#' of the pixels with each polygon grouped by category, indicated by the 'catg_col' parameter. 
#'
#' @param ccdc_img  (SpatRaster,stars) The CCDC image for which to extract coefficients, for one segment only, 
#' such as the output from 'gen_latest_ccdc_rast'.
#' @param zone_poly (SpatVector) Polygons for which to plot CCDC time series sample, by category. 
#' @param catg_col (Character) The name of the column that defines the category of each polygon. 
#' @param days The number of days for which to plot the CCDC time series, defaults to 730 (2 years)
#' @param band band (character) The name of the band for which to extract CCDC coefficients. One of 'blue', 'green', 'red'
#' 'nir', 'swir1' or 'swir2'.
#' @param N The number of pixels to sample (and plot) for each category. 
#' @return (void) Plots the CCDC time series for a sample of pixels stratified by polygon category. 
#' @export
plot_ccdc_ts_bypoly<-function(ccdc_img,zone_poly,catg_col,days=730,band,N=60)
{
  
  if(class(ccdc_img)=='stars')
  {
    ccdc_img<-terra::rast(ccdc_img)
  }
  
  zonal_vals <- terra::extract(ccdc_img,zone_poly)
  zonal_vals$type <- as.factor(as.data.frame(zone_poly)[,catg_col][zonal_vals$ID])
  zonal_vals$type_int<-as.numeric(zonal_vals$type)
  
  
  zonal_vals <- zonal_vals %>%
    dplyr::group_by(type) %>% 
    dplyr::slice_sample(n=N)
  
  zonal_vals<-as.data.frame(zonal_vals)
  zonal_vals<-zonal_vals[complete.cases(zonal_vals),]
  
  i<-1
  
  intp<-paste0(band,"_INTP")
  slp<-paste0(band,"_SLP")
  cos<-paste0(band,"_COS")
  sin<-paste0(band,"_SIN")
  cos2<-paste0(band,"_COS2")
  sin2<-paste0(band,"_SIN2")
  cos3<-paste0(band,"_COS3")
  sin3<-paste0(band,"_SIN3")
  
  plot(ccdc_func(c(zonal_vals$tStart[i]:(zonal_vals$tStart[i]+days)),
                 zonal_vals[i,intp],
                 zonal_vals[i,slp],
                 zonal_vals[i,cos],
                 zonal_vals[i,sin],
                 zonal_vals[i,cos2],
                 zonal_vals[i,sin2],
                 zonal_vals[i,cos3],
                 zonal_vals[i,sin3]),
       xlim=c(1,days),
       ylim=c(0,1.0),
       type='l',
       col=zonal_vals$type_int[i],
       xlab="Julian Day",
       ylab=paste0(band," Surface Refletance"))
  
  for(i in c(2:nrow(zonal_vals)))
  {
    lines(ccdc_func(c(zonal_vals$tStart[i]:(zonal_vals$tStart[i]+days)),
                    zonal_vals[i,intp],
                    zonal_vals[i,slp],
                    zonal_vals[i,cos],
                    zonal_vals[i,sin],
                    zonal_vals[i,cos2],
                    zonal_vals[i,sin2],
                    zonal_vals[i,cos3],
                    zonal_vals[i,sin3]),
          col=zonal_vals$type_int[i])
    
    
  }
  
  
  legend(x='topleft',legend=levels(zonal_vals$type),fill=unique(zonal_vals$type_int))
}


#' Plot CCDC  (Zhu & Woodcock 2014) time series quartiles by polygon.  
#'
#' Provided a CCDC image and a polygon layer with some category field, this function plots a sample
#' of the pixels with each polygon grouped by category, indicated by the 'catg_col' parameter. 
#'
#' @param ccdc_img  (SpatRaster,stars) The CCDC image for which to extract coefficients, for one segment only, 
#' such as the output from 'gen_latest_ccdc_rast'.
#' @param zone_poly (SpatVector) Polygons for which to plot CCDC time series sample, by category. 
#' @param catg_col (Character) The name of the column that defines the category of each polygon. 
#' @param days The number of days for which to plot the CCDC time series, defaults to 730 (2 years)
#' @param band band (character) The name of the band for which to extract CCDC coefficients. One of 'blue', 'green', 'red'
#' 'nir', 'swir1' or 'swir2'.
#' @param N The number of pixels to sample (and plot) for each category. 
#' @return (void) Plots the CCDC time series quartiles (i.e., 25, 50 & 75th percentiles) stratified by polygon category. 
#' @export
plot_ccdc_ts_quartiles<-function (ccdc_img, zone_poly, catg_col, days = 730, band) 
{
  if (class(ccdc_img) == "stars") {
    ccdc_img <- terra::rast(ccdc_img)
  }
  
  zonal_vals <- terra::extract(ccdc_img, zone_poly)
  zonal_vals$type <- as.factor(as.data.frame(zone_poly)[, catg_col][zonal_vals$ID])
  zonal_vals <- zonal_vals[complete.cases(zonal_vals),]
  
  i <- 1
  intp <- paste0(band, "_INTP")
  slp <- paste0(band, "_SLP")
  cos <- paste0(band, "_COS")
  sin <- paste0(band, "_SIN")
  cos2 <- paste0(band, "_COS2")
  sin2 <- paste0(band, "_SIN2")
  cos3 <- paste0(band, "_COS3")
  sin3 <- paste0(band, "_SIN3")
  
  tStartMin<-summary(zonal_vals$tStart)[1]
  
  ts_x<-c(tStartMin:(tStartMin + days))
  
  ts<-c(ccdc_func(ts_x,
                  zonal_vals[i, intp],
                  zonal_vals[i, slp],
                  zonal_vals[i, cos],
                  zonal_vals[i, sin],
                  zonal_vals[i, cos2],
                  zonal_vals[i, sin2],
                  zonal_vals[i, cos3],
                  zonal_vals[i, sin3]),zonal_vals$type[i])
  
  for (i in c(2:nrow(zonal_vals))) {
    ts<-rbind(ts,c(ccdc_func(ts_x,
                             zonal_vals[i, intp],
                             zonal_vals[i, slp],
                             zonal_vals[i, cos],
                             zonal_vals[i, sin],
                             zonal_vals[i, cos2],
                             zonal_vals[i, sin2],
                             zonal_vals[i, cos3],
                             zonal_vals[i, sin3]),zonal_vals$type[i]))
  }
  
  ts<-as.data.frame(ts)
  
  p25<-aggregate(ts,by=list(ts[,ncol(ts)]),quantile,probs=c(.25))
  med<-aggregate(ts,by=list(ts[,ncol(ts)]),quantile,probs=c(.5))
  p75<-aggregate(ts,by=list(ts[,ncol(ts)]),quantile,probs=c(.75))
  
  firstplot<-T
  ts_x<-c(1:(days+1))
  
  for(j in unique(ts[,ncol(ts)]))
  {
    if(firstplot==T)
    {
      plot(ts_x,med[med[,1]==j,c(3:ncol(med)-1)],type='l',col=j,ylim=c(0,1),xlab="Julian DOY",ylab=paste0(band," Surface Reflectance"))
      firstplot=F
    }else{
      lines(ts_x,med[med[,1]==j,c(3:ncol(med)-1)],type='l',col=j)
    }
    lines(ts_x,p25[med[,1]==j,c(3:ncol(p25)-1)],type='l',col=j)
    lines(ts_x,p75[med[,1]==j,c(3:ncol(p75)-1)],type='l',col=j)
  }
  legend(x = "topleft", legend = levels(zonal_vals$type), fill = sort(unique(ts[,ncol(ts)])))
}



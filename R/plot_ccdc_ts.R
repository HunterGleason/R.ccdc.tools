
#' Plot CCDC  (Zhu & Woodcock 2014) time series sample by polygon.  
#'
#' Provided a CCDC image and a polygon layer with some category field, this function plots a sample
#' of the pixels with each polygon grouped by category, indicated by the 'catg_col' parameter. 
#'
#' @param ccdc_img  (SpatRaster,stars) The CCDC image for which to extract coefficients, for one segment only, 
#' such as the output from 'gen_latest_ccdc_rast'.
#' @param ccdc_img_date (character) A representation of the CCDC image date that can be interpreted by as.Date()
#' @param zone_poly (SpatVector,sf) Polygons for which to plot CCDC time series sample, by category. 
#' @param catg_col (Character) The name of the column that defines the category of each polygon. 
#' @param days The number of days for which to plot the CCDC time series, defaults to 730 (2 years)
#' @param band band (character) The name of the band for which to extract CCDC coefficients. One of 'blue', 'green', 'red'
#' 'nir', 'swir1','swir2' or 'therm'.
#' @param N The number of pixels to sample (and plot) for each category. 
#' @return (void) Plots the CCDC time series for a sample of pixels stratified by polygon category. 
#' @export
plot_ccdc_ts_bypoly<-function(ccdc_img,ccdc_img_date,zone_poly,catg_col,days=730,band,N=60)
{
  
  if(class(ccdc_img)=='stars')
  {
    ccdc_img<-terra::rast(ccdc_img)
  }
  if(any(class(zone_poly)=='sf'))
  {
    zone_poly<-terra::vect(zone_poly)
  }
  
  zonal_vals <- terra::extract(ccdc_img,zone_poly)
  zonal_vals$type <- as.factor(as.data.frame(zone_poly)[,catg_col][zonal_vals$ID])
  zonal_vals$type_int<-as.numeric(zonal_vals$type)
  
  
  zonal_vals <- zonal_vals %>%
    dplyr::group_by(type) %>% 
    dplyr::slice_sample(n=N,replace = T)
  
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
  
  ccdc_img_date<-date_to_jday(ccdc_img_date)
  ts_x<-c(ccdc_img_date:(ccdc_img_date + days))
  
  plot(jday_to_date(ts_x),ccdc_func(c(zonal_vals$tStart[i]:(zonal_vals$tStart[i]+days)),
                 zonal_vals[i,intp],
                 zonal_vals[i,slp],
                 zonal_vals[i,cos],
                 zonal_vals[i,sin],
                 zonal_vals[i,cos2],
                 zonal_vals[i,sin2],
                 zonal_vals[i,cos3],
                 zonal_vals[i,sin3]),
       ylim=c(0,1.0),
       type='l',
       col=zonal_vals$type_int[i],
       xlab="Date",
       ylab=paste0(band," Surface Reflectance"))
  
  for(i in c(2:nrow(zonal_vals)))
  {
    lines(jday_to_date(ts_x),ccdc_func(c(zonal_vals$tStart[i]:(zonal_vals$tStart[i]+days)),
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
#' @param ccdc_img_date (character) A representation of the CCDC image date that can be interpreted by as.Date()
#' @param zone_poly (SpatVector) Polygons for which to plot CCDC time series sample, by category. 
#' @param catg_col (Character) The name of the column that defines the category of each polygon. 
#' @param days The number of days for which to plot the CCDC time series, defaults to 730 (2 years)
#' @param band band (character) The name of the band for which to extract CCDC coefficients. One of 'blue', 'green', 'red'
#' 'nir', 'swir1','swir2' or 'therm'.
#' @param N The number of pixels to sample (and plot) for each category. 
#' @return (void) Plots the CCDC time series quartiles (i.e., 25, 50 & 75th percentiles) stratified by polygon category. 
#' @export
plot_ccdc_ts_quartiles<-function (ccdc_img,ccdc_img_date,zone_poly, catg_col, days = 730, band) 
{
  if (class(ccdc_img) == "stars") {
    ccdc_img <- terra::rast(ccdc_img)
  }
  
  ccdc_img_date<-date_to_jday(ccdc_img_date)
  
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
  
  ts_x<-c(ccdc_img_date:(ccdc_img_date + days))
  
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
    ts<-rbind(ts,c(ccdc_func(jday_to_date(ts_x),
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
      plot(ts_x,med[med[,1]==j,c(3:ncol(med)-1)],type='l',col=j,ylim=c(0,1),xlab="Date",ylab=paste0(band," Surface Reflectance"))
      firstplot=F
    }else{
      lines(ts_x,med[med[,1]==j,c(3:ncol(med)-1)],type='l',col=j)
    }
    lines(ts_x,p25[med[,1]==j,c(3:ncol(p25)-1)],type='l',col=j)
    lines(ts_x,p75[med[,1]==j,c(3:ncol(p75)-1)],type='l',col=j)
  }
  legend(x = "topleft", legend = levels(zonal_vals$type), fill = sort(unique(ts[,ncol(ts)])))
}




#' Uses SAGA-GIS and RSAGA to create N number of random samples with provided polygons, inherits polygon attributes. Points also get attributed
#' with the coefficient from a CCDC image for a given date. Points are returned as a data.frame. Useful function for
#' a large area of interest, i.e., as a alternative to 'plot_ccdc_ts_quartiles'.   
#'
#'
#' @param polygons_pth  (character) File path to polygon vector file with categories of interest.   
#' @param ccdc_img_pth (character) File path to raster file (e.g. GeoTiff) CCDC image for given date (see 'gen_ccdc_at_jdoy')
#' @param pnts_count (integer) Total number of X-Y locations to sample throughout ALL polygons. 
#' @param sep_dist (integer) Minimum distance in map units that two sample locations can be. 
#' @param env (RSAGA environment) A RSAGA environment to use. 
#' @return (data.frame) A data.frame with the sample locations, attributed with the polygon data and CCDC grid coefficient values.  
#' @export
sample_ccdc_by_catg<-function(polygons_pth,ccdc_img_pth,pnts_count,sep_dist,env)
{
  
  cat("Loading Polygons ...","\n")
  RSAGA::rsaga.geoprocessor(lib="io_gdal",
                            module = 3, 
                            param = list(FILES=polygons_pth,
                                         SHAPES=file.path(tempdir(),'shapes')),
                            env=env)
  
  cat("Generating Random Points ...","\n")
  RSAGA::rsaga.geoprocessor(lib='shapes_points',module=21,param = list(POINTS=file.path(tempdir(),"random_pnts"),
                                                                       EXTENT=3,
                                                                       POLYGONS=file.path(tempdir(),'shapes.shp'),
                                                                       COUNT=pnts_count,
                                                                       DISTANCE=sep_dist))
  cat("Atributing Polygons to Points ...","\n")
  RSAGA::rsaga.geoprocessor(lib='shapes_points',module=10,param = list(INPUT=file.path(tempdir(),"random_pnts.shp"),
                                                                       POLYGONS=file.path(tempdir(),'shapes.shp')))
  
  offset<-ncol(sf::read_sf(file.path(tempdir(),"random_pnts.shp")))
  
  cat("Atributing Grids to Points, this may take some time ...","\n")
  RSAGA::rsaga.geoprocessor(lib='shapes_grid',module=0,param = list(SHAPES=file.path(tempdir(),"random_pnts.shp"),
                                                                    GRIDS=ccdc_img_pth,
                                                                    RESULT=file.path(tempdir(),"random_pnts.shp"),
                                                                    RESAMPLING=0))
  
  ccdc_img_at_pnts<-sf::read_sf(file.path(tempdir(),"random_pnts.shp"))
  
  
  band_names<-c("blue_INTP",
                "blue_SLP",
                "blue_COS",
                "blue_SIN",
                "blue_COS2",
                "blue_SIN2",
                "blue_COS3",
                "blue_SIN3",
                "green_INTP",
                "green_SLP",
                "green_COS",
                "green_SIN",
                "green_COS2",
                "green_SIN2",
                "green_COS3",
                "green_SIN3",
                "red_INTP",
                "red_SLP",
                "red_COS",
                "red_SIN",
                "red_COS2",
                "red_SIN2",
                "red_COS3",
                "red_SIN3",
                "nir_INTP",
                "nir_SLP",
                "nir_COS",
                "nir_SIN",
                "nir_COS2",
                "nir_SIN2",
                "nir_COS3",
                "nir_SIN3",
                "swir1_INTP",
                "swir1_SLP",
                "swir1_COS",
                "swir1_SIN",
                "swir1_COS2",
                "swir1_SIN2",
                "swir1_COS3",
                "swir1_SIN3",
                "swir2_INTP",
                "swir2_SLP",
                "swir2_COS",
                "swir2_SIN",
                "swir2_COS2",
                "swir2_SIN2",
                "swir2_COS3",
                "swir2_SIN3",
                "therm_INTP",
                "therm_SLP",
                "therm_COS",
                "therm_SIN",
                "therm_COS2",
                "therm_SIN2",
                "therm_COS3",
                "therm_SIN3",
                "tStart",
                "tEnd") 
  
  colnames(ccdc_img_at_pnts)[c(offset:61)]<-band_names
  
  
  ccdc_img_at_pnts<-as.data.frame(ccdc_img_at_pnts)
  
  cat("Done ...","\n")
  
  return(ccdc_img_at_pnts)
}




#' Function for plotting surface reflectance curves by category (i.e., output of 'sample_ccdc_by_catg'). 
#'
#'
#' @param ccdc_img_at_pnts  (data.frame) A data.frame containing the categories of interest, and associated CCDC coefficients for a given date (i.e., output of 'sample_ccdc_by_catg') 
#' @param ccdc_img_date (character) A representation of the CCDC image date that can be interpreted by as.Date()
#' @param catg_field (character) Name of the column indicating the categories of interest. 
#' @param band (character) The name of the band for which to forecast CCDC surface reflectance time series. One of 'blue', 'green', 'red'
#' 'nir', 'swir1','swir2' or 'therm'.
#' @param days (integer) Number of days to predict surface reflectance from the CCDC image data ('ccdc_img_date'). 
#' @return (List) A data.frame with all data used for plot generation ('srdata'), a 'ggplot' object ('srplot'), and a data.frame with the legend codes cross-walk ('crosswalk'). 
#' @export
plot_ccdc_by_catg<-function(ccdc_img_at_pnts,ccdc_img_date,catg_field,band,days=730)
{
  
  Rcpp::cppFunction('NumericMatrix predict_sr(int img_date,int days,NumericVector catg, NumericVector coef_intp,NumericVector coef_slp,NumericVector coef_cos,NumericVector coef_sin,NumericVector coef_cos2,NumericVector coef_sin2,NumericVector coef_cos3,NumericVector coef_sin3){

  int n = catg.size();
  NumericMatrix out(n*days,3);
  double pi = 3.1415926535;
  int row_idx=0;
  int end_date=img_date+days;
  
  for(int i = 0; i < n; i++)
  {
    double day = img_date;
    int current_catg=catg[i];
    
    while(day<(double)end_date)
    {
      double yhat=coef_intp[i]+coef_slp[i]*day+coef_cos[i]*cos((2.0*pi*day)/365.0)+coef_sin[i]*sin((2.0*pi*day)/365.0)+coef_cos2[i]*cos((4.0*pi*day)/365.0)+coef_sin2[i]*sin((4.0*pi*day)/365.0)+coef_cos3[i]*cos((6.0*pi*day)/365.0)+coef_sin3[i]*sin((6.0*pi*day)/365.0);
      out(row_idx,2)=yhat;
      out(row_idx,0)=current_catg;
      out(row_idx,1)=day;
      row_idx++;
      day=day+1.0;
    }
  }
  
  return out;
}')
  
  
  
  factor_vector <- as.numeric(as.factor(ccdc_img_at_pnts[,catg_field]))
  intp <- ccdc_img_at_pnts[,paste0(band,"_INTP")]
  slp <- ccdc_img_at_pnts[,paste0(band,"_SLP")]
  cos <- ccdc_img_at_pnts[,paste0(band,"_COS")]
  sin <- ccdc_img_at_pnts[,paste0(band,"_SIN")]
  cos2 <- ccdc_img_at_pnts[,paste0(band,"_COS2")]
  sin2 <- ccdc_img_at_pnts[,paste0(band,"_SIN2")]
  cos3 <- ccdc_img_at_pnts[,paste0(band,"_COS3")]
  sin3 <- ccdc_img_at_pnts[,paste0(band,"_SIN3")]
  
  ccdc_img_date<-date_to_jday(ccdc_img_date)
  
  sr_mat<-predict_sr(ccdc_img_date,days,factor_vector,intp,slp,cos,sin,cos2,sin2,cos3,sin3)
  sr_mat<-as.data.frame(sr_mat)
  
  str_fact<-unique(ccdc_img_at_pnts[,catg_field])
  num_fact<-unique(as.numeric(as.factor(ccdc_img_at_pnts[,catg_field])))
  
  lab_tabl<-as.data.frame(cbind(num_fact,str_fact))
  colnames(lab_tabl)<-c("Category","Code")
  
  colnames(sr_mat)<-c('catg','jdoy','SR')
  
  
  sr_mat$catg=as.factor(sr_mat$catg)
  sr_mat$jdoy<-jday_to_date(sr_mat$jdoy)
  
  median_sr<-sr_mat %>% 
    dplyr::group_by(jdoy,catg) %>%
    dplyr::summarise(sr=median(SR,na.rm=T))
  
  p25_sr<-sr_mat %>% 
    dplyr::group_by(jdoy,catg) %>%
    dplyr::summarise(sr=quantile(SR,probs=0.25,na.rm=T))
  
  p75_sr<-sr_mat %>% 
    dplyr::group_by(jdoy,catg) %>%
    dplyr::summarise(sr=quantile(SR,probs=0.75,na.rm=T))
  
  
  gg<-ggplot2::ggplot()+
    ggplot2::geom_line(data=p25_sr,ggplot2::aes(x=jdoy,y=sr,color=catg),linetype=2)+
    ggplot2::geom_line(data=median_sr,ggplot2::aes(x=jdoy,y=sr,color=catg),linetype=1)+
    ggplot2::geom_line(data=p75_sr,ggplot2::aes(x=jdoy,y=sr,color=catg),linetype=2)+
    ggplot2::xlab("Date")+
    ggplot2::ylab(paste0(band," Surface Reflectance"))+
    ggplot2::labs(color = "Category")+
    ggplot2::theme_classic()
  
  
  
  return(list(srdata = sr_mat,srplot=gg,crosswalk=lab_tabl))
}


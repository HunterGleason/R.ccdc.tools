


#' Predict surface reflectance using CCDC algorithm (Zhu & Woodcock 2014) 
#'
#' For a given Julian date, and set of coefficients, this function returns
#' the calculated surface reflectance value using the full model described in 
#' Zhu et al 2015. 
#'
#' @param julian_date Julian date for which to predict surface reflectance
#' @param coef_intp Coefficient for overall values for the ith Landsat Band
#' @param coef_slp Coefficient for inter-annual change for the ith Landsat Band
#' @param coef_cos Coefficient for intra-annual change for the ith Landsat Band (1st harmonic)
#' @param coef_sin Coefficient for intra-annual change for the ith Landsat Band (1st harmonic)
#' @param coef_cos2 Coefficient for intra-annual change for the ith Landsat Band (2nd harmonic)
#' @param coef_sin2 Coefficient for intra-annual change for the ith Landsat Band (2nd harmonic)
#' @param coef_cos3 Coefficient for intra-annual change for the ith Landsat Band (3rd harmonic)
#' @param coef_sin3 Coefficient for intra-annual change for the ith Landsat Band (3rd harmonic)  
#' @return A floating point value for the calculated surface reflectance 
#' @export
ccdc_func<-function(julian_date,coef_intp,coef_slp,coef_cos,coef_sin,coef_cos2,coef_sin2,coef_cos3,coef_sin3)
{
  val<-coef_intp+
    coef_slp*julian_date+
    coef_cos*cos((2*pi*julian_date)/365)+
    coef_sin*sin((2*pi*julian_date)/365)+
    coef_cos2*cos(((4*pi*julian_date)/365))+
    coef_sin2*sin(((4*pi*julian_date)/365))+
    coef_cos3*cos(((6*pi*julian_date)/365))+
    coef_sin3*sin(((6*pi*julian_date)/365))
  
  return(as.numeric(val))
}

#' Predict surface reflectance timeseries at a point (X,Y) using CCDC algorithm (Zhu & Woodcock 2014) 
#'
#'Provided a CCDC image output from the Google Earth Engine CCDC algorithm and 
#'a X-Y coordinate this function returns the surface reflectance time series for
#'the intersected Landsat pixel. 
#'
#' @param ccdc_img A multiband STARS raster of the CCDC output from Google Earth Engine
#' with all coefficients for each segment present (i.e., see EE script)
#' @param x_coord (float) Value of the X coordinate of interest, must intersect the CCDC image 
#' @param y_coord (float) Value of the Y coordinate of interest, must intersect the CCDC image 
#' @param epsg (integer) EPSG code for CRS of the CCDC image 
#' @param band (character) Band of interest, i.e., 'blue','green','red','nir','swir1','swir2' or 'therm'
#' @param n_seg (integer) Default 4. Number of segments to compute time series for, must be 
#' less then or equal to maximum number of exported segments (i.e., see EE script)
#' @return A 2-col matrix with the Julian date time series in the first and the
#' predicted CCDC time series in the second. 
#' @export
get_ccdc_ts <- function(ccdc_img,x_coord,y_coord,epsg,band,n_seg=4)
{
  
  DF <- data.frame(
    x=c(x_coord),
    y=c(y_coord))
  
  
  DF_sf = sf::st_as_sf(DF, coords = c("x", "y"), 
                   crs = sf::st_crs(epsg), agr = "constant")
  
  
  extracted<-sf::st_as_sf(stars::st_extract(ccdc_img,DF_sf))
  
  
  ccdc_coefs<-as.data.frame(extracted)
  
  ts<-c()
  jdoy_vec<-c()

  
  for(seg in c(1:n_seg))
  {
    coef_intp = paste0("S",seg,"_",band,"_coef_INTP")
    coef_slp = paste0("S",seg,"_",band,"_coef_SLP")
    coef_cos = paste0("S",seg,"_",band,"_coef_COS")
    coef_sin = paste0("S",seg,"_",band,"_coef_SIN")
    coef_cos2 = paste0("S",seg,"_",band,"_coef_COS2")
    coef_sin2 = paste0("S",seg,"_",band,"_coef_SIN2")
    coef_cos3 = paste0("S",seg,"_",band,"_coef_COS3")
    coef_sin3 = paste0("S",seg,"_",band,"_coef_SIN3")
    
    coef_intp = ccdc_coefs[1,coef_intp]
    coef_slp = ccdc_coefs[1,coef_slp]
    coef_cos = ccdc_coefs[1,coef_cos]
    coef_sin = ccdc_coefs[1,coef_sin]
    coef_cos2 = ccdc_coefs[1,coef_cos2]
    coef_sin2 = ccdc_coefs[1,coef_sin2]
    coef_cos3 = ccdc_coefs[1,coef_cos3]
    coef_sin3 = ccdc_coefs[1,coef_sin3]
    
    
    tStart = paste0("S",seg,"_tStart")
    tEnd = paste0("S",seg,"_tEnd")
    tStart = ccdc_coefs[1,tStart]
    tEnd = ccdc_coefs[1,tEnd]
    
    if(tStart!=0)
    {
      
      ts<-c(ts,ccdc_func(c(tStart:tEnd),coef_intp,coef_slp,coef_cos,coef_sin,coef_cos2,coef_sin2,coef_cos3,coef_sin3))
      jdoy_vec<-c(jdoy_vec,c(tStart:tEnd))
      
    }
  }
  
  return(cbind(jdoy_vec,ts))
  
}


# setwd("~/CDCC")
# ndmi_ts = stars::read_stars("koot_ts_seg4_lasso_pnt01.tif",n_proxy=10^10)
# ts<-plot_ts(ndmi_ts,1788456.97366881,633915.51542255,3005,'green',4)
# plot(ts[,1],ts[,2],type='l')

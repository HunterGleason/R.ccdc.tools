
#' Get the most recent segment coefficients from the CCDC image (see Zhu & Woodcock 2014) 
#'
#' For pixel locations where change has occurred (number of segments >1) this function returns 
#' the model coefficients associated with the most recent segment, as well as the start
#' date of the segment as a Julian date. If there is only one segment (no change), returns coefficients 
#' for the that segment. 
#'
#' @param ccdc_df (Data.frame) The CCDC image as a data frame.
#' @param band (character) The name of the band for which to extract CCDC coefficients. One of 'blue', 'green', 'red'
#' 'nir', 'swir1' or 'swir2'.
#' @param max_seg (integer) The maximum number of segments present in the CCDC image.
#' @param xy (boolean) True or False, return the CCDC coefficient data frame with xy coordinates, and 'tStart'  
#' @return (matrix) A matrix with the most current CCDC coefficients for the specified band at each pixel location, 
#' and possibly xy coordinates and 'tStart'.  
#' @export
get_latest_seg_coefs <- function(ccdc_df,band,max_seg,xy=F)
{
  
  #Variable for holding index of most current (latest) segment 
  latest_idx<-c()
  
  #Variable for holding holding coefficient matrix
  coefs<-c()
  
  #For each segment, starting with max_seg (the latest segment) down to the first
  for(seg in c(max_seg:1))
  {
    #Define the required column names for specified segment and band
    coef_intp = paste0("S",seg,"_",band,"_coef_INTP")
    coef_slp = paste0("S",seg,"_",band,"_coef_SLP")
    coef_cos = paste0("S",seg,"_",band,"_coef_COS")
    coef_sin = paste0("S",seg,"_",band,"_coef_SIN")
    coef_cos2 = paste0("S",seg,"_",band,"_coef_COS2")
    coef_sin2 = paste0("S",seg,"_",band,"_coef_SIN2")
    coef_cos3 = paste0("S",seg,"_",band,"_coef_COS3")
    coef_sin3 = paste0("S",seg,"_",band,"_coef_SIN3")
    tStart = paste0("S",seg,"_tStart")
    numObs = paste0("S",seg,"_numObs")
    
    #Check if xy is T
    if(xy==T)
    {
      #Get coefficient for current segment if they exist (numObs>0)
      seg_coefs<-ccdc_df[which(ccdc_df[,numObs]>0),c("x","y",coef_intp,coef_slp,coef_cos,coef_sin,coef_cos2,coef_sin2,coef_cos3,coef_sin3,tStart)]
      #Check is seg == max_seg
      if(seg!=max_seg)
      {
        #Remove observations with segment #'s > than current seg from coefficient matrix 
        seg_coefs<-seg_coefs[-latest_idx,]
      }
      #Rename cols
      colnames(seg_coefs)<-c('x','y',paste(band,c('INTP','SLP','COS','SIN','COS2','SIN2','COS3','SIN3'),sep="_"),'tStart')
    }else{
      
      seg_coefs<-ccdc_df[which(ccdc_df[,numObs]>0),c(coef_intp,coef_slp,coef_cos,coef_sin,coef_cos2,coef_sin2,coef_cos3,coef_sin3)]
      if(seg!=max_seg)
      {
        seg_coefs<-seg_coefs[-latest_idx,]
      }
      
      colnames(seg_coefs)<-paste(band,c('INTP','SLP','COS','SIN','COS2','SIN2','COS3','SIN3'),sep="_")
    }
    
    #Update the coefficient matrix
    coefs<-rbind(coefs,seg_coefs)
    
    #Update the latest index 
    latest_idx<-c(latest_idx,which(ccdc_df[,numObs]>0))
    latest_idx<-unique(latest_idx)
    
  }
  
  return(coefs)
}


#' Get the most recent segment coefficients from the CCDC image (see Zhu & Woodcock 2014) 
#'
#' For pixel locations where change has occurred (number of segments >1) this function returns 
#' the model coefficients associated with the most recent segment, as well as the start
#' date of the segment as a Julian date. If there is only one segment (no change), returns coefficients 
#' for the that segment. 
#'
#' @param ccdc_img (SpatRaster, stars) The CCDC image for which to extract coefficients.
#' @param max_seg (integer) The maximum number of segments present in the CCDC image.
#' @param as_SpatRaster (boolean) True or False, return the CCDC coefficient (and 'tStart') 
#' as a SpatRaster image, otherwise a data frame. default TRUE.
#' @return (SpatRaster) A SpatRaster with all CCDC coefficients (and tStart) associated with the most current segment.
#' If as_Spatrast is false, returns a data.frame.   
#' @export
gen_latest_ccdc_rast<-function(ccdc_img,max_seg,as_SpatRaster=T)
{
  if(class(ccdc_img)=="stars")
  {
    ccdc_img<-terra::rast(ccdc_img)
  }
  
  #Get original CRS
  orig_crs<-terra::crs(ccdc_img)
  
  #Convert the CCDC raster to a data frame 
  ccdc_df<-terra::as.data.frame(ccdc_img,xy=T)
  
  #Get the coefficients associated with the most current segment for each band 
  blue<-get_latest_seg_coefs(ccdc_df,'blue',max_seg,xy=T)
  green<-get_latest_seg_coefs(ccdc_df,'green',max_seg)
  red<-get_latest_seg_coefs(ccdc_df,'red',max_seg)
  nir<-get_latest_seg_coefs(ccdc_df,'nir',max_seg)
  swir1<-get_latest_seg_coefs(ccdc_df,'swir1',max_seg)
  swir2<-get_latest_seg_coefs(ccdc_df,'swir2',max_seg)
  
  #Combine the coefficients matrices from each band into one data frame 
  ccdc_latest<- as.data.frame(cbind(blue,green,red,nir,swir1,swir2))
  
  #If raster output is desired
  if(as_SpatRaster==T)
  {
    #Convert data.frame to SpatRast and return 
    ccdc_latest_rast<-terra::rast(ccdc_latest,type='xyz')
    terra::crs(ccdc_latest_rast)<-orig_crs
    
    return(ccdc_latest_rast)
    
  }else{
    
    return(ccdc_latest)
    
  }
  
}


#' Get the segment coefficients from the CCDC image at a specific Julian date (see Zhu & Woodcock 2014) 
#'
#' For pixel locations where change has occurred (number of segments >1) this function returns 
#' the CCDC model coefficients associated with the segment that corresponds in time with the specified Julian date,
#' as well as the start date of the segment as a Julian date. If there is only one segment (no change), returns coefficients 
#' for the that segment. !!Assumes that up to four segments may exist (see Earth Engine script)!!
#'
#' @param ccdc_img (SpatRaster, stars) The CCDC image for which to extract coefficients.
#' @param jdoy (integer) Julian date for which to extract CCDC coefficients. 
#' @return (SpatRaster) A SpatRaster with all CCDC coefficients (and tStart,tEnd) associated with the segment corresponding with the
#' specified Julian date. 
#' @export
gen_ccdc_at_jdoy<-function(ccdc_img,jdoy)
{
  ccdc_img<-terra::as.data.frame(ccdc_img,xy=T)
  
  Rcpp::cppFunction('NumericVector get_seg_at_jdoy(int jdoy,NumericVector s1tStart,NumericVector s1tEnd,NumericVector s2tStart,NumericVector s2tEnd,NumericVector s3tStart,NumericVector s3tEnd,NumericVector s4tStart,NumericVector s4tEnd) {
  
  int n = s1tStart.size();
  NumericVector out(n);
  
  for(int i = 0; i < n; ++i){
    if(jdoy >= s1tStart[i] && jdoy <= s1tEnd[i])
            {
            out[i]=1;
            }else if (jdoy >= s2tStart[i] && jdoy <= s2tEnd[i])
            {
            out[i]=2;
            }else if (jdoy >= s3tStart[i] && jdoy <= s3tEnd[i])
            {
            out[i]=3;
            }else if (jdoy >= s4tStart[i] && jdoy <= s4tEnd[i])
            {
            out[i]=4;
            }
  }
  
  return out;
}')
  
  seg_idx<-get_seg_at_jdoy(jdoy,
                           ccdc_img$S1_tStart,
                           ccdc_img$S1_tEnd,
                           ccdc_img$S2_tStart,
                           ccdc_img$S2_tEnd,
                           ccdc_img$S3_tStart,
                           ccdc_img$S3_tEnd,
                           ccdc_img$S4_tStart,
                           ccdc_img$S4_tEnd)
  
  
  s1_names_select<-c("x","y")
  s2_names_select<-c("x","y")
  s3_names_select<-c("x","y")
  s4_names_select<-c("x","y")
  names_final<-c("x","y")
  
  for(b in c("blue","green","red","nir","swir1","swir2"))
  {
    for(coef in c("INTP","SLP","COS","SIN","COS2","SIN2","COS3","SIN3"))
    {
      s1_names_select<-c(s1_names_select,paste0("S1_",b,"_coef_",coef))
      s2_names_select<-c(s2_names_select,paste0("S2_",b,"_coef_",coef))
      s3_names_select<-c(s3_names_select,paste0("S3_",b,"_coef_",coef))
      s4_names_select<-c(s4_names_select,paste0("S4_",b,"_coef_",coef))
      names_final<-c(names_final,paste0(b,"_",coef))
    }
  }
  
  
  ccdc_img<-rbind(as.matrix(ccdc_img[seg_idx==1,c(s1_names_select,"S1_tStart","S1_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==2,c(s2_names_select,"S2_tStart","S2_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==3,c(s3_names_select,"S3_tStart","S3_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==4,c(s4_names_select,"S3_tStart","S3_tEnd")]))
  
  ccdc_img<-as.data.frame(ccdc_img)
  
  colnames(ccdc_img)<-c(names_final,"tStart","tEnd")
  
  ccdc_img<-terra::rast(ccdc_img,type="xyz")
  
  return(ccdc_img)
}

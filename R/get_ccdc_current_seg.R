#' Get the segment coefficients from the CCDC image at a specific Julian date (see Zhu & Woodcock 2014) 
#'
#' For pixel locations where change has occurred (number of segments >1) this function returns 
#' the CCDC model coefficients associated with the segment that corresponds in time with the specified Julian date,
#' as well as the start date of the segment as a Julian date. If there is only one segment (no change), returns coefficients 
#' for the that segment. !!Assumes that up to four segments may exist (see Earth Engine script)!!
#'
#' @param ccdc_img (SpatRaster, stars) The CCDC image for which to extract coefficients.
#' @param date (Date) Date for which to extract CCDC coefficients (must be later then March 1 1984, and earlier than present date). 
#' @return (SpatRaster) A SpatRaster with all CCDC coefficients (and tStart,tEnd) associated with the segment corresponding with the
#' specified Julian date. If the date is very recent, some data may be missing.  
#' @export
gen_ccdc_at_jdoy<-function(ccdc_img,date)
{
  
  jdoy<-date_to_jday(date)
  
  if(class(ccdc_img)=="stars")
  {
    ccdc_img<-terra::rast(ccdc_img)
  }
  
  orig_crs<-crs(ccdc_img)
  
  ccdc_img<-terra::as.data.frame(ccdc_img,xy=T)
  
  Rcpp::cppFunction('NumericVector get_seg_at_jdoy(int jdoy,NumericVector s1tStart,NumericVector s1tEnd,NumericVector s2tStart,NumericVector s2tEnd,NumericVector s3tStart,NumericVector s3tEnd,NumericVector s4tStart,NumericVector s4tEnd,NumericVector s5tStart,NumericVector s5tEnd,NumericVector s6tStart,NumericVector s6tEnd,NumericVector s7tStart,NumericVector s7tEnd,NumericVector s8tStart,NumericVector s8tEnd) {
  
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
            }else if (jdoy >= s5tStart[i] && jdoy <= s5tEnd[i])
            {
            out[i]=5;
            }else if (jdoy >= s6tStart[i] && jdoy <= s6tEnd[i])
            {
            out[i]=6;
            }else if (jdoy >= s7tStart[i] && jdoy <= s7tEnd[i])
            {
            out[i]=7;
            }else if (jdoy >= s8tStart[i] && jdoy <= s8tEnd[i])
            {
            out[i]=8;
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
                           ccdc_img$S4_tEnd,
                           ccdc_img$S5_tStart,
                           ccdc_img$S5_tEnd,
                           ccdc_img$S6_tStart,
                           ccdc_img$S6_tEnd,
                           ccdc_img$S7_tStart,
                           ccdc_img$S7_tEnd,
                           ccdc_img$S8_tStart,
                           ccdc_img$S8_tEnd)
  
  
  s1_names_select<-c("x","y")
  s2_names_select<-c("x","y")
  s3_names_select<-c("x","y")
  s4_names_select<-c("x","y")
  s5_names_select<-c("x","y")
  s6_names_select<-c("x","y")
  s7_names_select<-c("x","y")
  s8_names_select<-c("x","y")
  names_final<-c("x","y")
  
  for(b in c("blue","green","red","nir","swir1","swir2","therm"))
  {
    for(coef in c("INTP","SLP","COS","SIN","COS2","SIN2","COS3","SIN3","RMSE"))
    {
      if(coef!="RMSE"){
        s1_names_select<-c(s1_names_select,paste0("S1_",b,"_coef_",coef))
        s2_names_select<-c(s2_names_select,paste0("S2_",b,"_coef_",coef))
        s3_names_select<-c(s3_names_select,paste0("S3_",b,"_coef_",coef))
        s4_names_select<-c(s4_names_select,paste0("S4_",b,"_coef_",coef))
        s5_names_select<-c(s5_names_select,paste0("S5_",b,"_coef_",coef))
        s6_names_select<-c(s6_names_select,paste0("S6_",b,"_coef_",coef))
        s7_names_select<-c(s7_names_select,paste0("S7_",b,"_coef_",coef))
        s8_names_select<-c(s8_names_select,paste0("S8_",b,"_coef_",coef))
        names_final<-c(names_final,paste0(b,"_",coef))
      }else{
        s1_names_select<-c(s1_names_select,paste0("S1_",b,"_RMSE"))
        s2_names_select<-c(s2_names_select,paste0("S2_",b,"_RMSE"))
        s3_names_select<-c(s3_names_select,paste0("S3_",b,"_RMSE"))
        s4_names_select<-c(s4_names_select,paste0("S4_",b,"_RMSE"))
        s5_names_select<-c(s5_names_select,paste0("S5_",b,"_RMSE"))
        s6_names_select<-c(s6_names_select,paste0("S6_",b,"_RMSE"))
        s7_names_select<-c(s7_names_select,paste0("S7_",b,"_RMSE"))
        s8_names_select<-c(s8_names_select,paste0("S8_",b,"_RMSE"))

        names_final<-c(names_final,paste0(b,"_RMSE"))
        
      }
    }
  }
  
  
  ccdc_img<-rbind(as.matrix(ccdc_img[seg_idx==1,c(s1_names_select,"S1_tStart","S1_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==2,c(s2_names_select,"S2_tStart","S2_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==3,c(s3_names_select,"S3_tStart","S3_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==4,c(s4_names_select,"S4_tStart","S4_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==5,c(s5_names_select,"S5_tStart","S5_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==6,c(s6_names_select,"S6_tStart","S6_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==7,c(s7_names_select,"S7_tStart","S7_tEnd")]),
                  as.matrix(ccdc_img[seg_idx==8,c(s8_names_select,"S8_tStart","S8_tEnd")]))
  
  ccdc_img<-as.data.frame(ccdc_img)
  
  colnames(ccdc_img)<-c(names_final,"tStart","tEnd")
  
  ccdc_img<-terra::rast(ccdc_img,type="xyz")
  
  crs(ccdc_img)<-orig_crs
  
  return(ccdc_img)
}


#' Simple function for converting a Date object to Julian date.
#'
#' @param date (Date) A R Date object to convert to Julian date.
#' @return (integer) A integer representing the Julian date (i.e., days since '0000-01-01')
#' @export
date_to_jday<-function(date) 
{
  as.numeric(difftime(as.Date(date),as.Date("0001-01-01 00:00:00"),tz='UTC'))
}

#' Simple function for converting a Julian date to Date.
#'
#' @param jday (integer) Julian date to convert to Date.
#' @return (Date) A Date corresponding to provided Julian date at UTC. 
#' @export
jday_to_date<-function(jday)
{
  as.Date(jday,origin = '0001-01-01 00:00:00 UTC')
}

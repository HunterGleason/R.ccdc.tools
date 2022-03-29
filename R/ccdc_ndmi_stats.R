#' Derive NDMI and NDWI summary statistics for a specified date range derived from a CCDC image (see Zhu & Woodcock 2014) 
#'
#'At each pixel in the CCDC image the NDMI and NDWI are calculated for the specified date range. From this time series the 
#'min, max, median as well as the number of days since the start date that mins and maxes occur are returned. 
#'!!Assumes that coefficients for up to 8 segments are present and properly named in CCDC image (see Earth Engine script)!!
#'
#' @param ccdc_img (SpatRaster, stars, XY data.frame (fastest)) The CCDC image for which to extract coefficients for computing NDMI and NDWI. 
#' @param start_date (Date,character) Start date for which to compute NDMI / NDWI (must be later then March 1 1984, and earlier than present date).
#' @param start_date (Date,character) End date for which to compute NDMI / NDWI (must be later then March 1 1984, and earlier than present date).
#' @param crs (terra::crs,optional) A CRS object interpreted by terra::crs(), only required if 'ccdc_img' is provided as a XY data.frame.  
#' @return (SpatRaster) A SpatRaster with the min, max, median NDMI and NDWI, as well as the number of days since the start days of the mins and maxes as bands. 
#' @export
ccdc_ndmi_stats<-function(ccdc_img,start_date,end_date,crs=NULL)
{
  
  
  sdt<-date_to_jdoy(start_date)
  edt<-date_to_jdoy(end_date)
  
  
  if(class(ccdc_img)=="stars")
  {
    ccdc_img<-terra::rast(ccdc_img)
  }
  
  if(is.null(crs))
  {
    crs<-terra::crs(ccdc_img)
  }
  
  
  if(class(ccdc_img)[1]=="SpatRaster")
  {
    ccdc_img<-terra::as.data.frame(ccdc_img,xy=T)
  }
  
  Rcpp::cppFunction('DataFrame ccdc_ndmi_summary ( DataFrame ccdc_df, int jdoy_start, int jdoy_end )
{
    int n = ccdc_df.nrows();

    double pi = 3.1415926535;

    NumericVector ndmi_min ( n );
    NumericVector ndmi_max ( n );
    NumericVector ndmi_med ( n );
    NumericVector ndwi_min ( n );
    NumericVector ndwi_max ( n );
    NumericVector ndwi_med ( n );
    IntegerVector ndmi_jdoy_max ( n );
    IntegerVector ndmi_jdoy_min ( n );
    IntegerVector ndwi_jdoy_max ( n );
    IntegerVector ndwi_jdoy_min ( n );



    NumericVector S1_green_INTP = ccdc_df["S1_green_coef_INTP"];
    NumericVector S1_green_SLP = ccdc_df["S1_green_coef_SLP"];
    NumericVector S1_green_COS = ccdc_df["S1_green_coef_COS"];
    NumericVector S1_green_SIN = ccdc_df["S1_green_coef_SIN"];
    NumericVector S1_green_COS2 = ccdc_df["S1_green_coef_COS2"];
    NumericVector S1_green_SIN2 = ccdc_df["S1_green_coef_SIN2"];
    NumericVector S1_green_COS3 = ccdc_df["S1_green_coef_COS3"];
    NumericVector S1_green_SIN3 = ccdc_df["S1_green_coef_SIN3"];
    NumericVector S2_green_INTP = ccdc_df["S2_green_coef_INTP"];
    NumericVector S2_green_SLP = ccdc_df["S2_green_coef_SLP"];
    NumericVector S2_green_COS = ccdc_df["S2_green_coef_COS"];
    NumericVector S2_green_SIN = ccdc_df["S2_green_coef_SIN"];
    NumericVector S2_green_COS2 = ccdc_df["S2_green_coef_COS2"];
    NumericVector S2_green_SIN2 = ccdc_df["S2_green_coef_SIN2"];
    NumericVector S2_green_COS3 = ccdc_df["S2_green_coef_COS3"];
    NumericVector S2_green_SIN3 = ccdc_df["S2_green_coef_SIN3"];
    NumericVector S3_green_INTP = ccdc_df["S3_green_coef_INTP"];
    NumericVector S3_green_SLP = ccdc_df["S3_green_coef_SLP"];
    NumericVector S3_green_COS = ccdc_df["S3_green_coef_COS"];
    NumericVector S3_green_SIN = ccdc_df["S3_green_coef_SIN"];
    NumericVector S3_green_COS2 = ccdc_df["S3_green_coef_COS2"];
    NumericVector S3_green_SIN2 = ccdc_df["S3_green_coef_SIN2"];
    NumericVector S3_green_COS3 = ccdc_df["S3_green_coef_COS3"];
    NumericVector S3_green_SIN3 = ccdc_df["S3_green_coef_SIN3"];
    NumericVector S4_green_INTP = ccdc_df["S4_green_coef_INTP"];
    NumericVector S4_green_SLP = ccdc_df["S4_green_coef_SLP"];
    NumericVector S4_green_COS = ccdc_df["S4_green_coef_COS"];
    NumericVector S4_green_SIN = ccdc_df["S4_green_coef_SIN"];
    NumericVector S4_green_COS2 = ccdc_df["S4_green_coef_COS2"];
    NumericVector S4_green_SIN2 = ccdc_df["S4_green_coef_SIN2"];
    NumericVector S4_green_COS3 = ccdc_df["S4_green_coef_COS3"];
    NumericVector S4_green_SIN3 = ccdc_df["S4_green_coef_SIN3"];
    NumericVector S5_green_INTP = ccdc_df["S5_green_coef_INTP"];
    NumericVector S5_green_SLP = ccdc_df["S5_green_coef_SLP"];
    NumericVector S5_green_COS = ccdc_df["S5_green_coef_COS"];
    NumericVector S5_green_SIN = ccdc_df["S5_green_coef_SIN"];
    NumericVector S5_green_COS2 = ccdc_df["S5_green_coef_COS2"];
    NumericVector S5_green_SIN2 = ccdc_df["S5_green_coef_SIN2"];
    NumericVector S5_green_COS3 = ccdc_df["S5_green_coef_COS3"];
    NumericVector S5_green_SIN3 = ccdc_df["S5_green_coef_SIN3"];
    NumericVector S6_green_INTP = ccdc_df["S6_green_coef_INTP"];
    NumericVector S6_green_SLP = ccdc_df["S6_green_coef_SLP"];
    NumericVector S6_green_COS = ccdc_df["S6_green_coef_COS"];
    NumericVector S6_green_SIN = ccdc_df["S6_green_coef_SIN"];
    NumericVector S6_green_COS2 = ccdc_df["S6_green_coef_COS2"];
    NumericVector S6_green_SIN2 = ccdc_df["S6_green_coef_SIN2"];
    NumericVector S6_green_COS3 = ccdc_df["S6_green_coef_COS3"];
    NumericVector S6_green_SIN3 = ccdc_df["S6_green_coef_SIN3"];
    NumericVector S7_green_INTP = ccdc_df["S7_green_coef_INTP"];
    NumericVector S7_green_SLP = ccdc_df["S7_green_coef_SLP"];
    NumericVector S7_green_COS = ccdc_df["S7_green_coef_COS"];
    NumericVector S7_green_SIN = ccdc_df["S7_green_coef_SIN"];
    NumericVector S7_green_COS2 = ccdc_df["S7_green_coef_COS2"];
    NumericVector S7_green_SIN2 = ccdc_df["S7_green_coef_SIN2"];
    NumericVector S7_green_COS3 = ccdc_df["S7_green_coef_COS3"];
    NumericVector S7_green_SIN3 = ccdc_df["S7_green_coef_SIN3"];
    NumericVector S8_green_INTP = ccdc_df["S8_green_coef_INTP"];
    NumericVector S8_green_SLP = ccdc_df["S8_green_coef_SLP"];
    NumericVector S8_green_COS = ccdc_df["S8_green_coef_COS"];
    NumericVector S8_green_SIN = ccdc_df["S8_green_coef_SIN"];
    NumericVector S8_green_COS2 = ccdc_df["S8_green_coef_COS2"];
    NumericVector S8_green_SIN2 = ccdc_df["S8_green_coef_SIN2"];
    NumericVector S8_green_COS3 = ccdc_df["S8_green_coef_COS3"];
    NumericVector S8_green_SIN3 = ccdc_df["S8_green_coef_SIN3"];
    NumericVector S1_nir_INTP = ccdc_df["S1_nir_coef_INTP"];
    NumericVector S1_nir_SLP = ccdc_df["S1_nir_coef_SLP"];
    NumericVector S1_nir_COS = ccdc_df["S1_nir_coef_COS"];
    NumericVector S1_nir_SIN = ccdc_df["S1_nir_coef_SIN"];
    NumericVector S1_nir_COS2 = ccdc_df["S1_nir_coef_COS2"];
    NumericVector S1_nir_SIN2 = ccdc_df["S1_nir_coef_SIN2"];
    NumericVector S1_nir_COS3 = ccdc_df["S1_nir_coef_COS3"];
    NumericVector S1_nir_SIN3 = ccdc_df["S1_nir_coef_SIN3"];
    NumericVector S2_nir_INTP = ccdc_df["S2_nir_coef_INTP"];
    NumericVector S2_nir_SLP = ccdc_df["S2_nir_coef_SLP"];
    NumericVector S2_nir_COS = ccdc_df["S2_nir_coef_COS"];
    NumericVector S2_nir_SIN = ccdc_df["S2_nir_coef_SIN"];
    NumericVector S2_nir_COS2 = ccdc_df["S2_nir_coef_COS2"];
    NumericVector S2_nir_SIN2 = ccdc_df["S2_nir_coef_SIN2"];
    NumericVector S2_nir_COS3 = ccdc_df["S2_nir_coef_COS3"];
    NumericVector S2_nir_SIN3 = ccdc_df["S2_nir_coef_SIN3"];
    NumericVector S3_nir_INTP = ccdc_df["S3_nir_coef_INTP"];
    NumericVector S3_nir_SLP = ccdc_df["S3_nir_coef_SLP"];
    NumericVector S3_nir_COS = ccdc_df["S3_nir_coef_COS"];
    NumericVector S3_nir_SIN = ccdc_df["S3_nir_coef_SIN"];
    NumericVector S3_nir_COS2 = ccdc_df["S3_nir_coef_COS2"];
    NumericVector S3_nir_SIN2 = ccdc_df["S3_nir_coef_SIN2"];
    NumericVector S3_nir_COS3 = ccdc_df["S3_nir_coef_COS3"];
    NumericVector S3_nir_SIN3 = ccdc_df["S3_nir_coef_SIN3"];
    NumericVector S4_nir_INTP = ccdc_df["S4_nir_coef_INTP"];
    NumericVector S4_nir_SLP = ccdc_df["S4_nir_coef_SLP"];
    NumericVector S4_nir_COS = ccdc_df["S4_nir_coef_COS"];
    NumericVector S4_nir_SIN = ccdc_df["S4_nir_coef_SIN"];
    NumericVector S4_nir_COS2 = ccdc_df["S4_nir_coef_COS2"];
    NumericVector S4_nir_SIN2 = ccdc_df["S4_nir_coef_SIN2"];
    NumericVector S4_nir_COS3 = ccdc_df["S4_nir_coef_COS3"];
    NumericVector S4_nir_SIN3 = ccdc_df["S4_nir_coef_SIN3"];
    NumericVector S5_nir_INTP = ccdc_df["S5_nir_coef_INTP"];
    NumericVector S5_nir_SLP = ccdc_df["S5_nir_coef_SLP"];
    NumericVector S5_nir_COS = ccdc_df["S5_nir_coef_COS"];
    NumericVector S5_nir_SIN = ccdc_df["S5_nir_coef_SIN"];
    NumericVector S5_nir_COS2 = ccdc_df["S5_nir_coef_COS2"];
    NumericVector S5_nir_SIN2 = ccdc_df["S5_nir_coef_SIN2"];
    NumericVector S5_nir_COS3 = ccdc_df["S5_nir_coef_COS3"];
    NumericVector S5_nir_SIN3 = ccdc_df["S5_nir_coef_SIN3"];
    NumericVector S6_nir_INTP = ccdc_df["S6_nir_coef_INTP"];
    NumericVector S6_nir_SLP = ccdc_df["S6_nir_coef_SLP"];
    NumericVector S6_nir_COS = ccdc_df["S6_nir_coef_COS"];
    NumericVector S6_nir_SIN = ccdc_df["S6_nir_coef_SIN"];
    NumericVector S6_nir_COS2 = ccdc_df["S6_nir_coef_COS2"];
    NumericVector S6_nir_SIN2 = ccdc_df["S6_nir_coef_SIN2"];
    NumericVector S6_nir_COS3 = ccdc_df["S6_nir_coef_COS3"];
    NumericVector S6_nir_SIN3 = ccdc_df["S6_nir_coef_SIN3"];
    NumericVector S7_nir_INTP = ccdc_df["S7_nir_coef_INTP"];
    NumericVector S7_nir_SLP = ccdc_df["S7_nir_coef_SLP"];
    NumericVector S7_nir_COS = ccdc_df["S7_nir_coef_COS"];
    NumericVector S7_nir_SIN = ccdc_df["S7_nir_coef_SIN"];
    NumericVector S7_nir_COS2 = ccdc_df["S7_nir_coef_COS2"];
    NumericVector S7_nir_SIN2 = ccdc_df["S7_nir_coef_SIN2"];
    NumericVector S7_nir_COS3 = ccdc_df["S7_nir_coef_COS3"];
    NumericVector S7_nir_SIN3 = ccdc_df["S7_nir_coef_SIN3"];
    NumericVector S8_nir_INTP = ccdc_df["S8_nir_coef_INTP"];
    NumericVector S8_nir_SLP = ccdc_df["S8_nir_coef_SLP"];
    NumericVector S8_nir_COS = ccdc_df["S8_nir_coef_COS"];
    NumericVector S8_nir_SIN = ccdc_df["S8_nir_coef_SIN"];
    NumericVector S8_nir_COS2 = ccdc_df["S8_nir_coef_COS2"];
    NumericVector S8_nir_SIN2 = ccdc_df["S8_nir_coef_SIN2"];
    NumericVector S8_nir_COS3 = ccdc_df["S8_nir_coef_COS3"];
    NumericVector S8_nir_SIN3 = ccdc_df["S8_nir_coef_SIN3"];
    NumericVector S1_swir1_INTP = ccdc_df["S1_swir1_coef_INTP"];
    NumericVector S1_swir1_SLP = ccdc_df["S1_swir1_coef_SLP"];
    NumericVector S1_swir1_COS = ccdc_df["S1_swir1_coef_COS"];
    NumericVector S1_swir1_SIN = ccdc_df["S1_swir1_coef_SIN"];
    NumericVector S1_swir1_COS2 = ccdc_df["S1_swir1_coef_COS2"];
    NumericVector S1_swir1_SIN2 = ccdc_df["S1_swir1_coef_SIN2"];
    NumericVector S1_swir1_COS3 = ccdc_df["S1_swir1_coef_COS3"];
    NumericVector S1_swir1_SIN3 = ccdc_df["S1_swir1_coef_SIN3"];
    NumericVector S2_swir1_INTP = ccdc_df["S2_swir1_coef_INTP"];
    NumericVector S2_swir1_SLP = ccdc_df["S2_swir1_coef_SLP"];
    NumericVector S2_swir1_COS = ccdc_df["S2_swir1_coef_COS"];
    NumericVector S2_swir1_SIN = ccdc_df["S2_swir1_coef_SIN"];
    NumericVector S2_swir1_COS2 = ccdc_df["S2_swir1_coef_COS2"];
    NumericVector S2_swir1_SIN2 = ccdc_df["S2_swir1_coef_SIN2"];
    NumericVector S2_swir1_COS3 = ccdc_df["S2_swir1_coef_COS3"];
    NumericVector S2_swir1_SIN3 = ccdc_df["S2_swir1_coef_SIN3"];
    NumericVector S3_swir1_INTP = ccdc_df["S3_swir1_coef_INTP"];
    NumericVector S3_swir1_SLP = ccdc_df["S3_swir1_coef_SLP"];
    NumericVector S3_swir1_COS = ccdc_df["S3_swir1_coef_COS"];
    NumericVector S3_swir1_SIN = ccdc_df["S3_swir1_coef_SIN"];
    NumericVector S3_swir1_COS2 = ccdc_df["S3_swir1_coef_COS2"];
    NumericVector S3_swir1_SIN2 = ccdc_df["S3_swir1_coef_SIN2"];
    NumericVector S3_swir1_COS3 = ccdc_df["S3_swir1_coef_COS3"];
    NumericVector S3_swir1_SIN3 = ccdc_df["S3_swir1_coef_SIN3"];
    NumericVector S4_swir1_INTP = ccdc_df["S4_swir1_coef_INTP"];
    NumericVector S4_swir1_SLP = ccdc_df["S4_swir1_coef_SLP"];
    NumericVector S4_swir1_COS = ccdc_df["S4_swir1_coef_COS"];
    NumericVector S4_swir1_SIN = ccdc_df["S4_swir1_coef_SIN"];
    NumericVector S4_swir1_COS2 = ccdc_df["S4_swir1_coef_COS2"];
    NumericVector S4_swir1_SIN2 = ccdc_df["S4_swir1_coef_SIN2"];
    NumericVector S4_swir1_COS3 = ccdc_df["S4_swir1_coef_COS3"];
    NumericVector S4_swir1_SIN3 = ccdc_df["S4_swir1_coef_SIN3"];
    NumericVector S5_swir1_INTP = ccdc_df["S5_swir1_coef_INTP"];
    NumericVector S5_swir1_SLP = ccdc_df["S5_swir1_coef_SLP"];
    NumericVector S5_swir1_COS = ccdc_df["S5_swir1_coef_COS"];
    NumericVector S5_swir1_SIN = ccdc_df["S5_swir1_coef_SIN"];
    NumericVector S5_swir1_COS2 = ccdc_df["S5_swir1_coef_COS2"];
    NumericVector S5_swir1_SIN2 = ccdc_df["S5_swir1_coef_SIN2"];
    NumericVector S5_swir1_COS3 = ccdc_df["S5_swir1_coef_COS3"];
    NumericVector S5_swir1_SIN3 = ccdc_df["S5_swir1_coef_SIN3"];
    NumericVector S6_swir1_INTP = ccdc_df["S6_swir1_coef_INTP"];
    NumericVector S6_swir1_SLP = ccdc_df["S6_swir1_coef_SLP"];
    NumericVector S6_swir1_COS = ccdc_df["S6_swir1_coef_COS"];
    NumericVector S6_swir1_SIN = ccdc_df["S6_swir1_coef_SIN"];
    NumericVector S6_swir1_COS2 = ccdc_df["S6_swir1_coef_COS2"];
    NumericVector S6_swir1_SIN2 = ccdc_df["S6_swir1_coef_SIN2"];
    NumericVector S6_swir1_COS3 = ccdc_df["S6_swir1_coef_COS3"];
    NumericVector S6_swir1_SIN3 = ccdc_df["S6_swir1_coef_SIN3"];
    NumericVector S7_swir1_INTP = ccdc_df["S7_swir1_coef_INTP"];
    NumericVector S7_swir1_SLP = ccdc_df["S7_swir1_coef_SLP"];
    NumericVector S7_swir1_COS = ccdc_df["S7_swir1_coef_COS"];
    NumericVector S7_swir1_SIN = ccdc_df["S7_swir1_coef_SIN"];
    NumericVector S7_swir1_COS2 = ccdc_df["S7_swir1_coef_COS2"];
    NumericVector S7_swir1_SIN2 = ccdc_df["S7_swir1_coef_SIN2"];
    NumericVector S7_swir1_COS3 = ccdc_df["S7_swir1_coef_COS3"];
    NumericVector S7_swir1_SIN3 = ccdc_df["S7_swir1_coef_SIN3"];
    NumericVector S8_swir1_INTP = ccdc_df["S8_swir1_coef_INTP"];
    NumericVector S8_swir1_SLP = ccdc_df["S8_swir1_coef_SLP"];
    NumericVector S8_swir1_COS = ccdc_df["S8_swir1_coef_COS"];
    NumericVector S8_swir1_SIN = ccdc_df["S8_swir1_coef_SIN"];
    NumericVector S8_swir1_COS2 = ccdc_df["S8_swir1_coef_COS2"];
    NumericVector S8_swir1_SIN2 = ccdc_df["S8_swir1_coef_SIN2"];
    NumericVector S8_swir1_COS3 = ccdc_df["S8_swir1_coef_COS3"];
    NumericVector S8_swir1_SIN3 = ccdc_df["S8_swir1_coef_SIN3"];
    IntegerVector S1_tStart = ccdc_df["S1_tStart"];
    IntegerVector S1_tEnd = ccdc_df["S1_tEnd"];
    IntegerVector S2_tStart = ccdc_df["S2_tStart"];
    IntegerVector S2_tEnd = ccdc_df["S2_tEnd"];
    IntegerVector S3_tStart = ccdc_df["S3_tStart"];
    IntegerVector S3_tEnd = ccdc_df["S3_tEnd"];
    IntegerVector S4_tStart = ccdc_df["S4_tStart"];
    IntegerVector S4_tEnd = ccdc_df["S4_tEnd"];
    IntegerVector S5_tStart = ccdc_df["S5_tStart"];
    IntegerVector S5_tEnd = ccdc_df["S5_tEnd"];
    IntegerVector S6_tStart = ccdc_df["S6_tStart"];
    IntegerVector S6_tEnd = ccdc_df["S6_tEnd"];
    IntegerVector S7_tStart = ccdc_df["S7_tStart"];
    IntegerVector S7_tEnd = ccdc_df["S7_tEnd"];
    IntegerVector S8_tStart = ccdc_df["S8_tStart"];
    IntegerVector S8_tEnd = ccdc_df["S8_tEnd"];


    for ( int i = 0; i < n; ++i ) {

        NumericVector ndmi_ts ( jdoy_end-jdoy_start );
        NumericVector ndwi_ts ( jdoy_end-jdoy_start );
        int day = 0;
        for ( int jdoy = jdoy_start; jdoy<jdoy_end; jdoy++ ) {
            if ( jdoy >= S1_tStart[i] && jdoy <= S1_tEnd[i] ) {


                float green_val =S1_green_INTP[i]+S1_green_SLP[i]*jdoy+S1_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S1_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S1_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S1_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S1_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S1_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S1_nir_INTP[i]+S1_nir_SLP[i]*jdoy+S1_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S1_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S1_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S1_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S1_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S1_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float swir1_val =S1_swir1_INTP[i]+S1_swir1_SLP[i]*jdoy+S1_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S1_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S1_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S1_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S1_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S1_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );



            } else if ( jdoy >= S2_tStart[i] && jdoy <= S2_tEnd[i] ) {

                float green_val = S2_green_INTP[i]+S2_green_SLP[i]*jdoy+S2_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S2_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S2_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S2_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S2_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S2_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S2_nir_INTP[i]+S2_nir_SLP[i]*jdoy+S2_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S2_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S2_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S2_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S2_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S2_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float swir1_val =S2_swir1_INTP[i]+S2_swir1_SLP[i]*jdoy+S2_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S2_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S2_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S2_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S2_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S2_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            } else if ( jdoy >= S3_tStart[i] && jdoy <= S3_tEnd[i] ) {

                float green_val =S3_green_INTP[i]+S3_green_SLP[i]*jdoy+S3_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S3_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S3_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S3_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S3_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S3_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S3_nir_INTP[i]+S3_nir_SLP[i]*jdoy+S3_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S3_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S3_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S3_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S3_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S3_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                float swir1_val =S3_swir1_INTP[i]+S3_swir1_SLP[i]*jdoy+S3_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S3_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S3_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S3_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S3_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S3_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            } else if ( jdoy >= S4_tStart[i] && jdoy <= S4_tEnd[i] ) {

                float green_val =S4_green_INTP[i]+S4_green_SLP[i]*jdoy+S4_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S4_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S4_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S4_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S4_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S4_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S4_nir_INTP[i]+S4_nir_SLP[i]*jdoy+S4_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S4_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S4_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S4_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S4_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S4_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                float swir1_val =S4_swir1_INTP[i]+S4_swir1_SLP[i]*jdoy+S4_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S4_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S4_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S4_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S4_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S4_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            } else if ( jdoy >= S5_tStart[i] && jdoy <= S5_tEnd[i] ) {

                float green_val =S5_green_INTP[i]+S5_green_SLP[i]*jdoy+S5_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S5_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S5_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S5_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S5_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S5_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S5_nir_INTP[i]+S5_nir_SLP[i]*jdoy+S5_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S5_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S5_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S5_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S5_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S5_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                float swir1_val =S5_swir1_INTP[i]+S5_swir1_SLP[i]*jdoy+S5_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S5_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S5_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S5_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S5_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S5_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            } else if ( jdoy >= S6_tStart[i] && jdoy <= S6_tEnd[i] ) {

                float green_val =S6_green_INTP[i]+S6_green_SLP[i]*jdoy+S6_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S6_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S6_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S6_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S6_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S6_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S6_nir_INTP[i]+S6_nir_SLP[i]*jdoy+S6_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S6_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S6_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S6_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S6_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S6_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                float swir1_val =S6_swir1_INTP[i]+S6_swir1_SLP[i]*jdoy+S6_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S6_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S6_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S6_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S6_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S6_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            } else if ( jdoy >= S7_tStart[i] && jdoy <= S7_tEnd[i] ) {

                float green_val =S7_green_INTP[i]+S7_green_SLP[i]*jdoy+S7_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S7_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S7_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S7_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S7_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S7_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S7_nir_INTP[i]+S7_nir_SLP[i]*jdoy+S7_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S7_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S7_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S7_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S7_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S7_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                float swir1_val =S7_swir1_INTP[i]+S7_swir1_SLP[i]*jdoy+S7_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S7_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S7_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S7_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S7_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S7_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            } else if ( jdoy >= S8_tStart[i] && jdoy <= S8_tEnd[i] ) {

                float green_val =S8_green_INTP[i]+S8_green_SLP[i]*jdoy+S8_green_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S8_green_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S8_green_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S8_green_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S8_green_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S8_green_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );

                float nir_val =S8_nir_INTP[i]+S8_nir_SLP[i]*jdoy+S8_nir_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S8_nir_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S8_nir_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S8_nir_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S8_nir_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S8_nir_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                float swir1_val =S8_swir1_INTP[i]+S8_swir1_SLP[i]*jdoy+S8_swir1_COS[i]*cos ( ( 2.0*pi*jdoy ) /365.25 )+S8_swir1_SIN[i]*sin ( ( 2.0*pi*jdoy ) /365.25 )+S8_swir1_COS2[i]*cos ( ( 4.0*pi*jdoy ) /365.25 )+S8_swir1_SIN2[i]*sin ( ( 4.0*pi*jdoy ) /365.25 )+S8_swir1_COS3[i]*cos ( ( 6.0*pi*jdoy ) /365.25 )+S8_swir1_SIN3[i]*sin ( ( 6.0*pi*jdoy ) /365.25 );
                
                ndmi_ts[day]= ( nir_val-swir1_val ) / ( nir_val+swir1_val );
                ndwi_ts[day]= ( green_val-nir_val ) / ( green_val+nir_val );

            }
            day++;
        }
        
        ndmi_min[i]=min(ndmi_ts);
        ndmi_max[i]=max(ndmi_ts);
        
        ndwi_min[i]=min(ndwi_ts);
        ndwi_max[i]=max(ndwi_ts);
        
        ndmi_med[i]=median(ndmi_ts);
        ndwi_med[i]=median(ndwi_ts);
        
        ndmi_jdoy_max[i]=which_max(ndmi_ts);
        ndmi_jdoy_min[i]=which_min(ndmi_ts);
        
        ndwi_jdoy_max[i]=which_max(ndwi_ts);
        ndwi_jdoy_min[i]=which_min(ndwi_ts);
        
    }
    DataFrame out = DataFrame::create ( Named ( "x" ) =ccdc_df["x"],
                                        Named ( "y" ) =ccdc_df["y"],
                                        Named ( "ndmi_min" ) = ndmi_min,
                                        Named ( "ndmi_max" ) =ndmi_max,
                                        Named ( "ndmi_med" ) =ndmi_med,
                                        Named ( "ndwi_min" ) =ndwi_min,
                                        Named ( "ndwi_max" ) =ndwi_max,
                                        Named ( "ndwi_med" ) =ndwi_med,
                                        Named ( "ndmi_jdoy_max")=ndmi_jdoy_max,
                                        Named ( "ndmi_jdoy_min")=ndmi_jdoy_min,
                                        Named ( "ndwi_jdoy_max")=ndwi_jdoy_max,
                                        Named ( "ndwi_jdoy_min")=ndwi_jdoy_min);

    return out;

}')
  
  
  smry<-ccdc_ndmi_summary(ccdc_img,sdt,edt)
  
  smry<-terra::rast(smry,type="xyz")
  
  terra::crs(smry)<-crs
  
  return(smry)
  
}

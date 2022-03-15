#' Get the segment coefficients from the CCDC image at a specific Julian date (see Zhu & Woodcock 2014) 
#'
#' Using the coefficients from the CCDC image a gap free Landsat Scene for any date (within the date range of the CCDC image) 
#' can be generated using this function. 
#' @param ccdc_img (SpatRaster, stars or data.frame) The CCDC image for which to extract coefficients as SpatRaster, stars or data.frame 
#' with XY coordinates (i.e., terra::as.data.frame(img,xy=T)), data.frame is the fastest. 
#' @param date (character) Date for which to extract CCDC coefficients (must be with in the date range for which the CCDC image was processed).
#' @param orig_crs (optional) In the case that a data.frame is provided this must be set to the original CRS of the image 
#' in format recognized by 'Terra' package. 
#' @return (SpatRaster) A SpatRaster with all 7 Landsat bands predicted from the CCDC coefficients for the specified date.   
#' @export
gen_ccdc_synthetic_img<-function(ccdc_img,date,orig_crs=NULL)
{
  
  jdoy<-date_to_jdoy(date)
  
  if(class(ccdc_img)=="stars")
  {
    ccdc_img<-terra::rast(ccdc_img)
  }
  
  if(is.null(orig_crs))
  {
    orig_crs<-terra::crs(ccdc_img)
  }
  
  if(unlist(class(terra::rast(ccdc_img)))[1]=="SpatRaster")
  {
    ccdc_img<-terra::as.data.frame(ccdc_img,xy=T)
  }
  
  Rcpp::cppFunction('DataFrame ccdc_synthetic_img(DataFrame ccdc_df, int jdoy)
{
    int n = ccdc_df.nrows();

    double pi = 3.1415926535;

    NumericVector blue_band(n);
    NumericVector green_band(n);
    NumericVector red_band(n);
    NumericVector nir_band(n);
    NumericVector swir1_band(n);
    NumericVector swir2_band(n);
    NumericVector therm_band(n);

    NumericVector S1_blue_INTP = ccdc_df["S1_blue_coef_INTP"];
    NumericVector S1_blue_SLP = ccdc_df["S1_blue_coef_SLP"];
    NumericVector S1_blue_COS = ccdc_df["S1_blue_coef_COS"];
    NumericVector S1_blue_SIN = ccdc_df["S1_blue_coef_SIN"];
    NumericVector S1_blue_COS2 = ccdc_df["S1_blue_coef_COS2"];
    NumericVector S1_blue_SIN2 = ccdc_df["S1_blue_coef_SIN2"];
    NumericVector S1_blue_COS3 = ccdc_df["S1_blue_coef_COS3"];
    NumericVector S1_blue_SIN3 = ccdc_df["S1_blue_coef_SIN3"];
    NumericVector S2_blue_INTP = ccdc_df["S2_blue_coef_INTP"];
    NumericVector S2_blue_SLP = ccdc_df["S2_blue_coef_SLP"];
    NumericVector S2_blue_COS = ccdc_df["S2_blue_coef_COS"];
    NumericVector S2_blue_SIN = ccdc_df["S2_blue_coef_SIN"];
    NumericVector S2_blue_COS2 = ccdc_df["S2_blue_coef_COS2"];
    NumericVector S2_blue_SIN2 = ccdc_df["S2_blue_coef_SIN2"];
    NumericVector S2_blue_COS3 = ccdc_df["S2_blue_coef_COS3"];
    NumericVector S2_blue_SIN3 = ccdc_df["S2_blue_coef_SIN3"];
    NumericVector S3_blue_INTP = ccdc_df["S3_blue_coef_INTP"];
    NumericVector S3_blue_SLP = ccdc_df["S3_blue_coef_SLP"];
    NumericVector S3_blue_COS = ccdc_df["S3_blue_coef_COS"];
    NumericVector S3_blue_SIN = ccdc_df["S3_blue_coef_SIN"];
    NumericVector S3_blue_COS2 = ccdc_df["S3_blue_coef_COS2"];
    NumericVector S3_blue_SIN2 = ccdc_df["S3_blue_coef_SIN2"];
    NumericVector S3_blue_COS3 = ccdc_df["S3_blue_coef_COS3"];
    NumericVector S3_blue_SIN3 = ccdc_df["S3_blue_coef_SIN3"];
    NumericVector S4_blue_INTP = ccdc_df["S4_blue_coef_INTP"];
    NumericVector S4_blue_SLP = ccdc_df["S4_blue_coef_SLP"];
    NumericVector S4_blue_COS = ccdc_df["S4_blue_coef_COS"];
    NumericVector S4_blue_SIN = ccdc_df["S4_blue_coef_SIN"];
    NumericVector S4_blue_COS2 = ccdc_df["S4_blue_coef_COS2"];
    NumericVector S4_blue_SIN2 = ccdc_df["S4_blue_coef_SIN2"];
    NumericVector S4_blue_COS3 = ccdc_df["S4_blue_coef_COS3"];
    NumericVector S4_blue_SIN3 = ccdc_df["S4_blue_coef_SIN3"];
    NumericVector S5_blue_INTP = ccdc_df["S5_blue_coef_INTP"];
    NumericVector S5_blue_SLP = ccdc_df["S5_blue_coef_SLP"];
    NumericVector S5_blue_COS = ccdc_df["S5_blue_coef_COS"];
    NumericVector S5_blue_SIN = ccdc_df["S5_blue_coef_SIN"];
    NumericVector S5_blue_COS2 = ccdc_df["S5_blue_coef_COS2"];
    NumericVector S5_blue_SIN2 = ccdc_df["S5_blue_coef_SIN2"];
    NumericVector S5_blue_COS3 = ccdc_df["S5_blue_coef_COS3"];
    NumericVector S5_blue_SIN3 = ccdc_df["S5_blue_coef_SIN3"];
    NumericVector S6_blue_INTP = ccdc_df["S6_blue_coef_INTP"];
    NumericVector S6_blue_SLP = ccdc_df["S6_blue_coef_SLP"];
    NumericVector S6_blue_COS = ccdc_df["S6_blue_coef_COS"];
    NumericVector S6_blue_SIN = ccdc_df["S6_blue_coef_SIN"];
    NumericVector S6_blue_COS2 = ccdc_df["S6_blue_coef_COS2"];
    NumericVector S6_blue_SIN2 = ccdc_df["S6_blue_coef_SIN2"];
    NumericVector S6_blue_COS3 = ccdc_df["S6_blue_coef_COS3"];
    NumericVector S6_blue_SIN3 = ccdc_df["S6_blue_coef_SIN3"];
    NumericVector S7_blue_INTP = ccdc_df["S7_blue_coef_INTP"];
    NumericVector S7_blue_SLP = ccdc_df["S7_blue_coef_SLP"];
    NumericVector S7_blue_COS = ccdc_df["S7_blue_coef_COS"];
    NumericVector S7_blue_SIN = ccdc_df["S7_blue_coef_SIN"];
    NumericVector S7_blue_COS2 = ccdc_df["S7_blue_coef_COS2"];
    NumericVector S7_blue_SIN2 = ccdc_df["S7_blue_coef_SIN2"];
    NumericVector S7_blue_COS3 = ccdc_df["S7_blue_coef_COS3"];
    NumericVector S7_blue_SIN3 = ccdc_df["S7_blue_coef_SIN3"];
    NumericVector S8_blue_INTP = ccdc_df["S8_blue_coef_INTP"];
    NumericVector S8_blue_SLP = ccdc_df["S8_blue_coef_SLP"];
    NumericVector S8_blue_COS = ccdc_df["S8_blue_coef_COS"];
    NumericVector S8_blue_SIN = ccdc_df["S8_blue_coef_SIN"];
    NumericVector S8_blue_COS2 = ccdc_df["S8_blue_coef_COS2"];
    NumericVector S8_blue_SIN2 = ccdc_df["S8_blue_coef_SIN2"];
    NumericVector S8_blue_COS3 = ccdc_df["S8_blue_coef_COS3"];
    NumericVector S8_blue_SIN3 = ccdc_df["S8_blue_coef_SIN3"];
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
    NumericVector S1_red_INTP = ccdc_df["S1_red_coef_INTP"];
    NumericVector S1_red_SLP = ccdc_df["S1_red_coef_SLP"];
    NumericVector S1_red_COS = ccdc_df["S1_red_coef_COS"];
    NumericVector S1_red_SIN = ccdc_df["S1_red_coef_SIN"];
    NumericVector S1_red_COS2 = ccdc_df["S1_red_coef_COS2"];
    NumericVector S1_red_SIN2 = ccdc_df["S1_red_coef_SIN2"];
    NumericVector S1_red_COS3 = ccdc_df["S1_red_coef_COS3"];
    NumericVector S1_red_SIN3 = ccdc_df["S1_red_coef_SIN3"];
    NumericVector S2_red_INTP = ccdc_df["S2_red_coef_INTP"];
    NumericVector S2_red_SLP = ccdc_df["S2_red_coef_SLP"];
    NumericVector S2_red_COS = ccdc_df["S2_red_coef_COS"];
    NumericVector S2_red_SIN = ccdc_df["S2_red_coef_SIN"];
    NumericVector S2_red_COS2 = ccdc_df["S2_red_coef_COS2"];
    NumericVector S2_red_SIN2 = ccdc_df["S2_red_coef_SIN2"];
    NumericVector S2_red_COS3 = ccdc_df["S2_red_coef_COS3"];
    NumericVector S2_red_SIN3 = ccdc_df["S2_red_coef_SIN3"];
    NumericVector S3_red_INTP = ccdc_df["S3_red_coef_INTP"];
    NumericVector S3_red_SLP = ccdc_df["S3_red_coef_SLP"];
    NumericVector S3_red_COS = ccdc_df["S3_red_coef_COS"];
    NumericVector S3_red_SIN = ccdc_df["S3_red_coef_SIN"];
    NumericVector S3_red_COS2 = ccdc_df["S3_red_coef_COS2"];
    NumericVector S3_red_SIN2 = ccdc_df["S3_red_coef_SIN2"];
    NumericVector S3_red_COS3 = ccdc_df["S3_red_coef_COS3"];
    NumericVector S3_red_SIN3 = ccdc_df["S3_red_coef_SIN3"];
    NumericVector S4_red_INTP = ccdc_df["S4_red_coef_INTP"];
    NumericVector S4_red_SLP = ccdc_df["S4_red_coef_SLP"];
    NumericVector S4_red_COS = ccdc_df["S4_red_coef_COS"];
    NumericVector S4_red_SIN = ccdc_df["S4_red_coef_SIN"];
    NumericVector S4_red_COS2 = ccdc_df["S4_red_coef_COS2"];
    NumericVector S4_red_SIN2 = ccdc_df["S4_red_coef_SIN2"];
    NumericVector S4_red_COS3 = ccdc_df["S4_red_coef_COS3"];
    NumericVector S4_red_SIN3 = ccdc_df["S4_red_coef_SIN3"];
    NumericVector S5_red_INTP = ccdc_df["S5_red_coef_INTP"];
    NumericVector S5_red_SLP = ccdc_df["S5_red_coef_SLP"];
    NumericVector S5_red_COS = ccdc_df["S5_red_coef_COS"];
    NumericVector S5_red_SIN = ccdc_df["S5_red_coef_SIN"];
    NumericVector S5_red_COS2 = ccdc_df["S5_red_coef_COS2"];
    NumericVector S5_red_SIN2 = ccdc_df["S5_red_coef_SIN2"];
    NumericVector S5_red_COS3 = ccdc_df["S5_red_coef_COS3"];
    NumericVector S5_red_SIN3 = ccdc_df["S5_red_coef_SIN3"];
    NumericVector S6_red_INTP = ccdc_df["S6_red_coef_INTP"];
    NumericVector S6_red_SLP = ccdc_df["S6_red_coef_SLP"];
    NumericVector S6_red_COS = ccdc_df["S6_red_coef_COS"];
    NumericVector S6_red_SIN = ccdc_df["S6_red_coef_SIN"];
    NumericVector S6_red_COS2 = ccdc_df["S6_red_coef_COS2"];
    NumericVector S6_red_SIN2 = ccdc_df["S6_red_coef_SIN2"];
    NumericVector S6_red_COS3 = ccdc_df["S6_red_coef_COS3"];
    NumericVector S6_red_SIN3 = ccdc_df["S6_red_coef_SIN3"];
    NumericVector S7_red_INTP = ccdc_df["S7_red_coef_INTP"];
    NumericVector S7_red_SLP = ccdc_df["S7_red_coef_SLP"];
    NumericVector S7_red_COS = ccdc_df["S7_red_coef_COS"];
    NumericVector S7_red_SIN = ccdc_df["S7_red_coef_SIN"];
    NumericVector S7_red_COS2 = ccdc_df["S7_red_coef_COS2"];
    NumericVector S7_red_SIN2 = ccdc_df["S7_red_coef_SIN2"];
    NumericVector S7_red_COS3 = ccdc_df["S7_red_coef_COS3"];
    NumericVector S7_red_SIN3 = ccdc_df["S7_red_coef_SIN3"];
    NumericVector S8_red_INTP = ccdc_df["S8_red_coef_INTP"];
    NumericVector S8_red_SLP = ccdc_df["S8_red_coef_SLP"];
    NumericVector S8_red_COS = ccdc_df["S8_red_coef_COS"];
    NumericVector S8_red_SIN = ccdc_df["S8_red_coef_SIN"];
    NumericVector S8_red_COS2 = ccdc_df["S8_red_coef_COS2"];
    NumericVector S8_red_SIN2 = ccdc_df["S8_red_coef_SIN2"];
    NumericVector S8_red_COS3 = ccdc_df["S8_red_coef_COS3"];
    NumericVector S8_red_SIN3 = ccdc_df["S8_red_coef_SIN3"];
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
    NumericVector S1_swir2_INTP = ccdc_df["S1_swir2_coef_INTP"];
    NumericVector S1_swir2_SLP = ccdc_df["S1_swir2_coef_SLP"];
    NumericVector S1_swir2_COS = ccdc_df["S1_swir2_coef_COS"];
    NumericVector S1_swir2_SIN = ccdc_df["S1_swir2_coef_SIN"];
    NumericVector S1_swir2_COS2 = ccdc_df["S1_swir2_coef_COS2"];
    NumericVector S1_swir2_SIN2 = ccdc_df["S1_swir2_coef_SIN2"];
    NumericVector S1_swir2_COS3 = ccdc_df["S1_swir2_coef_COS3"];
    NumericVector S1_swir2_SIN3 = ccdc_df["S1_swir2_coef_SIN3"];
    NumericVector S2_swir2_INTP = ccdc_df["S2_swir2_coef_INTP"];
    NumericVector S2_swir2_SLP = ccdc_df["S2_swir2_coef_SLP"];
    NumericVector S2_swir2_COS = ccdc_df["S2_swir2_coef_COS"];
    NumericVector S2_swir2_SIN = ccdc_df["S2_swir2_coef_SIN"];
    NumericVector S2_swir2_COS2 = ccdc_df["S2_swir2_coef_COS2"];
    NumericVector S2_swir2_SIN2 = ccdc_df["S2_swir2_coef_SIN2"];
    NumericVector S2_swir2_COS3 = ccdc_df["S2_swir2_coef_COS3"];
    NumericVector S2_swir2_SIN3 = ccdc_df["S2_swir2_coef_SIN3"];
    NumericVector S3_swir2_INTP = ccdc_df["S3_swir2_coef_INTP"];
    NumericVector S3_swir2_SLP = ccdc_df["S3_swir2_coef_SLP"];
    NumericVector S3_swir2_COS = ccdc_df["S3_swir2_coef_COS"];
    NumericVector S3_swir2_SIN = ccdc_df["S3_swir2_coef_SIN"];
    NumericVector S3_swir2_COS2 = ccdc_df["S3_swir2_coef_COS2"];
    NumericVector S3_swir2_SIN2 = ccdc_df["S3_swir2_coef_SIN2"];
    NumericVector S3_swir2_COS3 = ccdc_df["S3_swir2_coef_COS3"];
    NumericVector S3_swir2_SIN3 = ccdc_df["S3_swir2_coef_SIN3"];
    NumericVector S4_swir2_INTP = ccdc_df["S4_swir2_coef_INTP"];
    NumericVector S4_swir2_SLP = ccdc_df["S4_swir2_coef_SLP"];
    NumericVector S4_swir2_COS = ccdc_df["S4_swir2_coef_COS"];
    NumericVector S4_swir2_SIN = ccdc_df["S4_swir2_coef_SIN"];
    NumericVector S4_swir2_COS2 = ccdc_df["S4_swir2_coef_COS2"];
    NumericVector S4_swir2_SIN2 = ccdc_df["S4_swir2_coef_SIN2"];
    NumericVector S4_swir2_COS3 = ccdc_df["S4_swir2_coef_COS3"];
    NumericVector S4_swir2_SIN3 = ccdc_df["S4_swir2_coef_SIN3"];
    NumericVector S5_swir2_INTP = ccdc_df["S5_swir2_coef_INTP"];
    NumericVector S5_swir2_SLP = ccdc_df["S5_swir2_coef_SLP"];
    NumericVector S5_swir2_COS = ccdc_df["S5_swir2_coef_COS"];
    NumericVector S5_swir2_SIN = ccdc_df["S5_swir2_coef_SIN"];
    NumericVector S5_swir2_COS2 = ccdc_df["S5_swir2_coef_COS2"];
    NumericVector S5_swir2_SIN2 = ccdc_df["S5_swir2_coef_SIN2"];
    NumericVector S5_swir2_COS3 = ccdc_df["S5_swir2_coef_COS3"];
    NumericVector S5_swir2_SIN3 = ccdc_df["S5_swir2_coef_SIN3"];
    NumericVector S6_swir2_INTP = ccdc_df["S6_swir2_coef_INTP"];
    NumericVector S6_swir2_SLP = ccdc_df["S6_swir2_coef_SLP"];
    NumericVector S6_swir2_COS = ccdc_df["S6_swir2_coef_COS"];
    NumericVector S6_swir2_SIN = ccdc_df["S6_swir2_coef_SIN"];
    NumericVector S6_swir2_COS2 = ccdc_df["S6_swir2_coef_COS2"];
    NumericVector S6_swir2_SIN2 = ccdc_df["S6_swir2_coef_SIN2"];
    NumericVector S6_swir2_COS3 = ccdc_df["S6_swir2_coef_COS3"];
    NumericVector S6_swir2_SIN3 = ccdc_df["S6_swir2_coef_SIN3"];
    NumericVector S7_swir2_INTP = ccdc_df["S7_swir2_coef_INTP"];
    NumericVector S7_swir2_SLP = ccdc_df["S7_swir2_coef_SLP"];
    NumericVector S7_swir2_COS = ccdc_df["S7_swir2_coef_COS"];
    NumericVector S7_swir2_SIN = ccdc_df["S7_swir2_coef_SIN"];
    NumericVector S7_swir2_COS2 = ccdc_df["S7_swir2_coef_COS2"];
    NumericVector S7_swir2_SIN2 = ccdc_df["S7_swir2_coef_SIN2"];
    NumericVector S7_swir2_COS3 = ccdc_df["S7_swir2_coef_COS3"];
    NumericVector S7_swir2_SIN3 = ccdc_df["S7_swir2_coef_SIN3"];
    NumericVector S8_swir2_INTP = ccdc_df["S8_swir2_coef_INTP"];
    NumericVector S8_swir2_SLP = ccdc_df["S8_swir2_coef_SLP"];
    NumericVector S8_swir2_COS = ccdc_df["S8_swir2_coef_COS"];
    NumericVector S8_swir2_SIN = ccdc_df["S8_swir2_coef_SIN"];
    NumericVector S8_swir2_COS2 = ccdc_df["S8_swir2_coef_COS2"];
    NumericVector S8_swir2_SIN2 = ccdc_df["S8_swir2_coef_SIN2"];
    NumericVector S8_swir2_COS3 = ccdc_df["S8_swir2_coef_COS3"];
    NumericVector S8_swir2_SIN3 = ccdc_df["S8_swir2_coef_SIN3"];
    NumericVector S1_therm_INTP = ccdc_df["S1_therm_coef_INTP"];
    NumericVector S1_therm_SLP = ccdc_df["S1_therm_coef_SLP"];
    NumericVector S1_therm_COS = ccdc_df["S1_therm_coef_COS"];
    NumericVector S1_therm_SIN = ccdc_df["S1_therm_coef_SIN"];
    NumericVector S1_therm_COS2 = ccdc_df["S1_therm_coef_COS2"];
    NumericVector S1_therm_SIN2 = ccdc_df["S1_therm_coef_SIN2"];
    NumericVector S1_therm_COS3 = ccdc_df["S1_therm_coef_COS3"];
    NumericVector S1_therm_SIN3 = ccdc_df["S1_therm_coef_SIN3"];
    NumericVector S2_therm_INTP = ccdc_df["S2_therm_coef_INTP"];
    NumericVector S2_therm_SLP = ccdc_df["S2_therm_coef_SLP"];
    NumericVector S2_therm_COS = ccdc_df["S2_therm_coef_COS"];
    NumericVector S2_therm_SIN = ccdc_df["S2_therm_coef_SIN"];
    NumericVector S2_therm_COS2 = ccdc_df["S2_therm_coef_COS2"];
    NumericVector S2_therm_SIN2 = ccdc_df["S2_therm_coef_SIN2"];
    NumericVector S2_therm_COS3 = ccdc_df["S2_therm_coef_COS3"];
    NumericVector S2_therm_SIN3 = ccdc_df["S2_therm_coef_SIN3"];
    NumericVector S3_therm_INTP = ccdc_df["S3_therm_coef_INTP"];
    NumericVector S3_therm_SLP = ccdc_df["S3_therm_coef_SLP"];
    NumericVector S3_therm_COS = ccdc_df["S3_therm_coef_COS"];
    NumericVector S3_therm_SIN = ccdc_df["S3_therm_coef_SIN"];
    NumericVector S3_therm_COS2 = ccdc_df["S3_therm_coef_COS2"];
    NumericVector S3_therm_SIN2 = ccdc_df["S3_therm_coef_SIN2"];
    NumericVector S3_therm_COS3 = ccdc_df["S3_therm_coef_COS3"];
    NumericVector S3_therm_SIN3 = ccdc_df["S3_therm_coef_SIN3"];
    NumericVector S4_therm_INTP = ccdc_df["S4_therm_coef_INTP"];
    NumericVector S4_therm_SLP = ccdc_df["S4_therm_coef_SLP"];
    NumericVector S4_therm_COS = ccdc_df["S4_therm_coef_COS"];
    NumericVector S4_therm_SIN = ccdc_df["S4_therm_coef_SIN"];
    NumericVector S4_therm_COS2 = ccdc_df["S4_therm_coef_COS2"];
    NumericVector S4_therm_SIN2 = ccdc_df["S4_therm_coef_SIN2"];
    NumericVector S4_therm_COS3 = ccdc_df["S4_therm_coef_COS3"];
    NumericVector S4_therm_SIN3 = ccdc_df["S4_therm_coef_SIN3"];
    NumericVector S5_therm_INTP = ccdc_df["S5_therm_coef_INTP"];
    NumericVector S5_therm_SLP = ccdc_df["S5_therm_coef_SLP"];
    NumericVector S5_therm_COS = ccdc_df["S5_therm_coef_COS"];
    NumericVector S5_therm_SIN = ccdc_df["S5_therm_coef_SIN"];
    NumericVector S5_therm_COS2 = ccdc_df["S5_therm_coef_COS2"];
    NumericVector S5_therm_SIN2 = ccdc_df["S5_therm_coef_SIN2"];
    NumericVector S5_therm_COS3 = ccdc_df["S5_therm_coef_COS3"];
    NumericVector S5_therm_SIN3 = ccdc_df["S5_therm_coef_SIN3"];
    NumericVector S6_therm_INTP = ccdc_df["S6_therm_coef_INTP"];
    NumericVector S6_therm_SLP = ccdc_df["S6_therm_coef_SLP"];
    NumericVector S6_therm_COS = ccdc_df["S6_therm_coef_COS"];
    NumericVector S6_therm_SIN = ccdc_df["S6_therm_coef_SIN"];
    NumericVector S6_therm_COS2 = ccdc_df["S6_therm_coef_COS2"];
    NumericVector S6_therm_SIN2 = ccdc_df["S6_therm_coef_SIN2"];
    NumericVector S6_therm_COS3 = ccdc_df["S6_therm_coef_COS3"];
    NumericVector S6_therm_SIN3 = ccdc_df["S6_therm_coef_SIN3"];
    NumericVector S7_therm_INTP = ccdc_df["S7_therm_coef_INTP"];
    NumericVector S7_therm_SLP = ccdc_df["S7_therm_coef_SLP"];
    NumericVector S7_therm_COS = ccdc_df["S7_therm_coef_COS"];
    NumericVector S7_therm_SIN = ccdc_df["S7_therm_coef_SIN"];
    NumericVector S7_therm_COS2 = ccdc_df["S7_therm_coef_COS2"];
    NumericVector S7_therm_SIN2 = ccdc_df["S7_therm_coef_SIN2"];
    NumericVector S7_therm_COS3 = ccdc_df["S7_therm_coef_COS3"];
    NumericVector S7_therm_SIN3 = ccdc_df["S7_therm_coef_SIN3"];
    NumericVector S8_therm_INTP = ccdc_df["S8_therm_coef_INTP"];
    NumericVector S8_therm_SLP = ccdc_df["S8_therm_coef_SLP"];
    NumericVector S8_therm_COS = ccdc_df["S8_therm_coef_COS"];
    NumericVector S8_therm_SIN = ccdc_df["S8_therm_coef_SIN"];
    NumericVector S8_therm_COS2 = ccdc_df["S8_therm_coef_COS2"];
    NumericVector S8_therm_SIN2 = ccdc_df["S8_therm_coef_SIN2"];
    NumericVector S8_therm_COS3 = ccdc_df["S8_therm_coef_COS3"];
    NumericVector S8_therm_SIN3 = ccdc_df["S8_therm_coef_SIN3"];
    NumericVector S1_tStart = ccdc_df["S1_tStart"];
    NumericVector S1_tEnd = ccdc_df["S1_tEnd"];
    NumericVector S2_tStart = ccdc_df["S2_tStart"];
    NumericVector S2_tEnd = ccdc_df["S2_tEnd"];
    NumericVector S3_tStart = ccdc_df["S3_tStart"];
    NumericVector S3_tEnd = ccdc_df["S3_tEnd"];
    NumericVector S4_tStart = ccdc_df["S4_tStart"];
    NumericVector S4_tEnd = ccdc_df["S4_tEnd"];
    NumericVector S5_tStart = ccdc_df["S5_tStart"];
    NumericVector S5_tEnd = ccdc_df["S5_tEnd"];
    NumericVector S6_tStart = ccdc_df["S6_tStart"];
    NumericVector S6_tEnd = ccdc_df["S6_tEnd"];
    NumericVector S7_tStart = ccdc_df["S7_tStart"];
    NumericVector S7_tEnd = ccdc_df["S7_tEnd"];
    NumericVector S8_tStart = ccdc_df["S8_tStart"];
    NumericVector S8_tEnd = ccdc_df["S8_tEnd"];


    for(int i = 0; i < n; ++i) {
        if(jdoy >= S1_tStart[i] && jdoy <= S1_tEnd[i])
        {

            blue_band[i] =S1_blue_INTP[i]+S1_blue_SLP[i]*jdoy+S1_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S1_green_INTP[i]+S1_green_SLP[i]*jdoy+S1_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S1_red_INTP[i]+S1_red_SLP[i]*jdoy+S1_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S1_nir_INTP[i]+S1_nir_SLP[i]*jdoy+S1_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S1_swir1_INTP[i]+S1_swir1_SLP[i]*jdoy+S1_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S1_swir2_INTP[i]+S1_swir2_SLP[i]*jdoy+S1_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S1_therm_INTP[i]+S1_therm_SLP[i]*jdoy+S1_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S1_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S1_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S1_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S1_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S1_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);

        } else if (jdoy >= S2_tStart[i] && jdoy <= S2_tEnd[i])
        {
            blue_band[i] =S2_blue_INTP[i]+S2_blue_SLP[i]*jdoy+S2_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S2_green_INTP[i]+S2_green_SLP[i]*jdoy+S2_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S2_red_INTP[i]+S2_red_SLP[i]*jdoy+S2_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S2_nir_INTP[i]+S2_nir_SLP[i]*jdoy+S2_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S2_swir1_INTP[i]+S2_swir1_SLP[i]*jdoy+S2_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S2_swir2_INTP[i]+S2_swir2_SLP[i]*jdoy+S2_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S2_therm_INTP[i]+S2_therm_SLP[i]*jdoy+S2_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S2_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S2_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S2_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S2_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S2_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        } else if (jdoy >= S3_tStart[i] && jdoy <= S3_tEnd[i])
        {
            blue_band[i] =S3_blue_INTP[i]+S3_blue_SLP[i]*jdoy+S3_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S3_green_INTP[i]+S3_green_SLP[i]*jdoy+S3_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S3_red_INTP[i]+S3_red_SLP[i]*jdoy+S3_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S3_nir_INTP[i]+S3_nir_SLP[i]*jdoy+S3_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S3_swir1_INTP[i]+S3_swir1_SLP[i]*jdoy+S3_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S3_swir2_INTP[i]+S3_swir2_SLP[i]*jdoy+S3_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S3_therm_INTP[i]+S3_therm_SLP[i]*jdoy+S3_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S3_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S3_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S3_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S3_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S3_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        } else if (jdoy >= S4_tStart[i] && jdoy <= S4_tEnd[i])
        {
            blue_band[i] =S4_blue_INTP[i]+S4_blue_SLP[i]*jdoy+S4_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S4_green_INTP[i]+S4_green_SLP[i]*jdoy+S4_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S4_red_INTP[i]+S4_red_SLP[i]*jdoy+S4_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S4_nir_INTP[i]+S4_nir_SLP[i]*jdoy+S4_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S4_swir1_INTP[i]+S4_swir1_SLP[i]*jdoy+S4_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S4_swir2_INTP[i]+S4_swir2_SLP[i]*jdoy+S4_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S4_therm_INTP[i]+S4_therm_SLP[i]*jdoy+S4_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S4_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S4_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S4_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S4_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S4_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        } else if (jdoy >= S5_tStart[i] && jdoy <= S5_tEnd[i])
        {
            blue_band[i] =S5_blue_INTP[i]+S5_blue_SLP[i]*jdoy+S5_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S5_green_INTP[i]+S5_green_SLP[i]*jdoy+S5_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S5_red_INTP[i]+S5_red_SLP[i]*jdoy+S5_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S5_nir_INTP[i]+S5_nir_SLP[i]*jdoy+S5_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S5_swir1_INTP[i]+S5_swir1_SLP[i]*jdoy+S5_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S5_swir2_INTP[i]+S5_swir2_SLP[i]*jdoy+S5_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S5_therm_INTP[i]+S5_therm_SLP[i]*jdoy+S5_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S5_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S5_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S5_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S5_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S5_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        } else if (jdoy >= S6_tStart[i] && jdoy <= S6_tEnd[i])
        {
            blue_band[i] =S6_blue_INTP[i]+S6_blue_SLP[i]*jdoy+S6_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S6_green_INTP[i]+S6_green_SLP[i]*jdoy+S6_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S6_red_INTP[i]+S6_red_SLP[i]*jdoy+S6_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S6_nir_INTP[i]+S6_nir_SLP[i]*jdoy+S6_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S6_swir1_INTP[i]+S6_swir1_SLP[i]*jdoy+S6_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S6_swir2_INTP[i]+S6_swir2_SLP[i]*jdoy+S6_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S6_therm_INTP[i]+S6_therm_SLP[i]*jdoy+S6_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S6_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S6_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S6_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S6_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S6_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        } else if (jdoy >= S7_tStart[i] && jdoy <= S7_tEnd[i])
        {
            blue_band[i] =S7_blue_INTP[i]+S7_blue_SLP[i]*jdoy+S7_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S7_green_INTP[i]+S7_green_SLP[i]*jdoy+S7_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S7_red_INTP[i]+S7_red_SLP[i]*jdoy+S7_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S7_nir_INTP[i]+S7_nir_SLP[i]*jdoy+S7_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S7_swir1_INTP[i]+S7_swir1_SLP[i]*jdoy+S7_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S7_swir2_INTP[i]+S7_swir2_SLP[i]*jdoy+S7_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S7_therm_INTP[i]+S7_therm_SLP[i]*jdoy+S7_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S7_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S7_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S7_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S7_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S7_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        } else if (jdoy >= S8_tStart[i] && jdoy <= S8_tEnd[i])
        {
            blue_band[i] =S8_blue_INTP[i]+S8_blue_SLP[i]*jdoy+S8_blue_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_blue_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_blue_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_blue_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_blue_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_blue_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            green_band[i] =S8_green_INTP[i]+S8_green_SLP[i]*jdoy+S8_green_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_green_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_green_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_green_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_green_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_green_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            red_band[i] =S8_red_INTP[i]+S8_red_SLP[i]*jdoy+S8_red_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_red_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_red_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_red_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_red_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_red_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            nir_band[i] =S8_nir_INTP[i]+S8_nir_SLP[i]*jdoy+S8_nir_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_nir_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_nir_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_nir_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_nir_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_nir_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir1_band[i] =S8_swir1_INTP[i]+S8_swir1_SLP[i]*jdoy+S8_swir1_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_swir1_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_swir1_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_swir1_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_swir1_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_swir1_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            swir2_band[i] =S8_swir2_INTP[i]+S8_swir2_SLP[i]*jdoy+S8_swir2_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_swir2_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_swir2_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_swir2_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_swir2_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_swir2_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
            therm_band[i] =S8_therm_INTP[i]+S8_therm_SLP[i]*jdoy+S8_therm_COS[i]*cos((2.0*pi*jdoy)/365.25)+S8_therm_SIN[i]*sin((2.0*pi*jdoy)/365.25)+S8_therm_COS2[i]*cos((4.0*pi*jdoy)/365.25)+S8_therm_SIN2[i]*sin((4.0*pi*jdoy)/365.25)+S8_therm_COS3[i]*cos((6.0*pi*jdoy)/365.25)+S8_therm_SIN3[i]*sin((6.0*pi*jdoy)/365.25);
        }
    }

    DataFrame out = DataFrame::create(Named("x")=ccdc_df["x"],
                                      Named("y")=ccdc_df["y"],
                                      Named("blue")=blue_band,
                                      Named("green")=green_band,
                                      Named("red")=red_band,
                                      Named("nir")=nir_band,
                                      Named("swir1")=swir1_band,
                                      Named("swir2")=swir2_band,
                                      Named("therm")=therm_band);

    return out;

}')  
  
  img<-ccdc_synthetic_img(ccdc_img,jdoy)
  
  img<-terra::rast(img,type='xyz')
  
  terra::crs(img)<-orig_crs
  
  return(img)
}


#' Name missing CCDC image bands
#'
#' Uses know naming convention to name the bands of a CCDC image exported from Google Earth Engine script in the case that they are missing.
#' Typically only needed for VRTs and CCDC exports of >5 segments.  
#' @param ccdc_img (SpatRaster, stars) The CCDC image for which to name bands, can be NULL if names_only is True
#' @param n_seg (integer) The number of CCDC change segments exported by the Google Earth Engine script (must match ccdc_img)
#' @param names_only (boolean) Indicates if the renamed image should be returned, or just a char vector of band names (default False) 
#' @return (SpatRaster) The same CCDC image used as input with missing band names correctly named.   
#' @export
name_ccdc_bands<-function(ccdc_img=NULL,n_seg,names_only=F)
{
  band_names<-c()
  
  index<-1
  for(band in c('blue','green','red','nir','swir1','swir2','therm'))
  {
    for(seg in c(1:n_seg))
    {
      for(coef in c('INTP','SLP','COS','SIN','COS2','SIN2','COS3','SIN3'))
      {
        band_names[index]<-paste0("S",seg,"_",band,"_coef_",coef)
        index<-index+1
      }
    }
  }
  
  for(band in c('blue','green','red','nir','swir1','swir2','therm'))
  {
    for(coef in c('RMSE','MAG'))
    {
      for(seg in c(1:n_seg))
      {
        band_names[index]<-paste0("S",seg,"_",band,"_",coef)
        index<-index+1
      }
    }
  }
  
  
  
  for(coef in c('tStart','tEnd','tBreak','changeProb','numObs'))
  {
    for(seg in c(1:n_seg))
    {
      band_names[index]<-paste0("S",seg,"_",coef)
      index<-index+1
    }
  }
  if(names_only==F)
  {
    names(ccdc_img)<-band_names
    return(ccdc_img)
  }else{
    return(band_names)
  }
}


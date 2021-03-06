% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ccdc_ts.R
\name{plot_ccdc_by_catg}
\alias{plot_ccdc_by_catg}
\title{Function for plotting surface reflectance curves by category (i.e., output of 'sample_ccdc_by_catg').}
\usage{
plot_ccdc_by_catg(
  ccdc_img_at_pnts,
  ccdc_img_date,
  catg_field,
  band,
  days = 730,
  med_only = F,
  ylim = c(0, 1)
)
}
\arguments{
\item{ccdc_img_at_pnts}{(data.frame) A data.frame containing the categories of interest, and associated CCDC coefficients for a given date (i.e., output of 'sample_ccdc_by_catg')}

\item{ccdc_img_date}{(character) A representation of the CCDC image date that can be interpreted by as.Date()}

\item{catg_field}{(character) Name of the column indicating the categories of interest.}

\item{band}{(character) The name of the band for which to forecast CCDC surface reflectance time series. One of 'blue', 'green', 'red'
'nir', 'swir1','swir2' or 'therm'.}

\item{days}{(integer) Number of days to predict surface reflectance from the CCDC image data ('ccdc_img_date').}

\item{med_only}{(boolean) When true, only median surface reflectance values are plotted by category}

\item{ylim}{(numeric) Vector of length 2 defining the Y-Axis limits, defaults to c(0,1)}
}
\value{
(List) A data.frame with all data used for plot generation ('srdata'), a 'ggplot' object ('srplot'), and a data.frame with the legend codes cross-walk ('crosswalk').
}
\description{
Function for plotting surface reflectance curves by category (i.e., output of 'sample_ccdc_by_catg').
}

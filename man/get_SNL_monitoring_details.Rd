% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_SNL_monitoring_details}
\alias{get_SNL_monitoring_details}
\title{Returns a dataframe with monitoring species requirements per SNL-beheertype}
\usage{
get_SNL_monitoring_details(beheertype = "all")
}
\arguments{
\item{beheertype}{character with SNL-beheertype code,'all' returns all beheertypen. See https://www.bij12.nl/onderwerp/natuursubsidies/index-natuur-en-landschap/natuurtypen/ for details.}
}
\description{
Returns a dataframe with monitoring species requirements per SNL-beheertype
}
\examples{
library(VHRscope)

get_SNL_monitoring_details()

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fao_penman_monteith.R
\name{get_daylight_hours}
\alias{get_daylight_hours}
\title{Get daylight hours (Eq. 34)}
\usage{
get_daylight_hours(lat, date)
}
\arguments{
\item{lat}{latitude in decimal degrees}

\item{date}{date object or numeric jday}
}
\description{
Get daylight hours (Eq. 34)
}
\details{
Return number of daylight hours. Evaluates the equation:

N = 24 / pi * w_s
}
\examples{
lat <- 13.1
date <- "2019-04-15"
get_daylight_hours(lat, date)
}

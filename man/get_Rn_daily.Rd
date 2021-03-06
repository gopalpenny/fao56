% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fao_penman_monteith.R
\name{get_Rn_daily}
\alias{get_Rn_daily}
\title{Get net radiation (Rn)}
\usage{
get_Rn_daily(
  lat,
  date,
  Tmax_C,
  Tmin_C,
  ea,
  n,
  N,
  albedo = 0.23,
  z,
  as = 0.25,
  bs = 0.5
)
}
\arguments{
\item{lat}{latitude in decimal degrees}

\item{date}{date object or numeric jday}

\item{Tmax_C}{maximum absolute temperature during the 24-hour period, deg C}

\item{Tmin_C, }{minimum absolute temperature during the 24-hour period, deg C}

\item{ea}{actual vapor pressure, kPa}

\item{n}{actual duration of sunshine, hour}

\item{N}{maximum possible duration of sunshine or daylight hours, hour}

\item{albedo}{albedo or canopy reflectance, unitless}

\item{z}{elevation above sea level, m}

\item{as}{regression constant, expressing the fraction of extraterrestrial
radiation reaching the earth on overcast days (n = 0)}

\item{bs}{increase in solar radiation on clear days relative to overcast
days, ie, as + bs is the fraction of extraterrestrial radiation reaching
the earth on clear days (n = N).}
}
\description{
Get net radiation, Rnl (FAO 56, Eq. 40) in MJ/m^2/day
}
\details{
This function calculates net radiation, in MJ/m^2/day. It uses Eq.
  40 from FAO 56 in Chapter 3:

  Rn = Rns - Rnl

  In this function, Rns and Rnl are automatically calculated via the
  functions:

  \itemize{ \item{Ra <- get_Ra_daily(lat, date)} \item{Rs <- get_Rs_daily(Ra,
  n, N)} \item{Rso <- get_Rso_daily(Ra, z)} \item{Rns <- get_Rns_daily(Rs,
  albedo)} \item{Rnl <- get_Rnl_daily(Ra, Tmax_C, Tmin_C, ea, Rs, Rso)} }
}
\examples{
lat <- -22.9 # Rio de Janeiro
date <- "2019-05-15"
n <- 220 / 31 # 220 hours in a month / 31 days
N <- get_daylight_hours(lat, date)
Ra <- get_Ra_daily(lat, date)
Rso <- get_Rso_daily(Ra, z = 100)
Rs <- get_Rs_daily(Ra, n, N)
Tmax_C <- 25.1
Tmin_C <- 19.1
ea <- 2.1
Rn <- get_Rn_daily(lat, date, Tmax_C, Tmin_C, ea, n, N, z = 100)
}

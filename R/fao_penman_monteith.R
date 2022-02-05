# # FAO_ET
#
# library(tidyverse)

#' Read ET0 CSV
#' @param et0_path Path to ET0 csv file
#' @export
#' @examples
#' # Locate the path of the example et0 csv file
#' et0_path <- system.file("extdata", "ET0_example_file.csv", package = "fao56")
#'
#' # Read the file
#' et0 <- read_et0_csv(et0_path)
read_et0_csv <- function(et0_path) {
  et0_vars <- readr::read_csv(et0_path, skip = 1, n_max = 1, col_names = FALSE) %>%
    as.character() %>% ggsub("\\.","") %>% ggsub("Prc","precip") %>%
    ggsub(" ","_") %>%
    ggsub("[\\(\\)]","")
  et0_units <- readr::read_csv(et0_path, skip = 2, n_max = 1, col_names = FALSE) %>%
    as.character() %>%
    ggsub("\\%","pct") %>%
    ggsub(".C$","degC") %>%
    ggsub("\\/","_per_") %>%
    ggsub(" ","_")
  et0_colnames <- paste(et0_vars, et0_units, sep = "_")
  et0_data <- readr::read_csv(et0_path, skip = 3, col_names = et0_colnames)

  return(et0_data)
}


#' Extraterrestrial radiation for daily periods, Ra (MJ/m^2/day)
#'
#' Get daily extraterrestrial radiation (FAO 56, Ch 3, Eq. 21)
#' @param lat latitude in decimal degrees
#' @param date date object or numeric jday
#' @export
#' @details
#' Returns daily extraterrestrial radiation in units of MJ/m^2/day
#' per FAO 56, Ch 3, Eq. 21. Evaluates the equation
#'
#' Ra <- 24 * 60 / pi * Gsc * d_r *
#'   (w_s * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(w_s))
#'
#' To do this, it evaluates internal functions to obtain
#' Gsc solar constant 0.0820, MJ/m^2/day
#' d_r inverse relative distance Earth-Sun (Equation 23)
#' w_s sunset hour angle (Equation 25 or 26), rad,
#' phi latitude (Equation 22), rad
#' delta solar decimation (Equation 24), rad.
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' get_Ra_daily(lat, date)
get_Ra_daily <- function(lat, date) {#}, Gsc = 0.0820, d_r = NULL, w_s = NULL, phi = NULL, delta = NULL) {

  if (is.character(date)) {
    date <- as.Date(date)
    jday <- as.numeric(strftime(date, "%j"))
  } else if (assertthat::is.date(date)) {
    jday <- as.numeric(strftime(date, "%j"))
  } else {
    jday <- date
  }
  Gsc <- 0.0820 # solar constant
  phi <- get_phi_lat(lat)
  d_r <- get_d_r(jday)
  delta <- get_delta(jday)
  w_s <- get_w_s_angle(lat, date)

  Ra <- 24 * 60 / pi * Gsc * d_r *
    (w_s * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(w_s))
  return(Ra)
}

#' Get latitude in rad from decimal degrees (Eq. 22)
#' @param lat latitude in decimal degrees
#' @details
#' Return latitude in rad. Evaluates the equation
#'
#' lat * pi / 180
#' @examples
#' phi <- get_phi_lat(13.1)
get_phi_lat <- function(lat) {
  return(lat * pi / 180)
}

#' Get inverse relative distance Earth-Sun (Eq 23)
#' @param date date object or numeric jday
#' @details
#' Return inverse relative distance. Evaluates the equation:
#'
#' d_r = 1 + 0.033 * cos(2 * pi/ 365 * jday)
#' @examples
#' get_d_r("2019-04-15")
#' get_d_r(105, "%j")
get_d_r <- function(date) {
  if (is.character(date)) {
    date <- as.Date(date)
    jday <- as.numeric(strftime(date, "%j"))
  } else if (assertthat::is.date(date)) {
    jday <- as.numeric(strftime(date, "%j"))
  } else {
    jday <- date
  }
  return(1 + 0.033 * cos(2 * pi/ 365 * jday))
}

#' Get solar decimation (Eq 24)
#' @param date date object or numeric jday
#' @details
#' Return solar decimation in rad. Evaluates the equation:
#'
#' delta = 0.409 * sin(2*pi/365 * jday - 1.39)
#' @examples
#' get_delta("2019-04-15")
#' get_delta(105, "%j")
get_delta <- function(date) {
  if (is.character(date)) {
    date <- as.Date(date)
    jday <- as.numeric(strftime(date, "%j"))
  } else if (assertthat::is.date(date)) {
    jday <- as.numeric(strftime(date, "%j"))
  } else {
    jday <- date
  }
  return(0.409 * sin(2*pi/365 * jday - 1.39))
}


#' Get sunset hour angle (Eq 25)
#' @param phi latitude (Equation 22), rad
#' @param delta solar decimation (Equation 24), rad.
#' @details
#' Return solar decimation in rad. Evaluates the equation:
#'
#' w_s = acos(-tan(phi) * tan(delta))
#' @examples
#' lat <- 13.1
#' date <- "2019-04-15"
#' w_s <- get_w_s_angle(lat, date)
get_w_s_angle <- function(lat,date) {
  delta <- get_delta(date)
  phi <- get_phi_lat(lat)
  w_s <- acos(-tan(phi) * tan(delta))
  return(w_s)
}

#' Get daylight hours (Eq. 34)
#'
#' @param w_s sunset hour angle (Eq 25)
#' @export
#' @details
#' Return number of daylight hours. Evaluates the equation:
#'
#' N = 24 / pi * w_s
#' @examples
#' lat <- 13.1
#' date <- "2019-04-15"
#' get_daylight_hours(lat, date)
get_daylight_hours <- function(lat, date) {
  w_s <- get_w_s_angle(lat, date)
  N <- 24 / pi * w_s
  return(N)
}

#' Get solar radiation (Rs)
#'
#' Get solar radiation, Rs (FAO 56, Eq. 35) in MJ/m^2/day
#' @param Ra extraterrestrial radiation, MJ/m^2/day
#' @param n actual duration of sunshine, hour
#' @param N maximum possible duration of sunshine or daylight hours, hour
#' @param as regression constant, expressing the fraction of extraterrestrial
#'   radiation reaching the earth on overcast days (n = 0)
#' @param bs increase in solar radiation on clear days relative to overcast
#'   days, ie, as + bs is the fraction of extraterrestrial radiation reaching
#'   the earth on clear days (n = N).
#' @export
#' @details This function calculates solar or shortwave radiation, in
#'   MJ/m^2/day. It uses Eq. 35 from FAO 56 in Chapter 3:
#'
#'   Rs = (as + bs *  n / N) * Ra
#'
#'   Note that \code{n/N} is the relative sunshine duration (unitless). FAO56
#'   suggests using as = 0.25 and bs = 0.5 if observed values are not available.
#'   But better to use observations if possible.
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rs <- get_Rs_daily(Ra, n, N)
get_Rs_daily <- function(Ra, n, N, as = 0.25, bs = 0.5) {
  Rs <- (as + bs *  n / N) * Ra
  return(Rs)
}

#' Get net shortwave radiation (Rs)
#'
#' Get net shortwave radiation, Rs (FAO 56, Eq. 38) in MJ/m^2/day
#' @param Rs downward shortwave radiation, MJ/m^2/day
#' @param albedo albedo or canopy reflectance, unitless
#' @export
#' @details This function calculates net shortwave radiation, in MJ/m^2/day. It
#'   uses Eq. 38 from FAO 56 in Chapter 3:
#'
#'   Rns = (1 - albedo) * Rs
#'
#'   Note that for reference ET0, albedo can be taken as 0.23 for hypothetical
#'   grass reference crop.
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rs <- get_Rs_daily(Ra, n, N)
#' Rns <- get_Rns_daily(Rs, albedo = 0.23)
get_Rns_daily <- function(Rs, albedo = 0.23) {
  Rns <- (1 - albedo) * Rs
  return(Rns)
}

#' Get clear sky shortwave radiation (Rso)
#'
#' Get clear sky shortwave radiation, Rso (FAO 56, Eq. 36) in MJ/m^2/day
#' @param Ra downward shortwave radiation, MJ/m^2/day
#' @param z elevation above sea level, m
#' @details This function calculates clear-sky shortwave radiation, in MJ/m^2/day. It
#'   uses Eq. 37 from FAO 56 in Chapter 3:
#'
#'   Rso = (0.75 + 2e-5*z) * Ra
#'
#'   Note that different values for as and bs should be used if available and
#'   then a different equation (36) can be used.
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rso <- get_Rso_daily(Ra, z = 100)
get_Rso_daily <- function(Ra, z) {
  return((0.75 + 2e-5*z) * Ra)
}


#' Get net outgoing longwave radiation (Rnl)
#'
#' Get net outgoing longwave radiation, Rnl (FAO 56, Eq. 39) in MJ/m^2/day
#' @param Ra downward shortwave radiation, MJ/m^2/day
#' @param Tmax_K maximum absolute temperature during the 24-hour period, K = deg
#'   C + 273.16
#' @param Tmin_K, K minimum absolute temperature during the 24-hour period, K =
#'   deg C + 273.16
#' @param ea actual vapour pressure, kPa
#' @param Rs measured or calculated. (Equation 35) solar radiation [MJ m-2
#'   day-1],
#' @param Rso calculated (Equation 36 or 37) clear-sky radiation [MJ m-2 day-1].
#' @export
#' @details This function calculates net outgoing longwave radiation, in
#'   MJ/m^2/day. It uses Eq. 39 from FAO 56 in Chapter 3:
#'
#'   Rnl = sigma * (Tmax_K + Tmin_K)/2 * (0.34 - 0.14 * sqrt(e_a)) * (1.35 *
#'   Rs/Rso - 0.35)
#'
#'
#'   sigma is the Stefan-Boltzmann constant, 4.903e-9 MJ/K^4/m^2/day,
#'   Rs/Rso relative shortwave radiation (limited to <= 1.0).
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rso <- get_Rso_daily(Ra, z = 100)
#' Rs <- get_Rs_daily(Ra, n, N)
#' Tmax_K <- 25.1+273.16
#' Tmin_K <- 19.1+273.16
#' ea <- 2.1
#' Rnl <- get_Rnl_daily(Ra, Tmax_K, Tmin_K, ea, Rs, Rso)
get_Rnl_daily <- function(Ra, Tmax_K, Tmin_K, ea, Rs, Rso) {
  sigma <- 4.903e-9
  Rnl <- sigma * (Tmax_K^4 + Tmin_K^4)/2 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * Rs/Rso - 0.35)
  return(Rnl)
}



#' Get net radiation (Rn)
#'
#' Get net radiation, Rnl (FAO 56, Eq. 40) in MJ/m^2/day
#' @param lat latitude in decimal degrees
#' @param date date object or numeric jday
#' @param Tmax_K maximum absolute temperature during the 24-hour period, K = deg
#'   C + 273.16
#' @param Tmin_K, K minimum absolute temperature during the 24-hour period, K =
#'   deg C + 273.16
#' @param ea actual vapour pressure, kPa
#' @param n actual duration of sunshine, hour
#' @param N maximum possible duration of sunshine or daylight hours, hour
#' @param as regression constant, expressing the fraction of extraterrestrial
#'   radiation reaching the earth on overcast days (n = 0)
#' @param bs increase in solar radiation on clear days relative to overcast
#'   days, ie, as + bs is the fraction of extraterrestrial radiation reaching
#'   the earth on clear days (n = N).
#' @param albedo albedo or canopy reflectance, unitless
#' @param z elevation above sea level, m
#' @export
#' @details This function calculates net radiation, in MJ/m^2/day. It uses Eq.
#'   40 from FAO 56 in Chapter 3:
#'
#'   Rn = Rns - Rnl
#'
#'   In this function, Rns and Rnl are automatically calculated via the
#'   functions:
#'
#'   \itemize{ \item{Ra <- get_Ra_daily(lat, date)} \item{Rs <- get_Rs_daily(Ra,
#'   n, N)} \item{Rso <- get_Rso_daily(Ra, z)} \item{Rns <- get_Rns_daily(Rs,
#'   albedo)} \item{Rnl <- get_Rnl_daily(Ra, Tmax_K, Tmin_K, ea, Rs, Rso)} }
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rso <- get_Rso_daily(Ra, z = 100)
#' Rs <- get_Rs_daily(Ra, n, N)
#' Tmax_K <- 25.1+273.16
#' Tmin_K <- 19.1+273.16
#' ea <- 2.1
#' Rn <- get_Rn_daily(lat, date, Tmax_K, Tmin_K, ea, n, N, z = 100)
get_Rn_daily <- function(lat, date, Tmax_K, Tmin_K, ea, n, N, albedo = 0.23, z, as = 0.25, bs = 0.5) {

  Ra <- get_Ra_daily(lat, date)
  Rs <- get_Rs_daily(Ra, n, N)
  Rso <- get_Rso_daily(Ra, z)
  Rns <- get_Rns_daily(Rs, albedo)
  Rnl <- get_Rnl_daily(Ra, Tmax_K, Tmin_K, ea, Rs, Rso)

  Rn <- Rns - Rnl
  return(Rn)
}



#
# a <- 0.611
# b <- 17.502
# c1 <- 240.97
# Temp_C <- 25
#
# s <- a * b * c1 / (Temp_C + c1)^2 * exp(b * Temp_C / (Temp_C + c1))
# s
#
# RH <- 0.5
# R_N <-
#
#   e_s_T <- a * exp(b * Temp_C / (Temp_C + c1))
# e_s_T
# e_a <- RH * e_s_T
#
# et0_data %>% rename(R_N = )
#
# ET0_mm_day <- 0.408 * s * (R_N - G) + gamma * 900 / (Temp_C + 273) * u2 * (e_s_T - e_a)
# ET0_mm_day
#
# get_G_from_monthly_T <- function(T_month_iplus1, T_month_iminus1) {
#   G <- 0.07 * (T_month_iplus1 - T_month_iminus1) # eq 42 FAO 56, Ch 3
# }
#
# get_Rn_from

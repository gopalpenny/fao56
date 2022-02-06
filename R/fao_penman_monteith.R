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
#' @keywords internal
#' @details
#' Return latitude in rad. Evaluates the equation
#'
#' lat * pi / 180
#' @examples
#' \dontrun{
#' phi <- get_phi_lat(13.1)
#' }
get_phi_lat <- function(lat) {
  return(lat * pi / 180)
}

#' Get inverse relative distance Earth-Sun (Eq 23)
#' @param date date object or numeric jday
#' @keywords internal
#' @details
#' Return inverse relative distance. Evaluates the equation:
#'
#' d_r = 1 + 0.033 * cos(2 * pi/ 365 * jday)
#' @examples
#' \dontrun{
#' get_d_r("2019-04-15")
#' get_d_r(105, "%j")
#' }
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
#' @keywords internal
#' @details
#' Return solar decimation in rad. Evaluates the equation:
#'
#' delta = 0.409 * sin(2*pi/365 * jday - 1.39)
#' @examples
#' \dontrun{
#' get_delta("2019-04-15")
#' get_delta(105, "%j")
#' }
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
#' @param lat latitude in decimal degrees
#' @param date date object or numeric jday
#' @keywords internal
#' @details
#' Return solar decimation in rad. Evaluates the equation:
#'
#' w_s = acos(-tan(phi) * tan(delta))
#' @examples
#' \dontrun{
#' lat <- 13.1
#' date <- "2019-04-15"
#' w_s <- get_w_s_angle(lat, date)
#' }
get_w_s_angle <- function(lat,date) {
  delta <- get_delta(date)
  phi <- get_phi_lat(lat)
  w_s <- acos(-tan(phi) * tan(delta))
  return(w_s)
}

#' Get daylight hours (Eq. 34)
#'
#' @param lat latitude in decimal degrees
#' @param date date object or numeric jday
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
#' @export
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
#' @param Tmax_C maximum absolute temperature during the 24-hour period
#' @param Tmin_C minimum absolute temperature during the 24-hour period
#' @param ea actual vapor pressure, kPa
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
#'   Tmax_K is Tmax_C + 273.16
#'   Tmin_K is Tmin_C + 273.16
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
#' Tmax_C <- 25.1
#' Tmin_C <- 19.1
#' ea <- 2.1
#' Rnl <- get_Rnl_daily(Ra, Tmax_C, Tmin_C, ea, Rs, Rso)
get_Rnl_daily <- function(Ra, Tmax_C, Tmin_C, ea, Rs, Rso) {
  Tmax_K <- Tmax_C + 273.16
  Tmin_K <- Tmin_C + 273.16
  if (any(Tmax_K < Tmin_K)) {
    stop("Tmax_C is less than Tmin_C. Check the order of inputs.")
  }

  if (any(Rso < Rs)) { # Rs/Rso has an upper bound of 1
    # warning("Rs/Rso is greater than 1. Fixing Rs/Rso to 1")
    Rso <- pmax(Rs, Rso)
  }
  sigma <- 4.903e-9
  Rnl <- sigma * (Tmax_K^4 + Tmin_K^4)/2 * (0.34 - 0.14 * sqrt(ea)) * (1.35 * Rs/Rso - 0.35)
  return(Rnl)
}



#' Get net radiation (Rn)
#'
#' Get net radiation, Rnl (FAO 56, Eq. 40) in MJ/m^2/day
#' @param lat latitude in decimal degrees
#' @param date date object or numeric jday
#' @param Tmax_C maximum absolute temperature during the 24-hour period, deg C
#' @param Tmin_C, minimum absolute temperature during the 24-hour period, deg C
#' @param ea actual vapor pressure, kPa
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
#'   albedo)} \item{Rnl <- get_Rnl_daily(Ra, Tmax_C, Tmin_C, ea, Rs, Rso)} }
#' @examples
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rso <- get_Rso_daily(Ra, z = 100)
#' Rs <- get_Rs_daily(Ra, n, N)
#' Tmax_C <- 25.1
#' Tmin_C <- 19.1
#' ea <- 2.1
#' Rn <- get_Rn_daily(lat, date, Tmax_C, Tmin_C, ea, n, N, z = 100)
get_Rn_daily <- function(lat, date, Tmax_C, Tmin_C, ea, n, N, albedo = 0.23, z, as = 0.25, bs = 0.5) {

  Tmax_K <- Tmax_C + 273.16
  Tmin_K <- Tmin_C + 273.16
  if (any(Tmax_K < Tmin_K)) {
    stop("Tmax_C is less than Tmin_C. Check the order of inputs.")
  }

  if (any(n > N)) {
    stop("n > N. This cannot be the case. Check values.")
  }

  Ra <- get_Ra_daily(lat, date)
  Rs <- get_Rs_daily(Ra, n, N)
  Rso <- get_Rso_daily(Ra, z)
  Rns <- get_Rns_daily(Rs, albedo)
  Rnl <- get_Rnl_daily(Ra, Tmax_C, Tmin_C, ea, Rs, Rso)

  Rn <- Rns - Rnl
  return(Rn)
}

#' Calculate the psychrometric constant
#'
#' @param z elevation in m. Takes precedence over P_kPa
#' @param P_kPa Pressure in kPa
#' @export
#' @details
#' Calculates the psychrometric constant as
#'
#' gamma = cp * P / (M_water_air * l), org
#'
#' if P_kPa is given, it takes precedence over z. If z is used, pressure is calculated
#' according to get_P_atmosphere().
#'
#' gamma = 0.663e-3 P
#'
#' gamma psychrometric constant, kPa / deg C,
#' P atmospheric pressure, kPa
#' l latent heat of vaporization, 2.45 MJ / kg,
#' cp specific heat at constant pressure, 1.013e-3, MJ / kg / deg C
#' M_water_air ratio molecular weight of water vapor/dry air = 0.622.
#' @examples
#' get_psychrometric_constant(P = 101.325)
#' get_psychrometric_constant(z = 231)
#' get_psychrometric_constant(P = 98.59907)
get_psychrometric_constant <- function(z = 0, P_kPa = NULL) {
  if (is.null(P_kPa)) {
    P_kPa <- get_P_atmosphere(z)
  }
  gamma <- 0.665e-3*P_kPa
  return(gamma)
}

#' Calculate the saturation vapor pressure in kPa
#'
#' @param T_C Temperature in deg C
#' @export
#' @details
#' Calculates the saturation vapor pressure in kPa as a function of temperature (deg C) as
#'
#' es = a * exp(b * T_C / (T_C + c1)), where
#'
#' a = 0.6108
#' b = 17.27
#' c = 237.3
#'
#' @examples
#' get_es(24.5)
get_es <- function(T_C) {
  a <- 0.6108
  b <- 17.27
  c1 <- 237.3
  es <- a * exp(b * T_C / (T_C + c1))
  return(es)
}

#' Calculate the slope of the saturation vapor pressure curve
#' culate the slope of the saturation vapor pressure curve in kPa / deg C
#' @param T_C Temperature in deg C
#' @export
#' @details
#' Calculates the saturation vapor pressure in kPa / deg C as a function of temperature (deg C) as
#'
#' s = a * b * c / (T_C + c)^2 * exp(b * T_C / (T_C + c)), where
#'
#' a = 0.6108
#' b = 17.27
#' c = 237.3
#'
#' @examples
#' get_es_slope(24.5)
get_es_slope <- function(T_C) {
  a <- 0.6108
  b <- 17.27
  c1 <- 237.3
  s <- a * b * c1 / (T_C + c1)^2 * exp(b * T_C / (T_C + c1))
  return(s)
}

#' Get the ground heat flux in
#'
#' @param T_month_iminus1 air temperature at month i-1, deg C
#' @param T_month_iplus1 air temperature at month i-1, deg C
#' @param T_month_i air temperature at month i, deg C
#' @export
#' @details
#'
#' Must specify \code{T_month_iminus1} and either \code{T_month_iplus1} or
#' \code{T_month_i}. \code{T_month_iplus1} is given preference. The calculation
#' is then either
#'
#' G = 0.07 * (T_month_iplus1 - T_month_iminus1), eq 43 FAO 56, Ch 3, or
#'
#' G = 0.14 * (T_month_i - T_month_iminus1) # eq 44 FAO 56, Ch 3
#'
#' G soil heat flux, MJ / m^2/ day T_month_iplus1 air temperature at month i+1,
#' deg C T_month_i air temperature at month i+1, deg C T_month_iminus1 air
#' temperature at month i-1, deg C
#' @examples
#' get_G_from_monthly_T(15, 18)
#' get_G_from_monthly_T(15, T_month_i = 18)
get_G_from_monthly_T <- function(T_month_iminus1, T_month_iplus1 = NULL, T_month_i = NULL) {
  if (!is.null(T_month_iplus1)) {
    G <- 0.07 * (T_month_iplus1 - T_month_iminus1) # eq 43 FAO 56, Ch 3
  } else if (!is.null(T_month_i)) {
    G <- 0.14 * (T_month_i - T_month_iminus1) # eq 44 FAO 56, Ch 3
  } else {
    stop("Must specify either T_month_iplus1 or T_month_i")
  }
  return(G)
}

#' Get vapor pressure from RHmean and temperature
#'
#' @param RHmean Mean relative humidity, pct (0-100)
#' @param Tmax_C Max daily temperature, deg C
#' @param Tmin_C Min daily temperature, deg C
#' @export
#' @details Calculate the actual vapor pressure from mean relative humidity and
#'   max and min daily temperature. The calculation is as:
#'
#'   ea = RHmean / 100 * (es(Tmax_C) + es(Tmin_C)) / 2 (FAO 56 Eq. 19)
#'
#'   es(T) is the saturation vapor pressure at that temperature. The function
#'   \code{get_es(T_C)} is used for saturation vapor pressure.
#' @examples
#' get_ea_from_RHmean(RHmean = 68, Tmax_C = 25, Tmin_C = 18)
get_ea_from_RHmean <- function(RHmean, Tmax_C, Tmin_C) {
  if (any(Tmax_C < Tmin_C)) {
    stop("Tmax_C is greater than Tmin_C Check the order of inputs.")
  }
  ea <- RHmean / 100 * (get_es(Tmax_C) + get_es(Tmin_C)) / 2
  return(ea)
}

#' Get atmospheric pressure at elevation
#'
#' @param z elevation, masl
#' @details Get atmospheric pressure, in kPa, at elevation, in m
#' @examples
#' \dontrun{
#' get_P_atmosphere(0)
#' }
get_P_atmosphere <- function(z) {
  P_kPa <- 101.3*((293 - 0.0065 * z)/293)^5.26
  return(P_kPa)
}


#' FAO Penman Monteith reference ETo
#'
#' @param Rn net radiation at the crop surface, MJ / m^2 / day
#' @param G soil heat flux density, MJ / m^2 / day
#' @param T_C mean daily air temperature at 2 m height, deg C
#' @param u2 wind speed at 2 m height, m/s
#' @param es saturation vapor pressure, kPa
#' @param ea actual vapor pressure, kPa
#' @param s slope vapor pressure curve, kPa / deg C
#' @param gamma psychrometric constant, kPa / deg C
#' @export
#' @details
#' This function calculates ETo reference evapotranspiration in mm / day using the
#' FAO Penman-Monteith equation for ETo (FAO 56, Chapter 2, Eq 6):
#'
#' ETo_mm_day = (0.408 * s * (Rn - G) + gamma * 900 / (T_C + 273) * u2 * (es - ea)) / (s + gamma * (1 + 0.34 * u2))
#'
#' Note that the equation requires specific units for each variable, as noted above.
#'
#' es - ea is the saturation vapor pressure deficit, kPa
#' @examples
#' ## Example 1: Single value data
#' lat <- -22.9 # Rio de Janeiro
#' date <- "2019-05-15"
#' n <- 220 / 31 # 220 hours in a month / 31 days
#' N <- get_daylight_hours(lat, date)
#' Ra <- get_Ra_daily(lat, date)
#' Rso <- get_Rso_daily(Ra, z = 100)
#' Rs <- get_Rs_daily(Ra, n, N)
#' Tmax_C <- 25.1
#' Tmin_C <- 19.1
#' ea <- get_ea_from_RHmean(RHmean = 68, Tmax_C = Tmax_C, Tmin_C = Tmin_C)
#' Rn <- get_Rn_daily(lat, date, Tmax_C = Tmax_C, Tmin_C = Tmin_C, ea, n, N, z = 0)
#'
#' es_Tmin <- get_es(Tmin_C)
#' es_Tmax <- get_es(Tmax_C)
#' Tmean <- mean(c(Tmin_C, Tmax_C))
#' es <- mean(c(es_Tmin, es_Tmax))
#' G <- 0
#' gamma <- get_psychrometric_constant()
#' ETo <- fao_penman_monteith(Rn, G, gamma = get_psychrometric_constant(), T_C = Tmean, u2 = 2.2, es = es, ea = ea)
#'
#' ## Example 2: Using FAO climate data
#' # Locate and read the example et0 csv file
#' et0_path <- system.file("extdata", "ET0_example_file.csv", package = "fao56")
#' clim_prep <- read_et0_csv(et0_path)
#'
#' library(dplyr)
#' clim_prep$lat <- 25
#' clim_prep$month <- 1:12
#' clim_prep$date <- as.Date(paste("2019",clim_prep$month,"15",sep="-"))
#'
#' # Estimate vapor pressure
#'
#' clim_prep <- clim_prep %>%
#'   mutate(ea_kPa = get_ea_from_RHmean(Rel_Hum_pct,Tmp_max_degC, Tmp_min_degC),
#'          es_Tmin = get_es(Tmp_min_degC),
#'          es_Tmax = get_es(Tmp_max_degC),
#'          es_kPa = (es_Tmin + es_Tmax)/2)
#'
#'
#' # Estimate G
#' # calculating G requires getting temperature for the previous and subequent months
#' # to do this, add Jan to the end (month = 13) and Dec to the beginning (month = 0)
#' # then remove these months after the calculation
#' clim_prep <- clim_prep %>%
#'   bind_rows(clim_prep %>% filter(month == 1) %>% mutate(month = 13))%>%
#'   bind_rows(clim_prep %>% filter(month == 12) %>% mutate(month = 0))%>%
#'   arrange(month) %>%
#'   mutate(T_iminus1 = lag(Tmp_Mean_degC),
#'          T_iplus1 = lead(Tmp_Mean_degC),
#'          G_MJ_per_day = get_G_from_monthly_T(T_month_iminus1 = T_iminus1, T_month_iplus1 = T_iplus1)) %>%
#'   filter(month %in% 1:12)
#'
#' # Estimate Rn
#' clim_prep <- clim_prep %>%
#'   mutate(date = as.Date(paste("2019",month,"15",sep="-")),
#'          N = get_daylight_hours(lat, date),
#'          n = Sun_shine_pct * N / 100,
#'          Rn_MJ_per_day = get_Rn_daily(lat, date, Tmp_max_degC, Tmp_min_degC, ea, n, N, albedo = 0.23, z = 251))
#'
#' gamma <- get_psychrometric_constant()
#'
#' # Calculate ETo from data
#' clim <- clim_prep %>%
#'   select(Rn_MJ_per_day, G_MJ_per_day, Tmean_C = Tmp_Mean_degC, u2_m_per_s = Wind_2m_m_per_s, es_kPa, ea_kPa) %>%
#'   mutate(ETo = fao_penman_monteith(Rn = Rn_MJ_per_day,
#'                                    G = G_MJ_per_day,
#'                                    gamma = gamma,
#'                                    T_C = Tmean_C,
#'                                    u2 = u2_m_per_s,
#'                                    es = es_kPa,
#'                                    ea = ea_kPa))
fao_penman_monteith <- function(Rn, G, gamma, T_C, u2, es, ea) {
  s <- get_es_slope(T_C)
  ETo_mm_day <- (0.408 * s * (Rn - G) + gamma * 900 / (T_C + 273) * u2 * (es - ea)) /
    (s + gamma * (1 + 0.34 * u2))
  return(ETo_mm_day)
}

#
# ET0_mm_day


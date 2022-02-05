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
    as.character() %>% ggsub("\\.","") %>% ggsub("Prc","precip")
  et0_units <- readr::read_csv(et0_path, skip = 2, n_max = 1, col_names = FALSE) %>%
    as.character() %>% ggsub("\\%","pct") %>% ggsub(".C$","degC") #ggsub("°","deg")
  et0_colnames <- paste(et0_vars, et0_units, sep = "_")
  et0_data <- readr::read_csv(et0_path, skip = 3, col_names = et0_colnames)

  return(et0_data)
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

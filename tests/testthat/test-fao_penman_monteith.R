


et0_tbl <- tibble::tibble(precip_mm_per_m=c(10, 12, 21, 14, 14, 175, 356, 338, 219, 48, 8, 13),
                          precip_mm_per_d=c(0, 0, 1, 0, 0, 6, 11, 11, 7, 2, 0, 0),
                          precip_cv_pct=c(181.4, 226.5, 163.9, 126.4, 164, 56.2, 36.3, 48, 53.2, 115.4, 265, 299.8),
                          Wet_Days_days=c(0.9, 1, 1.2, 1, 1.3, 8.5, 14.3, 13.3, 8.5, 2.5, 0.7, 0.9),
                          Tmp_min_degC=c(13.9, 16.3, 20.2, 25, 28, 26.7, 24.4, 23.8, 23.3, 20.5, 15.9, 12.8),
                          Tmp_max_degC=c(29.4, 32.6, 37, 40.9, 42.8, 37.9, 32, 30.7, 32.1, 32.9, 30.7, 28.6),
                          Tmp_Mean_degC=c(21.6, 24.4, 28.6, 32.9, 35.4, 32.3, 28.2, 27.2, 27.7, 26.7, 23.3, 20.7),
                          Grnd_Frost_days=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Rel_Hum_pct=c(53.8, 43.5, 30.9, 25.9, 28.9, 55.6, 76.5, 79.6, 74, 61.9, 55.5, 55.8),
                          Sun_shine_pct=c(80, 83, 75.9, 76, 72.3, 46.3, 29, 30.3, 48.6, 70.4, 78.1, 79.6),
                          Wind_2m_m_per_s=c(1.1, 1.4, 1.4, 1.6, 2.2, 2.4, 2.1, 1.9, 1.4, 1.2, 1.2, 1),
                          ETo_mm_per_m=c(102, 126, 173, 208, 260, 193, 129, 116, 123, 134, 112, 95),
                          ETo_mm_per_d=c(3.3, 4.5, 5.6, 6.9, 8.4, 6.4, 4.2, 3.7, 4.1, 4.3, 3.7, 3.1))

et0_path <- system.file("extdata", "ET0_example_file.csv", package = "fao56")
et0 <- read_et0_csv(et0_path)
test_that("read_et0_csv works", {
  # expect_equal(TRUE, TRUE)
  # expect_equal(file.exists("../../inst/ET0 - 20.59-78.96.csv"), TRUE)
  # expect_equal(any(grepl("tbl",class(et0))), TRUE)
  expect_equal(as.matrix(et0), as.matrix(et0_tbl))
})


lat <- -22.9 # Rio de Janeiro
date <- "2019-05-15"
n <- 220 / 31 # 220 hours in a month / 31 days
N <- get_daylight_hours(lat, date)
Tmax_C <- 25.1
Tmin_C <- 19.1
ea <- 2.1

Ra <- get_Ra_daily(lat, date)
Rs <- get_Rs_daily(Ra, n, N)
Rso <- get_Rso_daily(Ra, z = 0)
Rns <- get_Rns_daily(Rs, albedo = 0.23)
Rnl <- get_Rnl_daily(Ra, Tmax_C, Tmin_C, ea, Rs, Rso)
Rn <- get_Rn_daily(lat, date, Tmax_C, Tmin_C, ea, n, N, z = 0)

test_that("FAO Penman-Monteith works for daily Ra, Rs, Rso, Rns, Rnl, Rn", {
  expect_equal(round(Ra,5), 25.11103)
  expect_equal(round(Rs,5), 14.4561)
  expect_equal(round(Rso,5), 18.83327)
  expect_equal(round(Rns,5), 11.1312)
  expect_equal(round(Rnl,6), 3.508541)
  expect_equal(round(Rn,6), 7.622655)
  expect_equal(round(get_Rn_daily(lat, date, Tmax_C, Tmin_C, ea, n, N, z = 100),6), 7.636745)
})

test_that("Humidity calculations work for es, psychrometric constant", {
  expect_equal(round(get_es(24.5), 6), 3.074649)
  expect_equal(round(get_es(15), 6), 1.705346)
  expect_equal(round(get_es_slope(24.5), 7), 0.1838427)
  expect_equal(round(get_es_slope(15), 7), 0.1097914)
  expect_equal(round(get_ea_from_RHmean(68, 25, 18), 6), 1.778801)
  expect_equal(round(get_psychrometric_constant(P = 101.325), 8), 0.06738113)
  expect_equal(round(get_psychrometric_constant(P = 98.5), 8), 0.0655025)
})

test_that("Monthly ground heat flux works", {
  expect_equal(get_G_from_monthly_T(15, 18), 0.21)
  expect_equal(get_G_from_monthly_T(15, T_month_i = 18), 0.42)
})




## Example 1: Single value data
lat <- -22.9 # Rio de Janeiro
date <- "2019-05-15"
n <- 220 / 31 # 220 hours in a month / 31 days
N <- get_daylight_hours(lat, date)
Ra <- get_Ra_daily(lat, date)
Rso <- get_Rso_daily(Ra, z = 100)
Rs <- get_Rs_daily(Ra, n, N)
Tmax_C <- 25.1
Tmin_C <- 19.1
ea <- get_ea_from_RHmean(RHmean = 68, Tmax_C = Tmax_C, Tmin_C = Tmin_C)
Rn <- get_Rn_daily(lat, date, Tmax_C = Tmax_C, Tmin_C = Tmin_C, ea, n, N, z = 0)

es_Tmin <- get_es(Tmin_C)
es_Tmax <- get_es(Tmax_C)
Tmean <- mean(c(Tmin_C, Tmax_C))
es <- mean(c(es_Tmin, es_Tmax))
G <- 0
gamma <- get_psychrometric_constant()
ETo <- fao_penman_monteith(Rn, G, gamma = get_psychrometric_constant(), T_C = Tmean, u2 = 2.2, es = es, ea = ea)

test_that("fao_penman_monteith for reference ETo works for single values",{
  expect_equal(round(ETo,6),3.11643)
})

## Example 2: Using FAO climate data
# Locate and read the example et0 csv file
et0_path <- system.file("extdata", "ET0_example_file.csv", package = "fao56")
et0_file <- read_et0_csv(et0_path)
clim_prep <- et0_file

library(dplyr)
clim_prep$lat <- 20.59
clim_prep$month <- 1:12
clim_prep$date <- as.Date(paste("2019",clim_prep$month,"15",sep="-"))

# Estimate vapor pressure

clim_prep <- clim_prep %>%
  mutate(ea_kPa = get_ea_from_RHmean(Rel_Hum_pct,Tmp_max_degC, Tmp_min_degC),
         es_Tmin = get_es(Tmp_min_degC),
         es_Tmax = get_es(Tmp_max_degC),
         es_kPa = (es_Tmin + es_Tmax)/2)


# Estimate G
# calculating G requires getting temperature for the previous and subequent months
# to do this, add Jan to the end (month = 13) and Dec to the beginning (month = 0)
# then remove these months after the calculation
clim_prep <- clim_prep %>%
  bind_rows(clim_prep %>% filter(month == 1) %>% mutate(month = 13))%>%
  bind_rows(clim_prep %>% filter(month == 12) %>% mutate(month = 0))%>%
  arrange(month) %>%
  mutate(T_iminus1 = lag(Tmp_Mean_degC),
         T_iplus1 = lead(Tmp_Mean_degC),
         G_MJ_per_day = get_G_from_monthly_T(T_month_iminus1 = T_iminus1, T_month_iplus1 = T_iplus1)) %>%
  filter(month %in% 1:12)

# Estimate Rn
clim_prep <- clim_prep %>%
  mutate(date = as.Date(paste("2019",month,"15",sep="-")),
         N = get_daylight_hours(lat, date),
         n = Sun_shine_pct * N / 100,
         Rn_MJ_per_day = get_Rn_daily(lat, date, Tmp_max_degC, Tmp_min_degC, ea, n, N, albedo = 0.23, z = 0))

gamma <- get_psychrometric_constant(z = 231)

# Calculate ETo from data
clim <- clim_prep %>%
  select(Rn_MJ_per_day, G_MJ_per_day, Tmean_C = Tmp_Mean_degC, u2_m_per_s = Wind_2m_m_per_s, es_kPa, ea_kPa) %>%
  mutate(ETo = fao_penman_monteith(Rn = Rn_MJ_per_day,
                                   G = G_MJ_per_day,
                                   gamma = gamma,
                                   T_C = Tmean_C,
                                   u2 = u2_m_per_s,
                                   es = es_kPa,
                                   ea = ea_kPa))

clim_eval <- clim %>% bind_cols(et0_file %>% select(starts_with("ETo"))) %>%
  mutate(ETo_ratio = (ETo_mm_per_d - ETo)/ETo_mm_per_d)
clim_eval %>% summarize(mean_error = mean(abs(ETo_ratio))) %>% pull(mean_error)

# test_that("fao_penman_monteith for reference ETo works for data frames",{
#   expect_equal(round(ETo,6),3.116199)
# })





## Example 3: Chapter 4, Example 17 -- Bangkok, Thailand
lat <- 13 + 44/60 # Bangkok
Tmax_C <- 34.8
Tmin_C <- 25.6
ea <- 2.85
Tmean <- mean(c(Tmax_C, Tmin_C))
n <- 8.5 #h/day
Tmean_april <- 30.2
Tmean_march <- 29.2
z = 2
D <- es - ea
P = 101.3
gamma <- get_psychrometric_constant(z = 2)
s <- get_es_slope(Tmean)

date <- "2019-04-15"
N <- get_daylight_hours(lat, date)
# Ra <- get_Ra_daily(lat, date)
# Rso <- get_Rso_daily(Ra, z = 100)
# Rs <- get_Rs_daily(Ra, n, N)
# # ea <- get_ea_from_RHmean(RHmean = 68, Tmax_C = Tmax_C, Tmin_C = Tmin_C)
# Rnl <- get_Rnl_daily(Ra, Tmax_C, Tmin_C, ea, Rs, Rso)
Rn <- get_Rn_daily(lat, date, Tmax_C = Tmax_C, Tmin_C = Tmin_C, ea, n, N, z = 0)

es_Tmin <- get_es(Tmin_C)
es_Tmax <- get_es(Tmax_C)
es <- mean(c(es_Tmin, es_Tmax))
G <- get_G_from_monthly_T(T_month_iminus1 = Tmean_march, T_month_i = Tmean_april)

ETo <- fao_penman_monteith(Rn, G, gamma = get_psychrometric_constant(), T_C = Tmean, u2 = 2, es = es, ea = ea)

test_that("fao_penman_monteith for reference ETo works for single values",{
  expect_equal(round(ETo,6),5.716046)
})


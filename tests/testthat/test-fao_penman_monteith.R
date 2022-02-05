


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
w_s <- get_w_s_angle(lat, date)
N <- get_daylight_hours(w_s)
Tmax_K <- 25.1+273.16
Tmin_K <- 19.1+273.16
ea <- 2.1

Ra <- get_Ra_daily(lat, date)
Rs <- get_Rs_daily(Ra, n, N)
Rso <- get_Rso_daily(Ra, z = 100)
Rns <- get_Rns_daily(Rs, albedo)
Rnl <- get_Rnl_daily(Ra, Tmax_K, Tmin_K, ea, Rs, Rso)
Rn <- get_Rn_daily(lat, date, Tmax_K, Tmin_K, ea, n, N, z = 100)

test_that("FAO Penman-Monteith works for daily Ra, Rs, Rso, Rns, Rnl, Rn", {
  expect_equal(round(Ra,5), 25.11103)
  expect_equal(round(Rs,5), 14.4561)
  expect_equal(round(Rso,5), 18.88349)
  expect_equal(round(Rns,5), 11.1312)
  expect_equal(round(Rnl,5), 3.49445)
  expect_equal(round(Rn,6), 7.636745)
})

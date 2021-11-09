# test_fao_crop_tables.R


test_that("get_crop_tables returns consistent crop growing periods",{
  expect_equal(suppressMessages(get_crop_tables()$crop_stages),fao_crop_stages)
})

test_that("get_crop_tables returns consistent crop coefficients",{
  expect_equal(suppressMessages(get_crop_tables()$crop_coeff),fao_crop_coefficients)
})

## code to prepare `fao_crop_tables_data` dataset goes here

fao_tables <- get_crop_tables()
fao_crop_stages <- fao_tables$crop_stages
fao_crop_coefficients <- fao_tables$crop_coeff

usethis::use_data(fao_crop_stages,fao_crop_coefficients)

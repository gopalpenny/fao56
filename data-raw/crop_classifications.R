## code to prepare `crop_classifications` dataset goes here

crop_classification <- get_crop_classes()
fao_crop_classes <- crop_classification$crop_classes
fao_botanical_names <- crop_classification$botanical_names

usethis::use_data(fao_crop_classes, fao_botanical_names, overwrite = TRUE)

# fao_CLIMWAT.R

#' Read CLIMWAT data
#'
#' Import CLIMWAT data from .cli and .pen files. Note that the files must
#' first be extracted from CLIMWAT and saved as .cli and .pen files.
#' @param directory_path Path to directory containing CLIMWAT files
#' @param station_name Name of station to import (will import .cli and .pen files)
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' directory_path <- "fao56"
#' station_name <- "CLIMWAT_example"
#' read_CLIMWAT(directory_path,station_name)
#' }
read_CLIMWAT <- function(directory_path,station_name) {
  rainfall_effective <- rainfall <- ref_ET_cli <- ref_ET_pen <- rainfall_check <- ET_check <- NULL

  if (directory_path=="fao56" & station_name=="CLIMWAT_example") {
    pen_filepath <- system.file("extdata", "CLIMWAT_example.pen", package = "fao56")
    directory_path <- gsub(paste0("\\/","CLIMWAT_example.pen"),"",pen_filepath)
  }

  cli_path <- file.path(directory_path,paste0(station_name,".cli"))
  pen_path <- file.path(directory_path,paste0(station_name,".pen"))

  cli_cols <- c("Temp_C_daily_max","Temp_C_daily_min","rel_humidity","wind_km_day",
                "sunshine_hrs_day","solar_radiation_MJ_m2_day","ref_ET_cli")
  cli_meta_cols <- c("Nr","Name","Alt_m","Lat","x1","Lon","x2")
  pen_cols <- c("ref_ET_pen","x2","rainfall","x1","rainfall_effective")

  rainfall_df <- utils::read.fwf(cli_path,c(10,16,10),skip=1,col.names = pen_cols,header=FALSE) %>%
    dplyr::select(-dplyr::matches("x[12]"))
  rainfall_header <- readr::read_csv(cli_path,n_max = 1,col_names = FALSE)
  climate_df <- utils::read.fwf(pen_path,rep(10,7),skip=1,col.names = cli_cols)
  climate_meta <- readr::read_csv(pen_path,n_max = 1,col_names = cli_meta_cols)

  climate_prep <- dplyr::bind_cols(rainfall_df,climate_df)

  check_climate <- climate_prep %>%
    dplyr::mutate(rainfall_check=rainfall_effective<rainfall,
                  ET_check=ref_ET_cli==ref_ET_pen) %>%
    dplyr::summarize(rainfall_check=purrr::every(rainfall_check,isTRUE),
                     ET_check=purrr::every(ET_check,isTRUE)) %>%
    purrr::map_lgl(as.logical) #%>% min()
  if (min(check_climate)==0) {
    stop("One of the climate checks failed")
  }
  climate <- climate_prep %>% dplyr::select(-ref_ET_cli) %>% dplyr::rename(ref_ET=ref_ET_pen)
  return(list(climate=climate,meta=climate_meta))
}


# fao_aquastat_climate.R

#' Get Aquastat Climate
#'
#' This function takes you to the download pages for AQUASTAT CLIMATE
#' @param lat latitude of location
#' @param lon longitude of location
#' @export
#' @details This function opens a webpage to download data at the lat/lon specified
get_aquastat_climate <- function(lat, lon) {

  aq_url_ORIG <- "https://aquastat.fao.org/climate-information-tool/eto-calculator?lat=LATITUDE&lon=LONGITUDE"
  aq_url <- gsub("LATITUDE", lat, aq_url_ORIG)
  aq_url <- gsub("LONGITUDE", lon, aq_url)

  utils::browseURL(aq_url)

  # Old stuff trying to scrape data
  # This does not seem to be working. I don't fully know why but it's possible that the
  # table is generated dynamically and therefore must be accessed using Rselenium not
  # only rvest.
  # cat("Getting FAO Aquastat Climate data from\n",
  #     aq_url,"\n")
  # aq_html <- read_html(aq_url)
  #
  # table_xpath <- "/html/body/app-root/div/div[2]/app-et0-calculator/div/div[2]/div[1]/mat-card/app-result-area/div/div[2]/table"
  # aq_header <- aq_html %>%
  #   html_nodes(xpath = "/html/body/app-root/div") #%>% html_table(fill=TRUE)
  # aq_header
}

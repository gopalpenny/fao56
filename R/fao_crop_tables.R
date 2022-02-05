# fao_crop_tables.R

# # scraping tables: https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
# # scraping text: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
# ggsub <- function(x,pattern,replacement) {
#   return(gsub(pattern,replacement,x))
# }

#' Get FAO 56 Tables
#'
#' Get FAO Crop Coefficients and growing periods
#' @return A list containing two \code{data.frame} objects: \code{crop_stages} and \code{crop_coeff}.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' fao_tables <- get_crop_tables()
#' crop_stages <- fao_tables$crop_stages
#' crop_coeff <- fao_tables$crop_coeff
get_crop_tables <- function() {
  Crop <- Crop_name <- Region <- Crop_group <- Crop_subcat <- Total <- . <- Crop...1 <- Crop...2 <- NULL


  message("Getting FAO crop coefficients from\n",
      "http://www.fao.org/3/X0490E/x0490e0b.htm \n",
      "on",paste(Sys.time()),"\n")

  fao_ch6_html <- xml2::read_html("http://www.fao.org/3/X0490E/x0490e0b.htm")

  all_tables <- fao_ch6_html %>%
    rvest::html_nodes(css="table")

  crop_stages_all <- all_tables[1] %>% rvest::html_table(fill=TRUE)
  crop_stages1 <- crop_stages_all[[1]] %>% tibble::as_tibble() %>% dplyr::select(-1) %>% stats::setNames(dplyr::slice(.,1)) %>% # remove first "Crop" column, set header from 1st row
    dplyr::filter(dplyr::row_number()!=1) %>% # remove first row (containing headers)
    dplyr::mutate(Crop_name=dplyr::if_else(grepl("^\\-",Crop),as.character(NA),Crop)) %>% # create new Crop_name column, excluding only subcategories (- dry)
    tidyr::fill(Crop_name) %>%
    dplyr::mutate(Crop_subcat = dplyr::if_else(Crop_name==Crop,as.character(NA),Crop)) %>%
    dplyr::mutate(Crop_group=dplyr::if_else(Crop_name==Region,Crop_name,as.character(NA))) %>%
    tidyr::fill(Crop_group) %>% dplyr::filter(Crop_group!=Region) %>%
    dplyr::select(dplyr::all_of(c("Crop_group","Crop_name","Crop_subcat","Init. (Lini)","Dev. (Ldev)","Mid (Lmid)","Late (Llate)","Total","Plant Date","Region"))) #%>%
  crop_stages2 <- crop_stages1 %>% dplyr::mutate(Crop_subcat= dplyr::if_else(grepl("year [12]",Crop_name),gsub(".*(year [12]).*","\\1",Crop_name),Crop_subcat),
                                          Crop_name  = dplyr::if_else(grepl("year [12]",Crop_name),"Cassava",Crop_name))
  crop_stages3 <- suppressWarnings(crop_stages2 %>%
    dplyr::mutate(Total_num=as.numeric(Total))) # NAs introduced by coercion

  crop_coeff_all <- all_tables[2] %>% rvest::html_table(fill=TRUE)
  crop_coeff1 <- crop_coeff_all[[1]] %>% #select(-1) %>%
    stats::setNames(gsub("^$","Kcini",dplyr::slice(.,1)))
  crop_coeff2 <- suppressMessages(crop_coeff1 %>% tibble::as_tibble(.name_repair = "unique")) # New names: Crop -> Crop...1, Crop -> Crop...2
  crop_coeff3 <- crop_coeff2 %>%
    dplyr::rename(Crop_name=Crop...1,Crop_subcat=Crop...2) %>%
    dplyr::filter(dplyr::row_number()!=1) %>%
    dplyr::mutate(Crop_name=dplyr::if_else(Crop_name=="",as.character(NA),Crop_name)) %>%
    tidyr::fill(Crop_name) %>%
    dplyr::mutate(Crop_subcat=dplyr::if_else(Crop_subcat==Crop_name,as.character(NA),Crop_subcat),
           Crop_group=dplyr::if_else(grepl("^[a-z]\\.",Crop_name),Crop_name,as.character(NA))) %>%
    tidyr::fill(Crop_group)
  return(list(crop_stages=crop_stages3,crop_coeff=crop_coeff3))
}

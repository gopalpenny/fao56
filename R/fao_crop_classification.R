# fao_crop_categories.R

# # scraping tables: https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
# # scraping text: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
# ggsub <- function(x,pattern,replacement) {
#   return(gsub(pattern,replacement,x))
# }

#' Get FAO Crop Classifications
#'
#' Get FAO Crop Classifications and botanical names
#' @return A list containing two \code{data.frame} objects: \code{crop_classes} and \code{botanical_names}.
#' @export
#' @examples
#' crop_classification <- get_crop_classes()
#' crop_classes <- crop_classification$crop_classes
#' botanical_names <- crop_classification$botanical_names
get_crop_classes <- function() {
  message("Getting FAO crop categories from\n",
          "https://www.fao.org/3/a0135e/A0135E10.htm \n",
          "on",paste(Sys.time()),"\n")

  fao_census_html <- xml2::read_html("https://www.fao.org/3/a0135e/A0135E10.htm")

  all_tables <- fao_census_html %>%
    rvest::html_nodes(css="table")

  crop_classes_all <- all_tables[1] %>% rvest::html_table(fill=TRUE)
  crop_classes_init <- crop_classes_all[[1]] %>% tibble::as_tibble() %>%
    dplyr::select(-dplyr::all_of(c("X5","X6"))) %>% setNames(dplyr::slice(.,1)) %>% # remove first "Crop" column, set header from 1st row
    setNames(gsub("Crop type1","Crop type",names(.))) %>%
    dplyr::filter(dplyr::row_number()!=1) %>% # remove first row (containing headers)
    dplyr::mutate(dplyr::across(dplyr::everything(), function(x) dplyr::if_else(x == "", as.character(NA), x)))

  crop_groups <- crop_classes_init %>%
    dplyr::filter(Group != "")
  crop_groups$Group_name <- crop_groups$Title
  crop_groups <- crop_groups %>%
    dplyr::select(-dplyr::all_of(c("Class","Sub-class","Order","Title")))
  crop_classes_filled <- crop_classes_init %>%
    tidyr::fill(Group) %>%
    dplyr::group_by(Group) %>%
    tidyr::fill(Class) %>%
    dplyr::group_by(Class) %>%
    tidyr::fill(`Sub-class`) %>%
    dplyr::group_by(`Sub-class`) %>%
    dplyr::filter(Class != "") %>%
    dplyr::left_join(crop_groups[,c("Group","Group_name")], by = "Group")

  botanical_names_all <- all_tables[2] %>% rvest::html_table(fill=TRUE)
  botanical_names_init <- botanical_names_all[[1]] %>% tibble::as_tibble() %>%
    setNames(dplyr::slice(.,1)) %>%
    dplyr::filter(dplyr::row_number()!=1, `Botanical name` != "") # remove first row (containing headers)

  return(list(crop_classes=crop_classes_filled,botanical_names=botanical_names_init))
}

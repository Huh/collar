adj_col_nms <- function(x){
  x %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(list(~ gsub("\\s", "", .)))
}


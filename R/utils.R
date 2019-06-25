adj_col_nms <- function(x){
  x %>%
    # remove non ASCII characters, like degree symbol
    iconv(., to = "ASCII", sub = "") %>%
    make.names(.) %>%
    tolower() %>%
    gsub("\\.+$", "", .) %>%
    gsub("\\.+", "_", .)
}


adj_col_nms <- function(x) {
  x %>%
    # remove non ASCII characters, like degree symbol
    iconv(to = "ASCII", sub = "") %>%
    make.names(.) %>%
    tolower() %>%
    gsub("\\.+$", "", .) %>%
    gsub("\\.+", "_", .)
}


# First match of a perl regex in each element, NA where there is no match.
# regmatches(x, regexpr()) alone drops non-matches and changes the length.
str_extract_na <- function(x, pattern) {
  m <- regexpr(pattern, x, perl = TRUE)
  out <- rep(NA_character_, length(x))
  out[!is.na(m) & m > 0] <- regmatches(x, m)
  out
}

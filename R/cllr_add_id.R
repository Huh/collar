#' Title
#'
#' @param x
#' @param id_col
#'
#' @return
#' @export
#'
#' @examples
cllr_add_id <- function(x, id_col){
  # a function to add an id column
  # returns the data frame unchaged if the id column specified already exists
  # creates a column named id with the value entered for id_col repeated if not

  idc <- rlang::enquo(id_col)
  if(!any(rlang::quo_text(idc) == names(x))){
    out <- dplyr::mutate(x, id = !!idc)
  }else{
    out <- x %>% dplyr::rename(id = id_col)
  }
  return(out)
}

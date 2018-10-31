# cllr_add_id
# A function to add an id column if it doesn't exist in the data frame

cllr_add_id <- function(x, id_col){
  # a function to add an id column
  # returns the data frame unchaged if the id column specified already exists
  # creates a column named id with the value entered for id_col repeated if not

  idc <- rlang::enquo(id_col)
  if(sum(rlang::quo_text(idc) == names(x)) == 0){
    out <- dplyr::mutate(x, id = rep(rlang::quo_text(idc), nrow(x)))
    }else{
    out <- x
  }
  return(out)
}

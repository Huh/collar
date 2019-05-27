#' Standardize GPS data to collar format
#'
#' @param x data.frame of GPS data with at least columns ID, Latitude, Longitude and Date-Time.
#' @param id_col Unquoted name of the ID column that signals the unique identifier for this collar.
#' @param dt_col Unquoted name of the date time column that represents the acquisition time.
#' @param dt_format character string giving a date-time format as used by strptime.
#' @param lon_col Unquoted name of the column containing Longitude location information.  See Details.
#' @param lat_col Unquoted name of the column containing Latitude location information.  See Details.
#' @param meta Key-value pairs supplied as a named list specifying new columns and values to be added to data.  See Details.
#' @param extra Unquoted column names of those columns beyond id, datetime, lon and lat that you wish to retain in the data set.
#'
#' @details stuff
#'
#' @return inherits from data.frame tbl_df
#' @export
#'
#' @examples
#' 2+2
morph_gps <- function(
                      x,
                      id_col = NULL,
                      dt_col = NULL,
                      dt_format = "%Y-%m-%d %H:%M:%S",
                      lon_col = NULL,
                      lat_col = NULL,
                      meta = list(),
                      extra = character()) {
  #  check x
  assertthat::assert_that(is.data.frame(x))
  #  Check meta
  assertthat::assert_that(
    is.list(meta) | is.null(meta),
    msg = "In morph_gps, meta must be NULL or a named list"
  )
  assertthat::assert_that(length(names(meta)) == length(meta))

  is_unquo <- function(x) {
    class(try(class(x), silent = T)) == "try-error"
  }

  assertthat::on_failure(is_unquo) <- function(call, env) {
    paste0("Column ", deparse(call$x), " must be unquoted. If special characters or spaces exist use back ticks (`A B`).")
  }

  # check unquo
  assertthat::assert_that(is_unquo(id_col))
  assertthat::assert_that(is_unquo(dt_col))
  assertthat::assert_that(is_unquo(lat_col))
  assertthat::assert_that(is_unquo(lon_col))
  # extras <- enquos(extra)
  # print(extras)
  # if(!is.null(extras)){
  #   assertthat::assert_that(is_unquo(extra))
  # }

  idc <- enquo(id_col)
  dtc <- enquo(dt_col)
  lonc <- enquo(lon_col)
  latc <- enquo(lat_col)

  out <- x %>%
    dplyr::transmute(
      id = as.character(!!idc),
      dt = as.POSIXct(!!dtc, format = dt_format),
      lon = as.numeric(!!lonc),
      lat = as.numeric(!!latc)
    )

  #  Add extra information
  extras <- enquos(extra)
  if (!is.null(extras)) {
    to_add <- x %>%
      dplyr::select(!!!extras)

    out <- dplyr::bind_cols(out, to_add)
  }

  #  Add meta information
  if (!is.null(meta)) {
    out <- tibble::as_tibble(c(out, meta))
  }

  out %>%
    dplyr::rename_all(tolower)
}

#' Standardize GPS data to collar format
#'
#' @param x data.frame of GPS data with at least columns ID, Latitude, Longitude and Date-Time
#' @param id_col Unquoted name of the ID column that signals the unique identifier for this collar
#' @param dt_col
#' @param dt_format
#' @param lon_col Unquoted name of the column containing Longitude location information.  See Details.
#' @param lat_col Unquoted name of the column containing Latitude location information.  See Details.
#' @param meta Key-value pairs supplied as a named list specifying new columns and values to be added to data.  See Details.
#' @param extra Unquoted column names of those columns beyond id, datetime, lon and lat that you wish to retain in the data set.
#'
#' @details stuff
#'
#' @return
#' @export
#'
#' @examples
cllr_morph_gps <- function(
                           x = NULL,
                           id_col = NULL,
                           dt_col = NULL,
                           dt_format = "%d/%m/%Y %H:%M:%S",
                           lon_col = NULL,
                           lat_col = NULL,
                           meta = list(),
                           extra = character()) {
    #  check x
    # assertthat::assert_that(assertthat::not_empty(x))
    # assertthat::assert_that(is.data.frame(x))
    # #  Check id, lat, lon
    # assertthat::assert_that(assertthat::is.string(id_col))
    # assertthat::assert_that(assertthat::is.string(lat_col))
    # assertthat::assert_that(assertthat::is.string(lon_col))
    #  Check meta
    assertthat::assert_that(is.list(meta)|is.null(meta))
    assertthat::assert_that(length(names(meta)) == length(meta))

    is_unquo <- function(x) {
      class(try(class(x), silent = T)) == "try-error"
    }

    assertthat::on_failure(is_unquo) <- function(call, env) {
      paste0("Column ", deparse(call$x), " must be unquoted. If special characters or spaces exist use back ticks (`A B`).")
    }

    assertthat::assert_that(is_unquo(id_col))
    assertthat::assert_that(is_unquo(dt_col))
    assertthat::assert_that(is_unquo(lat_col))
    assertthat::assert_that(is_unquo(lon_col))
    # extras <- enquos(extra)
    # print(extras)
    # if(!is.null(extras)){
    #   assertthat::assert_that(is_unquo(extra))
    # }

    dtc <- enquo(dt_col)
    lonc <- enquo(lon_col)
    latc <- enquo(lat_col)

    out <- x %>%
       cllr_add_id(id_col) %>%
       dplyr::select(id, !!dtc, !!lonc, !!latc) %>%
       dplyr::transmute(
         id = as.character(id),
         dt = as.character(!!dtc),
         lon = as.numeric(!!lonc),
         lat = as.numeric(!!latc)
       )

    #  Add extra information
    extras <- enquos(extra)
    if(!is.null(extras)){

      to_add <- x %>%
        dplyr::select(!!!extras)

      out <- dplyr::bind_cols(out, to_add)
    }

    #  Add meta information
    if(!is.null(meta)){
      out <- tibble::as_tibble(c(out, meta))
    }

    out %>%
      dplyr::rename_all(tolower)

  }

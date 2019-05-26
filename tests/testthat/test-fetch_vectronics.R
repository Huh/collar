context("test-fetch_vectronics.R")

test_that("Check fetch_vectronics", {
  key_dir <- system.file("extdata", package = "collar")
  keys <- get_paths(key_dir)

  expect_condition(
    fetch_vectronics(),
    "argument \"key_paths\" is missing, with no default",
    fixed = T
  )

  expect_s3_class(
    all_dat <- fetch_vectronics(keys, type = "gps"),
    "data.frame"
  )

  expect_s3_class(
    fetch_vectronics(keys, type = "act"),
    "data.frame"
  )

  expect_s3_class(
    fetch_vectronics(keys, type = "mit"),
    "data.frame"
  )

  expect_s3_class(
    fetch_vectronics(keys, type = "mor"),
    "data.frame"
  )

  expect_s3_class(
    fetch_vectronics(keys, type = "sep"),
    "data.frame"
  )

  trap_dir <- system.file("extdata", "TrapKeys", package = "collar")
  trap_keys <- get_paths(trap_dir)
  expect_s3_class(
    fetch_vectronics(trap_keys, type = "trap"),
    "data.frame"
  )

  expect_s3_class(
    fetch_vectronics(keys, type = "vit"),
    "data.frame"
  )

  id_pos <- all_dat$idposition
  data_id <- id_pos[which(id_pos == (max(id_pos) - 10))]
  new_dat <- fetch_vectronics(keys, type = "gps", after_data_id = data_id)
  expect_true(nrow(new_dat) == 10)

  after <- "2018-06-30T00:00:00"
  after_dat <- fetch_vectronics(
    keys,
    type = "gps",
    start_date = after,
    which_date = "acquisition"
  )
  expect_true(nrow(after_dat) == 246)
  expect_true(min(after_dat$acquisitiontime) > after)

})

test_that("Check fetch_vectronics assertions", {
  # Provide after_data_id and start_date, which cannot be true
  expect_condition(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = FALSE,
      after_data_id = "A",
      start_date = "B",
      which_date = NULL
    ),
    "The Vectronics API cannot accept both the after_data_id and start_date arguments at the same time.  Please change one or both to NULL."
  )

  # Bad data type
  expect_condition(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gpd",
      count = FALSE,
      after_data_id = NULL,
      start_date = NULL,
      which_date = NULL
    ),
    "Function build_vec_url called with type gpd, but type must be one of gps, act, mit, mor, prx, sep, trap, vit"
  )

  # Count value not logical
  expect_condition(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = "ABC",
      after_data_id = NULL,
      start_date = NULL,
      which_date = NULL
    ),
    "Function build_vec_url called with count set to ABC, but count must be TRUE or FALSE"
  )

  # Bad start_date format
  expect_condition(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = FALSE,
      after_data_id = NULL,
      start_date = "2018-12-0100:00:00",
      which_date = "scts"
    ),
    "In build_vec_url, the format of start_date must follow YYYY-MM-DDTHH:MM:SS, don't forget the T in the middle"
  )

  # Multiple start dates
  expect_condition(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = FALSE,
      after_data_id = NULL,
      start_date = c("2018-12-01T00:00:00", "2018-12-01T00:00:00"),
      which_date = "scts"
    ),
    "start_date must be a length one character vector, did you pass more than one start_date?"
  )

  # More date format testing
  dts <- paste("2018", c("01", "12"), c("01", "31"), sep = "-")
  tms <- paste(c("0", "24"), c("0", "60"), c("0", "60"), sep = ":")
  dt_tm <- paste(dts, tms, sep = "T")
  dt_tst <- purrr::map(dt_tm,
    ~ class(collar:::build_vec_url(
        base_url = NULL,
        collar_id = get_id_from_key(
          get_paths(
            system.file("extdata", package = "collar")
          )
        ),
        collar_key = get_keys(
          get_paths(
            system.file(
              "extdata",
              package = "collar"
            )
          )
        ),
        type = "gps",
        count = FALSE,
        after_data_id = NULL,
        start_date = .x,
        which_date = "scts"
      )
    )
  )
  expect_true(all(is.character(unlist(dt_tst))))

  # Bad value for which_date
  expect_condition(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = FALSE,
      after_data_id = NULL,
      start_date = "2018-01-01T00:00:00",
      which_date = "other"
    ),
    "Function build_vec_url called with which_date set to other, but which_date must be one of scts or acquisition."
  )

})

test_that("Check get_paths", {
  dir <- system.file("extdata", package = "collar")
  expect_is(
    get_paths(dir),
    "character"
  )
  expect_is(
    get_paths(dir, recursive = TRUE),
    "character"
  )
  expect_condition(
    get_paths(c(dir, dir)),
    "key_dir is not a string (a length one character vector).",
    fixed = T
  )
  expect_condition(
    get_paths(system.file("extdata", "telonics.csv", package = "collar")),
    "Path .* is not a directory"
  )
  expect_error(
    get_paths("C:/?\\Temp"),
    "Path 'C:/?\\Temp' does not exist",
    fixed = T
  )
})

test_that("Check get_id_from_key", {
  kp <- get_paths(system.file("extdata", package = "collar"))
  expect_is(
    get_id_from_key(kp),
    "character"
  )
  expect_condition(
    get_id_from_key("C:/?"),
    "In get_id_from_key, one or more key_paths point to files that do not exist"
  )
  expect_condition(
    get_id_from_key(get_paths(system.file("extdata", package = "collar"), "csv$")),
    "In get_id_from_key, one of more key_paths point to files that are not .keyx files"
  )
})

test_that("Check get_keys", {
  dir <- system.file("extdata", package = "collar")
  kp <- get_paths(dir)
  expect_is(
    get_keys(kp),
    "character"
  )
  expect_condition(
    get_keys(dir),
    "In get_keys, key_paths must be files not directories"
  )
  expect_condition(
    get_keys("C:/Temp/dave.keyx"),
    "In get_keys, one or more files in key_paths do not exist"
  )
  expect_condition(
    get_keys(get_paths(system.file("extdata", package = "collar"), "csv$")),
    "In get_keys, one or more files in key_paths do not have extension .keyx"
  )
})

test_that("Testing build_vec_url(s) calls", {
  kp <- get_paths(system.file("extdata", package = "collar"))
  expect_is(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = FALSE,
      after_data_id = NULL,
      start_date = NULL,
      which_date = NULL
    ),
    "character"
  )

  expect_is(
    collar:::build_vec_url(
      base_url = NULL,
      collar_id = get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ),
      collar_key = get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ),
      type = "gps",
      count = TRUE,
      after_data_id = NULL,
      start_date = NULL,
      which_date = NULL
    ),
    "character"
  )

  expect_condition(
    collar:::build_vec_urls(
      base_url = NULL,
      collar_id = rep(get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ), 3),
      collar_key = rep(get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ), 2),
      type = "gps",
      count = FALSE,
      after_data_id = NULL,
      start_date = NULL,
      which_date = NULL
    ),
    "In build_vec_urls, collar_id and collar_key must be the same length"
  )

  multi_urls <- collar:::build_vec_urls(
      base_url = NULL,
      collar_id = rep(get_id_from_key(
        get_paths(
          system.file("extdata", package = "collar")
        )
      ), 3),
      collar_key = rep(get_keys(
        get_paths(
          system.file(
            "extdata",
            package = "collar"
          )
        )
      ), 3),
      type = "gps",
      count = FALSE,
      after_data_id = NULL,
      start_date = NULL,
      which_date = NULL
    )

  expect_is(multi_urls, "character")

})




test_that("all_cllr_rm_header", {
  fl <- read.csv(
    system.file("extdata", "telonics.csv", package = "collar")
  )

  expect_s3_class(
    collar::cllr_remove_header(fl, `GPS Latitude`, rm_header = TRUE),
    "data.frame"
  )

  expect_s3_class(
    collar::cllr_remove_header(fl, `GPS Latitude`, rm_header = FALSE),
    "data.frame"
  )

  expect_true(
    colnames(
      collar::cllr_remove_header(fl, `GPS Latitude`, rm_header = FALSE)
    )[1] == "Telonics.Data.Report"
  )

  expect_condition(
    collar::cllr_remove_header(fl, "GPS Latitude", rm_header = TRUE),
    "Column col_nm must be unquoted. If special characters or spaces exist use back ticks (`A B`).",
    fixed = T
  )

  expect_condition(
    collar::cllr_remove_header(fl, `GPS Latitude`, rm_header = "ABC"),
    "Function cllr_remove_header called with rm_header set to ABC, but rm_header must be TRUE or FALSE",
    fixed = T
  )

  expect_condition(
    collar::cllr_remove_header(fl, `GPS Latitude`, rename_fun = "A"),
    "In cllr_remove_header rename_fun must be a function, received character"
  )

  expect_true(
    colnames(
      collar::cllr_remove_header(
        fl,
        `GPS Latitude`,
        rm_header = TRUE,
        rename_fun = make.names
      )
    )[1] == "Acquisition.Time"
  )


})

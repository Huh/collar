context("test-cllr_add_id.R")

test_that("Add ID column and values", {
  df <- data.frame(val = 1:3)

  expect_s3_class(
    cllr_add_id(df, "1A"),
    "data.frame"
  )

  expect_s3_class(
    cllr_add_id(df, as.character(1:3)),
    "data.frame"
  )

  expect_true(
    ncol(cllr_add_id(df, "1A")) == 2
  )

  expect_error(
    cllr_add_id(df)
  )

  expect_error(
    cllr_add_id("A", "ID")
  )

  expect_error(
    cllr_add_id(df, 1:3)
  )

  expect_error(
    cllr_add_id(df, NULL)
  )
})

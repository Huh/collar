context("test-cllr_rename_id.R")


test_that("Rename ID column", {
  df <- data.frame(val = 1:3, cname = 1)

  expect_s3_class(
    cllr_rename_id(df, cname),
    "data.frame"
  )

  expect_error(
    cllr_rename_id(df, c(ID, name))
  )

  expect_error(
    cllr_rename_id(df, "name")
  )


})

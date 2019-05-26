context("test-fetch_csv.R")

test_that("Check assertions", {
  # Error should occur when NULL is passed
  expect_error(fetch_csv(NULL))

  # Try to read file that does not exist
  my_file <- paste0(Sys.time(), runif(10))
  expect_error(fetch_csv("myfile"))

  # Successfully read and return csv
  tmp_dir <- tempdir()
  df <- data.frame(Val = c(2, 3))
  write.csv(df, file = file.path(tmp_dir, "mycsv.csv"), row.names = F)
  expect_s3_class(
    fetch_csv(file.path(tmp_dir, "mycsv.csv")),
    "data.frame"
  )
})

test_that("Read single csv", {
  # Create and pass a file to fetch_csv
  tmp_dir <- tempdir()

  df <- data.frame(Val = c(2, 3))

  write.csv(df, file = file.path(tmp_dir, "mycsv.csv"), row.names = F)

  expect_s3_class(
    fetch_csv(file.path(tmp_dir, "mycsv.csv")),
    "data.frame"
  )


})

test_that("Read multiple csv files", {
  # Create and pass multiple files to fetch_csv
  tmp_dir <- tempdir()

  df <- data.frame(Val = c(2, 3))

  write.csv(df, file = file.path(tmp_dir, "mycsv1.csv"), row.names = F)
  write.csv(df, file = file.path(tmp_dir, "mycsv2.csv"), row.names = F)

  fls <- paste(tmp_dir, c("mycsv1.csv", "mycsv2.csv"), sep = "/")

  expect_s3_class(
    fetch_csv(fls),
    "data.frame"
  )

  expect_equal(
    fetch_csv(fls)$val,
    rep(df$Val, 2)
  )
})

test_that("Check features of read_csv work", {
  tmp_dir <- tempdir()

  df <- data.frame(Val = c(" ab ", "cd", "Missing"))

  write.csv(df, file = file.path(tmp_dir, "readr.csv"), row.names = F)

  rd <- fetch_csv(
    file.path(tmp_dir, "readr.csv"),
    trim_ws = TRUE,
    na = "Missing"
  )

  expect_equal(
    rd$val[1],
    "ab"
  )
  expect_true(
    is.na(rd$val[3])
  )

})

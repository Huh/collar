context("test-fetch_csv.R")

test_that("Check file exists", {
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

test_that("Fail to read multiple csv files", {
  # Create and pass multiple files to fetch_csv, should fail
  tmp_dir <- tempdir()

  df <- data.frame(Val = c(2, 3))

  write.csv(df, file = file.path(tmp_dir, "mycsv1.csv"), row.names = F)
  write.csv(df, file = file.path(tmp_dir, "mycsv2.csv"), row.names = F)

  fls <- paste(tmp_dir, c("mycsv1.csv", "mycsv2.csv"), sep = "/")

  expect_error(
    fetch_csv(fls)
  )
})

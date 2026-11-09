
test_that("str_extract_na keeps length and returns NA for non-matches", {
  x <- c("Elk 12 (123456789)", "no id here", NA)
  expect_identical(
    str_extract_na(x, "(?<=\\()\\d{9}(?=\\))"),
    c("123456789", NA, NA)
  )
  expect_identical(str_extract_na(character(0), "x"), character(0))
})

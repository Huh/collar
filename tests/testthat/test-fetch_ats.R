context("test-fetch_ats.R")

test_that("Test ATS assertions", {
  expect_error(fetch_ats())
  expect_error(fetch_ats("A"))
  expect_error(fetch_ats("A", usr = "dave"))
  expect_error(fetch_ats(usr = "A", pwd = "B"))
})

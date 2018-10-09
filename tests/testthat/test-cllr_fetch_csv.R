context("test-cllr_fetch_csv.R")

test_that("bad inputs fail", {
  expect_error(cllr_fetch_csv("temp.xlsx"))
  expect_error(cllr_fetch_csv("./outrageous/temp.csv"))
})

telonics_path <- system.file(
  "extdata",
  "telonics.csv",
  package = "collar",
  mustWork = TRUE
)

r1 <- readr::read_csv(telonics_path)

test_that("read with skip", {

tlncs <- cllr_fetch_csv(telonics_path, skip = which(r1[,1] == "Acquisition Time"))

expect_equal(nrow(tlncs), 5)
expect_length(dim(tlncs), 2)
expect_type(tlncs, "list")

})

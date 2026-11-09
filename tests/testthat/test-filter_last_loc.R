test_that("filter_last_loc keeps the last row per group", {
  x <- tibble::tibble(
    id = c("a", "a", "b"),
    t = c(2, 1, 5),
    v = c("a2", "a1", "b5")
  )
  expect_identical(filter_last_loc(x, id)$v, c("a1", "b5"))
  expect_identical(filter_last_loc(x, id, order_by = t)$v, c("a2", "b5"))
})

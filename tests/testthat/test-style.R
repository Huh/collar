test_that("cllr_pal builds palettes, optionally reversed", {
  expect_identical(
    tolower(cllr_pal("main")(3)),
    unname(cllr_palettes$main)
  )
  expect_identical(
    cllr_pal("main", reverse = TRUE)(3),
    rev(cllr_pal("main")(3))
  )
  expect_identical(cllr_colors(), colors)
  expect_identical(unname(cllr_colors("green")), "#75cb0b")
})

test_that("style scales return ggplot2 scales for both modes", {
  expect_s3_class(style_scale_color(), "ScaleDiscrete")
  expect_s3_class(style_scale_color(discrete = FALSE), "ScaleContinuous")
  expect_s3_class(style_scale_fill("gray"), "ScaleDiscrete")
  expect_s3_class(style_scale_fill(discrete = FALSE), "ScaleContinuous")
})

test_that("style_theme returns a complete ggplot2 theme", {
  th <- style_theme()
  expect_s3_class(th, "theme")
  expect_identical(th$axis.title$size, 20)
})

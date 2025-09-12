test_that("ggMarginal info messages for ignored parameters work", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat, colour = factor(vs))) +
    ggplot2::geom_point()

  expect_no_message(ggMarginal(p))
  expect_no_message(ggMarginal(p, color = "red"))
  expect_no_message(ggMarginal(p, groupColour = TRUE))
  expect_message(ggMarginal(p, color = "red", groupColour = TRUE), "ignored")
  expect_no_message(ggMarginal(p, fill = "red"))
  expect_no_message(ggMarginal(p, groupFill = TRUE))
  expect_message(ggMarginal(p, fill = "red", groupFill = TRUE), "ignored")
})

test_that("ggMarginal error messages work", {
  expect_error(ggMarginal(), "must be provided")

  expect_error(
    ggMarginal(ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat))),
    "layer"
  )
  expect_no_error(
    ggMarginal(ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat)) + ggplot2::geom_jitter())
  )

  expect_error(
    ggMarginal(
      ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat)) + ggplot2::geom_point(),
      groupColour = TRUE
    ),
    "mapped"
  )
  expect_no_error(
    ggMarginal(
      ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = drat, color = factor(vs))) + ggplot2::geom_point(),
      groupColour = TRUE
    )
  )
})

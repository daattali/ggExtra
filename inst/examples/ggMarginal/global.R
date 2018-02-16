library(shiny)
library(ggplot2)
library(shinyjs)

randoms <- data.frame(
  "Normal" =
    rnorm(1000, 150, 20),
  "Bimodal" =
    c(rnorm(500, 200, 50), rnorm(500, 400, 50)),
  "Uniform" =
    runif(1000, 100, 200),
  check.names = FALSE
)

random_80_10_10 <- function(main, sec1, sec2) {
  rnd <- sample(10)
  if (rnd <= 8) main
  else if (rnd == 9) sec1
  else sec2
}

randoms$Class <- "A"
randoms$Class[randoms$Normal - 150 >= 25] <- "B"
randoms$Class[randoms$Normal - 150 <= -25] <- "C"
randoms$Class[sample(1000, 70)] <- "A"
randoms$Class[sample(1000, 40)] <- "B"
randoms$Class[sample(1000, 40)] <- "C"

datasets <- list(
  "Random distribution" = randoms,
  "iris" = iris,
  "cars" = cars,
  "faithful" = faithful,
  "rock" = rock
)

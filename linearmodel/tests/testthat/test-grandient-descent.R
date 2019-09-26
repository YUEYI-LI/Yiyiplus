library(testthat)

context("Test the output of gradient descent.")

test_that("Your gradient_descent() function works.", {

  data(iris)

  betahat <- gradient_descent(Sepal.Length ~ ., iris, alpha = 0.01, num_iters = 1500000, epsilon = 1e-12)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, betahat,
                    tolerance = 1e-3)
})

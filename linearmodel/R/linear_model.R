# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# output same $coefficient with lm()
# fit_linear_model <- linear_model(Sepal.Length ~ ., iris,
#                                 contrasts = list(Species = "contr.sum"))
# tougher  data(lm_patho)

# fit_linear_model <- linear_model(y ~., lm_patho)

# fit_lm <- lm(y ~., lm_patho)

# expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
#                  tolerance = 1e-5)


# formula, data and contrast
### Q: can use contrasts, model.matrix?
### load_all() ?
### need to include stats in imports in description ?   description install not attached should specify package::function
### How many dependencies? packages?  testthat?  packages used in data documentation or test?
## maintainer in description?
## license in description?
## if .R file contains multiple functions how to generate seperate .Rd (documentation) file?
### for iris need to data it?
## no .rbuildignore after using use_data_raw
## document the package?
### DID NOT PASS TEST PACKAGE? DATA NOT FOUND?

## Load the li
library(devtools)
library(testthat)
library(roxygen2)
library(usethis)

#' Fit a linear model
#'
#' \code{linear_model} returns the coefficients of the linear model fiited.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted.
#' @param dataframe a data frame containing the variables in the model.
#' @param contrasts an optional list that specifies a way to code a categorical variable.
#'   (default: contrasts.treatment)
#' @return \code{linear_model} returns an object of class 'list'. The list consists of 1
#'   component:
#'   \item{\code{coefficients}}{a named vector of coefficients}
#' @examples
#' lm(Sepal.Length  ~ ., iris)
#' lm(Sepal.Length  ~ ., iris, contrasts = list(Species = "contr.sum"))
#' @keywords linear model
linear_model <- function(formula, dataframe, contrasts = NULL){
  X<-model.matrix(formula, dataframe, contrasts = contrasts)
  y <- strsplit(as.character(formula)," ")[[2]][1]
  Y <- as.matrix(dataframe[y])
  QR <- qr(X)
  betahat <- solve.qr(QR,Y)
  betahat[betahat == 0] <- NA
  return(list(coefficients = betahat))
}


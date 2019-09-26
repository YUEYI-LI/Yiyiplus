#' Gradient descent for OLS
#'
#' \code{gradient_descent} returns the coefficients of the linear model fiited.
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted.
#' @param dataframe a data frame containing the variables in the model.
#' @param alpha a float value representing learning rate. Default value is 0.1.
#'   (default: contrasts.treatment)
#' @param num_iters  the maximal number of iterations. Default value is 10000.
#' @param epsilon The measure of convergion. Default value is 1e-08.
#' @return \code{gradient_descent} returns a vector of the coefficient of the fitted linear model
#'   derived through the method of ordinary least square, implemented by gradient descent method.
#' @examples
#' gradient_descent(Sepal.Length ~ ., iris, alpha = 0.01, num_iters = 150000, epsilon = 1e-12)
#' @keywords gradient descent
gradient_descent<-function(formula, dataframe, alpha = 0.01, num_iters = 10000, epsilon = 1e-12){
  # design matrix and Y
  X<-model.matrix(formula, dataframe)
  y <- strsplit(as.character(formula)," ")[[2]][1]
  Y <- as.matrix(dataframe[y])
  # initialize coefficients
  theta <- rep(0, ncol(X))
  m <- length(Y)
  # keep history
  loss_history <- double(num_iters)
  for(i in 1:num_iters){
    grad <- (1/m)*(t(X)%*%(X%*%theta - Y))
    theta <- theta - alpha* grad
    loss_history[i]  <- sum( (X %*% theta - Y)^2 ) / (2*length(Y))
    if (sum(grad^2) <= epsilon) {
      print(paste("Converged at", i))
      break
    }
  }
  # return theta and loss history
  return(theta)
}

#' BonusLab: Computations using QR Decomposition, implement method
#'
#' @param Formula formula object passed by user
#' @param data dataframe passed by the user
#' @param lambda value
#' @examples
#' newobj= Ridgereg(frm, iris,3)
#' @export
ridgereg_QR <- function(formula, data, lambda){
  call = match.call()
  X <- model.matrix(formula, data)
  standard_daviation <- c()
  mean_X <- c()
  result_matrix <- matrix(nrow=nrow(X), ncol=ncol(X))
  result_matrix[,1] <- X[,1]
  if(ncol(X) >= 2){
    i<-2
    while(i<=ncol(X))
    {
      standard_daviation[i] <- sd(X[,i])
      mean_X[i] <- mean(X[,i])
      if(standard_daviation[i] != 0){
        result_matrix[,i] <- (X[,i] - mean_X[i]) / standard_daviation[i]
      }
      i<-i+1
    }
  }

  y <- data[all.vars(formula)[1]]
  mean_y <- mean(y)
  y <- y - mean_y
  y <- c(y, rep(0, ncol(result_matrix)))
  Y <- rbind(result_matrix, sqrt(lambda) * diag(ncol(result_matrix)))
  QR_factor <- qr(Y)
  Q <- qr.Q(QR_factor)
  R <- qr.R(QR_factor)
  coeffResults <- qr.coef(QR_factor, y)
  coefficient <- coeffResults / sd_X
  coefficient[1] <- coefficient[1] - as.numeric(coefficient[-1] %*% mean_X[-1]) + mean_y
  names(coefficient) <- colnames(X)
  predicted_y <- X %*% coefficient
  predicted_y <- as.numeric(predicted_y[,1])
  result <- list(formula = formula,
                 call = call,
                 regression_coefficient = coefficient,
                 fitted_values = predicted_y
  )
  class(result) <- "ridgereg"
  return(result)
}

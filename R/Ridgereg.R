#' BonusLab: Computations using ordinary least squares, implement methods in RC class
#'
#' @param FunctionFormula formula object passed by user
#' @param data dataframe passed by the user
#' @param lambda value
#' @field ridge_VarianceOfTheRegressionCoefficients matrix.
#' @field ridge_RegressionCoeficientMatrix matrix.
#' @field ridge_TValues t-values in vec form
#' @field ridge_Residuals matrix.
#' @field ridge_ResidualVariance matrix.
#' @field ridge_DegreesOfFreedom numeric.
#' @field ridge_DataName Name of the dataframe passed.
#' @field ridge_FittedValues matrix.
#' @field ridge_PValues t-values in vec form
#' @examples
#' newobj= Ridgereg$new(frm, iris,3)
#' newobj$summary()
#'
#' @return class
#' @import methods
#' @export
Ridgereg<-setRefClass("Ridgereg", fields = list(
  FunctionFormula = "formula",
  ridge_RegressionCoeficientMatrix = "matrix",
  ridge_FittedValues = "matrix",
  ridge_Residuals = "matrix",
  ridge_DegreesOfFreedom = "numeric",
  ridge_ResidualVariance = "matrix",
  ridge_VarianceOfTheRegressionCoefficients ="vector",
  ridge_TValues = "vector",
  DataName = "character",
  ridge_PValues = "vector",
  lambda="numeric"
),

methods = list(
  initialize = function(formula, data, lam)
  {"Constructor to initialize the data fields of this class"
    if (!(is.numeric(lambda)))
    {
      stop()
    }
    FunctionFormula <<- formula
    DataName <<- deparse(substitute(data)) #get the name of dataframe
    lambda<<-lam
    X <- model.matrix(FunctionFormula, data)
    #nom<-(X-mean(X))
    #dnom<-sqrt(var(X)))
    #x_norm<-nom/dnom
    y <- data[all.vars(FunctionFormula)[1]]
    y <- unname(data.matrix(y))
    ridge_setRegCofficient(X,y,lambda)
    ridge_setFittedValues(ridge_RegressionCoeficientMatrix, X)
    ridge_setResidual(y)
    ridge_setDegreesOfFreedom(ridge_RegressionCoeficientMatrix, X)
    ridge_setResidualVariance()
    ridge_setVarianceOfTheRegressionCoefficients(X)
    ridge_setTValues()
    ridge_setPValues()

  }, #constructor

  #' Setter for regression coefficient
  #' @description
  #' Sets the value for RegressionCoeficientMatrix field
  #' @param X independent variables matrix
  #' @param y dependent variables matrix
  #' @param l value of Lambda
  #' @return the value that has been set
  ridge_setRegCofficient = function(X, y, l)
  {"Sets the value for RegressionCoeficientMatrix field"
    return (ridge_RegressionCoeficientMatrix <<- solve((t(X)%*%X)+(l*diag(col(X)))) %*% (t(X)%*%y))
  },


  #' Setter for Fitted Values
  #' @description
  #' Sets the value for FittedValues
  #' @param RegCofMatrix independent variables matrix
  #' @param X independent variables matrix
  #' @return the value that has been set
  ridge_setFittedValues = function(RegCofMatrix, X)
  {"Sets the value for FittedValues"
    return (ridge_FittedValues <<- X %*% RegCofMatrix)
  },


  #' Setter for Fitted Values
  #' @description
  #' Sets the value for Residuals
  #' @param y dependent variables matrix
  #' @return the value that has been set
  ridge_setResidual = function(y)
  {"Sets the value for Residuals"
    return (ridge_Residuals <<- y - ridge_FittedValues)
  },

  #' Setter for degree of freedom
  #' @description
  #' Sets the value for DegreesOfFreedom
  #' @param RegCofMatrix independent variables matrix
  #' @param X independent variables matrix
  #' @return the value that has been set
  ridge_setDegreesOfFreedom = function(RegCofMatrix, X)
  {"Sets the value for DegreesOfFreedom"
    return (ridge_DegreesOfFreedom <<- dim(X)[1] - dim(RegCofMatrix)[1])
  },

  #' Setter for degree of residual variance
  #' @description
  #' Sets the value for ResidualVariance
  #' @return the value that has been set
  ridge_setResidualVariance = function()
  {"Sets the value for ResidualVariance"
    return (ridge_ResidualVariance <<- (t(ridge_Residuals) %*% ridge_Residuals) / ridge_DegreesOfFreedom)
  },

  #' Setter for degree of variance of reg coefficients
  #' @description
  #' Sets the value for VarianceOfTheRegressionCoefficients
  #' @param X independent variables matrix
  #' @return the value that has been set
  ridge_setVarianceOfTheRegressionCoefficients = function(X)
  {"Sets the value for VarianceOfTheRegressionCoefficients"
    ans= ridge_ResidualVariance[1,1] * (solve(t(X) %*% X))
    return (ridge_VarianceOfTheRegressionCoefficients <<- diag(ans))
  },

  #' Setter T Values
  #' @description
  #' Sets the value for TValues
  #' @return the value that has been set
  ridge_setTValues = function()
  {"Sets the value for TValues"
    return (ridge_TValues <<- as.vector(ridge_RegressionCoeficientMatrix)/sqrt(ridge_VarianceOfTheRegressionCoefficients))
  },

  #' Setter P Values
  #' @description
  #' Sets the value for TValues
  #' @return the value that has been set
  ridge_setPValues = function()
  {"Sets the value for TValues"
    return (ridge_PValues <<- pt(as.vector(ridge_RegressionCoeficientMatrix),df=ridge_DegreesOfFreedom))
  },


  ############################################# <1.3 Implementing methods for your class> ##########################################

  #' Print like lm
  #' @description
  #' Prints out the coefficient and coefficent names
  #' @return nothing
  print = function()
  {"Prints out the coefficient and coefficent names"

    cat("Call:\n","linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n", sep = "")
    cat("Coefficients:\n",dimnames(ridge_RegressionCoeficientMatrix)[[1]], "\n",ridge_RegressionCoeficientMatrix)

  },


  #' Plot using ggplot2
  #' @description
  #' Plots 2 graphs on a grid that are mention in the lab manual
  #' @return nothing
  plot = function()
  {"Plots 2 graphs on a grid that are mention in the lab manual"
    VecFittedValues= unlist (ridge_FittedValues)
    VecResiduals= unlist (ridge_Residuals)
    DataFrame2PlotResidualFit <- data.frame(VecResiduals, VecFittedValues, c(1:length(ridge_FittedValues)))
    names(DataFrame2PlotResidualFit) <- c("Residuals", "Fitted_values", "Number")

    ResidualFit <-
      ggplot2::ggplot(DataFrame2PlotResidualFit) +
      ggplot2::aes(Fitted_values, Residuals) +
      #to add a trend line:
      ggplot2::geom_smooth(ggplot2::aes(Fitted_values, Residuals), formula = y~0+x, se = FALSE, span = 1, color = "#FF0000") +
      #plot geom points
      ggplot2::geom_point(size = 2,  shape = 1) +
      # title of the graph, appears on top
      ggplot2::ggtitle("\t\t\t\t Residuals vs Fitted") +
      #this part appears at the bottom
      ggplot2::xlab(paste("Fitted values", "\n lm(", format( FunctionFormula), ")", sep = ""))


    StandardizedResiduals = unlist(sqrt(abs(ridge_Residuals)))
    VecStandardizedResiduals = unlist(StandardizedResiduals)
    DataFrame2PlotScaleLocation = data.frame(VecStandardizedResiduals, VecFittedValues ,c(1:length(ridge_FittedValues)))
    names(DataFrame2PlotScaleLocation) <- c("StandResiduals", "Fitted_values", "Number")


    ScaleLocation <-
      ggplot2::ggplot(data = DataFrame2PlotScaleLocation) +
      ggplot2::aes(Fitted_values,StandResiduals) +
      ggplot2::geom_point(size = 2,  shape = 1) +
      # title of the graph, appears on top
      ggplot2::ggtitle("\t\t\t\t Scale - Location") +
      ggplot2::geom_smooth(ggplot2::aes(Fitted_values, StandResiduals), formula = y~0+x, color = "#FF0000") +
      ggplot2::ggtitle("\t\t\t\tScale-Location") +
      ggplot2::xlab(paste("Fitted Values", "\n lm(", format( FunctionFormula), ")", sep = "")) +
      ggplot2::ylab(expression(sqrt("Standardized residuals")))

    gridExtra::grid.arrange(ResidualFit,ScaleLocation)

  },


  #' Residuals calculation
  #' @description
  #' return only residuals as vec form
  #' @return Residuals
  resid = function()
  {"return only residuals as vec form"
    return(as.vector(ridge_Residuals))
  },


  #' predicted values
  #' @description
  #' return the preticted values (y-hat)
  #' @return FittedValues
  pred = function()
  {"return the preticted values (y-hat)"
    return(ridge_FittedValues)
  },

  #' coefficeints as named vec
  #' @description
  #' return the coefficeints as named vec
  #' @return coefficeints as named vec
  coef = function()
  {"return the coefficeints as named vec"
    c= as.vector(ridge_RegressionCoeficientMatrix)
    names(c) <- rownames(ridge_RegressionCoeficientMatrix)
    return(c)
  },

  #' Print the Summary
  #' @description
  #' Print the simmary along with the pvalue, tvalue, sigma and degree of freedom
  #' @return coefficeints as named vec
  summary = function(){"Print the simmary along with the pvalue, tvalue, sigma and degree of freedom"

    summaryDetails <- matrix(round(c(as.vector(ridge_RegressionCoeficientMatrix), as.vector(sqrt(ridge_VarianceOfTheRegressionCoefficients)), as.vector(TValues), as.vector(PValues)),4), ncol = 4)
    PThreshold <- ifelse(ridge_PValues<0.001, "***",no = ifelse(ridge_PValues<0.01, "**", ifelse(ridge_PValues<0.05, "*", ifelse(ridge_PValues<0.1, ".", " "))))
    summaryDetails <- cbind(summaryDetails, PThreshold)

    colnames(summaryDetails) <- c("Coefficients", "Std Error" ,"T-values", "P-Values", " ")
    rownames(summaryDetails) <- dimnames(ridge_RegressionCoeficientMatrix)[[1]]
    cat("Call:\n")
    cat("linreg(formula = ", format(FunctionFormula), ", data = ", DataName ,")\n\n", sep = "")
    write.table((summaryDetails), quote = FALSE)
    cat("\n Residual standard error:", sqrt(ridge_ResidualVariance),"on", ridge_DegreesOfFreedom,"degrees of freedom")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  }
))

my_ridge<-Ridgereg$new(Petal.Length~Species, data =iris,lam=3)
print(my_ridge$ridge_RegressionCoeficientMatrix)

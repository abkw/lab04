library(ggplot2)
library(png)
library(grid)
#' This function uses RC class to generate Linear Regressions
#'
#' @field formula formula.
#' @field data data.frame.
#' @field reg_coef matrix.
#' @field fitted_val matrix.
#' @field residuals matrix.
#' @field degree_of_freedom numeric.
#' @field residul_variance numeric.
#' @field variance_reg_coef matrix.
#' @field t_value matrix.
#' @field p_value matrix.
#'
#' @return This class returns linear regression data including t-values and p-values
#' @export
#'
#' @examples
#'

linreg <- setRefClass(Class =  "linreg",

                      fields = list(
                        formula = "formula",
                        data = "data.frame",
                        dataName = "character",
                        reg_coef = "matrix",
                        fitted_val = "matrix",
                        residuals = "matrix",
                        degree_of_freedom = "numeric",
                        residul_variance = "numeric",
                        variance_reg_coef = "matrix",
                        t_value = "matrix",
                        p_value = "matrix"
                      ),

                      methods = list(

                        # initialize11 = function(formula, data) {
                        #   formula <<- formula
                        #   data <<- data
                        #   dataName <- deparse(substitute(data))
                        # },

                        initialize = function(formula = as.formula, data = as.data.frame) {
                          formula <<- formula
                          data <<- data
                          dataName <<- deparse(substitute(data))
                          all_variables <- all.vars(formula)
                          indenpendent_var <- all_variables[-1]
                          dependent_var <- all_variables[1]

                          X <- model.matrix(formula, data)
                          y <- as.matrix(data[dependent_var])

                          # Regression Coefficients

                          regressionsCoefficients <- solve(t(X) %*% X) %*%  (t(X) %*% y)
                          #print(regressionsCoefficients)
                          reg_coef <<- regressionsCoefficients

                          # Fitted Values

                          fittedValue <- X %*% regressionsCoefficients
                          #print(fittedValue)
                          fitted_val <<- fittedValue

                          # The residuals

                          theResiduals <- y - fittedValue
                          #print(theResiduals)
                          residuals <<- theResiduals

                          # The Degree of Freedom

                          degreeOfFreedom <- nrow(X) - ncol(X)
                          #print(degreeOfFreedom)
                          degree_of_freedom <<- degreeOfFreedom

                          # The Residual Variance

                          residualVariance <- as.numeric((t(theResiduals) %*% theResiduals) / degreeOfFreedom)
                          #print(residualVariance)
                          residul_variance <<- residualVariance


                          # The Variance Of The Regression Coefficients

                          varianceOfRegressionCoefficients <- c(residualVariance) * solve(t(X) %*% X)
                          #print(varianceOfRegressionCoefficients)
                          variance_reg_coef <<- varianceOfRegressionCoefficients

                          # The t-values For Each Coefficient

                          tValueForEachCoefficient <- regressionsCoefficients / sqrt(diag(varianceOfRegressionCoefficients))
                          #print(tValueForEachCoefficient)
                          t_value <<- tValueForEachCoefficient

                          # p-values
                          p_value <<- 2 * pt(-abs(tValueForEachCoefficient), df = degreeOfFreedom)
                          #print(p_value)
                        },
                        print = function() {
                          cat(sep = "\n")
                          cat("Call:")
                          cat(sep = "\n")
                          cat(paste("linreg(", "formula = ", formula[2], " ", formula[1], " ", formula[3], ", ", "data = ", dataName, ")", sep = "" ))

                          cat(sep = "\n",sep = "\n")
                          cat(sep = "\n")
                          cat("Coefficients:")

                          cat(sep = "\n")
                          cat("     ",row.names(reg_coef))
                          cat(sep = "\n")
                          cat(paste("        ",reg_coef[1], "           ",reg_coef[2], "          ",reg_coef[3]))
                          cat(reg_coef)
                        },
                        plot = function(){

                          par(mfcol = c(2,2))
                          plotData <- data.frame(fittedValues = fitted_val,residuals = residuals)
                          risidualsVsFitted <- ggplot(data = plotData, aes(x = fitted_val, y = residuals, fill = residuals)) +
                          geom_bar(stat = "identity") +
                          xlab("Fitted Values\n lm(Petal.Length ~ Species)") +
                          ylab("Residuals") +
                          ggtitle("Residuals vs Fitted")

                          scaleLocation <- ggplot(data = plotData,
                          aes(x = fitted_val, y = sqrt(abs((residuals - mean(residuals)) / sqrt(residul_variance)
                          )))) +
                          geom_bar(stat = "identity") +
                          xlab("Fitted Values\n lm(Petal.Length ~ Species)") +
                          ylab(expression(sqrt(abs("Standardized Residuals")))) +
                          ggtitle("Scale−Location")
                          grid.arrange(risidualsVsFitted,scaleLocation, nrow=1)
                          return (grid.arrange(risidualsVsFitted,scaleLocation, nrow=2))
                        }
                      )
)

data("iris")
item <- linreg$new(formula = Petal.Length~Species, data = iris)
item$plot()

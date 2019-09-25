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

linregOne <- setRefClass(Class =  "linregOne",

                      fields = list(
                        formula = "formula",
                        data = "data.frame",
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

                        initialize = function(formula, data) {
                          formula <<- formula
                          data <<- data
                        },

                        calcParams = function() {
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
                        }

                      )
)

data("iris")
item <- linregOne$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
print(item$calcParams())
print(item$reg_coef)
print(item$t_value)
print(item$p_value)

linearMod <- lm(Petal.Length~Sepal.Width+Sepal.Length, data=iris)  # build linear regression model on full data
summary(linearMod)


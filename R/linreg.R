library(ggplot2)
library(png)
library(gridExtra)
library(methods)
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
#' @import ggplot2 png gridExtra methods
#'
#' @examples
#' \dontrun{
#' data("iris")
#' item <- linreg(formula = Petal.Length~Species, data = iris)
#' }

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
                        p_value = "matrix",
                        coefVector = "vector"
                      ),

                      methods = list(

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

                          reg_coef <<- regressionsCoefficients
                          coefVector <<- as.vector(reg_coef)
                          # Fitted Values

                          fittedValue <- X %*% regressionsCoefficients

                          fitted_val <<- fittedValue

                          # The residuals

                          theResiduals <- y - fittedValue

                          residuals <<- theResiduals

                          # The Degree of Freedom

                          degreeOfFreedom <- nrow(X) - ncol(X)

                          degree_of_freedom <<- degreeOfFreedom

                          # The Residual Variance

                          residualVariance <- as.numeric((t(theResiduals) %*% theResiduals) / degreeOfFreedom)

                          residul_variance <<- residualVariance


                          # The Variance Of The Regression Coefficients

                          varianceOfRegressionCoefficients <- c(residualVariance) * solve(t(X) %*% X)

                          variance_reg_coef <<- varianceOfRegressionCoefficients

                          # The t-values For Each Coefficient

                          tValueForEachCoefficient <- regressionsCoefficients / sqrt(diag(varianceOfRegressionCoefficients))

                          t_value <<- tValueForEachCoefficient

                          # p-values
                          p_value <<- 2 * pt(-abs(tValueForEachCoefficient), df = degreeOfFreedom)

                          ?pt
                        },

                        print = function() {
                          cat(sep = "\n")
                          cat("Call:")
                          cat(sep = "\n")
                          cat(paste("linreg(", "formula = ", formula[2], " ", formula[1], " ", formula[3], ", ", "data = ", dataName, ")", sep = "" ))

                          cat(sep = "\n")
                          cat(sep = "\n")
                          cat("Coefficients:")

                          cat(sep = "\n")
                          coef_matrix <- matrix(c(round(reg_coef, digits = 2)), nrow = 1)
                          colnames(coef_matrix) <- rownames(reg_coef)
                          rownames(coef_matrix) <- c("")
                          print.default(coef_matrix)
                          },

                        plot = function() {

                          par(mfcol = c(2,1))
                          plotData <- data.frame(fittedValues = fitted_val,residuals = residuals)

                          risidualsVsFitted <- ggplot(data = plotData, aes(x = fitted_val, y = residuals)) +
                          #geom_bar(stat = "identity") +
                          geom_point(colour = "black", fill = "white") +
                          xlab("Fitted Values\n lm(Petal.Length ~ Species)") +
                          ylab("Residuals") +
                          ggtitle("Residuals vs Fitted") +
                          theme(plot.title = element_text(hjust = 0.5))

                          scaleLocation <- ggplot(data = plotData,
                          aes(x = fitted_val, y = sqrt(abs((residuals - mean(residuals)) / sqrt(residul_variance)
                          )))) +
                          #geom_bar(stat = "identity") +
                          geom_point() +
                          xlab("Fitted Values\n lm(Petal.Length ~ Species)") +
                          ylab(expression(sqrt(abs("Standardized Residuals")))) +
                          ggtitle("Scale-Location") +
                          theme(plot.title = element_text(hjust = 0.5))

                          return (grid.arrange(risidualsVsFitted,scaleLocation, nrow=2))
                        },

                        resid = function() {
                          return(residuals)
                        },

                        pred = function() {
                          return(fitted_val)
                        },

                        coef = function() {
                        names(coefVector) <<- rownames(reg_coef)
                        return(coefVector)
                        },
                        lmtest = function(formula,data){
                          return (lm(formula,data))
                        },

                        summary = function() {
                        cat ("Coefficients:\n")

                        cat (paste("              Estimate ", "  Std. Error   ", "  t value ", "  Pr(>|t|)"))

                        cat ("\n"); cat ("   ")
                        for (i in 1:nrow(reg_coef)){
                        cat (paste(rownames(reg_coef)[i], reg_coef[i], sqrt(diag(variance_reg_coef))[i],t_value[i],p_value[i], "***","\n"))
                        }
                        cat(paste("\nResidual standard error: ", sqrt(residul_variance), " on ",degree_of_freedom ," degrees of freedom",sep = ""))
                        }
                      )
)


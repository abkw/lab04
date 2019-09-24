#This function uses RC class
library(expm)
linreg <- setRefClass("linreg", fields = list(formula= "formula", data="data.frame"),
                      methods = list(
                        linreg = function(formula = as.formula(), data=as.data.frame()) {
                          yName <- all.vars(formula)[1]
                          y <- data[,as.character(yName)]
                          X <- model.matrix(formula,data)
                          matTranspose <- t(X)
                          #---------------------------------------------Regression Coefficients-------------------------------
                          regressionsCoefficients <- solve((t(X) %*% X) , t(X) %*% y) #((matTranspose%*%theMatrix)^-1)%*%matTranspose%*%y
                          #---------------------------------------------Fitted Values-----------------------------------------
                          fittedValue <- X %*% regressionsCoefficients
                          #---------------------------------------------- The residuals --------------------------------------
                          theResiduals <- y - fittedValue
                          #----------------------------------------------The Degree of Freedom--------------------------------
                          degreeOfFreedom <- nrow(X) - ncol(X)
                          #----------------------------------------------The Residual Variance--------------------------------
                          residualVariance <- as.numeric(t(theResiduals) %*% theResiduals / degreeOfFreedom)
                          #----------------------------------------------The Variance Of The Regression Coefficients----------
                          varianceOfRegressionCoefficients <- residualVariance * solve((t(X) %*% X))
                          #----------------------------------------------The t-values For Each Coefficient--------------------
                          tValueForEachCoefficient <- regressionsCoefficients %*% (sqrt(diag(varianceOfRegressionCoefficients)))
                          #----------------------------------------------Calculating the P values ----------------------------
                          pValue <- pt(abs(tValueForEachCoefficient), df = degreeOfFreedom,lower.tail=FALSE)
                          print(pValue)
                          return(mat)
                        }
                      )
)

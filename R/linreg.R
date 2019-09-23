#This function uses RC class
library(expm)
linreg <- setRefClass("linreg", fields = list(formula= "formula", data="data.frame"),
                      methods = list(
                        linreg = function(formula = as.formula(), data=as.data.frame(),...) {
                          yName <- all.vars(formula)[1]
                          y <- data[,as.character(yName)]
                          mat <- model.matrix(formula,data)
                          mat <- cbind(mat,y)
                          return(mat)
                        },
                        linreg1 = function(formula = as.formula, data=as.data.frame){
                          mat <- model.matrix(formula,data)
                          x <- lm(formula,data)
                          return (summary(x))
                        }
                        )
)

data("iris")
linreg_mod <- linreg$new()
linreg_mod1<- linreg$new()

linreg_mod1 <- linreg_mod$linreg1(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris)
print(linreg_mod1)
print(summary(linreg_mod1$sigma)**2)

print(linreg_mod1$coefficients[,3])

theMatrix <- linreg_mod$linreg(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris)
y <- theMatrix[,ncol(theMatrix)]
X <- theMatrix[,-ncol(theMatrix)]
matTranspose <- t(theMatrix)
#---------------------------------------------Regression Coefficients-------------------------------
# regressionsCoefficients <- ((matTranspose%*%theMatrix)^-1)%*%matTranspose%*%y
# print(regressionsCoefficients)

regressionsCoefficients <- solve((t(X) %*% X) , (t(X) %*% y)) #((matTranspose%*%theMatrix)^-1)%*%matTranspose%*%y
print(X)
print(regressionsCoefficients)
#---------------------------------------------Fitted Values-----------------------------------------
fittedValue <- X %*% regressionsCoefficients
print(fittedValue)
#---------------------------------------------- The residuals --------------------------------------
theResiduals <- y - fittedValue
print(theResiduals)
#----------------------------------------------The Degree of Freedom--------------------------------
# p <- regressionsCoefficients[1,]
# print (p)

degreeOfFreedom <- nrow(X) - ncol(X)
print(degreeOfFreedom)

#----------------------------------------------The Residual Variance--------------------------------

residualVariance <- as.numeric((t(theResiduals) %*% theResiduals) / degreeOfFreedom)
print(residualVariance)


#----------------------------------------------The Variance Of The Regression Coefficients----------

varianceOfRegressionCoefficients <- residualVariance * solve(t(X) %*% X)
print(varianceOfRegressionCoefficients)

#----------------------------------------------The t-values For Each Coefficient--------------------

tValueForEachCoefficient <- t(regressionsCoefficients) %*% sqrtm(varianceOfRegressionCoefficients)

print(t(regressionsCoefficients))
print(solve(sqrtm(varianceOfRegressionCoefficients)))
print(tValueForEachCoefficient)

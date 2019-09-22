#This function uses RC class
linreg <- setRefClass("linreg", fields = list(formula= "formula", data="data.frame"),
                      methods = list(
                        linreg = function(formula = as.formula(), data=as.data.frame(),...) {
                          yName <- all.vars(formula)[1]
                          y <- data[,as.character(yName)]
                          mat <- model.matrix(formula,data)
                          mat <- cbind(mat,y)
                          return(mat)
                        }
                        )
)

data("iris")
linreg_mod <- linreg$new()
theMatrix <- linreg_mod$linreg(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris)
y <- theMatrix[,ncol(theMatrix)]
theMatrix <- theMatrix[,-ncol(theMatrix)]
matTranspose <- t(theMatrix)
#---------------------------------------------Regression Coefficients-------------------------------
regressionsCoefficients <- ((matTranspose%*%theMatrix)^-1)%*%matTranspose%*%y
print(regressionsCoefficients)
#---------------------------------------------Vitted Values-----------------------------------------
fittedValue <- theMatrix %*% regressionsCoefficients
#---------------------------------------------- The residuals --------------------------------------
theResiduals <- y - fittedValue
#----------------------------------------------The Degree of Freedom--------------------------------
p <- regressionsCoefficients[1,]
print (p)

degreeOfFreedom <- nrow(theMatrix) - ncol(theMatrix)
print(degreeOfFreedom)

#----------------------------------------------The Residual Variance--------------------------------

residualVariance <- (t(theResiduals)%*%theResiduals) / degreeOfFreedom
print(residualVariance)

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
result <- ((matTranspose%*%theMatrix)^-1)%*%matTranspose%*%y
print(result)

#This function uses RC class
linreg <- setRefClass("linreg", fields = list(formula= "formula", data="data.frame"),
                      methods = list(
                        linreg = function(formula = as.formula(), data=as.data.frame(),...) {
                          mat <- model.matrix(formula,data)
                          return (summary(lm(formula,data)))

                        }
                        )
)

data("iris")
linreg_mod <- linreg$new()
print(linreg_mod$linreg(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris))


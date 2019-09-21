#This function uses RC class
linreg <- setRefClass("linear",
                      fields = list(formula= "formula", data="data.frame"),
                      methods = list(
                        linreg = function(formula = as.formula(), data=as.data.frame()) {
                          mat <- model.matrix(formula,data)
                          return (all.vars(mat))
                        }
                        )
)

data("iris")
print(linreg_mod$linreg(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris))

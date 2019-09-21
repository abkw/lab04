#This function uses RC class
linreg <- setRefClass("linear",
                      fields = list(formula= "formula", data="data.frame"),
                      methods = list(
                        linreg = function(formula = "formula", data="data.frame") {
                          if (formula == !as.formula()){
                            stop("formula has to be a formula")
                          }
                          if (data == !as.data.frame()){
                            stop("data has to be a dataframe")
                          }
                          all.vars(formula, functions = FALSE, max.names = -1L, unique = TRUE)
                          obj <- linear$new()
                          obj <- model.matrix(data)
                          return (all.vars(obj, functions = FALSE, max.names = -1L, unique = TRUE))
                        }
                        )
)
data("iris")
linreg_mod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris)
print(linreg_mod)

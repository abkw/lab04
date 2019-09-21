#This function uses RC class
linear <- setRefClass("linear",
                      fields = list(formula = "formula", data = "character"),
                      methods = list(
                        linreg = function(formula, data) {
                          obj <- linear$new()
                          obj <- model.matrix(data)
                          return (obj)
                        }
                        )
)
x <- as.formula("y ~ x1 + x2")
linregResult <- linear$new(formula =x,data = "ng")
linregResult$linreg(formula = x,data = "ng")

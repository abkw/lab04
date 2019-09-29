## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(lab04)

## ----two-----------------------------------------------------------------
item <- linreg$new(formula = Petal.Length~Species, data = iris)
item <- linreg(formula = Petal.Length~Species, data = iris)
item$print()

## ----plot----------------------------------------------------------------
item$plot()

## ----resid---------------------------------------------------------------
item$resid()

## ----pred----------------------------------------------------------------
item$pred()

## ----coef----------------------------------------------------------------
item$coef()

## ----summary-------------------------------------------------------------
item$summary()

## ----one, include=TRUE---------------------------------------------------
item <- linreg(formula = Petal.Length~Species, data = iris)
item$print()
item$summary()



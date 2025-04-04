
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BMIplot

<!-- badges: start -->
<!-- badges: end -->

The goal of BMIplot is to write a BMI formula which will derive the BMI
of an individual for whom the height and weight are known. Here we need
to use the height in cm and weight in kg.

Here is the formtula for the same

bmi_value \<- weight / (height^2)

``` r
height <- 170
weight <- 70
bmi_value <- (weight / ((height/100)^2))
bmi_value
#> [1] 24.22145
```

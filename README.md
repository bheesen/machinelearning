
<!-- README.md is generated from README.Rmd. Please edit that file -->

# machinelearning

<!-- badges: start -->
<!-- badges: end -->

The goal of the package machinelearningis to support the readers of the
book “Künstliche Intelligenz und Machine Learning mit R” with samples,
functions and tutorials as described in the book.

The function ml.summary() automatically evaluates the main descriptive
statistical figures for a variable and creates different forms of
graphics for qualitative and quantitative variables.

## Installation

You can install the development version of machinelearning from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bheesen/machinelearning")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(machinelearning)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
ergebnis<-ml.summary.1(autos$Modell,"Autos","Modell","Anzahl",bar=T,barminmax=20)
ergebnis
```

<img src="man/figures/README-studierende-1.png" width="100%" /><img src="man/figures/README-studierende-2.png" width="100%" />

    #>              [,1]                                                                                       
    #> n            "357"                                                                                      
    #> na           "0"                                                                                        
    #> maxanzahl    "42"                                                                                       
    #> modus        "170"                                                                                      
    #> median       "170"                                                                                      
    #> mean         "172.039215686275"                                                                         
    #> skewness     "0.519968092098753"                                                                        
    #> skewness.txt "Schiefe 0.519968092098753 > 0, Rechte Schiefe: positive Schiefe, linkssteil, rechtsschief"
    #> kurtosis     "0.395699862577392"                                                                        
    #> kurtosis.txt "Exzess Kurtosis 0.395699862577392 > 0, steilgipflig"                                      
    #> spannweite   "54"                                                                                       
    #> min          "150"                                                                                      
    #> max          "204"                                                                                      
    #> var          "82.6613791584049"                                                                         
    #> sd           "9.09183035248706"               

``` r
ergebnis<-ml.summary.1(autos$Modell,"Autos","Modell","Anzahl",bar=T,barminmax=20)
ergebnis
```

<img src="man/figures/README-studierende-3.png" width="100%"/>

The `README.Rmd` file was used to create this `README.md` file using the
function `devtools::build_readme()`.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# codforbid

<!-- badges: start -->
<!-- badges: end -->

The goal of codforbid is to automate the Atlantic Cod discard
calculation for Georges Bank (described in Gavaris et al. 2007) as much
as possible, and to make it reproducible by replacing the combination of
Access, Excel, and APL applications previously used.

## Installation

You can install the development version of codforbid from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CMClark1/codforbid")
```

## Example

Use marfispull and isdbpull functions:

``` r
library(codforbid)
marfis <- marfispull(year=2023)
isdb <- isdbpull(year=2023)
```

Use the DO_RATIOS function to calculate the landings multiplier for a
given dataset:

``` r
library(codforbid)
COD=runif(4)
HAD=runif(4)
OBS=c("N","N","Y","Y")
df<-data.frame(COD,HAD,OBS)
example1 <- DO_RATIOS(df)
```

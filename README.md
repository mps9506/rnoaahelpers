
<!-- README.md is generated from README.Rmd. Please edit that file -->
rnoaahelpers
============

Just some functions I use to help bulk download and summarize data with ['rnoaa'](https://github.com/ropensci/rnoaa)

Installation
------------

There are no plans to release to CRAN. You can install from Github via

``` r
install.packages("remotes")
remotes::install_github("mps9506/rnoaahelpers")
```

Example
-------

You will need an API key to use the function below. Visit <https://github.com/ropensci/rnoaa> to see how to access and store your key.

*Download multiple years of NCDC data*:

``` r
library(rnoaahelpers)

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2018-12-31")
station_id <- "GHCND:USC00419101"
data_type_id <- "PRCP"
token <- Sys.getenv("noaakey")

df <- download_ncdc(start_date, end_date, station_id, data_type_id, token)
df
```

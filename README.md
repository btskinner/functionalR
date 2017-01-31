# Functional programming with R/Rcpp

Short script to show the basics of functional programming in R and Rcpp. Presented during the Vanderbilt University LPO Quantitative Methods Workshop on 31 January 2017.

### Required R libraries

* `dplyr`
* `ggplot2`
* `microbenchmark`
* `readr`
* `Rcpp`


### Contents

|File|Description|Size|
|:--|:--|:--|
|`fp_main.R`|Primary work script|14 KB|
|`dist_func.R`|Distance functions in base R sourced in `fp_main.R`|3 KB|
|`dist_func.cpp`|Distance functions in Rcpp-flavored C++ sourced in `fp_main.R`|3 KB|
|`college_loc.csv`|Lon/lat of US colleges in 2015|530 KB|
|`cbg_loc.csv`|Lon/lat of Census block groups from 2010 US Census|8.4 MB|

################################################################################
##
## <PROJ> Functional Programming with R/Rcpp
## <FILE> fp_hp.R
## <AUTH> Benjamin Skinner
## <INIT> 5 January 2017
##
################################################################################

## libraries
libs <- c('dplyr', 'ggplot2', 'microbenchmark', 'readr', 'Rcpp')

## install if you don't have
## lapply(libs, install.packages)

## load
lapply(libs, require, character.only = TRUE)

## =============================================================================
## SIMPLE FUNCTIONS
## =============================================================================

## simple useless function that returns argument
return_argument <- function(argument = 'default') {
  return(argument)
}

## check various arguments
return_argument('Hello, World!')        # I: string --> O: string
return_argument()                       # I: nothing --> O: string (default)
return_argument(TRUE)                   # I: bool --> O: bool
return_argument(100)                    # I: numeric --> O: numeric
return_argument(100^2)                  # I: expression --> O: expression
return_argument('100^2')                # ...but now a string

## more than one argument
look_ma <- function(arg_1 = 'a', arg_2 = 'b', arg_3 = 'c') {
    return(paste('I can say:', arg_1, arg_2, arg_3))
}

## check order
look_ma()                                      # defaults
look_ma('c','b','a')                           # using order to define
look_ma(arg_3 = 'c', arg_2 = 'b', arg_1 = 'a') # same as default
look_ma(arg_3 = 'z')                           # mix of defaults and new
look_ma(arg_3 = 'z', '1', '2')                 # use explicit definition
look_ma('1', arg_1 = 'z')                      # ...to futz with order

## TAKEAWAY --------------------------------------------------------------------
##
## (1) R functions take arguments and return results (usually).
## (2) Arguments can have default values.
## (3) Call function by: function_name(...), where ... are arguments.
## (4) Unless function requires it, don't need to tell R what data types
##     to expect as arguments or output ("loosely/weakly/dynamically" typed).
## (5) Multiple arguments are taken in order unless explicitly defined.
##
## -----------------------------------------------------------------------------

## -----------------------------------------------
## COMMON TASK: FIX MISSING
## -----------------------------------------------

## create tbl_df with around 10% missing values (-99) in three columns
df <- tbl_df(data.frame('id' = 1:100,
                        'age' = sample(c(seq(11,20,1), -99),
                                       size = 100,
                                       replace = TRUE,
                                       prob = c(rep(.09, 10), .1)),
                        'sibage' = sample(c(seq(5,12,1), -99),
                                          size = 100,
                                          replace = TRUE,
                                          prob = c(rep(.115, 8), .08)),
                        'parage' = sample(c(seq(45,55,1), -99),
                                          size = 100,
                                          replace = TRUE,
                                          prob = c(rep(.085, 11), .12))
                        )
             )

## table of values
table(df$age)
table(df$sibage)
table(df$parage)

## could fix manually...
df_mfix <- df
df_mfix$age[df_mfix$age == -99] <- NA
df_mfix$sibage[df_mfix$sibage == -99] <- NA
df_mfix$parage[df_mfix$parage == -99] <- NA

## ...or better, make a function to replace missing with NA
fix_missing <- function(x, missval) {
    x[x == missval] <- NA
    return(x)
}

## fix missing ages and check
df$age <- fix_missing(df$age, -99)
table(df$age, useNA = 'ifany')

## fix all columns using dplyr
df_fix <- df %>%
    mutate_each(funs(fix_missing(., -99)))

## fix all columns using lapply
## NB: using [] means subset, which retains data.frame class
df[] <- lapply(df, FUN = function(x) { fix_missing(x, -99) })

## TAKEAWAY --------------------------------------------------------------------
##
## (1) If you have a repeated task, make a function.
## (2) Have the function do one thing well.
## (3) Can embed one function in another if you need to repeat.
##
## -----------------------------------------------------------------------------

## -----------------------------------------------
## SPEED: SUMMING INTEGERS
## -----------------------------------------------

## sum function: loop
sum_loop <- function(x) {
    n <- length(x)
    total <- 0
    for (i in 1:n) {
        total <- total + x[i]
    }
    return(total)
}

## sum function: (first + last) * middle value(s)
sum_trick <- function(x) {
    n <- length(x)
    m <- n / 2
    if (m %% 2 == 0) {
        return((x[1] + x[n]) * x[m])
    } else {
        return((x[1] + x[n]) * ((x[m] + x[m + 1]) / 2))
    }
}

## get even and odd length sequences
x <- seq(1,100,1)
y <- seq(1,101,1)

## compare even length sequences
sum(x)
sum_loop(x)
sum_trick(x)

## ...and now odd length sequences
sum(y)
sum_loop(y)
sum_trick(y)

## compare speeds, including base R sum() function
(tm_sum <- microbenchmark(sum_loop = sum_loop(x),
                          sum_trick = sum_trick(x),
                          sum = sum(x),
                          times = 1000))

autoplot(tm_sum)

## get even and odd length sequences: really long
x_long <- seq(1,1000000,1)
y_long <- seq(1,1000001,1)

## compare speeds
(tm_sum_long <- microbenchmark(sum_loop = sum_loop(x_long),
                               sum_trick = sum_trick(x_long),
                               sum = sum(x_long),
                               times = 100))

autoplot(tm_sum_long)

## confirm that answers are the same
identical(sum_loop(x_long), sum_trick(x_long), sum(x_long))
identical(sum_loop(y_long), sum_trick(y_long), sum(y_long))

## BUT...keep in mind the limits of your function:

## (A) trick only works with positive consecutive integers
x_decimal <- abs(rnorm(1000))
x_negative <- seq(-50,49,1)
x_skip <- seq(2,200,2)

identical(sum(x_decimal), sum_trick(x_decimal))
identical(sum(x_negative), sum_trick(x_negative))
identical(sum(x_skip), sum_trick(x_skip))

## (B) trick doesn't account for missing
x_missing <- x
x_missing[10] <- NA

sum(x_missing, na.rm = TRUE)            # correct!
sum_trick(x_missing)                    # incorrect!

## TAKEAWAY --------------------------------------------------------------------
##
## (1) For standard functions, you probably won't beat the base R version,
##     which is usually a call to an optimized C/C++ function.
## (2) For special problems, however, you may be able to write a better
##     algorithm that improves performance...
## (3) ...but be aware that base R functions may be slower in part b/c they
##     are robust to a variety of situations (e.g., missing)
##
## -----------------------------------------------------------------------------

## =============================================================================
## COMPLEX FUNCTIONS: GREAT CIRCLE (GC) DISTANCE
## =============================================================================

## -----------------------------------------------
## SOURCE FUNCTIONS FROM EXTERNAL FILE
## -----------------------------------------------

## Why source?
##
## (1) Keep file from becoming overly long.
## (2) Easily reuse common functions in different projects.

## source base R functions for distance
source('./dist_func.R')

## read in location data: 2015 colleges and 2010 census block groups
col_df <- read_csv('./college_loc.csv')
cbg_df <- read_csv('./cbg_loc.csv')

## -----------------------------------------------
## GC DISTANCE BETWEEN TWO POINTS
## -----------------------------------------------

## store first census block group point (x) and first college point (y)
(xlon <- cbg_df[[1, 'lon']])
(xlat <- cbg_df[[1, 'lat']])
(ylon <- col_df[[1, 'lon']])
(ylat <- col_df[[1, 'lat']])

## test single distance function
(d <- dist_haversine(xlon, xlat, ylon, ylat))

## -----------------------------------------------
## GC DISTANCE BETWEEN MANY POINTS
## -----------------------------------------------

## test matrix (limit to only 10 starting points)
distmat <- dist_mtom(cbg_df$lon[1:10], cbg_df$lat[1:10],
                     col_df$lon, col_df$lat,
                     cbg_df$fips11[1:10], col_df$unitid)

distmat[1:5,1:5]

## -----------------------------------------------
## MINIMUM GC DISTANCE BETWEEN MANY POINTS
## -----------------------------------------------

## option (1): use dist_mtom and then compute post hoc
min_distmat <- cbind(row.names(distmat),
                     apply(distmat, 1, FUN = function(x) names(which.min(x))),
                     apply(distmat, 1, min))
rownames(min_distmat) <- NULL
colnames(min_distmat) <- c('fips11', 'unitid', 'meters')
min_distmat

## option (2): new function!

## test matrix (limit to only 10 starting points)
mindf <- dist_min(cbg_df$lon[1:10], cbg_df$lat[1:10],
                  col_df$lon, col_df$lat,
                  cbg_df$fips11[1:10], col_df$unitid)
mindf

## -----------------------------------------------
## GC DISTANCE FUNCTIONS USING RCPP
## -----------------------------------------------

## compile C++ code and make functions available
sourceCpp('./dist_func.cpp', rebuild = TRUE)

## single distance
(d_Rcpp <- dist_haversine_(xlon, xlat, ylon, ylat))
identical(d, d_Rcpp)

## many to many matrix
distmat_Rcpp <- dist_mtom_(cbg_df$lon[1:10], cbg_df$lat[1:10],
                           col_df$lon, col_df$lat,
                           cbg_df$fips11[1:10], col_df$unitid)

distmat_Rcpp[1:5,1:5]
all.equal(distmat, distmat_Rcpp)

## minimum distance
mindf_Rcpp <- dist_min_(cbg_df$lon[1:10], cbg_df$lat[1:10],
                        col_df$lon, col_df$lat,
                        cbg_df$fips11[1:10], col_df$unitid)
mindf_Rcpp
all.equal(mindf, mindf_Rcpp)

## -----------------------------------------------
## BENCHMARKS!
## -----------------------------------------------

## ---------------------------
## SINGLE
## ---------------------------

tm_single <- microbenchmark(base_R = dist_haversine(xlon, xlat, ylon, ylat),
                            Rcpp = dist_haversine_(xlon, xlat, ylon, ylat),
                            times = 1000)

tm_single

## plot
autoplot(tm_single)

## ---------------------------
## MANY TO MANY
## ---------------------------

## time for base R to do many to many with 100 starting points
system.time(dist_mtom(cbg_df$lon[1:100], cbg_df$lat[1:100],
                      col_df$lon, col_df$lat,
                      cbg_df$fips11[1:100], col_df$unitid))

## ...and now Rcpp version
system.time(dist_mtom_(cbg_df$lon[1:100], cbg_df$lat[1:100],
                       col_df$lon, col_df$lat,
                       cbg_df$fips11[1:100], col_df$unitid))

## compare just 10 many to many
tm_mtom <- microbenchmark(base_R = dist_mtom(cbg_df$lon[1:10], cbg_df$lat[1:10],
                                             col_df$lon, col_df$lat,
                                             cbg_df$fips11[1:10], col_df$unitid),
                          Rcpp = dist_mtom_(cbg_df$lon[1:10], cbg_df$lat[1:10],
                                            col_df$lon, col_df$lat,
                                            cbg_df$fips11[1:10], col_df$unitid),
                          times = 100)

tm_mtom

## plot
autoplot(tm_mtom)

## ---------------------------
## MINIMUM
## ---------------------------

## time for base R to do many to many with 100 starting points
system.time(dist_min(cbg_df$lon[1:100], cbg_df$lat[1:100],
                     col_df$lon, col_df$lat,
                     cbg_df$fips11[1:100], col_df$unitid))

## ...and now Rcpp version
system.time(dist_min_(cbg_df$lon[1:100], cbg_df$lat[1:100],
                      col_df$lon, col_df$lat,
                      cbg_df$fips11[1:100], col_df$unitid))

## compare just 10 min
tm_min <- microbenchmark(base_R = dist_min(cbg_df$lon[1:10], cbg_df$lat[1:10],
                                           col_df$lon, col_df$lat,
                                           cbg_df$fips11[1:10], col_df$unitid),
                         Rcpp = dist_min_(cbg_df$lon[1:10], cbg_df$lat[1:10],
                                          col_df$lon, col_df$lat,
                                          cbg_df$fips11[1:10], col_df$unitid),
                         times = 100)

tm_min

## plot
autoplot(tm_min)

## ---------------------------
## FULL RUN FOR RCPP
## ---------------------------

## find minimum
system.time(full_min <- dist_min_(cbg_df$lon, cbg_df$lat,
                                  col_df$lon, col_df$lat,
                                  cbg_df$fips11, col_df$unitid))

full_min %>% tbl_df()

## build full matrix (at your own risk! really big!)
## system.time(full_dist <- dist_mtom_(cbg_df$lon, cbg_df$lat,
##                                     col_df$lon, col_df$lat,
##                                     cbg_df$fips11, col_df$unitid))

## TAKEAWAY --------------------------------------------------------------------
##
## (1) Compiled languages can be MUCH faster than base R, even when the
##     underlying code is basically the same.
## (2) Compiled language requires stronger typing.
## (3) Rcpp serves as a bridge between R and pure C++.
## (4) Take compilation time into account (is it worth conversion?).
##
## -----------------------------------------------------------------------------

## =============================================================================
## END SCRIPT
################################################################################

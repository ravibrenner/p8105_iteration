Iteration and Listcols
================
Ravi Brenner
2024-10-29

``` r
library(tidyverse)
library(rvest)
set.seed(1)
```

## some lists

``` r
l <- list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.26550866 0.37212390 0.57285336 0.90820779 0.20168193 0.89838968
    ##   [7] 0.94467527 0.66079779 0.62911404 0.06178627 0.20597457 0.17655675
    ##  [13] 0.68702285 0.38410372 0.76984142 0.49769924 0.71761851 0.99190609
    ##  [19] 0.38003518 0.77744522 0.93470523 0.21214252 0.65167377 0.12555510
    ##  [25] 0.26722067 0.38611409 0.01339033 0.38238796 0.86969085 0.34034900
    ##  [31] 0.48208012 0.59956583 0.49354131 0.18621760 0.82737332 0.66846674
    ##  [37] 0.79423986 0.10794363 0.72371095 0.41127443 0.82094629 0.64706019
    ##  [43] 0.78293276 0.55303631 0.52971958 0.78935623 0.02333120 0.47723007
    ##  [49] 0.73231374 0.69273156 0.47761962 0.86120948 0.43809711 0.24479728
    ##  [55] 0.07067905 0.09946616 0.31627171 0.51863426 0.66200508 0.40683019
    ##  [61] 0.91287592 0.29360337 0.45906573 0.33239467 0.65087047 0.25801678
    ##  [67] 0.47854525 0.76631067 0.08424691 0.87532133 0.33907294 0.83944035
    ##  [73] 0.34668349 0.33377493 0.47635125 0.89219834 0.86433947 0.38998954
    ##  [79] 0.77732070 0.96061800 0.43465948 0.71251468 0.39999437 0.32535215
    ##  [85] 0.75708715 0.20269226 0.71112122 0.12169192 0.24548851 0.14330438
    ##  [91] 0.23962942 0.05893438 0.64228826 0.87626921 0.77891468 0.79730883
    ##  [97] 0.45527445 0.41008408 0.81087024 0.60493329
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.70816 -0.03532 -0.01783  0.68933  3.81028

``` r
#ways to access elements of the list
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[[3]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

make a list that’s a bit more useful

``` r
list_norm = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )

list_norm[["b"]]
```

    ##  [1]  8.27759611 -0.09981563  3.38198620  5.27474118 12.59463169 -0.79271764
    ##  [7] -4.02155131 -5.22804711  6.77868593  3.69940404  7.86043152  3.29580306
    ## [13]  5.96546963  5.12109287  4.11770993  0.88518670 10.31004690  1.97112979
    ## [19]  7.33381886  4.82319577

reuse a function we wrote last time

``` r
mean_and_sd <- function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for vectors of length 1")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x)
}
```

use the function to take mean and sd of all samples

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.08  4.46

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.847  10.0

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.119  8.86

## use a for loop

Could turn this into a loop (especially if we have a much larger list)

create output list, and run a for loop

``` r
output <- vector("list", length = 4)

for (i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}
```

And better yet, turn that loop into a `map` function

``` r
output = map(list_norm, mean_and_sd)
```

try a few other things

can use any function

``` r
output = map(list_norm, IQR)
output
```

    ## $a
    ## [1] 8.73998
    ## 
    ## $b
    ## [1] 5.217825
    ## 
    ## $c
    ## [1] 10.5479
    ## 
    ## $d
    ## [1] 8.754516

can map_TO specific things

``` r
output = map_dbl(list_norm, IQR)
output
```

    ##         a         b         c         d 
    ##  8.739980  5.217825 10.547902  8.754516

``` r
output = map_dfr(list_norm, mean_and_sd)
output
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37
    ## 2  4.08   4.46
    ## 3  0.847 10.0 
    ## 4  0.119  8.86

## List columns

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

``` r
listcol_df <- tibble(
  name = c("a","b","c","d"),
  samp = list_norm
)

listcol_df 
```

    ## # A tibble: 4 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>  
    ## 3 c     <dbl [20]>  
    ## 4 d     <dbl [20]>

``` r
listcol_df |>
  filter(name %in% c("a","b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df |>
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

extract one in a sort of naive way

``` r
listcol_df[["samp"]][["a"]]
```

    ##  [1]   0.2099938   1.1171116  -5.0523254  12.0061105   4.0098090  -1.2560398
    ##  [7]   6.0644469  -3.1362904   8.5557925  -1.9718678 -11.6074543   6.8205960
    ## [13]   5.6611457  -3.8715816  -7.0518748  -9.1726379  -1.3450677  -9.1696429
    ## [19]  -4.0723401   0.8178606

compute mean and sd

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37

``` r
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.622  6.37
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.08  4.46
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.847  10.0
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.119  8.86

Better to create a new column that has these results (i.e. a list
column)

``` r
listcol_df |>
  mutate(output = map(samp, mean_and_sd),
         iqr = map_dbl(samp, IQR)) |>
  select(-samp) |>
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -0.622  6.37  8.74
    ## 2 b      4.08   4.46  5.22
    ## 3 c      0.847 10.0  10.5 
    ## 4 d      0.119  8.86  8.75

## NSDUH data example

``` r
nsduh_table_format = function(html, table_num) {
  
  out_table = 
    html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    select(-contains("P Value"))
  
  return(out_table)
  
}
```

now need to import the html, then extract correct tables

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html, table_num = 1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_df = tibble(
  drug = c("Marijuana","Cocaine","Heroin"),
  table_n = c(1,4,5)) |>
  mutate(table = map(table_n, nsduh_table_format, html = nsduh_html)) |>
  unnest(table)

# a slightly different way to use the map function
nsduh_df = tibble(
  drug = c("Marijuana","Cocaine","Heroin"),
  table_n = c(1,4,5)) |>
  mutate(table = map(table_n, \(x) nsduh_table_format(html = nsduh_html, table_num = x))) |>
  unnest(table)

nsduh_df |>
  filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug      table_n State   `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>       <dbl> <chr>   <chr>            <chr>            <chr>             
    ## 1 Marijuana       1 New Yo… 14.24b           15.04            13.94             
    ## 2 Cocaine         4 New Yo… 2.28             2.54             0.71              
    ## 3 Heroin          5 New Yo… 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>

## weather data example

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/ravibrenner/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:17:27.971396 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/ravibrenner/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:17:32.413103 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/ravibrenner/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:17:33.925392 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

create a list column

``` r
weather_nest <- 
  weather_df |>
  nest(data = date:tmin)
```

``` r
weather_nest[["data"]][1]
```

    ## [[1]]
    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

try regressing tmax on tmin

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

But the better way to do this

``` r
weather_nest |>
  mutate(model_fit = map(data, \(x) lm(tmax ~ tmin, data = x)),
         model_stats = map(model_fit, broom::tidy))  |>
  unnest(model_stats)
```

    ## # A tibble: 6 × 9
    ##   name     id    data     model_fit term  estimate std.error statistic   p.value
    ##   <chr>    <chr> <list>   <list>    <chr>    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 Central… USW0… <tibble> <lm>      (Int…    7.51     0.160       47.0 1.46e-222
    ## 2 Central… USW0… <tibble> <lm>      tmin     1.03     0.0119      86.9 0        
    ## 3 Molokai… USW0… <tibble> <lm>      (Int…   21.8      0.516       42.2 1.35e-197
    ## 4 Molokai… USW0… <tibble> <lm>      tmin     0.322    0.0251      12.8 4.72e- 34
    ## 5 Waterho… USS0… <tibble> <lm>      (Int…    7.53     0.110       68.6 6.37e-316
    ## 6 Waterho… USS0… <tibble> <lm>      tmin     1.14     0.0180      63.3 1.37e-294

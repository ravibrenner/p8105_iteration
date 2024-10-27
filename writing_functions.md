Writing Funcitons
================
Ravi Brenner
2024-10-27

``` r
library(tidyverse)
library(rvest)
set.seed(1)
```

## Basic function

First, very basic function. Standardize normal distribution / create Z
scores

``` r
x_vec <- rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

``` r
z_scores <- function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for vectors of length 1")
  }
  z = (x - mean(x)) / sd(x)
  z
}

z_scores(x_vec)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872 -1.04107494
    ##  [7]  0.33550276  0.59957343  0.42849461 -0.49894708  1.41364561  0.23279252
    ## [13] -0.83138529 -2.50852027  1.00648110 -0.22481531 -0.19456260  0.81587675
    ## [19]  0.68682298  0.44756609  0.78971253  0.64568566 -0.09904161 -2.27133861
    ## [25]  0.47485186

## Multiple outputs

List

``` r
mean_and_sd <- function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for vectors of length 1")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  
  list(mean = mean_x,
       sd = sd_x)
}

mean_and_sd(x_vec)
```

    ## $mean
    ## [1] 5.505996
    ## 
    ## $sd
    ## [1] 2.850324

or store in dataframe

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

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.51  2.85

## multiple inputs

``` r
sim_data <- tibble(
  x = rnorm(30,mean = 2, sd = 3)
)

sim_data |>
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.12      2.22

Create function

``` r
sim_mean_sd <- function(n, mu = 2, sigma = 3) {
  
  sim_data <- tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )
  
  sim_data |>
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}

sim_mean_sd(n = 300)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.14      2.97

## worked example

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything())
```

Learning Assessment: Try to write a function that can be used to
abstract the data loading and cleaning process. Use this function to
recreate the tidied LoTR dataset.

``` r
lotr_load_and_tidy <- function(path, range, movie_name) {
  df <- readxl::read_excel(path, range = range) |>
    mutate(movie = movie_name) |>
    janitor::clean_names() |>
    pivot_longer(female:male, 
                 names_to = "sex", 
                 values_to = "words") |>
    mutate(race = str_to_lower(race)) |>
    select(movie, everything())
  
  df
}

lotr_tidy <- lotr_load_and_tidy("./data/LotR_Words.xlsx","B3:D6",
                               "fellowship_ring") |>
  bind_rows(lotr_load_and_tidy("./data/LotR_Words.xlsx","F3:H6",
                               "two_towers")) |>
  bind_rows(lotr_load_and_tidy("./data/LotR_Words.xlsx","J3:L6",
                               "return_king"))
```

Another worked example, reading data from web

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

data_marj = 
  nsduh_html |> 
  html_table() |> 
  nth(1) |>
  slice(-1) |> 
  select(-contains("P Value")) |>
  pivot_longer(
    -State,
    names_to = "age_year", 
    values_to = "percent") |>
  separate(age_year, into = c("age", "year"), sep = "\\(") |>
  mutate(
    year = str_replace(year, "\\)", ""),
    percent = str_replace(percent, "[a-c]$", ""),
    percent = as.numeric(percent)) |>
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
```

``` r
nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
  
  table
  
}

nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )
```

## functions as arguments

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 0.9547719

``` r
my_summary(x_vec, IQR)
```

    ## [1] 0.9134571

``` r
my_summary(x_vec, var)
```

    ## [1] 0.9115895

A tricky example: what should happen here?

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

How could this be improved?

``` r
f = function(x,y) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y, y = y)
```

    ## [1] 4

These outputs are the same, but the 2nd one is clearer for sure!

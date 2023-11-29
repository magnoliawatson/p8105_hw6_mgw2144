hw6
================
2023-11-29

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
set.seed(1)
```

# Problem 1

# Problem 2

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2022-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

``` r
lm(tmax ~ tmin + prcp, data = weather_df)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin + prcp, data = weather_df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin         prcp  
    ##    8.042803     1.013386    -0.001541

``` r
boot_sample = function(df) {
  sample_frac(df, replace=TRUE)
}

boot_straps = 
  tibble(strap_number = 1:5000) %>% 
  mutate(
    strap_sample = map(strap_number, ~boot_sample(df = weather_df))
  )

boot_straps %>% 
  slice(1:3) %>% 
  mutate(strap_sample = map(strap_sample, arrange, tmax)) %>%  
  pull(strap_sample)
```

    ## [[1]]
    ## # A tibble: 365 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2022-12-24     0  -9.3 -13.8
    ##  2 CentralPark_NY USW00094728 2022-12-24     0  -9.3 -13.8
    ##  3 CentralPark_NY USW00094728 2022-01-15     0  -6   -12.1
    ##  4 CentralPark_NY USW00094728 2022-01-21     0  -5.5  -9.9
    ##  5 CentralPark_NY USW00094728 2022-02-14     0  -3.8  -8.8
    ##  6 CentralPark_NY USW00094728 2022-02-14     0  -3.8  -8.8
    ##  7 CentralPark_NY USW00094728 2022-02-05     0  -2.7  -7.1
    ##  8 CentralPark_NY USW00094728 2022-12-25     0  -2.1  -9.9
    ##  9 CentralPark_NY USW00094728 2022-12-25     0  -2.1  -9.9
    ## 10 CentralPark_NY USW00094728 2022-01-22     0  -1.6 -10.5
    ## # ℹ 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2022-01-11     0  -4.3  -9.3
    ##  2 CentralPark_NY USW00094728 2022-01-11     0  -4.3  -9.3
    ##  3 CentralPark_NY USW00094728 2022-02-14     0  -3.8  -8.8
    ##  4 CentralPark_NY USW00094728 2022-01-30     0  -3.2 -11  
    ##  5 CentralPark_NY USW00094728 2022-02-05     0  -2.7  -7.1
    ##  6 CentralPark_NY USW00094728 2022-02-05     0  -2.7  -7.1
    ##  7 CentralPark_NY USW00094728 2022-12-26     0  -1.6  -7.7
    ##  8 CentralPark_NY USW00094728 2022-01-08     0  -1    -6.6
    ##  9 CentralPark_NY USW00094728 2022-01-08     0  -1    -6.6
    ## 10 CentralPark_NY USW00094728 2022-01-31     0  -0.5  -6  
    ## # ℹ 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2022-01-21     0  -5.5  -9.9
    ##  2 CentralPark_NY USW00094728 2022-01-11     0  -4.3  -9.3
    ##  3 CentralPark_NY USW00094728 2022-02-14     0  -3.8  -8.8
    ##  4 CentralPark_NY USW00094728 2022-02-14     0  -3.8  -8.8
    ##  5 CentralPark_NY USW00094728 2022-02-05     0  -2.7  -7.1
    ##  6 CentralPark_NY USW00094728 2022-01-26     0  -2.1  -6.6
    ##  7 CentralPark_NY USW00094728 2022-01-29   196  -1.6 -11  
    ##  8 CentralPark_NY USW00094728 2022-01-22     0  -1.6 -10.5
    ##  9 CentralPark_NY USW00094728 2022-01-08     0  -1    -6.6
    ## 10 CentralPark_NY USW00094728 2022-02-15     0  -1    -8.8
    ## # ℹ 355 more rows

``` r
bootstrap_results = 
  boot_straps %>% 
  mutate(
    models = map(strap_sample, ~lm(tmax ~ tmin + prcp, data = .)
  ), 
    results = map(models, broom::tidy),
    r_squared = map_dbl(models, ~broom::glance(.x)$r.squared),
    log_beta = map_dbl(models, ~ {
      coef <- coefficients(.x)
      if (coef[2] * coef[3] >= 0) log(coef[2] * coef[3]) else NA_real_
    })
  ) %>% 
  select(-strap_sample, -models) %>% 
  unnest(results)
```

## Plotting the distributions of estimates for r_squared and log_beta

``` r
estimate_plot_rs = 
bootstrap_results %>% 
  ggplot(aes(x = r_squared)) + geom_density()

estimate_plot_rs
```

![](hw6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The distribution of the r sqaured estimates appears approximately normal
but may have a slight left skew. This slight skew is likely due to
random sampling variability.

``` r
estimate_plot_log = 
bootstrap_results %>% 
  ggplot(aes(x = log_beta)) + geom_density()

estimate_plot_log
```

    ## Warning: Removed 10083 rows containing non-finite values (`stat_density()`).

![](hw6_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The distribution of the log(beta_1\*beta_2) estimates is strongly left
skewed. This may be due to the fact that there were several negative
beta estimates that could not be used in the log calculations,
indicating that this may not be representative of the dataset the
bootstrap was performed on.

## Identifying 95% Confidence Intervals

``` r
bootstrap_results %>%  
  group_by(term) %>% 
  summarize(
    ci_lower = quantile(r_squared, 0.025), 
    ci_upper = quantile(r_squared, 0.975))
```

    ## # A tibble: 3 × 3
    ##   term        ci_lower ci_upper
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)    0.889    0.941
    ## 2 prcp           0.889    0.941
    ## 3 tmin           0.889    0.941

We are 95% confident that the true r-squared value is between 0.889 and
0.941.

``` r
bootstrap_results %>%  
  group_by(term) %>% 
  summarize(
    ci_lower = quantile(log_beta, 0.025, na.rm = TRUE), 
    ci_upper = quantile(log_beta, 0.975, na.rm = TRUE))
```

    ## # A tibble: 3 × 3
    ##   term        ci_lower ci_upper
    ##   <chr>          <dbl>    <dbl>
    ## 1 (Intercept)    -8.98    -4.60
    ## 2 prcp           -8.98    -4.60
    ## 3 tmin           -8.98    -4.60

We are 95% confident that the true log(beta_1\*beta_2) value is between
-8.98 and -4.60.

# Problem 3

## tidying the data

``` r
birthweight_df = 
  read_csv("data/birthweight.csv") %>% 
  janitor::clean_names() %>% 
  mutate(across(c(babysex, frace, malform, mrace), as_factor))
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
missing_val_check = 
  birthweight_df %>% 
  summarise_all(~sum(is.na(.)))
```

Propose a regression model for birthweight. This model may be based on a
hypothesized structure for the factors that underly birthweight, on a
data-driven model-building process, or a combination of the two.
Describe your modeling process and show a plot of model residuals
against fitted values – use add_predictions and add_residuals in making
this plot.

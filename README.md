Introduction to tqr package
================
Mitsuo Shiota
2019-07-19

-   [tqr: add-on to tsibble, inspired by
    tidyquant](#tqr-add-on-to-tsibble-inspired-by-tidyquant)
    -   [Installation](#installation)
    -   [Libraries](#libraries)
    -   [Transform a tsibble into lower
        frequency](#transform-a-tsibble-into-lower-frequency)
    -   [Time-wise functions to transform a numeric vector are useful
        and
        flexible](#time-wise-functions-to-transform-a-numeric-vector-are-useful-and-flexible)
    -   [cal\_time\_wise: function factory to produce a function to
        transform a
        tsibble](#cal_time_wise-function-factory-to-produce-a-function-to-transform-a-tsibble)
    -   [tq\_diff: calculate
        differences](#tq_diff-calculate-differences)
    -   [“order\_by” argument in time-wise functions to transform a
        numeric
        vector](#order_by-argument-in-time-wise-functions-to-transform-a-numeric-vector)
    -   [moving\_average and tq\_ma: calculate moving
        averages](#moving_average-and-tq_ma-calculate-moving-averages)
    -   [growth\_rate and tq\_gr: calculate growth
        rates](#growth_rate-and-tq_gr-calculate-growth-rates)
    -   [season\_adjust and tq\_sa: calculate seasonally adjusted
        values](#season_adjust-and-tq_sa-calculate-seasonally-adjusted-values)

<!-- badges: start -->

[![R-CMD-check](https://github.com/mitsuoxv/tqr/workflows/R-CMD-check/badge.svg)](https://github.com/mitsuoxv/tqr/actions)
<!-- badges: end -->

Updated: 2021-09-04

# tqr: add-on to tsibble, inspired by tidyquant

When I utilize [`tidyquant`
package](https://cran.r-project.org/web/packages/tidyquant/readme/README.html)
for economic analyses, I often find myself writing similar functions
repeatedly. I also find the idea of time aware dataframe of [`tsibble`
package](https://cran.r-project.org/web/packages/tsibble/index.html) may
help me deal with time series data more confidently. So I have decided
to build this package to facilitate my work.

As of version 0.0.0.9011, I have added time-wise functions to transform
a numeric vector, and a function factory using those time-wise
functions, rewritten functions to transform a tsibble using that
function factory, and deprecated old function factories.

1.  Time-wise functions to transform a numeric vector

-   moving\_average: transform into moving averages
-   growth\_rate: transform into growth rates
-   season\_adjust: transform into seasonally adjusted values

2.  A function factory to produce functions to transform a tsibble

-   cal\_time\_wise: produce functions using time\_wise vector functions

3.  Functions to transform a tsibble

-   tq\_diff: transform numeric columns into differences
-   tq\_ma: transform numeric columns into moving averages
-   tq\_gr: transform numeric columns into growth rates
-   tq\_sa: transform numeric columns into seasonally adjusted values

4.  Deprecated function factories

-   cal\_factory: produce functions
-   cal\_factory\_zoo: produce functions utilizing zoo package
-   cal\_factory\_ts: produce functions utilizing ts class
-   cal\_factory\_xts: produce functions utilizing xts package

## Installation

This package is only for my own use for now. “You” means “future me”.

You can install the development version with:

``` r
remotes::install_github("mitsuoxv/tqr")
```

## Libraries

As I read [Hyndman, R.J., & Athanasopoulos, G. (2021) Forecasting:
principles and practice, 3rd edition, OTexts: Melbourne, Australia.
OTexts.com/fpp3.](https://otexts.com/fpp3/), I use `fpp3` meta package
here.

``` r
library(fpp3)
library(tqr)
```

## Transform a tsibble into lower frequency

If you are familiar with tsibble functions, like `index_by` and
`group_by_key`, you can convert to lower frequency.

``` r
aus_livestock
#> # A tsibble: 29,364 x 4 [1M]
#> # Key:       Animal, State [54]
#>       Month Animal                     State                        Count
#>       <mth> <fct>                      <fct>                        <dbl>
#>  1 1976 Jul Bulls, bullocks and steers Australian Capital Territory  2300
#>  2 1976 Aug Bulls, bullocks and steers Australian Capital Territory  2100
#>  3 1976 Sep Bulls, bullocks and steers Australian Capital Territory  2100
#>  4 1976 Oct Bulls, bullocks and steers Australian Capital Territory  1900
#>  5 1976 Nov Bulls, bullocks and steers Australian Capital Territory  2100
#>  6 1976 Dec Bulls, bullocks and steers Australian Capital Territory  1800
#>  7 1977 Jan Bulls, bullocks and steers Australian Capital Territory  1800
#>  8 1977 Feb Bulls, bullocks and steers Australian Capital Territory  1900
#>  9 1977 Mar Bulls, bullocks and steers Australian Capital Territory  2700
#> 10 1977 Apr Bulls, bullocks and steers Australian Capital Territory  2300
#> # … with 29,354 more rows

aus_livestock %>% 
  index_by(Quarter = yearquarter(Month)) %>% 
  group_by_key() %>% 
  summarize(Count = sum(Count))
#> # A tsibble: 9,788 x 4 [1Q]
#> # Key:       Animal, State [54]
#> # Groups:    Animal [7]
#>    Animal                     State                        Quarter Count
#>    <fct>                      <fct>                          <qtr> <dbl>
#>  1 Bulls, bullocks and steers Australian Capital Territory 1976 Q3  6500
#>  2 Bulls, bullocks and steers Australian Capital Territory 1976 Q4  5800
#>  3 Bulls, bullocks and steers Australian Capital Territory 1977 Q1  6400
#>  4 Bulls, bullocks and steers Australian Capital Territory 1977 Q2  7700
#>  5 Bulls, bullocks and steers Australian Capital Territory 1977 Q3  7000
#>  6 Bulls, bullocks and steers Australian Capital Territory 1977 Q4  6900
#>  7 Bulls, bullocks and steers Australian Capital Territory 1978 Q1  7800
#>  8 Bulls, bullocks and steers Australian Capital Territory 1978 Q2  8500
#>  9 Bulls, bullocks and steers Australian Capital Territory 1978 Q3  7900
#> 10 Bulls, bullocks and steers Australian Capital Territory 1978 Q4  7900
#> # … with 9,778 more rows
```

If you are familiar with dplyr and tidyselect functions, you can convert
multiple variables at once.

``` r
aus_production
#> # A tsibble: 218 x 7 [1Q]
#>    Quarter  Beer Tobacco Bricks Cement Electricity   Gas
#>      <qtr> <dbl>   <dbl>  <dbl>  <dbl>       <dbl> <dbl>
#>  1 1956 Q1   284    5225    189    465        3923     5
#>  2 1956 Q2   213    5178    204    532        4436     6
#>  3 1956 Q3   227    5297    208    561        4806     7
#>  4 1956 Q4   308    5681    197    570        4418     6
#>  5 1957 Q1   262    5577    187    529        4339     5
#>  6 1957 Q2   228    5651    214    604        4811     7
#>  7 1957 Q3   236    5317    227    603        5259     7
#>  8 1957 Q4   320    6152    222    582        4735     6
#>  9 1958 Q1   272    5758    199    554        4608     5
#> 10 1958 Q2   233    5641    229    620        5196     7
#> # … with 208 more rows

aus_production %>% 
  index_by(Year = year(Quarter)) %>% 
  summarize(across(!Quarter, sum))
#> # A tsibble: 55 x 7 [1Y]
#>     Year  Beer Tobacco Bricks Cement Electricity   Gas
#>    <dbl> <dbl>   <dbl>  <dbl>  <dbl>       <dbl> <dbl>
#>  1  1956  1032   21381    798   2128       17583    24
#>  2  1957  1046   22697    850   2318       19144    25
#>  3  1958  1055   23466    911   2457       20390    26
#>  4  1959  1052   23919    981   2617       22176    26
#>  5  1960  1084   24328   1077   2800       24240    29
#>  6  1961  1124   25088    995   2859       25204    27
#>  7  1962  1145   25117   1027   2934       27659    29
#>  8  1963  1190   25541   1115   3121       30639    30
#>  9  1964  1256   26396   1310   3625       34226    30
#> 10  1965  1311   26346   1364   3812       37056    30
#> # … with 45 more rows
```

## Time-wise functions to transform a numeric vector are useful and flexible

In the above examples, `sum` function transforms a numeric vector.

tsibble package provides `difference` function, which also transforms a
time-wise numeric vector. Using it, I can create a new variable “diff”.

``` r
aus_arrivals %>% 
  group_by_key() %>% 
  mutate(diff = difference(Arrivals)) %>% 
  ungroup()
#> # A tsibble: 508 x 4 [1Q]
#> # Key:       Origin [4]
#>    Quarter Origin Arrivals  diff
#>      <qtr> <chr>     <int> <int>
#>  1 1981 Q1 Japan     14763    NA
#>  2 1981 Q2 Japan      9321 -5442
#>  3 1981 Q3 Japan     10166   845
#>  4 1981 Q4 Japan     19509  9343
#>  5 1982 Q1 Japan     17117 -2392
#>  6 1982 Q2 Japan     10617 -6500
#>  7 1982 Q3 Japan     11737  1120
#>  8 1982 Q4 Japan     20961  9224
#>  9 1983 Q1 Japan     20671  -290
#> 10 1983 Q2 Japan     12235 -8436
#> # … with 498 more rows
```

Or I can transform a existing variable “Arrivals”. I can use
`difference` function flexibly.

``` r
aus_arrivals %>% 
  group_by_key() %>% 
  mutate(Arrivals = difference(Arrivals)) %>% 
  ungroup()
#> # A tsibble: 508 x 3 [1Q]
#> # Key:       Origin [4]
#>    Quarter Origin Arrivals
#>      <qtr> <chr>     <int>
#>  1 1981 Q1 Japan        NA
#>  2 1981 Q2 Japan     -5442
#>  3 1981 Q3 Japan       845
#>  4 1981 Q4 Japan      9343
#>  5 1982 Q1 Japan     -2392
#>  6 1982 Q2 Japan     -6500
#>  7 1982 Q3 Japan      1120
#>  8 1982 Q4 Japan      9224
#>  9 1983 Q1 Japan      -290
#> 10 1983 Q2 Japan     -8436
#> # … with 498 more rows
```

## cal\_time\_wise: function factory to produce a function to transform a tsibble

`cal_time_wise` function is functionized from the last example, and is a
function factory. It receives a time-wise function to transform a
numeric vector as its first argument, and it returns a function to
transform a tsibble.

## tq\_diff: calculate differences

`tq_diff` function is created like below. `tq_ma`, `tq_gr` and `tq_sa`
functions are also created by this function factory.

``` r
tq_diff <- cal_time_wise(
  tsibble::difference
)
```

The variables, which these created functions transform, are all numeric
variables which are not key or index variables.

In the below example, “Month” is the index variable, and “State” and
“Industry” are the key variables. “Series ID” is a character vector. So
`tq_diff` transforms just “Turnover”, a numeric variable.

``` r
aus_retail %>% 
  tq_diff()
#> # A tsibble: 64,532 x 5 [1M]
#> # Key:       State, Industry [152]
#>    State                        Industry           `Series ID`    Month Turnover
#>    <chr>                        <chr>              <chr>          <mth>    <dbl>
#>  1 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Apr   NA    
#>  2 Australian Capital Territory Cafes, restaurant… A3349849A   1982 May   -1    
#>  3 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Jun    0.200
#>  4 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Jul    0.4  
#>  5 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Aug   -0.4  
#>  6 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Sep    0.6  
#>  7 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Oct    0.600
#>  8 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Nov    0.600
#>  9 Australian Capital Territory Cafes, restaurant… A3349849A   1982 Dec    1.5  
#> 10 Australian Capital Territory Cafes, restaurant… A3349849A   1983 Jan   -3.1  
#> # … with 64,522 more rows
```

For arguments other than the first one, a tsibble, of `tq_diff`
function, look at `tsibble::difference` document. Note that `n` in the
previous version of `tq_diff` is changed to `lag`.

## “order\_by” argument in time-wise functions to transform a numeric vector

You can find “order\_by” argument in `tsibble::difference` document. It
is optional in `difference` function.

``` r
tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.

wrong <- mutate(scrambled, diff = difference(value))
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
arrange(wrong, year)
#> # A tsibble: 6 x 3 [1Y]
#>    year value  diff
#>   <int> <dbl> <dbl>
#> 1  2000     0    NA
#> 2  2001     1   -24
#> 3  2002     4     4
#> 4  2003     9     5
#> 5  2004    16    15
#> 6  2005    25    16

right <- mutate(scrambled, diff = difference(value, order_by = year))
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
arrange(right, year)
#> # A tsibble: 6 x 3 [1Y]
#>    year value  diff
#>   <int> <dbl> <dbl>
#> 1  2000     0    NA
#> 2  2001     1     1
#> 3  2002     4     3
#> 4  2003     9     5
#> 5  2004    16     7
#> 6  2005    25     9
```

`cal_time_wise` function assigns the index variable in a tsibble to
“order\_by” argument of `difference` function. As a result, `tq_diff`
function returns right even if the rows are reshuffled.

``` r
tsbl %>% 
  tq_diff()
#> # A tsibble: 6 x 2 [1Y]
#>    year value
#>   <int> <dbl>
#> 1  2000    NA
#> 2  2001     1
#> 3  2002     3
#> 4  2003     5
#> 5  2004     7
#> 6  2005     9

scrambled %>% 
  tq_diff() %>% 
  arrange(year)
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
#> # A tsibble: 6 x 2 [1Y]
#>    year value
#>   <int> <dbl>
#> 1  2000    NA
#> 2  2001     1
#> 3  2002     3
#> 4  2003     5
#> 5  2004     7
#> 6  2005     9
```

## moving\_average and tq\_ma: calculate moving averages

`moving_average` function calculates moving averages using `slider`
package. “n” specifies the window width. “order\_by” is optional, and
works in the same way as in `difference` function. For other arguments,
refer to its document.

``` r
aus_livestock %>% 
  group_by_key() %>% 
  mutate(ma3 = moving_average(Count, n = 3)) %>% 
  ungroup()
#> # A tsibble: 29,364 x 5 [1M]
#> # Key:       Animal, State [54]
#>       Month Animal                     State                        Count   ma3
#>       <mth> <fct>                      <fct>                        <dbl> <dbl>
#>  1 1976 Jul Bulls, bullocks and steers Australian Capital Territory  2300   NA 
#>  2 1976 Aug Bulls, bullocks and steers Australian Capital Territory  2100   NA 
#>  3 1976 Sep Bulls, bullocks and steers Australian Capital Territory  2100 2167.
#>  4 1976 Oct Bulls, bullocks and steers Australian Capital Territory  1900 2033.
#>  5 1976 Nov Bulls, bullocks and steers Australian Capital Territory  2100 2033.
#>  6 1976 Dec Bulls, bullocks and steers Australian Capital Territory  1800 1933.
#>  7 1977 Jan Bulls, bullocks and steers Australian Capital Territory  1800 1900 
#>  8 1977 Feb Bulls, bullocks and steers Australian Capital Territory  1900 1833.
#>  9 1977 Mar Bulls, bullocks and steers Australian Capital Territory  2700 2133.
#> 10 1977 Apr Bulls, bullocks and steers Australian Capital Territory  2300 2300 
#> # … with 29,354 more rows
```

`tq_ma` function transforms “Count” variable. For arguments other than
the first one, a tsibble, look at `moving_average` document.

``` r
aus_livestock %>% 
  tq_ma(n = 3)
#> # A tsibble: 29,364 x 4 [1M]
#> # Key:       Animal, State [54]
#>       Month Animal                     State                        Count
#>       <mth> <fct>                      <fct>                        <dbl>
#>  1 1976 Jul Bulls, bullocks and steers Australian Capital Territory   NA 
#>  2 1976 Aug Bulls, bullocks and steers Australian Capital Territory   NA 
#>  3 1976 Sep Bulls, bullocks and steers Australian Capital Territory 2167.
#>  4 1976 Oct Bulls, bullocks and steers Australian Capital Territory 2033.
#>  5 1976 Nov Bulls, bullocks and steers Australian Capital Territory 2033.
#>  6 1976 Dec Bulls, bullocks and steers Australian Capital Territory 1933.
#>  7 1977 Jan Bulls, bullocks and steers Australian Capital Territory 1900 
#>  8 1977 Feb Bulls, bullocks and steers Australian Capital Territory 1833.
#>  9 1977 Mar Bulls, bullocks and steers Australian Capital Territory 2133.
#> 10 1977 Apr Bulls, bullocks and steers Australian Capital Territory 2300 
#> # … with 29,354 more rows
```

## growth\_rate and tq\_gr: calculate growth rates

`growth_rate` function calculates growth rates. “n” specifies the lag.
“order\_by” is optional, and works in the same way as in `difference`
function. For other arguments, refer to its document.

``` r
aus_arrivals %>% 
  group_by_key() %>% 
  mutate(
    gr_yoy = growth_rate(Arrivals, n = 4),
    gr_annualized = growth_rate(Arrivals, n = 1, annualize = 4),
    gr_yoy_not_pct = growth_rate(Arrivals, n = 4, pct = FALSE)
    )
#> # A tsibble: 508 x 6 [1Q]
#> # Key:       Origin [4]
#> # Groups:    Origin [4]
#>    Quarter Origin Arrivals gr_yoy gr_annualized gr_yoy_not_pct
#>      <qtr> <chr>     <int>  <dbl>         <dbl>          <dbl>
#>  1 1981 Q1 Japan     14763  NA            NA           NA     
#>  2 1981 Q2 Japan      9321  NA           -84.1         NA     
#>  3 1981 Q3 Japan     10166  NA            41.5         NA     
#>  4 1981 Q4 Japan     19509  NA          1256.          NA     
#>  5 1982 Q1 Japan     17117  15.9         -40.7          0.159 
#>  6 1982 Q2 Japan     10617  13.9         -85.2          0.139 
#>  7 1982 Q3 Japan     11737  15.5          49.4          0.155 
#>  8 1982 Q4 Japan     20961   7.44        917.           0.0744
#>  9 1983 Q1 Japan     20671  20.8          -5.42         0.208 
#> 10 1983 Q2 Japan     12235  15.2         -87.7          0.152 
#> # … with 498 more rows
```

`tq_gr` function transforms “Arrivals” variable in this example. For
arguments other than the first one, a tsibble, look at `growth_rate`
document.

``` r
aus_arrivals %>% 
  tq_gr(n = 4)
#> # A tsibble: 508 x 3 [1Q]
#> # Key:       Origin [4]
#>    Quarter Origin Arrivals
#>      <qtr> <chr>     <dbl>
#>  1 1981 Q1 Japan     NA   
#>  2 1981 Q2 Japan     NA   
#>  3 1981 Q3 Japan     NA   
#>  4 1981 Q4 Japan     NA   
#>  5 1982 Q1 Japan     15.9 
#>  6 1982 Q2 Japan     13.9 
#>  7 1982 Q3 Japan     15.5 
#>  8 1982 Q4 Japan      7.44
#>  9 1983 Q1 Japan     20.8 
#> 10 1983 Q2 Japan     15.2 
#> # … with 498 more rows
```

## season\_adjust and tq\_sa: calculate seasonally adjusted values

`season_adjust` function calculates seasonally adjusted values using
`season` package. It requires not just “x” argument, a numeric vector,
but also “order\_by” argument, a time vector. For other arguments, refer
to `season` document.

``` r
aus_arrivals %>% 
  group_by_key() %>% 
  mutate(sa = season_adjust(Arrivals, Quarter)) %>% 
  ungroup()
#> # A tsibble: 508 x 4 [1Q]
#> # Key:       Origin [4]
#>    Quarter Origin Arrivals     sa
#>      <qtr> <chr>     <int>  <dbl>
#>  1 1981 Q1 Japan     14763 12179.
#>  2 1981 Q2 Japan      9321 12742.
#>  3 1981 Q3 Japan     10166 13107.
#>  4 1981 Q4 Japan     19509 14065.
#>  5 1982 Q1 Japan     17117 14004.
#>  6 1982 Q2 Japan     10617 14564.
#>  7 1982 Q3 Japan     11737 14909.
#>  8 1982 Q4 Japan     20961 15542.
#>  9 1983 Q1 Japan     20671 16868.
#> 10 1983 Q2 Japan     12235 16584.
#> # … with 498 more rows
```

`tq_sa` function transforms “Arrivals” variable in this example.

``` r
aus_arrivals %>% 
  tq_sa()
#> # A tsibble: 508 x 3 [1Q]
#> # Key:       Origin [4]
#>    Quarter Origin Arrivals
#>      <qtr> <chr>     <dbl>
#>  1 1981 Q1 Japan    12179.
#>  2 1981 Q2 Japan    12742.
#>  3 1981 Q3 Japan    13107.
#>  4 1981 Q4 Japan    14065.
#>  5 1982 Q1 Japan    14004.
#>  6 1982 Q2 Japan    14564.
#>  7 1982 Q3 Japan    14909.
#>  8 1982 Q4 Japan    15542.
#>  9 1983 Q1 Japan    16868.
#> 10 1983 Q2 Japan    16584.
#> # … with 498 more rows
```

EOL

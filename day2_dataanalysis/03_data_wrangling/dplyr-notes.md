<h1 class="title toc-ignore display-3">
Diving into the tdiyverse with dplyr
</h1>
================
Danielle Navarro
4 December 2018

<!--

  html_document:
    includes:
      in_header: header.html
    theme: flatly
    highlight: textmate
    css: mystyle.css

-->
The frames data
---------------

The frames data set comes from a simple experiment I ran a little while ago (it’s experiment two from this paper). What we were interested in was understanding how people use statistical information to guide inductive inferences. For example, suppose you observe a sample of “robins” that have “plaxium blood” (whatever that is). How likely is it that “sparrows” will possess plaxium blood? Or “cows”? Does it matter how many robins you have seen? Does it matter whether you specifically selected robins and they turned out to have plaxium blood (category sampling) as opposed to detecting animals with plaxium blood that then turned out to all be robins (property sampling)? In that paper we had a computational model of inductive reasoning that made specific predictions about how the sample size (number of robins) and sampling method (property or category) would inflence people’s judgments.

In this particular experiment we didn’t show people animals (though we have done those too!) we just showed them small “alien rocks” called “sodor spheres”, and asked people to make guesses about new rocks of different sizes: test\_loc values of 1 and 2 were very similar to the items they were shown during training, whereas value 7 was quite dissimilar. The number of training observations ranged from 2 (sample\_size = "small") to 12 (sample\_size = "large") and was varied within-subject. So everyone saw two observations, made some generalization judgments (response on a scale from 0 to 9), then saw more training observations and so on. Participants were randomly assigned to a "property" sampling condition or to a category sampling one. We also recorded age, gender, and assigned each person a unique id.

The variable key: - id: the participant id number - gender: male or female - age: numeric, in years - condition: (between subject). category sampling = people were told observations were selected on the basis of their category membership (e.g., because it's a small bird, or a small rock, or whatever) vs property sampling = people were told observations were selected because of a property they posses (e.g., it has plaxium blood, or a plaxium coating). - sample\_size: (within subject) small, medium, large - indicating how many observations they'd been shown at this point in the experiment - n\_obs: (within subject). same as "sample\_size", but it's the actual number (2, 6 or 12) - test\_item: (within subject). what stimulus are they now being shown? numeric: 1 to 7. this is ordinal (or really, quasi-interval) where items 1-2 are essentially identical to observations they'd seen before, and 3-7 become progressively less similar (e.g., bigger bird, bigger rock, whatever...) - response: (the outcome, within subject). the rating the person gave (0-9 scale) for "how likely is it that this new stimulus possesses the property (e.g., plaxium blood)?" where 0 = not at all, 9 = certain (or something like that) There's quite a bit going on in the data since it's a two within-subject and one between-subject manipulation

1. Getting started
------------------

Step 1 is making sure you have the packages:

``` r
library(here)
```

    ## here() starts at /Users/dan/GitHub/chdss2018/day2_dataanalysis/03_data_wrangling

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## ── Conflicts ─────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(janitor)
library(skimr)
```

Step 2 is creating an RMarkdown document that will contain your analyses.

Step 3 is loading the data.

``` r
data_location <- here("analysis","data","frames_ex2.csv")
data_location
```

    ## [1] "/Users/dan/GitHub/chdss2018/day2_dataanalysis/03_data_wrangling/analysis/data/frames_ex2.csv"

Then

``` r
frames <- read_csv(file = data_location)
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_integer(),
    ##   gender = col_character(),
    ##   age = col_integer(),
    ##   condition = col_character(),
    ##   sample_size = col_character(),
    ##   n_obs = col_integer(),
    ##   test_item = col_integer(),
    ##   response = col_integer()
    ## )

``` r
frames
```

    ## # A tibble: 4,725 x 8
    ##       id gender   age condition sample_size n_obs test_item response
    ##    <int> <chr>  <int> <chr>     <chr>       <int>     <int>    <int>
    ##  1     1 male      36 category  small           2         1        8
    ##  2     1 male      36 category  small           2         2        7
    ##  3     1 male      36 category  small           2         3        6
    ##  4     1 male      36 category  small           2         4        6
    ##  5     1 male      36 category  small           2         5        5
    ##  6     1 male      36 category  small           2         6        6
    ##  7     1 male      36 category  small           2         7        3
    ##  8     1 male      36 category  medium          6         1        9
    ##  9     1 male      36 category  medium          6         2        7
    ## 10     1 male      36 category  medium          6         3        5
    ## # ... with 4,715 more rows

It's not a bad idea to take a quick `glimpse()` at the data:

``` r
glimpse(frames)
```

    ## Observations: 4,725
    ## Variables: 8
    ## $ id          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    ## $ gender      <chr> "male", "male", "male", "male", "male", "male", "m...
    ## $ age         <int> 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36...
    ## $ condition   <chr> "category", "category", "category", "category", "c...
    ## $ sample_size <chr> "small", "small", "small", "small", "small", "smal...
    ## $ n_obs       <int> 2, 2, 2, 2, 2, 2, 2, 6, 6, 6, 6, 6, 6, 6, 12, 12, ...
    ## $ test_item   <int> 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3,...
    ## $ response    <int> 8, 7, 6, 6, 5, 6, 3, 9, 7, 5, 6, 4, 4, 2, 8, 7, 6,...

I'm also a fan of the `skim()` function for getting descriptive statistics quickly:

``` r
skim(frames)
```

    ## Skim summary statistics
    ##  n obs: 4725 
    ##  n variables: 8 
    ## 
    ## ── Variable type:character ──────────────────────────────────────────────────
    ##     variable missing complete    n min max empty n_unique
    ##    condition       0     4725 4725   8   8     0        2
    ##       gender       0     4725 4725   4   6     0        2
    ##  sample_size       0     4725 4725   5   6     0        3
    ## 
    ## ── Variable type:integer ────────────────────────────────────────────────────
    ##   variable missing complete    n   mean    sd p0 p25 p50 p75 p100     hist
    ##        age       0     4725 4725  34.92 11.63 20  27  32  40   84 ▇▇▃▂▁▁▁▁
    ##         id       0     4725 4725 113    64.96  1  57 113 169  225 ▇▇▇▇▇▇▇▇
    ##      n_obs       0     4725 4725   6.67  4.11  2   2   6  12   12 ▇▁▁▇▁▁▁▇
    ##   response       0     4725 4725   4.9   3.04  0   2   5   8    9 ▆▂▂▃▂▂▃▇
    ##  test_item       0     4725 4725   4     2     1   2   4   6    7 ▇▇▇▇▁▇▇▇

practical notes

-   irl you'd hae to do a lot of ugly data cleaning to get to this point
-   Mention that data vis is a later section

2. Basic operations with dplyr
------------------------------

-   Introduce the pipe operator
-   Group and summarise (and ungroup)
-   Arrange, filter, select
-   Mutate

3. Other useful things
----------------------

-   Gather and spread
-   Unify and separate
-   janitor::clean\_names
-   Tiny bit of stringr
-   Mention the existence of purrr & lubridate

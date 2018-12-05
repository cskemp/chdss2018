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

    ## ── Attaching packages ─────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## ── Conflicts ────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
    ## ── Variable type:character ─────────────────────────────────────────────────────────────────────────────
    ##     variable missing complete    n min max empty n_unique
    ##    condition       0     4725 4725   8   8     0        2
    ##       gender       0     4725 4725   4   6     0        2
    ##  sample_size       0     4725 4725   5   6     0        3
    ## 
    ## ── Variable type:integer ───────────────────────────────────────────────────────────────────────────────
    ##   variable missing complete    n   mean    sd p0 p25 p50 p75 p100     hist
    ##        age       0     4725 4725  34.92 11.63 20  27  32  40   84 ▇▇▃▂▁▁▁▁
    ##         id       0     4725 4725 113    64.96  1  57 113 169  225 ▇▇▇▇▇▇▇▇
    ##      n_obs       0     4725 4725   6.67  4.11  2   2   6  12   12 ▇▁▁▇▁▁▁▇
    ##   response       0     4725 4725   4.9   3.04  0   2   5   8    9 ▆▂▂▃▂▂▃▇
    ##  test_item       0     4725 4725   4     2     1   2   4   6    7 ▇▇▇▇▁▇▇▇

(Side note: the histograms in skim are text-based and rely on unicode characters. In some cases Windows does weird things and doesn't display them - we'll talk about data vis later)s

practical notes

-   irl you'd hae to do a lot of ugly data cleaning to get to this point
-   Mention that data vis is a later section

2. Basic operations with dplyr
------------------------------

### Pipes

Data wrangling in R used to be hard. Ever since the `tidyverse` family of packages appeared in a stable form, it's become easy. However, it requires you to have a bit of a shift in mindset:

``` r
frames %>% skim(.)
```

The key idea behind `%>%` is a bit of "magic", in which the command is reorganised so that the "thing" on the left (i.e., `frames`) gets inserted into the expression on the right, replacing the `.`). So when you type the command above, it gets reorganised into this...

``` r
skim(frames)
```

... and then this reorganised command is evaluated. Why is that useful? Well, imagine we have a series of operations we want to do, where the output of the first operation is fed (or "piped") in as the input to the second, and so on. In piped code, it looks like this:

``` r
output <- frames %>% 
  do_thing1(.) %>%
  do_thing2(.) %>%
  do_thing3(.) %>%
  do_thing4(.)
```

In traditional code, it would look like this:

``` r
output <- do_thing4(
  do_thing3(
    do_thing2(
      do_thing1(
        frames
      )
    )
  )
)
```

which forces you to start reading in the middle and then upwards. It's pretty uncomfortable to read as it is, and it gets *much* worse when each of your `do_thing` functions has additional arguments. So in practice we would write it like this,

``` r
a <- do_thing1(frames)
b <- do_thing2(a)
c <- do_thing3(b)
output <- do_thing4(c)
```

which is a little nicer, but the `a`, `b` and `c` values are just dummy variables that we don't actually want, so then you have to get rid of them. Piped code makes it much more readable, so from now on we'll tend to work with pipes.

### Group, summarise, arrange, filter, select, mutate

Averaging across subjects.

``` r
average_response <- frames %>%
  group_by(test_item, sample_size, n_obs, condition) %>%
  summarise(response = mean(response))
```

Now let's look

``` r
average_response
```

    ## # A tibble: 42 x 5
    ## # Groups:   test_item, sample_size, n_obs [?]
    ##    test_item sample_size n_obs condition response
    ##        <int> <chr>       <int> <chr>        <dbl>
    ##  1         1 large          12 category      7.60
    ##  2         1 large          12 property      7.16
    ##  3         1 medium          6 category      7.32
    ##  4         1 medium          6 property      6.66
    ##  5         1 small           2 category      6.07
    ##  6         1 small           2 property      5.78
    ##  7         2 large          12 category      7.51
    ##  8         2 large          12 property      7.20
    ##  9         2 medium          6 category      7.17
    ## 10         2 medium          6 property      6.95
    ## # ... with 32 more rows

What if we want to include only some cases (e.g., the small sample size)? The `filter()` command:

``` r
average_response %>%
  filter(sample_size == "small")
```

    ## # A tibble: 14 x 5
    ## # Groups:   test_item, sample_size, n_obs [7]
    ##    test_item sample_size n_obs condition response
    ##        <int> <chr>       <int> <chr>        <dbl>
    ##  1         1 small           2 category      6.07
    ##  2         1 small           2 property      5.78
    ##  3         2 small           2 category      6.26
    ##  4         2 small           2 property      6.21
    ##  5         3 small           2 category      5.87
    ##  6         3 small           2 property      5.24
    ##  7         4 small           2 category      5.11
    ##  8         4 small           2 property      4.68
    ##  9         5 small           2 category      4.55
    ## 10         5 small           2 property      3.99
    ## 11         6 small           2 category      4.16
    ## 12         6 small           2 property      3.68
    ## 13         7 small           2 category      3.98
    ## 14         7 small           2 property      3.67

Hm, this is nice, but I'd prefer to have it sorted by condition rather than by test item. The `arrange()` function will do that:

``` r
average_response %>%
  filter(sample_size == "small") %>%
  arrange(condition)
```

    ## # A tibble: 14 x 5
    ## # Groups:   test_item, sample_size, n_obs [7]
    ##    test_item sample_size n_obs condition response
    ##        <int> <chr>       <int> <chr>        <dbl>
    ##  1         1 small           2 category      6.07
    ##  2         2 small           2 category      6.26
    ##  3         3 small           2 category      5.87
    ##  4         4 small           2 category      5.11
    ##  5         5 small           2 category      4.55
    ##  6         6 small           2 category      4.16
    ##  7         7 small           2 category      3.98
    ##  8         1 small           2 property      5.78
    ##  9         2 small           2 property      6.21
    ## 10         3 small           2 property      5.24
    ## 11         4 small           2 property      4.68
    ## 12         5 small           2 property      3.99
    ## 13         6 small           2 property      3.68
    ## 14         7 small           2 property      3.67

Okay, but do we really need the sample size variables? Maybe I only want to `select()` the other three variables:

``` r
average_response %>%
  filter(sample_size == "small") %>%
  arrange(condition) %>%
  select(test_item, condition, response)
```

    ## Adding missing grouping variables: `sample_size`, `n_obs`

    ## # A tibble: 14 x 5
    ## # Groups:   test_item, sample_size, n_obs [7]
    ##    sample_size n_obs test_item condition response
    ##    <chr>       <int>     <int> <chr>        <dbl>
    ##  1 small           2         1 category      6.07
    ##  2 small           2         2 category      6.26
    ##  3 small           2         3 category      5.87
    ##  4 small           2         4 category      5.11
    ##  5 small           2         5 category      4.55
    ##  6 small           2         6 category      4.16
    ##  7 small           2         7 category      3.98
    ##  8 small           2         1 property      5.78
    ##  9 small           2         2 property      6.21
    ## 10 small           2         3 property      5.24
    ## 11 small           2         4 property      4.68
    ## 12 small           2         5 property      3.99
    ## 13 small           2         6 property      3.68
    ## 14 small           2         7 property      3.67

Wait why doesn't that work? The `dplyr` package is pretty picky about grouping variables, and won't let you drop them! Remember, to construct the `average_response` data set, we grouped the original `frames` data, and when we used `group_by()` to do this, R has retained some information about this grouping (hidden in an invisible attribute). So if you do want to get rid of this, you'll need to `ungroup()` before you `select()`. So now this version does what we're expecting:

``` r
average_response %>%
  filter(sample_size == "small") %>%
  arrange(condition) %>%
  ungroup() %>%
  select(test_item, condition, response)
```

    ## # A tibble: 14 x 3
    ##    test_item condition response
    ##        <int> <chr>        <dbl>
    ##  1         1 category      6.07
    ##  2         2 category      6.26
    ##  3         3 category      5.87
    ##  4         4 category      5.11
    ##  5         5 category      4.55
    ##  6         6 category      4.16
    ##  7         7 category      3.98
    ##  8         1 property      5.78
    ##  9         2 property      6.21
    ## 10         3 property      5.24
    ## 11         4 property      4.68
    ## 12         5 property      3.99
    ## 13         6 property      3.68
    ## 14         7 property      3.67

At this point, our "chain" of piped operations is getting quite long, and maybe we'd like to save the output to a new variable!

``` r
average_response_small <- average_response %>%
  filter(sample_size == "small") %>%
  arrange(condition) %>%
  ungroup() %>%
  select(test_item, condition, response)
```

The response data are on a 0 to 9 scale, but maybe it makes more sense to rescale so it to a "generalisation" value that ranges from 0 to 1. That's a simple transformation (divide by 9), but how do we create a new variable inside the data frame? Enter `mutate()`

``` r
average_response_small <- average_response_small %>%
  mutate(generalisation = response/9)
```

Now:

``` r
average_response_small
```

    ## # A tibble: 14 x 4
    ##    test_item condition response generalisation
    ##        <int> <chr>        <dbl>          <dbl>
    ##  1         1 category      6.07          0.674
    ##  2         2 category      6.26          0.696
    ##  3         3 category      5.87          0.652
    ##  4         4 category      5.11          0.568
    ##  5         5 category      4.55          0.506
    ##  6         6 category      4.16          0.462
    ##  7         7 category      3.98          0.442
    ##  8         1 property      5.78          0.643
    ##  9         2 property      6.21          0.690
    ## 10         3 property      5.24          0.583
    ## 11         4 property      4.68          0.521
    ## 12         5 property      3.99          0.443
    ## 13         6 property      3.68          0.409
    ## 14         7 property      3.67          0.407

Gather and spread
-----------------

What if we want to have the two conditions as separate variables?

``` r
average_response_small %>%
  spread(key = condition, value = generalisation)
```

    ## # A tibble: 14 x 4
    ##    test_item response category property
    ##        <int>    <dbl>    <dbl>    <dbl>
    ##  1         1     5.78   NA        0.643
    ##  2         1     6.07    0.674   NA    
    ##  3         2     6.21   NA        0.690
    ##  4         2     6.26    0.696   NA    
    ##  5         3     5.24   NA        0.583
    ##  6         3     5.87    0.652   NA    
    ##  7         4     4.68   NA        0.521
    ##  8         4     5.11    0.568   NA    
    ##  9         5     3.99   NA        0.443
    ## 10         5     4.55    0.506   NA    
    ## 11         6     3.68   NA        0.409
    ## 12         6     4.16    0.462   NA    
    ## 13         7     3.67   NA        0.407
    ## 14         7     3.98    0.442   NA

Why did that not work? Remember we still have `response` in the data, and there's a unique value of response for everything. Gr. Okay, so let's select out that one before spreading...

``` r
wide_avrs <- average_response_small %>%
  select(-response) %>%
  spread(key = condition, value = generalisation)

wide_avrs
```

    ## # A tibble: 7 x 3
    ##   test_item category property
    ##       <int>    <dbl>    <dbl>
    ## 1         1    0.674    0.643
    ## 2         2    0.696    0.690
    ## 3         3    0.652    0.583
    ## 4         4    0.568    0.521
    ## 5         5    0.506    0.443
    ## 6         6    0.462    0.409
    ## 7         7    0.442    0.407

Want to `gather()` it back into long form?

``` r
wide_avrs %>% gather(key = "condition", value = "generalisation", category, property)
```

    ## # A tibble: 14 x 3
    ##    test_item condition generalisation
    ##        <int> <chr>              <dbl>
    ##  1         1 category           0.674
    ##  2         2 category           0.696
    ##  3         3 category           0.652
    ##  4         4 category           0.568
    ##  5         5 category           0.506
    ##  6         6 category           0.462
    ##  7         7 category           0.442
    ##  8         1 property           0.643
    ##  9         2 property           0.690
    ## 10         3 property           0.583
    ## 11         4 property           0.521
    ## 12         5 property           0.443
    ## 13         6 property           0.409
    ## 14         7 property           0.407

Exercise. Try spreading and gathering by test item rather than condition

Getting fancier
---------------

``` r
frames %>% 
  group_by(test_item, sample_size, condition) %>%
  summarise(response = mean(response)) %>%
  spread(key = sample_size, value = response)
```

    ## # A tibble: 14 x 5
    ## # Groups:   test_item [7]
    ##    test_item condition large medium small
    ##        <int> <chr>     <dbl>  <dbl> <dbl>
    ##  1         1 category   7.60   7.32  6.07
    ##  2         1 property   7.16   6.66  5.78
    ##  3         2 category   7.51   7.17  6.26
    ##  4         2 property   7.20   6.95  6.21
    ##  5         3 category   6.39   5.98  5.87
    ##  6         3 property   5.23   5.49  5.24
    ##  7         4 category   5.39   4.97  5.11
    ##  8         4 property   3.07   3.56  4.68
    ##  9         5 category   4.72   4.22  4.55
    ## 10         5 property   2.26   2.75  3.99
    ## 11         6 category   4.43   3.85  4.16
    ## 12         6 property   1.91   2.50  3.68
    ## 13         7 category   4.18   3.61  3.98
    ## 14         7 property   1.90   2.19  3.67

Hm, those are ordered wrong. Why? Well, they're alphabetical.

``` r
frames %>% 
  group_by(test_item, sample_size, condition) %>%
  summarise(response = mean(response)) %>%
  ungroup() %>%
  mutate(
    sample_size = sample_size %>%
      as_factor() %>%
      fct_relevel("small","medium","large")
  ) %>%
  spread(key = sample_size, value = response)
```

    ## # A tibble: 14 x 5
    ##    test_item condition small medium large
    ##        <int> <chr>     <dbl>  <dbl> <dbl>
    ##  1         1 category   6.07   7.32  7.60
    ##  2         1 property   5.78   6.66  7.16
    ##  3         2 category   6.26   7.17  7.51
    ##  4         2 property   6.21   6.95  7.20
    ##  5         3 category   5.87   5.98  6.39
    ##  6         3 property   5.24   5.49  5.23
    ##  7         4 category   5.11   4.97  5.39
    ##  8         4 property   4.68   3.56  3.07
    ##  9         5 category   4.55   4.22  4.72
    ## 10         5 property   3.99   2.75  2.26
    ## 11         6 category   4.16   3.85  4.43
    ## 12         6 property   3.68   2.50  1.91
    ## 13         7 category   3.98   3.61  4.18
    ## 14         7 property   3.67   2.19  1.90

What if we want to spread by two variables at once!!!

``` r
new_data <- frames %>% 
  group_by(test_item, sample_size, condition) %>%
  summarise(response = mean(response)) %>%
  unite(col = "cond_ss", condition, sample_size) 
```

... then you would `spread()` using `cond_ss` as the `key`.

Which brings us to a question. What if you get a data set where you have a variable like `cond_ss` that needs to be separated into two? Well...

``` r
new_data %>% separate(col = cond_ss, into = c("condition", "sample_size"))
```

    ## # A tibble: 42 x 4
    ## # Groups:   test_item [7]
    ##    test_item condition sample_size response
    ##        <int> <chr>     <chr>          <dbl>
    ##  1         1 category  large           7.60
    ##  2         1 property  large           7.16
    ##  3         1 category  medium          7.32
    ##  4         1 property  medium          6.66
    ##  5         1 category  small           6.07
    ##  6         1 property  small           5.78
    ##  7         2 category  large           7.51
    ##  8         2 property  large           7.20
    ##  9         2 category  medium          7.17
    ## 10         2 property  medium          6.95
    ## # ... with 32 more rows

Other notes?
------------

-   The data that we were given has nice variable names. No spaces, no fancy characters, etc. It's a pain to rename variables one at a time. Check out the `janitor` package (and the `clean_names()` function specifically)
-   If you need to do text manipulation, the `stringr` package is your friend
-   If you need to parse dates (pray you don't because they suck), the `lubridate` package is the least painful way known
-   Later on, if you find yourself writing lots of loops in your R code an they're running really slowly, the `purrr` package will be your friend, but it's not easy to learn so give it a bit of time.

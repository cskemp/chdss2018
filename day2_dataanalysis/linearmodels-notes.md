<h1 class="title toc-ignore display-3">
Linear models in R
</h1>
================
Danielle Navarro
5 December 2018

<!--

  html_document:
    includes:
      in_header: header.html
    theme: flatly
    highlight: textmate
    css: mystyle.css

-->
``` r
library(here)
```

    ## here() starts at /Users/dan/GitHub/chdss2018/day2_dataanalysis

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
frames <- read_csv(here("analysis","data","frames_ex2.csv"))
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

What this section is not
------------------------

Because R is a *statistical* programming language it comes with a lot of hypothesis tests and tools built in, and of course there is an overwhelming number of packages out there that extend this. It is impossible to cover the whole thing in a brief tutorial, so I'm going to be a little picky. For example, I'm going to skip over the most commonly used classical tests, because they're comparatively easy to learn and it's not the best use of our time! For future reference though:

-   The `t.test()` function handles one-sample, independent samples and paired samples t-tests
-   The `chisq.test()` function handles chi-square tests of independence and Pearson goodness of fit tests
-   The `prop.test()` function tests for the equality of two proportions.
-   The `binom.test()` function allows you to do a binomial test of choice proportion against a known rate
-   The `wilcox.test()` function handles one- and two-sample nonparametric tests of equality of means
-   The `cor.test()` function tests the significance of a correlation

Of course, there are many, many others! What we're going to focus on here is:

-   Linear modelling with the `lm()` function
-   Linear mixed models with the `lmer()` function (in the `lme4` package)
-   Generalised linear mixed models with the `glmer()` function (also from `lme4`)

Linear models
-------------

Linear models should be fairly familiar to most: it's essentially what we were all taught in undergraduate under the name multiple regression. However, what is sometimes underemphasised is the fact that correlation, ANOVA, and t-tests can all be cast within the linear modelling framework, and R allows you do do all these using the `lm()` function. So that's where we're going to start.

To begin with, we need a data set. For this purpose, let's construct a simplified version of the `frames` data, by averaging all the responses made by a person, regardless of the number of observations or the test item:

``` r
tinyframes <- frames %>%
  group_by(id, age, condition) %>%
  summarise(
    response = mean(response)
    ) %>%
  ungroup()
```

Let's take a look at the `tinyframes` dataset we've just created:

``` r
glimpse(tinyframes)
```

    ## Observations: 225
    ## Variables: 4
    ## $ id        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1...
    ## $ age       <int> 36, 46, 33, 71, 23, 31, 23, 31, 37, 46, 27, 30, 22, ...
    ## $ condition <chr> "category", "category", "property", "property", "pro...
    ## $ response  <dbl> 5.333333, 7.047619, 4.857143, 3.857143, 9.000000, 7....

A very typical way to produce descriptive statistics is to calculate mean and standard deviation for each condition, and count the number of people in each condition.

``` r
tinyframes %>%
  group_by(condition) %>%
  summarise(
    mean_resp = mean(response), 
    sd_resp = sd(response),
    n = n()
  )
```

    ## # A tibble: 2 x 4
    ##   condition mean_resp sd_resp     n
    ##   <chr>         <dbl>   <dbl> <int>
    ## 1 category       5.40    1.56   114
    ## 2 property       4.39    1.37   111

We would also want to visualise the data. It is almost always a mistake to start trying to model a data set without properly exploring it and making sure you have a good "feel" for what is going on. So let's draw a picture!

``` r
tinyframes %>% 
  ggplot(aes(x = age, y = response, colour = condition)) + 
  geom_smooth(method = "lm") + 
  geom_point()
```

![](linearmodels-notes_files/figure-markdown_github/unnamed-chunk-5-1.png)

Intuitively, it looks like the two conditions are likely to be different to one another; but if there's any effect of age it would have to be tiny.

### Using lm to do a t-test

So let's start with a simple question. Is there a "significant" difference between the two conditions? I'm not a fan of orthodox null hypothesis testing, to be honest, but it does have it's place. Traditionally, the solution is the t-test:

``` r
t.test(
  formula = response ~ condition, 
  data = tinyframes, 
  var.equal = TRUE
)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  response by condition
    ## t = 5.1625, df = 223, p-value = 5.388e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.6259535 1.3988834
    ## sample estimates:
    ## mean in group category mean in group property 
    ##               5.397661               4.385242

Okay we have a significant difference. So we reject the null hypothesis (i.e., that the two groups have the same population mean) and accept the alternative (i.e., that they have different population means). Yay. I guess. The moment we start caring about data analysis in any detail, though, it helps to recast these "hypotheses" in terms of *statistical models*.

``` r
mod1 <- lm(formula = response ~ 1, data = tinyframes) 
mod2 <- lm(formula = response ~ condition, data = tinyframes)
```

To give you a sense of what R has just done, it has estimated the coefficients for two different regression models: `mod1` only includes an intercept term (i.e., the "grand mean"), wherese `mod2` contains two terms:

``` r
mod2
```

    ## 
    ## Call:
    ## lm(formula = response ~ condition, data = tinyframes)
    ## 
    ## Coefficients:
    ##       (Intercept)  conditionproperty  
    ##             5.398             -1.012

Notice that the coefficients have a clear relationship to the group means: the "intercept" term is identical to the group mean for category sampling, and the "conditionproperty" term is what you have to add to that to get the group mean for property sampling (i.e., 5.4 - 1.0 = 4.4). It's expressed in different language than the t-test, but `mod2` nevertheless maps onto the alternative hypothesis.

To compare these two linear models, we can call the `anova()` function:

``` r
anova(mod1, mod2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: response ~ 1
    ## Model 2: response ~ condition
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1    224 539.98                                  
    ## 2    223 482.33  1    57.645 26.652 5.388e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

This doesn't look like the output of a t-test, but if we look carefully we notice that:

-   The *p*-values are the same
-   The test statistics are related: $t = \\sqrt{F}$
-   The residual df in the ANOVA table is the same as the t-test df

In a sense, what we've just done is illustrate the fact that the Student t-test is equivalent to a one-way ANOVA with two groups (which we were all taught as undergraduates), but we've used linear models to do it!

Before moving on, I should mention that this is a slightly different ANOVA table than what you might be expecting to see. You can get the more traditional version like this,

``` r
anova(lm(response ~ condition, tinyframes))
```

    ## Analysis of Variance Table
    ## 
    ## Response: response
    ##            Df Sum Sq Mean Sq F value    Pr(>F)    
    ## condition   1  57.65  57.645  26.652 5.388e-07 ***
    ## Residuals 223 482.33   2.163                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

but it's essentially the same thing and I don't want to spend too much time on this since the focus is really on how we can extend these ideas.

### Mixing continuous and discrete predictors

One nice thing about linear models is that, because the framework is pretty general, there's nothing stopping you from including continuous variables and categorical variables in the same model (as one might do in an ANCOVA). So, sticking with out `tinyframes` data, perhaps we should check to see if we can detect an effect of age. It doesn't look very likely, but let's run the model anyway. What we'll do this time is take the same two models as before `mod1` and `mod2`, but now add a third model that includes `age` as an additional predictor:

``` r
mod3 <- lm(formula = response ~ condition + age, data = tinyframes)
```

If we take a quick look at the coefficients

``` r
mod3
```

    ## 
    ## Call:
    ## lm(formula = response ~ condition + age, data = tinyframes)
    ## 
    ## Coefficients:
    ##       (Intercept)  conditionproperty                age  
    ##          5.102572          -1.018548           0.008536

we can see that age really isn't having much of an effect, if any. To compare all three models using an *F*-test, what we would do is this:

``` r
anova(mod1, mod2, mod3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: response ~ 1
    ## Model 2: response ~ condition
    ## Model 3: response ~ condition + age
    ##   Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
    ## 1    224 539.98                                   
    ## 2    223 482.33  1    57.645 26.6544 5.399e-07 ***
    ## 3    222 480.12  1     2.214  1.0238    0.3127    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

In this ANOVA table, what you're looking at is a test of `mod2` against `mod1`, followed by `mod3` against `mod2`. This suggests that `mod2` is preferred over `mod1` (reject the null), but `mod3` isn't preferred over `mod2` (retain the null).

### From hypothesis tests to model selection

I have a confession to make. I really dislike using null hypothesis tests the way we did in the previous analysis. Most of the framework for null hypothesis testing comes from work by Jerzy Neyman, and in his framework the goal is to *prespecify* a formal procedure such that you can input a data set, and output a binary decision; and specifically to ensure that this decision procedure controls your Type 1 error rate. This approach to statistics has its place, but it's not actually all that consistent with what we're doing here. Neyman's approach is completely automated: you *must* follow the procedure no matter what your data looks like, or else your Type 1 error isn't controlled. So if explore your data and they turn out to be super weird, you must apply your prespecified decision rule. If you don't not only is the p-value for this test completely meaningless, it also strongly invalidates any other p-values you report, even if you did stick to the procedure in those cases: because it implies that, had the data turned out some other way, you wouldn't have stuck to the procedure, and Neyman's theory only works if you *always* follow the prespecified analysis plan. No excuses, no exceptions.

In real life, this never actually happens. Science doesn't work that way. What I find bizarre, is that while statisticians for the most part have recognised that this presents a problem for Neyman's theory, there is a school of thought within psychology that the problem lies with the *scientist* for not adhering to this stupid statistical theory. Well, nuts to that. I have no particular interest in following Neyman's absurd rules, and my main goal as a scientist is something more akin to Ockham's razor: find the simplest model that provides an good enough account of the data.

In essence, what I've done here is reframed the statistical problem, and changed it from a "hypothesis testing" problem to a "model selection" problem. The tools used for model selection are often somewhat different, and the underlying philosophy is often more aligned with the Ockham's razor idea. Two (very simple, and often flawed) approaches to this are the Akaike information criterion (AIC) and the Bayesian information criterion (BIC), both of which have been around since the 1970s. For our linear models, we can evaluate them using the `AIC()` and `BIC()` functions:

``` r
AIC(mod1, mod2, mod3)
```

    ##      df      AIC
    ## mod1  2 839.4940
    ## mod2  3 816.0928
    ## mod3  4 817.0575

``` r
BIC(mod1, mod2, mod3)
```

    ##      df      BIC
    ## mod1  2 846.3262
    ## mod2  3 826.3411
    ## mod3  4 830.7219

Smaller values of AIC and BIC are better, and it's hardly a surprise that `mod2` turns out to be the best one!

### Exploring the model

Overall, `mod2` looks pretty sensible:

``` r
summary(mod2)
```

    ## 
    ## Call:
    ## lm(formula = response ~ condition, data = tinyframes)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2424 -0.9691 -0.3024  0.8052  4.6148 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         5.3977     0.1377  39.187  < 2e-16 ***
    ## conditionproperty  -1.0124     0.1961  -5.163 5.39e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.471 on 223 degrees of freedom
    ## Multiple R-squared:  0.1068, Adjusted R-squared:  0.1027 
    ## F-statistic: 26.65 on 1 and 223 DF,  p-value: 5.388e-07

``` r
confint(mod2)
```

    ##                       2.5 %     97.5 %
    ## (Intercept)        5.126217  5.6691049
    ## conditionproperty -1.398883 -0.6259535

TODO:

-   the `predict()` function
-   the `residuals()` function
-   regression diagnostics using `plot()`

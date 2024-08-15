
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quantrines

<!-- badges: start -->

[![R-CMD-check](https://github.com/dajmcdon/quantrines/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dajmcdon/quantrines/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{quantrines}` (a portmanteau of “quantiles” and “engines”,
with an “r” in the middle for “regression”… this name stinks) is to
demonstrate the `{parsnip}` “quantile regression” mode.

This currently contains 2 engines:

- `grf`: allows for quantile regression as well as regression and
  classification using random forests (see `?grf-engine`); and
- `quantreg`: allows for quantile regression (see `?quantreg-engine`).

Other functions contained here: \* `vec_quantiles()` to create a
list-col of quantile values with an attribute for the levels. \*
`weighted_interval_score()` computes the score for quantile predictions
relative to observed values.

## Installation

You can install the development version of quantrines from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("dajmcdon/quantrines")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(quantrines)
#> Loading required package: parsnip
## basic example code
```

## Issues encountered in development

### Missing or hard-to-find documentation

A careful description of the `parsnip::set_fit(..., value = fit_obj)`
requirements. The docs say “see below”, but I can’t find it. It’s
described a bit through examples in the [vignette “How to build a
parsnip model”](https://www.tidymodels.org/learn/develop/models/), but
it would be nice to give complete details as well. Specifically,

1.  Do we `protect` args to the original function or the parsnip fit
    function?
2.  The `data` argument is important here.

### Specifying the `quantile_levels`

Adding the `mode = "quantile regression"` allows me to have both
quantile regression and standard regression with the same engine. But
where do I pass the quantile levels? Currently, these are still an
engine argument:

``` r
rand_forest(mode = "quantile regression") %>% 
  set_engine("grf", tau = 1:4 / 5)
```

This isn’t really desirable because

1.  you need to know the argument name from the package;
2.  this is required for all engines of this mode.

Is there a way to have a “required mode argument”?

Another possibility would be to specify it in `fit()` since this is the
first place it would be necessary.

### Modifications to `{parsnip}` quantile-mode branch

1.  Added “quantile regression” to `check_pred_type()`.

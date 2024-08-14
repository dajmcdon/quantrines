#' Random quantile forests via grf
#'
#' [grf::quantile_forest()] fits random forests in a way that makes it easy
#' to calculate _quantile_ forests.
#'
#' @section Tuning Parameters:
#'
#' This model has 3 tuning parameters:
#'
#' - `mtry`: # Randomly Selected Predictors (type: integer, default: see below)
#' - `trees`: # Trees (type: integer, default: 2000L)
#' - `min_n`: Minimal Node Size (type: integer, default: 5)
#'
#' `mtry` depends on the number of columns in the design matrix.
#' The default in [grf::quantile_forest()] is `min(ceiling(sqrt(ncol(X)) + 20), ncol(X))`.
#'
#' For categorical predictors, a one-hot encoding is always used. This makes
#' splitting efficient, but has implications for the `mtry` choice. A factor
#' with many levels will become a large number of columns in the design matrix
#' which means that some of these may be selected frequently for potential splits.
#' This is different than in other implementations of random forest. For more
#' details, see [the `grf` discussion](https://grf-labs.github.io/grf/articles/categorical_inputs.html).
#'
#' @section Translation from parsnip to the original package:
#'
#' ```{r, translate-engine}
#' rand_forest(
#'   mode = "quantile regression",
#'   mtry = integer(1),
#'   trees = integer(1),
#'   min_n = integer(1)
#' ) %>%
#'   set_engine("grf") %>%
#'   translate()
#' ```
#'
#' @section Case weights:
#'
#' Case weights are not supported for quantile regression with this engine.
#'
#' @examples
#' library(grf)
#' tib <- data.frame(
#'   y = rnorm(100), x = rnorm(100), z = rnorm(100),
#'   f = factor(sample(letters[1:3], 100, replace = TRUE))
#' )
#' spec <- rand_forest(engine = "grf", mode = "quantile regression")
#' out <- fit(spec, formula = y ~ x + z, data = tib)
#' predict(out, new_data = tib[1:5, ])
#'
#' # -- adjusting the desired quantiles
#'
#' spec <- rand_forest(mode = "quantile regression") %>%
#'   set_engine(engine = "grf", quantiles = c(1:9 / 10))
#' out <- fit(spec, formula = y ~ x + z, data = tib)
#' predict(out, new_data = tib[1:5, ])
#' @name grf_quantiles
NULL

# turn the predictions into a tibble with a vec_quantiles column
process_quantile_forest_preds <- function(x, object) {
  quantile_levels <- parsnip::extract_fit_engine(object)$quantiles.orig
  x <- x$predictions
  out <- lapply(vctrs::vec_chop(x), function(x) sort(drop(x)))
  point_pred_loc <- match(0.5, quantile_levels, 0L)
  if (point_pred_loc) {
    .pred = sapply(out, function(x) x[[point_pred_loc]])
  } else {
    .pred = NULL
  }
  out <- vec_quantiles(out, quantile_levels)
  tibble::tibble(.pred = .pred, .pred_quantile = out)
}
process_regression_forest_preds <- function(x, object) {
  tibble::tibble(.pred = x$predictions)
}
process_probability_forest_class <- function(x, object) {
  x <- x$predictions
  max_class <- factor(
    names(x)[apply(x, 1, which.max)],
    levels = names(x)
  )
  tibble::tibble(.pred = max_class)
}
process_probability_forest_prob <- function(x, object) {
  tibble::as_tibble(x$predictions)
}

#' Compute weighted interval score
#'
#' Weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param x A vector of class [vctrs_quantiles][vec_quantiles()].
#' @param actual double. Actual value(s)
# @param quantile_levels probabilities. If specified, the score will be
#   computed at this set of levels, rather than those in `x`.
#' @param na.rm character. How are NA predictions handled.
# @param na_handling character. Determines how `quantile_levels` without a
#   corresponding `value` are handled. For `"impute"`, missing values will be
#   calculated if possible using the available quantiles. For `"drop"`,
#   explicitly missing values are ignored in the calculation of the score, but
#   implicitly missing values are imputed if possible.
#   For `"propogate"`, the resulting score will be `NA` if any missing values
#   exist in the original `quantile_levels`. Finally, if
#   `quantile_levels` is specified, `"fail"` will result in
#   the score being `NA` when any required quantile levels (implicit or explicit)
#   are do not have corresponding values.
#' @param ... not used
#'
#' @return a vector of nonnegative scores.
#'
#' @export
#' @examples
#' quantile_levels <- c(.2, .4, .6, .8)
#' predq_1 <- 1:4 #
#' predq_2 <- 8:11
#' vq <- vec_quantiles(list(predq_1, predq_2), quantile_levels)
#' actual <- c(3.3, 7.1)
#' weighted_interval_score(vq, actual)
#' weighted_interval_score(vq, 5.1)
weighted_interval_score <- function(
    x, actual, na.rm = c("propogate", "drop", "fail"), ...
) {
  UseMethod("weighted_interval_score")
}

#' @export
weighted_interval_score.default <- function(
    x, actual, na.rm = c("propogate", "drop", "fail"), ...
) {
  cli_abort(c(
    "Weighted interval score can only be calculated if `x`",
    "has class {.cls vctrs_quantiles}."
  ))
}

#' @export
weighted_interval_score.vctrs_quantiles <- function(
    x, actual, na.rm = c("propogate", "drop", "fail"), ...
) {
  rlang::check_dots_empty()
  checkmate::assert_numeric(actual, finite = TRUE)
  na.rm <- rlang::arg_match(na.rm)
  l <- vec_recycle_common(x = x, actual = actual)
  map2_dbl(
    .x = vec_data(l$x),
    .y = l$actual,
    .f = wis_one_pred,
    quantile_levels = attr(x, "levels"),
    na.rm = na.rm
  )
}


wis_one_pred <- function(x, actual, quantile_levels, na.rm) {
  if (is.na(actual)) {
    return(NA)
  }
  if (all(is.na(x))) {
    return(NA)
  }
  if (na.rm == "fail" && any(is.na(x))) {
    return(NA)
  }
  tau <- quantile_levels
  na.rm <- (na.rm == "drop")
  2 * mean(pmax(tau * (actual - x), (1 - tau) * (x - actual)), na.rm = na.rm)
}

#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name quantrines-vctrs
NULL


#' @export
vec_ptype_abbr.vctrs_quantiles <- function(x, ...) "qntls"

#' @export
vec_ptype_full.vctrs_quantiles <- function(x, ...) "quantiles"

#' @importFrom rlang is_list is_double
#' @importFrom checkmate assert_double
new_vec_quantiles <- function(
    values = list(),
    quantile_levels = double()
) {
  quantile_levels <- vec_cast(quantile_levels, double())
  n <- vec_size_common(!!!values)
  assert_double(quantile_levels, lower = 0, upper = 1, len = n)
  new_vctr(
    values, levels = quantile_levels, class = "vctrs_quantiles"
  )
}


#' A vector containing sets of quantiles
#'
#' @param values A data.frame/matrix/vector of values. If a named data.frame,
#'   the column names will be used as the `quantile_levels` if those are missing.
#' @param quantile_levels A vector of probabilities corresponding to `values`.
#'   May be `NULL` if `values` is a named data.frame.
#'
#' @export
#'
#' @examples
#' preds <- vec_quantiles(list(1:4, 8:11), c(.2, .4, .6, .8))
#'
#' vec_quantiles(1:4, 1:4 / 5)
vec_quantiles <- function(values, quantile_levels = NULL) {
  if (is.null(quantile_levels)) {
    if (!is.data.frame(values)) {
      cli_abort("If `quantile_levels` is `NULL`, `values` must be a data.frame.")
    }
    quantile_levels <- as.numeric(names(values))
    if (any(is.na(quantile_levels))) {
      cli_abort("If `quantile_levels` is `NULL`, `values` must be a data.frame with numeric names.")
    }
  }
  quantile_levels <- vec_cast(quantile_levels, double())
  if (is.data.frame(values) || (is.matrix(values) && dim(values) == 2)) {
    values <- lapply(vec_chop(values), function(v) sort(drop(v)))
  } else if (is.list(values)) {
    values <- values
  } else if (is.null(dim(values))) {
    values <- list(values)
  } else {
    cli_abort("`values` must be a {.cls list}, {.cls matrix}, or {.cls data.frame}, not a {.cls {class(values)}}.")
  }
  new_vec_quantiles(values, quantile_levels)
}

#' @export
format.vctrs_quantiles <- function(x, ...) {
  quantile_levels <- attr(x, "levels")
  if (length(quantile_levels) == 1L) {
    x <- unlist(x)
    out <- paste0(round(quantile_levels, 2L), ": ", round(unlist(x)))
    out[is.na(x)] <- NA
  } else {
    rng <- sapply(x, range)
    out <- paste0("[", round(rng[1, ], 3L), ", ", round(rng[2, ], 3L), "]")
    out[is.na(rng[1, ]) | is.na(rng[2, ])] <- NA
  }
  out
}

#' @export
obj_print_footer.vctrs_quantiles <- function(x, ...) {
  lvls <- attr(x, "levels")
  cat("# Qntl levels: ", format(lvls, digits = 3), "\n", sep = " ")
}

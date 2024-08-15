make_pred_tibble_from_matrix <- function(pred_mat, quantile_levels) {
  loc <- match(0.5, quantile_levels, 0L)
  out <- lapply(vec_chop(pred_mat), function(x) sort(drop(x)))
  .pred <- if (loc) sapply(out, function(x) x[loc]) else NULL
  out <- vec_quantiles(out, quantile_levels)
  tibble::tibble(.pred = .pred, .pred_quantile = out)
}

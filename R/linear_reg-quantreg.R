process_quantreg_preds <- function(x, object) {
  object <- parsnip::extract_fit_engine(object)
  type <- class(object)[1]


  # can't make a method because object is second
  out <- switch(
    type,
    rq = vec_quantiles(unname(as.list(x)), object$tau), # one quantile
    rqs = {
      x <- lapply(vctrs::vec_chop(x), function(x) sort(drop(x)))
      vec_quantiles(x, object$tau)
    },
    cli_abort(c(
      "Prediction is not implemented for this `rq` type.",
      i = "See {.fun quantreg::rq}."
    ))
  )
  return(tibble::tibble(.pred_quantile = out))
}

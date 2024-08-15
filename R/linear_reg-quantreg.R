#' Linear quantile regression via quantreg
#'
#' [quantreg::rq()] is used to estimate a linear model using quantile loss.
#'
#' @section Tuning Parameters:
#'
#' This model has no tuning parameters.:
#'
#'
#' @section Translation from parsnip to the original package:
#'
#' ```{r, translate-engine}
#' linear_reg(mode = "quantile regression", engine = "quantreg") %>%
#'   translate()
#' ```
#'
#' @section Case weights:
#'
#' Case weights are maybe? supported for quantile regression with this engine.
#'
#' @examples
#' tib <- data.frame(
#'   y = rnorm(100), x = rnorm(100), z = rnorm(100),
#'   f = factor(sample(letters[1:3], 100, replace = TRUE))
#' )
#' spec <- linear_reg(engine = "quantreg", mode = "quantile regression")
#' out <- fit(spec, formula = y ~ x + z, data = tib)
#' predict(out, new_data = tib[1:5, ])
#'
#' # -- adjusting the desired quantiles
#'
#' spec <- linear_reg(mode = "quantile regression") %>%
#'   set_engine(engine = "quantreg", tau = c(1:9 / 10))
#' out <- fit(spec, formula = y ~ x + z, data = tib)
#' predict(out, new_data = tib[1:5, ])
#'
#' @name quantreg-engine
NULL


process_quantreg_preds <- function(x, object) {
  object <- parsnip::extract_fit_engine(object)
  quantile_levels <- object$tau
  make_pred_tibble_from_matrix(x, quantile_levels)
}

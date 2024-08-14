# These functions define the linear regression models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_quantreg_quantiles <- function() {
  parsnip::set_model_mode("linear_reg", "quantile regression")
  parsnip::set_model_engine(
    model = "linear_reg", mode = "quantile regression", eng = "quantreg"
  )
  parsnip::set_dependency(
    model = "linear_reg", mode = "quantile regression" ,
    eng = "quantreg", pkg = "quantreg"
  )
  parsnip::set_dependency(
    model = "linear_reg", mode = "quantile regression" ,
    eng = "quantreg", pkg = "quantrines"
  )

  parsnip::set_fit(
    model = "linear_reg",
    eng = "quantreg",
    mode = "quantile regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "quantreg", fun = "rq"),
      defaults = list(
        method = "br",
        tau = c(.1, .5, .9),
        na.action = rlang::expr(stats::na.omit),
        model = FALSE
      )
    )
  )

  parsnip::set_encoding(
    model = "linear_reg",
    eng = "quantreg",
    mode = "quantile regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "linear_reg",
    eng = "quantreg",
    mode = "quantile regression",
    type = "quantile",
    value = list(
      pre = NULL,
      post = process_quantreg_preds,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )
}

# nocov end

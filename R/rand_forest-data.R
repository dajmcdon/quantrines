# These functions define the random forest models.
# They are executed when this package is loaded via `.onLoad()` and modify the
# parsnip package's model environment.

# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_grf_quantiles <- function() {
  parsnip::set_model_mode("rand_forest", "quantile regression")
  parsnip::set_model_engine(
    model = "rand_forest", mode = "quantile regression", eng = "grf"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf", pkg = "grf",
    mode = "quantile regression"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf", pkg = "quantrines",
    mode = "quantile regression"
  )


  # These are the arguments to the parsnip::rand_forest() that must be
  # translated from grf::quantile_forest
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf",
    parsnip = "trees",
    original = "num.trees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf",
    parsnip = "min_n",
    original = "min.node.size",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  # the `value` list describes how grf::quantile_forest expects to receive
  # arguments. In particular, it needs X and Y to be passed in as a matrices.
  # But the matrix interface in parsnip calls these x and y. So the data
  # slot translates them
  #
  # protect - prevents the user from passing x and y arguments themselves
  # defaults - engine specific arguments (not model specific) that we allow
  #   the user to change
  parsnip::set_fit(
    model = "rand_forest",
    eng = "grf",
    mode = "quantile regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y"),
      data = c(x = "X", y = "Y"),
      func = c(pkg = "grf", fun = "quantile_forest"),
      defaults = list(
        quantiles = c(0.1, 0.5, 0.9),
        num.threads = 1L,
        seed = rlang::expr(stats::runif(1, 0, .Machine$integer.max))
      )
    )
  )


  parsnip::set_encoding(
    model = "rand_forest",
    eng = "grf",
    mode = "quantile regression",
    options = list(
      # one hot is the closest to typical factor handling in randomForest
      # (1 vs all splitting), though since we aren't bagging,
      # factors with many levels could be visited frequently
      predictor_indicators = "one_hot",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )


  parsnip::set_pred(
    model = "rand_forest",
    eng = "grf",
    mode = "quantile regression",
    type = "quantile",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = process_quantile_forest_preds,
      func = c(fun = "predict"),
      object = quote(object$fit),
      newdata = quote(new_data),
      seed = rlang::expr(sample.int(10^5, 1)),
      verbose = FALSE
    )
  )
}

make_grf_regression <- function() {
  parsnip::set_model_engine(
    model = "rand_forest", mode = "regression", eng = "grf"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf", pkg = "grf",
    mode = "regression"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf", pkg = "quantrines",
    mode = "regression"
  )
  parsnip::set_encoding(
    model = "rand_forest",
    eng = "grf",
    mode = "regression",
    options = list(
      # one hot is the closest to typical factor handling in randomForest
      # (1 vs all splitting), though since we aren't bagging,
      # factors with many levels could be visited frequently
      predictor_indicators = "one_hot",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )
  parsnip::set_fit(
    model = "rand_forest",
    eng = "grf",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      data = c(x = "X", y = "Y", weights = "sample.weights"),
      func = c(pkg = "grf", fun = "regression_forest"),
      defaults = list(
        num.threads = 1L,
        seed = rlang::expr(stats::runif(1, 0, .Machine$integer.max))
      )
    )
  )
  parsnip::set_pred(
    model = "rand_forest",
    eng = "grf",
    mode = "regression",
    type = "numeric",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = process_regression_forest_preds,
      func = c(fun = "predict"),
      # map between parsnip::predict args and grf::quantile_forest args
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )
}


make_grf_classification <- function() {
  parsnip::set_model_engine(
    model = "rand_forest", mode = "classification", eng = "grf"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf", pkg = "grf",
    mode = "classification"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf", pkg = "quantrines",
    mode = "classification"
  )
  parsnip::set_fit(
    model = "rand_forest",
    eng = "grf",
    mode = "classification",
    value = list(
      interface = "matrix",
      protect = c("x", "y", "weights"),
      data = c(x = "X", y = "Y", weights = "sample.weights"),
      func = c(pkg = "grf", fun = "probability_forest"),
      defaults = list(
        num.threads = 1L,
        seed = rlang::expr(stats::runif(1, 0, .Machine$integer.max))
      )
    )
  )
  parsnip::set_pred(
    model = "rand_forest",
    eng = "grf",
    mode = "classification",
    type = "class",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = process_probability_forest_class,
      func = c(fun = "predict"),
      # map between parsnip::predict args and grf::quantile_forest args
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )
  parsnip::set_pred(
    model = "rand_forest",
    eng = "grf",
    mode = "classification",
    type = "prob",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = process_probability_forest_prob,
      func = c(fun = "predict"),
      object = quote(object$fit),
      newdata = quote(new_data)
    )
  )
}

# nocov end



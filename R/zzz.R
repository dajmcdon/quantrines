# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

# nocov start
.onLoad <- function(libname, pkgname) {
  make_grf_quantiles()
  make_grf_regression()
  make_grf_classification()

  make_quantreg_quantiles()
}

# nocov end

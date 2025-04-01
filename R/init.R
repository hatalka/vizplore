# R/init.R

.onLoad <- function(libname, pkgname) {
  # Disable oneDNN optimizations to avoid warnings
  Sys.setenv(TF_ENABLE_ONEDNN_OPTS = "0")
  # Reduce TensorFlow verbosity to only show errors
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "2")
}

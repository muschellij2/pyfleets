#' Use datafleets conda environment
#'
#' @return No return value
#' @export
use_datafleets_conda = function() {
  reticulate::use_condaenv("datafleets", required = TRUE)
}

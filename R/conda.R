#' Use datafleets conda environment
#'
#' @return No return value
#' @export
use_datafleets_conda = function() {
  reticulate::use_condaenv("datafleets", required = TRUE)
}


#' @rdname use_datafleets_conda
#' @export
#' @param yaml yaml file for the datafleets configuration
#' @note Download the file from
#' \url{https://docs.datafleets.io/docs/-MS9MPljCpDYQUqw9T7X/getting-started/preparing-your-python-environment}
create_fleet_conda = function(yaml = "datafleets.yml") {
  stopifnot(file.exists(yaml))
  conda <- reticulate::conda_binary(conda = "auto")
  conda_envs <- suppressWarnings(system2(
    conda, args = c("env", "create", "-f", yaml),
    stdout = TRUE, stderr = FALSE))

}

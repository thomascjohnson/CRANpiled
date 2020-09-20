#' Create a bare CRAN repository
#'
#' Creates a src/contrib directory under the \code{repo_dir}
#'
#' @param repo_dir file path to a directory that repository
#'
#' @return The original repo_dir argument
#' @export
create_repository <- function(repo_dir) {
  contrib_dir <- file.path(repo_dir, "src", "contrib")

  dir.create(contrib_dir, recursive = TRUE, showWarnings = FALSE)

  packages_file <- file.path(contrib_dir, "PACKAGES")

  if (!file.exists(packages_file))
    file.create(packages_file)

  repo_dir
}

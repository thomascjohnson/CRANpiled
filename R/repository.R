#' Create a bare repository
#'
#' @param repo_dir file path to a repository
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

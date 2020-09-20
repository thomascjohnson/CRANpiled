validate_dir <- function(repo_dir) {
  stopifnot(dir.exists(repo_dir))

  stopifnot(is.character(repo_dir))

  stopifnot(length(repo_dir) == 1)

  invisible()
}

is_CRAN_repo_dir <- function(repo_dir) {
  repo_contrib_path <- file.path(repo_dir, "src", "contrib")

  if (file.exists(file.path(repo_contrib_path, "PACKAGES")))
    TRUE
  else
    FALSE
}

validate_CRAN_rep_dir <- function(repo_dir) {
  if (!is_CRAN_repo_dir(repo_dir))
    stop(repo_dir, " does not have CRAN repository structure. ",
         "Be sure it has a src/contrib/PACKAGES file (even if blank) and a ",
         "src/contrib directory. You can also create a repository with these",
         "requirements with CRANpiled::create_repository.")

  invisible()
}

get_tempdir <- function(subpath, recursive = TRUE) {
  path <- file.path(tempdir(), subpath)

  if (!dir.exists(path))
    dir.create(path, recursive = recursive)

  path
}

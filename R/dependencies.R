#' Parse package dependency strings.
#'
#' Copied from \pkg{pkgload} in order to avoid dependency
#'
#' @param string to parse. Should look like `"R (>= 3.0), ggplot2"` etc.
#' @return list of two character vectors: `name` package names,
#'   and `version` package versions. If version is not specified,
#'   it will be stored as NA.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' parse_deps("httr (< 2.1),\nRCurl (>= 3)")
#' # only package dependencies are returned
#' parse_deps("utils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
#' }
parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, "[[:space:]]*,[[:space:]]*")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare  <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna   <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if (!all(compare_valid)) {
    stop("Invalid comparison operator in dependency: ",
         paste(compare_nna[!compare_valid], collapse = ", "))
  }

  deps <- data.frame(name = names, compare = compare,
                     version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}


get_package_deps <- function(packages,
                             available_packages = available.packages()) {
  available_packages <- clean_available_packages(available_packages)

  raw_deps <- lapply(packages, function(pkg) {
    if (dir.exists(pkg)) {
      imports <- read.dcf(
        file.path(pkg, "DESCRIPTION"), fields = "Imports"
      )

      depends <- read.dcf(
        file.path(pkg, "DESCRIPTION"), fields = "Depends"
      )

      raw_dependencies <- c(unname(imports[1, ]), unname(depends[1, ]))

      raw_dependencies <- Filter(Negate(is.na), raw_dependencies)

      if (length(raw_dependencies) > 0) {
        deps <- parse_deps(paste0(raw_dependencies, collapse = ", "))$name
      } else {
        deps <- c()
      }
    } else if (pkg %in% available_packages[, "Package"]) {
      deps <- tools::package_dependencies(
        pkg,
        db = available_packages,
        which = c("Depends", "Imports"),
        recursive = TRUE
      )[[pkg]]
    } else {
      stop("Unable to find the package ", pkg)
    }

    recursive_deps <- Reduce(
      c,
      lapply(deps, function(dep) {
        if (dep %in% available_packages[, "Package"])
          tools::package_dependencies(
            dep,
            db = available_packages,
            which = c("Depends", "Imports"),
            recursive = TRUE
          )[[dep]]
      })
    )

    deps <- unique(c(deps, recursive_deps))

    filter_base_dependencies(deps)
  })

  unique(Reduce(c, raw_deps))
}

filter_base_dependencies <- function(dependencies) {
  installed_packages <- installed.packages()
  installed_packages <- installed_packages[
    !is.na(installed_packages[, "Priority"]),
  ]

  base_packages <- installed_packages[
    installed_packages[, "Priority"] == "base", "Package"
  ]

  setdiff(dependencies, base_packages)
}

clean_available_packages <- function(
  available_packages = available.packages()
) {
  # If a repo only has one package, then the result of available.packages can't
  # be cast into a data frame -- but if it is transposed, it can be.
  if (is.null(dim(available_packages)))
    available_packages <- t(available_packages)

  as.data.frame(available_packages, stringsAsFactors = FALSE)
}



build_package <- function(package_dirs, package_names, compile = TRUE,
                          quiet = TRUE) {
  if (!(length(package_dirs) > 0))
    return(NULL)

  work_dir <- getwd()
  built_package_dir <- get_tempdir("CRANpiled-built-packages")
  setwd(built_package_dir)
  on.exit(setwd(work_dir))

  names(package_dirs) <- package_names
  if (!quiet) cat("\nBuilding packages\n")

  if (!quiet) {
    progress <- txtProgressBar(min = 1, max = length(package_dirs) + 1,
                               initial = 1, style = 3)
    cat("\n")
  }

  package_locs <- sapply(names(package_dirs), function(package_name) {
    package_dir <- package_dirs[[package_name]]

    if (!quiet) cat("Building", package_name)

    build_flag <- ifelse(compile, "--build", NULL)

    build_output <- system2(
      command = file.path(Sys.getenv("R_HOME"), "bin", "R"),
      args = c("CMD", "INSTALL", build_flag, package_dir),
      stdout = TRUE, stderr = TRUE
    )

    if (!quiet) {
      setTxtProgressBar(progress, getTxtProgressBar(progress) + 1)
      cat("\n")
    }

    tryCatch({
      get_built_tarball(package_name, built_package_dir)
    }, error = function(e) {
      stop("Error building ", package_name, "\nBuild output:\n", build_output,
           "\nError message: ", e)
    })
  })

  if (!quiet) close(progress)

  package_locs
}

get_DESCRIPTION_package_name <- function(package_dirs) {
  package_names <- sapply(package_dirs, function(package_dir) {
    read.dcf(file.path(package_dir, "DESCRIPTION"))[, "Package"]
  })

  unname(package_names)
}

is_built_tarball <- function(package_tarball) {
  isTRUE(
    grepl(
      "^([a-zA-Z0-9.]{2,}_[^_]+).*(([.]tar[.]gz)|([.]tgz))$",
      basename(package_tarball)
    )
  )
}

correct_tarball_name <- function(package_tarball) {
  stopifnot(is_built_tarball(package_tarball))

  gsub(
    "^([a-zA-Z0-9.]{2,}_[^_]+).*(([.]tar[.]gz)|([.]tgz))$",
    "\\1.tar.gz",
    package_tarball
  )
}

get_built_tarball <- function(package_name, built_package_dir) {
  built_tarball <- dir(
    built_package_dir,
    pattern = paste0("^", package_name, "_.*"),
    full.names = TRUE
  )

  if (length(built_tarball) != 1)
    stop("Unable to find built tarball")

  built_tarball
}

get_package_version <- function(package_tarball) {
  stopifnot(is_built_tarball(package_tarball))

  gsub("^[a-zA-Z0-9.]{2,}_([^_]+).*[.](tar[.]gz|tgz)$", "\\1", package_tarball)
}

get_package_name <- function(package_tarball) {
  stopifnot(is_built_tarball(package_tarball))

  gsub("^([a-zA-Z0-9.]{2,})_[^_]+.*", "\\1", package_tarball)
}

archive_packages <- function(package_tarballs, repository_dir) {
  contrib_dir <- file.path(repository_dir, "src", "contrib")
  archive_dir <- file.path(contrib_dir, "Archive")

  if (!dir.exists(archive_dir))
    dir.create(archive_dir)

  outofdate_tarballs <- lapply(package_tarballs, function(pkg_tarball) {
    package_version <- get_package_version(basename(pkg_tarball))
    package_name <- get_package_name(basename(pkg_tarball))

    existing_packages <- clean_available_packages(
      available.packages(contriburl = paste0("file://", contrib_dir))
    )

    if (package_name %in% existing_packages$Package) {
      existing_version <- existing_packages[package_name, "Version"]

      version_comparison <- compareVersion(package_version, existing_version)
      if (version_comparison %in% c(-1, 1)) {
        package_archive_dir <- file.path(archive_dir, package_name)

        if (!dir.exists(package_archive_dir))
          dir.create(package_archive_dir)

        if (version_comparison == 1) {
          contrib_tarball <- file.path(
            contrib_dir,
            paste0(package_name, "_", existing_version, ".tar.gz")
          )

          archive_tarball_path <- file.path(
            package_archive_dir,
            paste0(package_name, "_", existing_version, ".tar.gz")
          )

          file.copy(contrib_tarball, archive_tarball_path)
        } else if (version_comparison == -1) {
          archive_tarball_path <- file.path(
            package_archive_dir,
            paste0(package_name, "_", package_version, ".tar.gz")
          )

          file.copy(pkg_tarball, archive_tarball_path)
          return(pkg_tarball)
        }
      }
    }

    invisible()
  })

  setdiff(package_tarballs, outofdate_tarballs)
}

move_to_repo <- function(package_tarballs, repository_dir) {
  new_package_tarballs <- archive_packages(package_tarballs, repository_dir)

  contrib_dir <- file.path(repository_dir, "src", "contrib")

  lapply(new_package_tarballs, function(package_tarball) {
    correct_tarball <- correct_tarball_name(basename(package_tarball))
    contrib_tarball <- file.path(contrib_dir, correct_tarball)

    file.copy(package_tarball, contrib_tarball)
    file.remove(package_tarball)
  })

  invisible()
}

filter_existing <- function(packages, repository, available_packages) {
  available_packages <- clean_available_packages(available_packages)

  repo_avail_pkgs <- clean_available_packages(
    available.packages(repos = paste0("file://", normalizePath(repository)))
  )

  repo_packages <- repo_avail_pkgs[repo_avail_pkgs$Package %in% packages, ]

  CRAN_packages <- available_packages[
    available_packages$Package %in% repo_packages$Package,
  ]

  new_packages <- lapply(repo_packages$Package, function(package) {
    version_compare <- compareVersion(
      repo_packages[package, "Version"],
      CRAN_packages[package, "Version"]
    )

    if (version_compare != 0)
      package
    else
      NULL
  })

  new_packages <- Reduce(c, Filter(Negate(is.null), new_packages))

  c(new_packages, setdiff(packages, repo_packages$Package))
}

filter_installed <- function(packages, available_packages) {
  # installed.packages faces same issues as available.packages, so
  # clean_available_packages can be used here to turn it into a data frame.
  installed_packages <- clean_available_packages(installed.packages())

  available_packages <- clean_available_packages(available_packages)

  already_installed <- installed_packages[
    installed_packages$Package %in% packages,
  ]

  CRAN_packages <- available_packages[
    available_packages$Package %in% already_installed$Package,
  ]

  new_packages <- lapply(CRAN_packages$Package, function(package) {
    version_compare <- compareVersion(
      CRAN_packages[package, "Version"],
      already_installed[package, "Version"]
    )

    if (version_compare == 1)
      package
    else
      NULL
  })

  new_packages <- Reduce(c, Filter(Negate(is.null), new_packages))

  c(new_packages, setdiff(packages, installed_packages$Package))
}

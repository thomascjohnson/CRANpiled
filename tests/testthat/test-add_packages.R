context("Test adding packages")

repo_dir <- get_tempdir("test-adding-packages-cran")
dir.create(repo_dir, showWarnings = FALSE)

download_dir <- get_tempdir("test-adding-packages-download")
dir.create(download_dir, showWarnings = FALSE)
example_no_deps <- "quietR"
package_tarball <- download.packages(example_no_deps, tempdir(),
                                     repos = "cloud.r-project.org",
                                     quiet = TRUE)[, 2]
untar(package_tarball, exdir = download_dir)
package_dir <- file.path(download_dir, example_no_deps)

test_that("Invalid repo raises error", {
  expect_error(
    add_packages("ggplot2", tempdir(),
                 available.packages(repos = "cloud.r-project.org"))
  )
})

test_that("Invalid packages raises error", {
  expect_error(
    add_packages(c("ggplot2", "google.com/r-package.zip"), tempdir(),
                 available.packages(repos = "cloud.r-project.org"))
  )

  expect_error(
    add_packages("ggplot3", tempdir(),
                 available.packages(repos = "cloud.r-project.org"))
  )
})

test_that("Single package dir added successfully", {
  create_repository(repo_dir)

  avail_pkgs <- clean_available_packages(
    available.packages(repos = file.path("file://", repo_dir))
  )

  expect_false("quietR" %in% avail_pkgs$Package)

  add_package_dirs(package_dir, repo_dir,
                   available.packages(repos = "cloud.r-project.org"))

  avail_pkgs <- clean_available_packages(
    available.packages(repos = file.path("file://", repo_dir))
  )

  expect_true("quietR" %in% avail_pkgs$Package)
})

test_that("Single package by name added successfully", {
  packages_name_repo <- create_repository(
    get_tempdir("test-adding-packages-cran")
  )

  add_packages_by_name("quietR", packages_name_repo,
                       available.packages(repos = "cloud.r-project.org"))

  avail_pkgs <- clean_available_packages(
    available.packages(repos = file.path("file://", packages_name_repo))
  )

  expect_true("quietR" %in% avail_pkgs$Package)

  unlink(get_tempdir("test-adding-packages-cran"), TRUE)
})

test_that("add_packages", {
  add_packages_repo <- create_repository(
    get_tempdir("test-adding-packages-cran")
  )

  add_packages(
    c(
      "https://cran.r-project.org/src/contrib/Archive/jsonlite/jsonlite_0.9.1.tar.gz",
      "https://cran.r-project.org/src/contrib/Archive/yaml/yaml_2.0.0.tar.gz",
      "https://github.com/tidyverse/glue/archive/master.zip"
    ),
    add_packages_repo,
    available.packages(repos = "cloud.r-project.org")
  )

  avail_pkgs <- clean_available_packages(
    available.packages(repos = file.path("file://", add_packages_repo))
  )

  added_packages <- c("jsonlite", "yaml", "glue")

  expect_true(all(added_packages %in% avail_pkgs$Package))

  add_packages(c(package_dir, "jsonlite", "yaml", "mongolite"),
               add_packages_repo,
               available.packages(repos = "cloud.r-project.org"))

  avail_pkgs <- clean_available_packages(
    available.packages(repos = file.path("file://", add_packages_repo))
  )

  expect_true(
    all(c("quietR", "jsonlite", "yaml", "mongolite") %in% avail_pkgs$Package)
  )

  remove.packages("jsonlite")
  add_packages("json64", add_packages_repo,
               available.packages(repos = "cloud.r-project.org"))

  avail_pkgs <- clean_available_packages(
    available.packages(repos = file.path("file://", add_packages_repo))
  )

  expect_true("json64" %in% avail_pkgs$Package)


  expect_equal(compareVersion(avail_pkgs["jsonlite", "Version"], "0.9.1"), 1)
  expect_equal(compareVersion(avail_pkgs["yaml", "Version"], "2.0.0"), 1)
})

test_that("add_packages output", {
  expect_output(
    add_packages("gh", get_tempdir("test-adding-packages-cran"),
                 available.packages(repos = "cloud.r-project.org"),
                 quiet = FALSE)
  )
})

test_that("Un-buildable packages fail", {
  if (all(Sys.which("gdal-config") == ""))
    expect_error(
      suppressWarnings(
        add_packages("rgdal", get_tempdir("test-adding-packages-cran"),
                     available.packages(repos = "cloud.r-project.org"),
                     quiet = FALSE)
      )
    )
})



unlink(get_tempdir("test-adding-packages-cran"), recursive = TRUE,
       force = TRUE)
unlink(get_tempdir("test-adding-packages-download"), recursive = TRUE,
       force = TRUE)

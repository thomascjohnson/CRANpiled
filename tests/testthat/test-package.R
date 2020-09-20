context("Test package code")

example_build_tarball <- "utf8_1.1.4_R_x86_64-pc-linux-gnu.tar.gz"
correct_tarball <- "utf8_1.1.4.tar.gz"

test_that("correct_tarball_name corrects built tarball filename", {
  corrected_name <- correct_tarball_name(example_build_tarball)

  expect_equal(corrected_name, correct_tarball)
})

test_that("get_built_tarball", {
  expect_error(get_built_tarball("thispackagedoesn'texist", tempdir()))
})


test_that("get_package_version works", {
  expect_equal(get_package_version(correct_tarball), "1.1.4")
  expect_equal(get_package_version(example_build_tarball), "1.1.4")
})

test_that("correct_tarball_name", {
  expect_equal(
    correct_tarball_name("ggplot2_2.0.1_linux12345.tgz"),
    "ggplot2_2.0.1.tar.gz"
  )
})

test_that("get_DESCRIPTION_package_name", {
  package <- "ggplot2"
  tarball <- download.packages(
    package,
    get_tempdir("test-get_DESCRIPTION_package_name"),
    repos = "cloud.r-project.org",
    quiet = TRUE
  )[, 2]

  untar(tarball, exdir = get_tempdir("test-get_DESCRIPTION_package_name"))

  package_dir <- file.path(
    get_tempdir("test-get_DESCRIPTION_package_name"), package
  )

  expect_equal(get_DESCRIPTION_package_name(package_dir), package)

  unlink(get_tempdir("test-get_DESCRIPTION_package_name"), TRUE)
})

test_that("is_built_tarball works", {
  expect_true(is_built_tarball(example_build_tarball))
  expect_true(is_built_tarball(correct_tarball))
  expect_false(is_built_tarball("blah blah"))
})

test_that("build_package", {
  package <- download.packages(
    "quietR",
    get_tempdir("test-build_packages"),
    repos = "cloud.r-project.org",
    quiet = TRUE
  )[, 2]

  invisible(
    capture.output(
      built_package <- build_package(package, "quietR", quiet = TRUE)
    )
  )

  expect_true(is_built_tarball(built_package))
  expect_silent(install.packages(built_package, repos = NULL, quiet = TRUE))

  unlink(get_tempdir("test-build_packages"), TRUE)
})

test_that("get_package_name works", {
  expect_equal(get_package_name(example_build_tarball), "utf8")
  expect_equal(get_package_name(correct_tarball), "utf8")
  expect_error(get_package_name("blah blah"))
})

test_that("move_to_repo and archive_packages", {
  downloaded_packages <- get_tempdir("test-move_to_repo")
  temp_CRAN <- get_tempdir("test-move_to_repo")
  contrib_dir <- file.path(temp_CRAN, "src", "contrib")
  dir.create(contrib_dir, recursive = TRUE, showWarnings = FALSE)

  new_ggplot <- file.path(downloaded_packages, "ggplot2_3.3.2.tar.gz")

  download.file(
    "https://cloud.r-project.org/src/contrib/Archive/ggplot2/ggplot2_0.5.1.tar.gz",
    file.path(contrib_dir, "ggplot2_0.5.1.tar.gz"),
    quiet = TRUE
  )

  tools::write_PACKAGES(contrib_dir)

  download.file(
    "https://cloud.r-project.org/src/contrib/ggplot2_3.3.2.tar.gz",
    new_ggplot,
    quiet = TRUE
  )


  old_avail_pkgs <- clean_available_packages(
    available.packages(repos = paste0("file://", normalizePath(temp_CRAN)))
  )

  move_to_repo(new_ggplot, temp_CRAN)

  tools::write_PACKAGES(contrib_dir)

  new_avail_pkgs <- clean_available_packages(
    available.packages(repos = paste0("file://", normalizePath(temp_CRAN)))
  )

  expect_equal(old_avail_pkgs$Version, "0.5.1")
  expect_equal(new_avail_pkgs$Version, "3.3.2")
  expect_true(
    compareVersion(new_avail_pkgs$Version, old_avail_pkgs$Version) == 1
  )

  ggplot330 <- file.path(downloaded_packages, "ggplot2_3.3.0.tar.gz")

  download.file(
    "https://cloud.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.3.0.tar.gz",
    ggplot330,
    quiet = TRUE
  )

  move_to_repo(ggplot330, temp_CRAN)

  ggplot2_archive <- dir(file.path(contrib_dir, "Archive", "ggplot2"))

  expect_true("ggplot2_3.3.0.tar.gz" %in% ggplot2_archive)

  quietR <- download.packages("quietR", temp_CRAN,
                              repos = "cloud.r-project.org", quiet = TRUE)[, 2]

  expect_invisible(move_to_repo(quietR, temp_CRAN))

  unlink(temp_CRAN, TRUE)
})

test_that("filter_existing", {
  avail_packages <- clean_available_packages(
    available.packages(repos = "cloud.r-project.org")
  )

  td <- get_tempdir("filter_existing/src/contrib")
  PACKAGES <- file.path(td, "PACKAGES")
  download.file("cloud.r-project.org/src/contrib/PACKAGES", PACKAGES,
                repos = "cloud.r-project.org", quiet = TRUE)

  expect_equal(
    filter_existing(avail_packages$Package, get_tempdir("filter_existing"),
                    avail_packages),
    character(0)
  )

  unlink(get_tempdir("filter_existing"), TRUE)
})

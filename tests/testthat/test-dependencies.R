context("Test dependency related code")

# Copied from \package{pkgload} in order to avoid dependency
test_that("Parse dependencies", {
  deps <- parse_deps("\nhttr (< 2.1),\nRCurl (>= 3),\nutils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
  expect_equal(nrow(deps), 5)
  expect_false("R" %in% deps$name)

  expect_equal(deps$compare, c("<", ">=", "==", NA, NA))
  expect_equal(deps$version, c("2.1", "3", "2.12.1", NA, NA))
  expect_null(parse_deps(NULL))
  expect_null(parse_deps("  "))

  # Invalid version specifications
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (3.0)"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl ( 3.0)"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (==3.0)"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (==3.0 )"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl ( ==3.0)"))

  # This should be OK (no error)
  deps <- parse_deps("\nhttr (< 2.1),\nRCurl (==  3.0.1)")
  expect_equal(deps$compare, c("<", "=="))
  expect_equal(deps$version, c("2.1", "3.0.1"))
})

test_that("Base dependencies are filtered", {
  expect_equal(filter_base_dependencies(c("tools", "stats")), character(0))
  deps <- c("ggplot2", "dplyr")
  expect_equal(filter_base_dependencies(c("tools", "stats", deps)), deps)
})

test_that("Get package dependencies", {
  example_deps <- c("dplyr", "ggplot2", "sf", "rgdal")
  example_deps_versioned <- c("dplyr (< 1.0.0)", "ggplot2 (== 3.3.2)", "sf", "rgdal")
  example_package_dir <- get_tempdir("test-package-dependencies")

  write.dcf(
    list("Imports" = paste0("\n  ", example_deps_versioned, collapse = ",\n  ")),
    file.path(example_package_dir, "DESCRIPTION"),
    keep.white = "Imports"
  )

  avail_pkgs <- available.packages(repos = "cloud.r-project.org")

  # Creates a subset of available packages for test case, removes Imports so
  # that the changes in dependencies going forward doesn't break this test
  specific_avail_pkgs <- avail_pkgs[c("dplyr", "ggplot2", "sf", "rgdal"), ]
  specific_avail_pkgs[, "Imports"] <- NA
  specific_avail_pkgs[, "Depends"] <- NA

  package_deps <- get_package_deps(example_package_dir, specific_avail_pkgs)
  expect_setequal(package_deps, example_deps)

  write.dcf(
    list("Depends" = ""),
    file.path(example_package_dir, "DESCRIPTION"),
    keep.white = "Depends"
  )

  expect_equal(
    get_package_deps(example_package_dir, specific_avail_pkgs),
    c()
  )

  expect_true(length(get_package_deps("quietR", avail_pkgs)) == 0)
  expect_error(get_package_deps("thispackagedoesn'texist", avail_pkgs))

  unlink(example_package_dir, TRUE)
})

test_that("clean_available_packages", {
  avail_packages <- clean_available_packages(
    available.packages(repos = "cloud.r-project.org")
  )

  expect_true(is.data.frame(avail_packages))

  example_PACKAGES <- file.path(get_tempdir("test-clean_available_packages"),
                                "PACKAGES")

  write.dcf(avail_packages[1, ], example_PACKAGES,
            keep.white = names(avail_packages))

  single_avail_pkgs <- clean_available_packages(
    available.packages(paste0("file://", dirname(example_PACKAGES)))
  )

  expect_true(is.data.frame(single_avail_pkgs))

  unlink(get_tempdir("test-clean_available_packages"), TRUE)
})

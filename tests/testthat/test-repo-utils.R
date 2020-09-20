context("Test repo utils")

CRAN_temp_dir <- get_tempdir("testCRAN")
CRAN_contrib_dir <- file.path(CRAN_temp_dir, "src", "contrib")

test_that("Repo path validation works", {
  expect_error(validate_dir(1))
  expect_error(validate_dir(c(tempdir(), tempdir())))
  expect_error(validate_dir(file.path(tempdir(), "idontexist")))
  expect_invisible(validate_dir(CRAN_temp_dir))
})

test_that("is_CRAN_repo_dir identifies CRAN repos", {
  expect_false(is_CRAN_repo_dir(CRAN_temp_dir))
  dir.create(CRAN_contrib_dir, recursive = TRUE)
  file.create(file.path(CRAN_contrib_dir, "PACKAGES"))
  expect_true(is_CRAN_repo_dir(CRAN_temp_dir))
})

test_that("get_tempdir", {
  td <- get_tempdir("blah")
  expect_true(dir.exists(td))
  expect_equal(get_tempdir("blah"), td)

  unlink(get_tempdir("blah"), TRUE)
})

test_that("validate_CRAN_rep_dir", {
  expect_error(validate_CRAN_rep_dir(tempdir()))
  expect_invisible(validate_CRAN_rep_dir(CRAN_temp_dir))
})

unlink(CRAN_temp_dir, TRUE)

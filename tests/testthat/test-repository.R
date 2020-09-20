context("Test make_repository")

test_that("make_repository creates a repository", {
  repo_dir <- get_tempdir("test_repository")
  create_repository(repo_dir)
  expect_true(is_CRAN_repo_dir(repo_dir))
  unlink(get_tempdir("test_repository"), TRUE)
})

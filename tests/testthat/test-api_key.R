# Tests for census_api_key() and get_api_key()
# These functions manage the Census API key storage
# Note: .census_env is defined in helper-mocks.R

# Helper to access internal function
get_api_key <- getFromNamespace("get_api_key", "censuschat")

# =============================================================================
# census_api_key() tests
# =============================================================================

test_that("census_api_key sets key in system environment", {
  skip_if_not_installed("withr")

  withr::local_envvar(CENSUS_API_KEY = "")

  census_api_key("test_key_123")

  expect_equal(Sys.getenv("CENSUS_API_KEY"), "test_key_123")
})

test_that("census_api_key sets key in package environment", {
  skip_if_not_installed("withr")

  # Clear package env
  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  .census_env$api_key <- NULL

  census_api_key("pkg_test_key")

  expect_equal(.census_env$api_key, "pkg_test_key")
})

test_that("census_api_key returns key invisibly", {
  skip_if_not_installed("withr")

  withr::local_envvar(CENSUS_API_KEY = "")

  result <- census_api_key("invisible_key")

  expect_equal(result, "invisible_key")
  expect_invisible(census_api_key("another_key"))
})

test_that("census_api_key with install=TRUE writes to .Renviron", {
  skip_if_not_installed("withr")

  # Create a temporary HOME directory
 temp_home <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_home)

  # Also need to handle R_USER on Windows
  withr::local_envvar(R_USER = temp_home)

  census_api_key("persistent_key", install = TRUE)

  renviron_path <- file.path(temp_home, ".Renviron")

  # File should exist
  expect_true(file.exists(renviron_path))

  # Should contain our key
  lines <- readLines(renviron_path)
  expect_true(any(grepl("CENSUS_API_KEY=persistent_key", lines)))
})

test_that("census_api_key with install=TRUE preserves existing .Renviron content", {
  skip_if_not_installed("withr")

  temp_home <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_home)
  withr::local_envvar(R_USER = temp_home)

  # Create existing .Renviron with other content
  renviron_path <- file.path(temp_home, ".Renviron")
  writeLines(c("OTHER_VAR=some_value", "ANOTHER_VAR=another_value"), renviron_path)

  census_api_key("new_key", install = TRUE)

  lines <- readLines(renviron_path)

  # Original content should still be there
  expect_true(any(grepl("OTHER_VAR=some_value", lines)))
  expect_true(any(grepl("ANOTHER_VAR=another_value", lines)))

  # New key should be added
  expect_true(any(grepl("CENSUS_API_KEY=new_key", lines)))
})

test_that("census_api_key with install=TRUE replaces existing key", {
  skip_if_not_installed("withr")

  temp_home <- withr::local_tempdir()
  withr::local_envvar(HOME = temp_home)
  withr::local_envvar(R_USER = temp_home)

  # Create existing .Renviron with old Census key
  renviron_path <- file.path(temp_home, ".Renviron")
  writeLines(c("OTHER_VAR=value", "CENSUS_API_KEY=old_key"), renviron_path)

  census_api_key("new_key", install = TRUE)

  lines <- readLines(renviron_path)

  # Should only have ONE Census key entry (the new one)
  census_lines <- grep("CENSUS_API_KEY=", lines, value = TRUE)
  expect_length(census_lines, 1)
  expect_equal(census_lines, "CENSUS_API_KEY=new_key")
})

# =============================================================================
# get_api_key() tests
# =============================================================================

test_that("get_api_key retrieves from package environment first", {
  skip_if_not_installed("withr")

  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  withr::local_envvar(CENSUS_API_KEY = "env_key")
  .census_env$api_key <- "pkg_key"

  # Package environment should take priority
  expect_equal(get_api_key(), "pkg_key")
})

test_that("get_api_key falls back to system environment", {
  skip_if_not_installed("withr")

  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  .census_env$api_key <- NULL
  withr::local_envvar(CENSUS_API_KEY = "system_env_key")

  expect_equal(get_api_key(), "system_env_key")
})

test_that("get_api_key errors when no key is found", {
  skip_if_not_installed("withr")

  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  .census_env$api_key <- NULL
  withr::local_envvar(CENSUS_API_KEY = "")

  expect_error(get_api_key(), "Census API key not found")
})

test_that("get_api_key error message is helpful", {
  skip_if_not_installed("withr")

  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  .census_env$api_key <- NULL
  withr::local_envvar(CENSUS_API_KEY = "")

  error <- expect_error(get_api_key())

  # Should mention how to set the key
  expect_match(error$message, "census_api_key", ignore.case = TRUE)
  # Should mention where to get a key
  expect_match(error$message, "census.gov", ignore.case = TRUE)
})

test_that("get_api_key treats empty string as missing", {
  skip_if_not_installed("withr")

  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  .census_env$api_key <- ""
  withr::local_envvar(CENSUS_API_KEY = "")

  expect_error(get_api_key(), "Census API key not found")
})

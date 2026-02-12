# Helper functions for mocking external APIs in tests
# This file is automatically loaded by testthat before running tests

# Get reference to package internal environment
# This is evaluated when the helper file is loaded (after library(censuschat))
.census_env <- getFromNamespace(".census_env", "censuschat")

# =============================================================================
# Mock Census API Responses (for httr2)
# =============================================================================

#' Create a mock Census datasets API response
#' @param datasets List of dataset objects, or NULL for default
#' @return A function suitable for httr2::local_mocked_responses()
mock_census_datasets_response <- function(datasets = NULL) {
  if (is.null(datasets)) {
    datasets <- list(
      list(
        title = "American Community Survey 5-Year Data",
        description = "ACS 5-year estimates providing detailed demographic data",
        c_dataset = c("acs", "acs5"),
        c_vintage = 2022
      ),
      list(
        title = "Decennial Census",
        description = "Population and housing counts from the decennial census",
        c_dataset = c("dec", "dhc"),
        c_vintage = 2020
      ),
      list(
        title = "American Community Survey 1-Year Data",
        description = "ACS 1-year estimates for areas with 65,000+ population",
        c_dataset = c("acs", "acs1"),
        c_vintage = 2022
      )
    )
  }

  function(req) {
    httr2::response_json(
      status_code = 200L,
      body = list(dataset = datasets)
    )
  }
}

# =============================================================================
# Mock tidycensus Responses
# =============================================================================

#' Create sample ACS data for testing
#' @param n Number of rows
#' @param geography Geography type for naming
#' @return A data.frame resembling tidycensus::get_acs() output
sample_acs_data <- function(n = 5, geography = "state") {
  data.frame(
    GEOID = sprintf("%02d", seq_len(n)),
    NAME = paste("State", seq_len(n)),
    variable = rep("B19013_001", n),
    estimate = seq(50000, by = 5000, length.out = n),
    moe = rep(500, n),
    stringsAsFactors = FALSE
  )
}

#' Create sample decennial census data for testing
#' @param n Number of rows
#' @return A data.frame resembling tidycensus::get_decennial() output
sample_decennial_data <- function(n = 5) {
  data.frame(
    GEOID = sprintf("%02d", seq_len(n)),
    NAME = paste("State", seq_len(n)),
    variable = rep("P1_001N", n),
    value = seq(1000000, by = 500000, length.out = n),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Mock ellmer Chat Object
# =============================================================================

#' Create a mock ellmer Chat object for testing
#' @return An R6-like object that mimics ellmer::Chat
mock_ellmer_chat <- function() {
  # Create a simple list-based mock that behaves like an R6 object
  chat_env <- new.env(parent = emptyenv())
  chat_env$tools <- list()
  chat_env$system_prompt <- NULL
  chat_env$turns <- list()

  structure(
    list(
      set_tools = function(tools) {
        chat_env$tools <- tools
        invisible(NULL)
      },
      get_tools = function() {
        chat_env$tools
      },
      chat = function(msg) {
        chat_env$turns <- c(chat_env$turns, list(msg))
        "Mock response from LLM"
      },
      get_turns = function() {
        chat_env$turns
      }
    ),
    class = c("MockChat", "Chat")
  )
}

# =============================================================================
# Helper to temporarily set Census API key
# =============================================================================

#' Set a temporary Census API key for testing
#' @param key The API key to use (default: "test_key_for_testing")
#' @return Invisibly returns the previous key value
with_mock_census_key <- function(key = "test_key_for_testing") {
  old_env_key <- Sys.getenv("CENSUS_API_KEY")
  old_pkg_key <- .census_env$api_key

  Sys.setenv(CENSUS_API_KEY = key)
  .census_env$api_key <- key

  withr::defer({
    if (old_env_key == "") {
      Sys.unsetenv("CENSUS_API_KEY")
    } else {
      Sys.setenv(CENSUS_API_KEY = old_env_key)
    }
    .census_env$api_key <- old_pkg_key
  }, envir = parent.frame())

  invisible(old_pkg_key)
}

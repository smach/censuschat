# Tests for census_chat_create()
# This function creates an ellmer chat object with Census tools
# Note: .census_env is defined in helper-mocks.R

# =============================================================================
# Basic functionality tests
# =============================================================================

test_that("census_chat_create requires API key", {
  skip_if_not_installed("withr")

  old_key <- .census_env$api_key
  withr::defer(.census_env$api_key <- old_key)

  .census_env$api_key <- NULL
  withr::local_envvar(CENSUS_API_KEY = "")

  expect_error(census_chat_create(), "Census API key not found")
})

test_that("census_chat_create returns a chat object", {
  skip_if_not_installed("withr")

  # Set up mock API key
  with_mock_census_key()

  # Create a mock chat object that tracks tool registration
  mock_chat_obj <- list(
    tools = list(),
    set_tools = function(tools) {
      mock_chat_obj$tools <<- tools
    },
    get_tools = function() {
      mock_chat_obj$tools
    }
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(...) mock_chat_obj,
    .package = "censuschat"
  )

  chat <- census_chat_create()

  # Should return the chat object
  expect_true(inherits(chat, "Chat"))
})

test_that("census_chat_create registers Census tools", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  registered_tools <- NULL

  # Track what tools get registered
  mock_chat_obj <- list(
    tools = list(),
    set_tools = function(tools) {
      registered_tools <<- tools
      mock_chat_obj$tools <<- tools
    }
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(...) mock_chat_obj,
    .package = "censuschat"
  )

  census_chat_create()

  # Should register 5 tools
  expect_length(registered_tools, 5)
})

test_that("census_chat_create passes system prompt to chat", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_system_prompt <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, system_prompt = NULL, ...) {
      captured_system_prompt <<- system_prompt
      mock_chat_obj
    },
    .package = "censuschat"
  )

  census_chat_create()

  # Default system prompt should not be NULL
  expect_false(is.null(captured_system_prompt))
  expect_type(captured_system_prompt, "character")
})

test_that("census_chat_create uses custom system prompt when provided", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_system_prompt <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, system_prompt = NULL, ...) {
      captured_system_prompt <<- system_prompt
      mock_chat_obj
    },
    .package = "censuschat"
  )

  custom_prompt <- "You are a Census data expert. Be concise."
  census_chat_create(system_prompt = custom_prompt)

  expect_equal(captured_system_prompt, custom_prompt)
})

test_that("census_chat_create passes provider to chat", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_provider <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, ...) {
      captured_provider <<- provider
      mock_chat_obj
    },
    .package = "censuschat"
  )

  census_chat_create(provider = "openai/gpt-4o")

  expect_equal(captured_provider, "openai/gpt-4o")
})

test_that("census_chat_create uses default provider", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_provider <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, ...) {
      captured_provider <<- provider
      mock_chat_obj
    },
    .package = "censuschat"
  )

  census_chat_create()

  # Default should be Anthropic Claude
  expect_match(captured_provider, "anthropic", ignore.case = TRUE)
})

# =============================================================================
# System prompt content tests
# =============================================================================

test_that("default system prompt mentions Census data", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_system_prompt <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, system_prompt = NULL, ...) {
      captured_system_prompt <<- system_prompt
      mock_chat_obj
    },
    .package = "censuschat"
  )

  census_chat_create()

  expect_match(captured_system_prompt, "Census", ignore.case = TRUE)
})

test_that("default system prompt mentions ACS and decennial", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_system_prompt <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, system_prompt = NULL, ...) {
      captured_system_prompt <<- system_prompt
      mock_chat_obj
    },
    .package = "censuschat"
  )

  census_chat_create()

  expect_match(captured_system_prompt, "ACS", ignore.case = TRUE)
  expect_match(captured_system_prompt, "decennial", ignore.case = TRUE)
})

test_that("default system prompt mentions tidycensus", {
  skip_if_not_installed("withr")

  with_mock_census_key()

  captured_system_prompt <- NULL

  mock_chat_obj <- list(
    set_tools = function(tools) invisible(NULL)
  )
  class(mock_chat_obj) <- c("Chat", "R6")

  local_mocked_bindings(
    chat = function(provider, system_prompt = NULL, ...) {
      captured_system_prompt <<- system_prompt
      mock_chat_obj
    },
    .package = "censuschat"
  )

  census_chat_create()

  expect_match(captured_system_prompt, "tidycensus", ignore.case = TRUE)
})

# Tests for census_tools() and tool structure validation

test_that("census_tools returns a list of 5 tools", {
  tools <- census_tools()


  expect_type(tools, "list")
  expect_length(tools, 5)
})

test_that("each tool is a ToolDef object", {
  tools <- census_tools()

  for (i in seq_along(tools)) {
    # S7 classes use namespace-qualified class names
    expect_true(inherits(tools[[i]], "ellmer::ToolDef"))
  }
})

test_that("tool_list_datasets has correct structure", {
  tool <- tool_list_datasets()

  # S7 classes use namespace-qualified class names
  expect_true(inherits(tool, "ellmer::ToolDef"))
  expect_match(tool@description, "Census", ignore.case = TRUE)
  expect_match(tool@description, "dataset", ignore.case = TRUE)

  # Verify tool is callable with expected parameter
  # (functional test instead of checking internal structure)
  expect_no_error(tool(search_term = NULL))
})

test_that("tool_fetch_data has required arguments", {
  tool <- tool_fetch_data()

  # S7 classes use namespace-qualified class names
  expect_true(inherits(tool, "ellmer::ToolDef"))

  # Verify tool is callable with expected parameters
  # (functional test instead of checking internal structure)
  expect_no_error(tool(
    geography = "state",
    variables = c("B19013_001"),
    code_only = TRUE
  ))
})

test_that("tool_resolve_fips has correct structure", {
  tool <- tool_resolve_fips()

  # S7 classes use namespace-qualified class names
  expect_true(inherits(tool, "ellmer::ToolDef"))
  expect_match(tool@description, "FIPS", ignore.case = TRUE)

  # Verify tool is callable with expected parameters
  expect_no_error(tool(name = "California", geography_type = "state"))
})

test_that("tool_get_variables has correct structure", {
  tool <- tool_get_variables()

  # S7 classes use namespace-qualified class names
  expect_true(inherits(tool, "ellmer::ToolDef"))
  expect_match(tool@description, "variable", ignore.case = TRUE)

  # Verify tool is callable with expected parameters
  expect_no_error(tool(category = "income", dataset = "acs"))
})

test_that("tool_get_place_tracts has correct structure", {
  tool <- tool_get_place_tracts()

  # S7 classes use namespace-qualified class names
  expect_true(inherits(tool, "ellmer::ToolDef"))
  expect_match(tool@description, "tract", ignore.case = TRUE)

  # Verify tool is callable with expected parameters
  expect_no_error(tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001")
  ))
})

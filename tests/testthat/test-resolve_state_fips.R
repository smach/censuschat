# Tests for resolve_state_fips()
# This is an internal function that maps state names/abbreviations to FIPS codes

# Helper to access the internal function
resolve_state_fips <- getFromNamespace("resolve_state_fips", "censuschat")

# =============================================================================
# State name tests
# =============================================================================

test_that("resolve_state_fips handles full state names", {
  expect_equal(resolve_state_fips("California"), "06")
  expect_equal(resolve_state_fips("Texas"), "48")
  expect_equal(resolve_state_fips("New York"), "36")
  expect_equal(resolve_state_fips("Massachusetts"), "25")
  expect_equal(resolve_state_fips("Florida"), "12")
})

test_that("resolve_state_fips is case-insensitive for state names", {
  expect_equal(resolve_state_fips("california"), "06")
  expect_equal(resolve_state_fips("CALIFORNIA"), "06")
  expect_equal(resolve_state_fips("California"), "06")
  expect_equal(resolve_state_fips("CaLiFoRnIa"), "06")
})

test_that("resolve_state_fips handles whitespace", {
  expect_equal(resolve_state_fips("  California  "), "06")
  expect_equal(resolve_state_fips(" texas "), "48")
  expect_equal(resolve_state_fips("\tNew York\t"), "36")
})

# =============================================================================
# State abbreviation tests
# =============================================================================

test_that("resolve_state_fips handles state abbreviations", {
  expect_equal(resolve_state_fips("CA"), "06")
  expect_equal(resolve_state_fips("TX"), "48")
  expect_equal(resolve_state_fips("NY"), "36")
  expect_equal(resolve_state_fips("MA"), "25")
  expect_equal(resolve_state_fips("FL"), "12")
})

test_that("resolve_state_fips is case-insensitive for abbreviations", {
  expect_equal(resolve_state_fips("ca"), "06")
  expect_equal(resolve_state_fips("Ca"), "06")
  expect_equal(resolve_state_fips("CA"), "06")
  expect_equal(resolve_state_fips("tx"), "48")
  expect_equal(resolve_state_fips("Tx"), "48")
})

# =============================================================================
# FIPS code passthrough tests
# =============================================================================

test_that("resolve_state_fips passes through valid FIPS codes", {
  expect_equal(resolve_state_fips("06"), "06")
  expect_equal(resolve_state_fips("48"), "48")
  expect_equal(resolve_state_fips("36"), "36")
  expect_equal(resolve_state_fips("01"), "01")
  expect_equal(resolve_state_fips("72"), "72")
})

# =============================================================================
# Special case tests (DC, Puerto Rico)
# =============================================================================

test_that("resolve_state_fips handles District of Columbia", {
  expect_equal(resolve_state_fips("District of Columbia"), "11")
  expect_equal(resolve_state_fips("district of columbia"), "11")
  expect_equal(resolve_state_fips("DC"), "11")
  expect_equal(resolve_state_fips("dc"), "11")
  expect_equal(resolve_state_fips("D.C."), "11")
  expect_equal(resolve_state_fips("d.c."), "11")
})

test_that("resolve_state_fips handles Puerto Rico", {
  expect_equal(resolve_state_fips("Puerto Rico"), "72")
  expect_equal(resolve_state_fips("puerto rico"), "72")
  expect_equal(resolve_state_fips("PR"), "72")
  expect_equal(resolve_state_fips("pr"), "72")
})

# =============================================================================
# Invalid input tests
# =============================================================================

test_that("resolve_state_fips returns NULL for invalid state names", {
  expect_null(resolve_state_fips("InvalidState"))
  expect_null(resolve_state_fips("Atlantis"))
  expect_null(resolve_state_fips("NotAState"))
})

test_that("resolve_state_fips returns NULL for invalid abbreviations", {
  expect_null(resolve_state_fips("XX"))
  expect_null(resolve_state_fips("ZZ"))
  expect_null(resolve_state_fips("AB"))
})

test_that("resolve_state_fips returns NULL for non-2-digit invalid codes", {
  # 3 digits - not a valid FIPS format
  expect_null(resolve_state_fips("999"))
  # 1 digit - not a valid FIPS format
  expect_null(resolve_state_fips("1"))
})

test_that("resolve_state_fips passes through any 2-digit code", {
  # The function doesn't validate FIPS codes - it passes through any 2-digit string

  # This is intentional: if user provides a 2-digit code, assume they know what they want
  expect_equal(resolve_state_fips("00"), "00")
  expect_equal(resolve_state_fips("99"), "99")
})

# =============================================================================
# Comprehensive state coverage tests
# =============================================================================

test_that("resolve_state_fips handles all 50 states", {
  states <- list(
    c("Alabama", "AL", "01"),
    c("Alaska", "AK", "02"),
    c("Arizona", "AZ", "04"),
    c("Arkansas", "AR", "05"),
    c("California", "CA", "06"),
    c("Colorado", "CO", "08"),
    c("Connecticut", "CT", "09"),
    c("Delaware", "DE", "10"),
    c("Florida", "FL", "12"),
    c("Georgia", "GA", "13"),
    c("Hawaii", "HI", "15"),
    c("Idaho", "ID", "16"),
    c("Illinois", "IL", "17"),
    c("Indiana", "IN", "18"),
    c("Iowa", "IA", "19"),
    c("Kansas", "KS", "20"),
    c("Kentucky", "KY", "21"),
    c("Louisiana", "LA", "22"),
    c("Maine", "ME", "23"),
    c("Maryland", "MD", "24"),
    c("Massachusetts", "MA", "25"),
    c("Michigan", "MI", "26"),
    c("Minnesota", "MN", "27"),
    c("Mississippi", "MS", "28"),
    c("Missouri", "MO", "29"),
    c("Montana", "MT", "30"),
    c("Nebraska", "NE", "31"),
    c("Nevada", "NV", "32"),
    c("New Hampshire", "NH", "33"),
    c("New Jersey", "NJ", "34"),
    c("New Mexico", "NM", "35"),
    c("New York", "NY", "36"),
    c("North Carolina", "NC", "37"),
    c("North Dakota", "ND", "38"),
    c("Ohio", "OH", "39"),
    c("Oklahoma", "OK", "40"),
    c("Oregon", "OR", "41"),
    c("Pennsylvania", "PA", "42"),
    c("Rhode Island", "RI", "44"),
    c("South Carolina", "SC", "45"),
    c("South Dakota", "SD", "46"),
    c("Tennessee", "TN", "47"),
    c("Texas", "TX", "48"),
    c("Utah", "UT", "49"),
    c("Vermont", "VT", "50"),
    c("Virginia", "VA", "51"),
    c("Washington", "WA", "53"),
    c("West Virginia", "WV", "54"),
    c("Wisconsin", "WI", "55"),
    c("Wyoming", "WY", "56")
  )

  for (state in states) {
    state_name <- state[1]
    state_abbr <- state[2]
    expected_fips <- state[3]

    expect_equal(
      resolve_state_fips(state_name),
      expected_fips,
      info = paste("Failed for state name:", state_name)
    )
    expect_equal(
      resolve_state_fips(state_abbr),
      expected_fips,
      info = paste("Failed for abbreviation:", state_abbr)
    )
  }
})

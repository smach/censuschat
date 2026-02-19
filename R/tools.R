#' Create Census data tools for ellmer
#'
#' Returns a list of ellmer tool objects that can be registered with a chat
#'
#' @return List of ellmer tool objects
#' @export
#' @examples
#' \dontrun{
#' library(ellmer)
#' chat <- chat_anthropic()
#' chat$set_tools(census_tools())
#' chat$chat("What's the median income in California?")
#' }
census_tools <- function() {
  list(
    tool_list_datasets(),
    tool_fetch_data(),
    tool_resolve_fips(),
    tool_get_variables(),
    tool_get_place_tracts()
  )
}

#' Tool: List Census Datasets
#' @return An ellmer [tool()] definition.
#' @keywords internal
tool_list_datasets <- function() {
  tool(
    function(search_term = NULL) {
      req <- request("https://api.census.gov/data.json")
      resp <- req_perform(req)
      data <- resp_body_json(resp)

      datasets <- lapply(data$dataset, function(ds) {
        list(
          title = ds$title %||% NA,
          description = ds$description %||% NA,
          identifier = paste(ds$c_dataset, collapse = "/"),
          vintage = ds$c_vintage %||% NA
        )
      })

      if (!is.null(search_term) && search_term != "") {
        search_lower <- tolower(search_term)
        datasets <- Filter(function(ds) {
          grepl(search_lower, tolower(ds$title %||% "")) ||
            grepl(search_lower, tolower(ds$description %||% ""))
        }, datasets)
      }

      if (length(datasets) > 30) datasets <- datasets[1:30]

      toJSON(list(
        count = length(datasets),
        datasets = datasets
      ), auto_unbox = TRUE, pretty = TRUE)
    },
    description = "List available Census Bureau datasets. Use search_term to filter.",
    arguments = list(
      search_term = type_string("Optional keyword to filter datasets", required = FALSE)
    )
  )
}

#' Tool: Fetch Census Data using tidycensus
#' @return An ellmer [tool()] definition.
#' @keywords internal
tool_fetch_data <- function() {

  tool(
    function(geography, variables, year = NULL, state = NULL, county = NULL,
             survey = "acs5", decennial_sumfile = NULL, code_only = FALSE) {

      # Determine if this is decennial or ACS
      is_decennial <- !is.null(decennial_sumfile)

      # Build the tidycensus code string
      if (is_decennial) {
        # Decennial census - year is required for decennial
        if (is.null(year)) year <- 2020  # Most recent decennial
        code_parts <- sprintf(
          'library(tidycensus)\n\ndata <- get_decennial(\n  geography = "%s",\n  variables = c("%s"),\n  year = %d,\n  sumfile = "%s"',
          geography,
          paste(variables, collapse = '", "'),
          year,
          decennial_sumfile
        )
        if (!is.null(state)) code_parts <- paste0(code_parts, sprintf(',\n  state = "%s"', state))
        if (!is.null(county)) code_parts <- paste0(code_parts, sprintf(',\n  county = "%s"', county))
        tidycensus_code <- paste0(code_parts, "\n)\n\nprint(data)")
      } else {
        # ACS - let tidycensus use its default year when not specified
        code_parts <- sprintf(
          'library(tidycensus)\n\ndata <- get_acs(\n  geography = "%s",\n  variables = c("%s"),\n  survey = "%s"',
          geography,
          paste(variables, collapse = '", "'),
          survey
        )
        if (!is.null(year)) code_parts <- paste0(code_parts, sprintf(',\n  year = %d', year))
        if (!is.null(state)) code_parts <- paste0(code_parts, sprintf(',\n  state = "%s"', state))
        if (!is.null(county)) code_parts <- paste0(code_parts, sprintf(',\n  county = "%s"', county))
        tidycensus_code <- paste0(code_parts, "\n)\n\nprint(data)")
      }

      # CODE ONLY MODE: Return just the code
      if (code_only) {
        return(toJSON(list(
          mode = "code_only",
          message = "Here's the tidycensus R code. Run this in R to get the data.",
          tidycensus_code = tidycensus_code
        ), auto_unbox = TRUE, pretty = TRUE))
      }

      # EXECUTE MODE: Run tidycensus and return results
      tryCatch({
        if (is_decennial) {
          data <- tidycensus::get_decennial(
            geography = geography,
            variables = variables,
            year = year,
            sumfile = decennial_sumfile,
            state = state,
            county = county
          )
        } else {
          # Build args dynamically so tidycensus uses its default year when not specified
          acs_args <- list(
            geography = geography,
            variables = variables,
            survey = survey,
            state = state,
            county = county
          )
          if (!is.null(year)) acs_args$year <- year
          data <- do.call(tidycensus::get_acs, acs_args)
        }

        n_rows <- nrow(data)

        # Convert to list for JSON
        data_list <- lapply(seq_len(nrow(data)), function(i) as.list(data[i, ]))

        # SMART RESPONSE SIZING
        if (n_rows <= 15) {
          # Small: Full data
          toJSON(list(
            rows_returned = n_rows,
            tidycensus_code = tidycensus_code,
            data = data_list
          ), auto_unbox = TRUE, pretty = TRUE)

        } else if (n_rows <= 60) {
          # Medium: Full data with note
          toJSON(list(
            rows_returned = n_rows,
            tidycensus_code = tidycensus_code,
            data = data_list,
            note = "For further analysis, use the tidycensus code in R"
          ), auto_unbox = TRUE, pretty = TRUE)

        } else {
          # Large: Preview only
          toJSON(list(
            rows_returned = n_rows,
            preview = data_list[1:10],
            preview_note = paste("Showing 10 of", n_rows, "rows"),
            tidycensus_code = tidycensus_code,
            recommendation = "Run the tidycensus code in R for full dataset"
          ), auto_unbox = TRUE, pretty = TRUE)
        }

      }, error = function(e) {
        toJSON(list(
          error = e$message,
          tidycensus_code = tidycensus_code,
          suggestion = "Try running this code directly in R to debug"
        ), auto_unbox = TRUE, pretty = TRUE)
      })
    },
    description = paste(
      "Fetch Census data using tidycensus.",
      "For ACS data: use survey='acs5' (default) or 'acs1'.",
      "For Decennial data: set decennial_sumfile='dhc' (2020), 'pl' (2020), or 'sf1' (2010).",
      "Use code_only=TRUE for large queries or when user wants just the code."
    ),
    arguments = list(
      geography = type_string("Geography level: 'state', 'county', 'place', 'tract', 'block group', 'zcta', etc."),
      variables = type_array(type_string(), "Variable codes, e.g. ['B19013_001'] for median income, ['P1_001N'] for decennial pop"),
      year = type_integer("Data year. For ACS, omit to use tidycensus default (latest available). For decennial, defaults to 2020.", required = FALSE),
      state = type_string("State name, abbreviation, or FIPS code", required = FALSE),
      county = type_string("County name or FIPS code (requires state)", required = FALSE),
      survey = type_string("ACS survey type: 'acs5' (default) or 'acs1'", required = FALSE),
      decennial_sumfile = type_string("For decennial census: 'dhc' (2020), 'pl' (2020), or 'sf1' (2010). If set, uses get_decennial().", required = FALSE),
      code_only = type_boolean("If TRUE, return only code without fetching data", required = FALSE)
    )
  )
}

#' Tool: Resolve FIPS Codes
#' @return An ellmer [tool()] definition.
#' @keywords internal
tool_resolve_fips <- function() {
  tool(
    function(name, geography_type = "state") {

      # State lookup using internal map
      if (geography_type == "state") {
        state_fips <- resolve_state_fips(name)
        if (!is.null(state_fips)) {
          return(toJSON(list(
            found = TRUE,
            name = name,
            fips_code = state_fips,
            geography_type = "state",
            note = "tidycensus accepts state names/abbreviations directly - FIPS not usually needed"
          ), auto_unbox = TRUE, pretty = TRUE))
        }
      }

      # For places and counties, tidycensus handles names directly
      toJSON(list(
        found = FALSE,
        message = paste("For", geography_type, "queries, use the name directly with tidycensus."),
        example = "tidycensus::get_acs(geography = 'place', state = 'MA') returns all places in MA",
        tip = "Filter results by name after fetching, or use geography = 'place' with state parameter"
      ), auto_unbox = TRUE, pretty = TRUE)
    },
    description = paste(
      "Look up FIPS codes. Note: tidycensus accepts state names directly,",
      "so FIPS lookup is mainly useful for verifying state codes."
    ),
    arguments = list(
      name = type_string("Place/state name to look up"),
      geography_type = type_string("Type: 'state', 'county', or 'place' (default: 'state')", required = FALSE)
    )
  )
}

#' Tool: Get Common Variables
#' @return An ellmer [tool()] definition.
#' @keywords internal
tool_get_variables <- function() {
  tool(
    function(category = NULL, dataset = "acs") {
      # ACS variables (for get_acs)
      acs_variables <- list(
        population = list(
          list(code = "B01003_001", name = "Total Population"),
          list(code = "B01002_001", name = "Median Age")
        ),
        income = list(
          list(code = "B19013_001", name = "Median Household Income"),
          list(code = "B19301_001", name = "Per Capita Income"),
          list(code = "B19001_001", name = "Household Income (total for distribution)")
        ),
        housing = list(
          list(code = "B25077_001", name = "Median Home Value"),
          list(code = "B25064_001", name = "Median Gross Rent"),
          list(code = "B25001_001", name = "Total Housing Units")
        ),
        education = list(
          list(code = "B15003_022", name = "Bachelor's Degree"),
          list(code = "B15003_023", name = "Master's Degree"),
          list(code = "B15003_025", name = "Doctorate Degree")
        ),
        race = list(
          list(code = "B02001_001", name = "Total Population (race table)"),
          list(code = "B02001_002", name = "White alone"),
          list(code = "B02001_003", name = "Black or African American alone"),
          list(code = "B03001_003", name = "Hispanic or Latino")
        )
      )

      # Decennial variables (for get_decennial with sumfile="dhc" for 2020)
      decennial_variables <- list(
        population = list(
          list(code = "P1_001N", name = "Total Population (2020 DHC)", sumfile = "dhc"),
          list(code = "P001001", name = "Total Population (2010 SF1)", sumfile = "sf1")
        ),
        housing = list(
          list(code = "H1_001N", name = "Total Housing Units (2020 DHC)", sumfile = "dhc"),
          list(code = "H001001", name = "Total Housing Units (2010 SF1)", sumfile = "sf1")
        ),
        race = list(
          list(code = "P2_001N", name = "Total Population (Hispanic origin, 2020)", sumfile = "dhc"),
          list(code = "P1_003N", name = "White alone (2020 DHC)", sumfile = "dhc"),
          list(code = "P1_004N", name = "Black alone (2020 DHC)", sumfile = "dhc")
        )
      )

      if (dataset == "decennial") {
        if (!is.null(category) && category %in% names(decennial_variables)) {
          toJSON(decennial_variables[[category]], auto_unbox = TRUE, pretty = TRUE)
        } else {
          toJSON(decennial_variables, auto_unbox = TRUE, pretty = TRUE)
        }
      } else {
        if (!is.null(category) && category %in% names(acs_variables)) {
          toJSON(acs_variables[[category]], auto_unbox = TRUE, pretty = TRUE)
        } else {
          toJSON(acs_variables, auto_unbox = TRUE, pretty = TRUE)
        }
      }
    },
    description = paste(
      "Get common Census variable codes.",
      "Use dataset='acs' for ACS surveys (income, housing values, education - most topics).",
      "Use dataset='decennial' for decennial census (population counts, basic demographics)."
    ),
    arguments = list(
      category = type_string("Category: population, income, housing, education, race", required = FALSE),
      dataset = type_string("Dataset type: 'acs' (default) or 'decennial'", required = FALSE)
    )
  )
}

#' Tool: Get Tract Data for a Place
#' @return An ellmer [tool()] definition.
#' @keywords internal
tool_get_place_tracts <- function() {
  tool(
    function(place_name, state, variables, year = NULL, survey = "acs5") {

      # Build tidycensus + tigris code for spatial filtering
      # Build get_acs call - omit year to use tidycensus default when not specified
      acs_args <- sprintf(
        '  geography = "tract",\n  variables = c("%s"),\n  state = "%s",\n  survey = "%s"',
        paste(variables, collapse = '", "'),
        state,
        survey
      )
      if (!is.null(year)) {
        acs_args <- paste0(acs_args, sprintf(',\n  year = %d', year))
      }

      # Build places call - also omit year to use tigris default
      places_args <- sprintf('state = "%s"', state)
      if (!is.null(year)) {
        places_args <- paste0(places_args, sprintf(', year = %d', year))
      }

      tidycensus_code <- sprintf(
        'library(tidycensus)\nlibrary(tigris)\nlibrary(sf)\nlibrary(dplyr)\n\n# Get tract data with geometries\ntracts <- get_acs(\n%s,\n  geometry = TRUE\n)\n\n# Get place boundary and filter to tracts that intersect\nplace <- places(%s) |>\n  filter(grepl("%s", NAME, ignore.case = TRUE))\n\ntracts_in_place <- tracts |>\n  st_filter(place, .predicate = st_intersects)\n\nprint(tracts_in_place)',
        acs_args,
        places_args,
        place_name
      )

      # This tool always returns code because tract-in-place filtering
      # requires spatial operations with tigris and sf packages
      toJSON(list(
        message = paste("To get tract data for", place_name, ", run this code in R."),
        explanation = paste(
          "Tracts don't nest directly under places (cities/towns),",
          "so we need to use spatial filtering with tigris boundaries."
        ),
        tidycensus_code = tidycensus_code,
        packages_needed = c("tidycensus", "tigris", "sf", "dplyr"),
        install_command = 'install.packages(c("tidycensus", "tigris", "sf", "dplyr"))'
      ), auto_unbox = TRUE, pretty = TRUE)
    },
    description = paste(
      "Get Census tract data for a city/town.",
      "Returns tidycensus + tigris code for spatial filtering.",
      "Use this when user asks for tract-level data within a specific place."
    ),
    arguments = list(
      place_name = type_string("City/town name, e.g. 'Boston', 'Framingham'"),
      state = type_string("State name or abbreviation, e.g. 'MA', 'Massachusetts'"),
      variables = type_array(type_string(), "Variable codes, e.g. ['B19013_001'] for median income"),
      year = type_integer("Data year. Omit to use tidycensus default (latest available).", required = FALSE),
      survey = type_string("ACS survey: 'acs5' (default) or 'acs1'", required = FALSE)
    )
  )
}

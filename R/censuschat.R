# =============================================================================
# censuschat: Chat with U.S. Census Data
# =============================================================================
# An R package that lets you query Census Bureau data through natural language
# using ellmer for the LLM interface. No MCP server needed!
#
# Usage:
#   library(censuschat)
#   census_chat()              # Interactive web interface
#   census_chat_console()      # Command-line interface
#   
#   # Or use programmatically:
#   chat <- census_chat_create()
#   chat$chat("What's the population of Boston?")
#
# =============================================================================

#' @import ellmer
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom jsonlite toJSON fromJSON
NULL

# =============================================================================
# Package-level variables
# =============================================================================

# Store the API key
.census_env <- new.env(parent = emptyenv())

#' Set Census API Key
#'
#' Store your Census API key for use in queries. Get a free key at
#' https://api.census.gov/data/key_signup.html
#'
#' @param key Your Census API key
#' @param install If TRUE, saves to .Renviron for future sessions
#' @export
#' @examples
#' census_api_key("your_key_here")
census_api_key <- function(key, install = FALSE) {
  .census_env$api_key <- key
  Sys.setenv(CENSUS_API_KEY = key)
  
 if (install) {
    # Append to .Renviron
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
    
    # Read existing content
    if (file.exists(renviron_path)) {
      lines <- readLines(renviron_path)
      # Remove any existing CENSUS_API_KEY line
      lines <- lines[!grepl("^CENSUS_API_KEY=", lines)]
    } else {
      lines <- character(0)
    }
    
    # Add new key
    lines <- c(lines, paste0("CENSUS_API_KEY=", key))
    writeLines(lines, renviron_path)
    
    message("API key saved to ~/.Renviron. Restart R for it to take effect.")
  }
  
  invisible(key)
}

#' Get Census API Key
#' @keywords internal
get_api_key <- function() {
  key <- .census_env$api_key %||% Sys.getenv("CENSUS_API_KEY")
  if (is.null(key) || key == "") {
    stop(
      "Census API key not found. Set it with:\n",
      "  census_api_key('your_key')\n\n",
      "Get a free key at: https://api.census.gov/data/key_signup.html"
    )
  }
  key
}

# =============================================================================
# Helper Functions (internal)
# =============================================================================

#' Resolve state name/abbreviation to FIPS code
#' @keywords internal
resolve_state_fips <- function(state) {
  state_map <- c(
    "alabama" = "01", "al" = "01", "alaska" = "02", "ak" = "02",
    "arizona" = "04", "az" = "04", "arkansas" = "05", "ar" = "05",
    "california" = "06", "ca" = "06", "colorado" = "08", "co" = "08",
    "connecticut" = "09", "ct" = "09", "delaware" = "10", "de" = "10",
    "florida" = "12", "fl" = "12", "georgia" = "13", "ga" = "13",
    "hawaii" = "15", "hi" = "15", "idaho" = "16", "id" = "16",
    "illinois" = "17", "il" = "17", "indiana" = "18", "in" = "18",
    "iowa" = "19", "ia" = "19", "kansas" = "20", "ks" = "20",
    "kentucky" = "21", "ky" = "21", "louisiana" = "22", "la" = "22",
    "maine" = "23", "me" = "23", "maryland" = "24", "md" = "24",
    "massachusetts" = "25", "ma" = "25", "michigan" = "26", "mi" = "26",
    "minnesota" = "27", "mn" = "27", "mississippi" = "28", "ms" = "28",
    "missouri" = "29", "mo" = "29", "montana" = "30", "mt" = "30",
    "nebraska" = "31", "ne" = "31", "nevada" = "32", "nv" = "32",
    "new hampshire" = "33", "nh" = "33", "new jersey" = "34", "nj" = "34",
    "new mexico" = "35", "nm" = "35", "new york" = "36", "ny" = "36",
    "north carolina" = "37", "nc" = "37", "north dakota" = "38", "nd" = "38",
    "ohio" = "39", "oh" = "39", "oklahoma" = "40", "ok" = "40",
    "oregon" = "41", "or" = "41", "pennsylvania" = "42", "pa" = "42",
    "rhode island" = "44", "ri" = "44", "south carolina" = "45", "sc" = "45",
    "south dakota" = "46", "sd" = "46", "tennessee" = "47", "tn" = "47",
    "texas" = "48", "tx" = "48", "utah" = "49", "ut" = "49",
    "vermont" = "50", "vt" = "50", "virginia" = "51", "va" = "51",
    "washington" = "53", "wa" = "53", "west virginia" = "54", "wv" = "54",
    "wisconsin" = "55", "wi" = "55", "wyoming" = "56", "wy" = "56",
    "district of columbia" = "11", "dc" = "11", "d.c." = "11",
    "puerto rico" = "72", "pr" = "72"
  )
  
  if (grepl("^\\d{2}$", state)) return(state)
  
  state_lower <- tolower(trimws(state))
  if (state_lower %in% names(state_map)) {
    return(state_map[[state_lower]])
  }
  
  NULL
}

# =============================================================================
# Tool Definitions for ellmer
# =============================================================================

#' Create Census data tools for ellmer
#'
#' Returns a list of ellmer tool objects that can be registered with a chat
#'
#' @return List of ellmer tool objects
#' @export
#' @examples
#' library(ellmer)
#' chat <- chat_anthropic()
#' chat$set_tools(census_tools())
#' chat$chat("What's the median income in California?")
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
        'library(tidycensus)\nlibrary(tigris)\nlibrary(sf)\nlibrary(dplyr)\n\n# Get tract data with geometries\ntracts <- get_acs(\n%s,\n  geometry = TRUE\n)\n\n# Get place boundary and filter to tracts that intersect\nplace <- places(%s) %%>%%\n  filter(grepl("%s", NAME, ignore.case = TRUE))\n\ntracts_in_place <- tracts %%>%%\n  st_filter(place, .predicate = st_intersects)\n\nprint(tracts_in_place)',
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

# =============================================================================
# Chat Interface Functions
# =============================================================================

#' Create a Census Chat Object
#'
#' Creates an ellmer chat object pre-configured with Census tools.
#' Use this for programmatic access or to customize the chat.
#'
#' @param provider LLM provider, optionally with model: "provider" or "provider/model".
#'   Defaults to "anthropic/claude-sonnet-4-5-20250929".
#'   Examples: "anthropic/claude-sonnet-4-5-20250929", "openai/gpt-4o", "gemini", "ollama/llama3".
#'   See [ellmer supported providers](https://ellmer.tidyverse.org/) for the full list.
#' @param system_prompt Custom system prompt (optional)
#' @return An ellmer Chat object with Census tools registered
#' @export
#' @examples
#' chat <- census_chat_create()
#' response <- chat$chat("What's the population of Texas?")
#' cat(response)
#'
#' # Use a different provider/model
#' chat <- census_chat_create("openai/gpt-4o")
#' chat <- census_chat_create("ollama/llama3")
census_chat_create <- function(provider = "anthropic/claude-sonnet-4-5-20250929", system_prompt = NULL) {
  # Validate API key exists
  get_api_key()

  # Default system prompt
  if (is.null(system_prompt)) {
    system_prompt <- paste(
      "You are a helpful assistant that answers questions about U.S. Census data.",
      "You have tools that use the tidycensus R package to fetch data.",
      "",
      "CRITICAL - DATA YEARS:",
      "- For ACS data: NEVER hardcode a year. Omit the year parameter to use tidycensus defaults (latest available).",
      "- Only specify a year if the user explicitly requests a specific year.",
      "- When writing R code, do NOT set year variables like 'year_use <- 2022'. Just omit year entirely.",
      "",
      "CHOOSING THE RIGHT DATA SOURCE:",
      "- ACS (American Community Survey): Income, housing values, education, detailed demographics.",
      "  Use survey='acs5' (default, best for small areas) or 'acs1' (more current, 65k+ pop only).",
      "- Decennial Census: Official population counts every 10 years.",
      "  Use decennial_sumfile='dhc' for 2020, 'sf1' for 2010.",
      "",
      "COMMON VARIABLES:",
      "- ACS: B19013_001 (median income), B25077_001 (median home value), B01003_001 (population estimate)",
      "- Decennial 2020: P1_001N (total population), H1_001N (housing units)",
      "- Decennial 2010: P001001 (total population), H001001 (housing units)",
      "- Use the get_variables tool if unsure which variable to use.",
      "",
      "GEOGRAPHY LEVELS: 'state', 'county', 'place' (cities/towns), 'tract', 'block group', 'zcta'",
      "",
      "ZCTA (ZIP CODE) LIMITATIONS:",
      "- ZCTAs don't nest within places/cities - they cross municipal boundaries.",
      "- For 'zip codes in [city]', explain this limitation and offer county-level ZCTA data instead.",
      "- ZCTA geometry files may not be available for recent years. Avoid geometry=TRUE with ZCTA.",
      "",
      "WHEN TO USE code_only=TRUE:",
      "- Query will return many rows (all tracts in a county, all places in a state)",
      "- User explicitly asks for code or wants to do their own analysis",
      "",
      "ALWAYS PROVIDE:",
      "1. The actual data (or preview for large results)",
      "2. The tidycensus R code so users can reproduce the analysis",
      "3. Notes about data source, year, and any limitations",
      "",
      "IMPORTANT: For official population counts (like 'largest cities'), use decennial census, not ACS."
    )
  }

  # Create chat using ellmer's generic chat() function
  chat_obj <- chat(provider, system_prompt = system_prompt)

  # Register Census tools
  chat_obj$set_tools(census_tools())

  chat_obj
}

#' Start Interactive Console Chat
#'
#' Launches an interactive chat session in the R console.
#' Type your questions and press Enter. Type 'quit' to exit.
#'
#' @param provider LLM provider, optionally with model: "provider" or "provider/model".
#'   Defaults to "anthropic/claude-sonnet-4-5-20250929".
#'   Examples: "anthropic/claude-sonnet-4-5-20250929", "openai/gpt-4o", "gemini", "ollama/llama3".
#'   See [ellmer supported providers](https://ellmer.tidyverse.org/) for the full list.
#' @export
#' @examples
#' \dontrun{
#' census_chat_console()
#' census_chat_console("openai/gpt-4o")
#' census_chat_console("ollama/llama3")
#' # > What's the median income in Boston?
#' # > Compare housing costs in MA vs CA
#' # > quit
#' }
census_chat_console <- function(provider = "anthropic/claude-sonnet-4-5-20250929") {
  chat <- census_chat_create(provider = provider)
  
  cat("\n")
  cat("=== Census Data Chat ===\n")
  cat("Ask questions about U.S. Census data.\n")
  cat("Type 'quit' to exit.\n")
  cat("\n")
  
  repeat {
    # Get user input
    question <- readline(prompt = "> ")
    
    # Check for exit
    if (tolower(trimws(question)) %in% c("quit", "exit", "q")) {
      cat("Goodbye!\n")
      break
    }
    
    # Skip empty input
    if (trimws(question) == "") next
    
    # Get response
    tryCatch({
      response <- chat$chat(question)
      cat("\n")
      cat(response)
      cat("\n\n")
    }, error = function(e) {
      cat("Error:", e$message, "\n\n")
    })
  }
}

#' Launch Web Chat Interface
#'
#' Opens an interactive chat interface in your browser.
#' Requires the shinychat package for the best experience.
#'
#' @param provider LLM provider, optionally with model: "provider" or "provider/model".
#'   Defaults to "anthropic/claude-sonnet-4-5-20250929".
#'   Examples: "anthropic/claude-sonnet-4-5-20250929", "openai/gpt-4o", "gemini", "ollama/llama3".
#'   See [ellmer supported providers](https://ellmer.tidyverse.org/) for the full list.
#' @param port Port for the Shiny app (default: random available port)
#' @param launch.browser Open in browser automatically? (default: TRUE)
#' @export
#' @examples
#' \dontrun{
#' census_chat()
#' census_chat("openai/gpt-4o")
#' census_chat("ollama/llama3")
#' }
census_chat <- function(provider = "anthropic/claude-sonnet-4-5-20250929", port = NULL, launch.browser = TRUE) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required. Install with: install.packages('shiny')")
  }
  
  # Check for shinychat or fall back to basic Shiny
  has_shinychat <- requireNamespace("shinychat", quietly = TRUE)
  
  if (has_shinychat) {
    # Use shinychat for nice UI
    chat <- census_chat_create(provider = provider)
    app <- shinychat::chat_app(chat)
    shiny::runApp(app, port = port, launch.browser = launch.browser)
  } else {
    # Fall back to basic Shiny interface
    message("For a nicer UI, install shinychat: pak::pak('posit-dev/shinychat')")
    census_chat_shiny_basic(provider = provider, port = port, launch.browser = launch.browser)
  }
}

#' Basic Shiny Chat Interface
#' @keywords internal
census_chat_shiny_basic <- function(provider = "anthropic/claude-sonnet-4-5-20250929", port = NULL, launch.browser = TRUE) {
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Census Data Chat"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::p("Ask questions about U.S. Census data."),
        shiny::p("Examples:"),
        shiny::tags$ul(
          shiny::tags$li("What's the population of California?"),
          shiny::tags$li("Median income by tract in Framingham, MA"),
          shiny::tags$li("Compare housing costs: MA vs TX")
        ),
        width = 3
      ),
      shiny::mainPanel(
        shiny::div(
          id = "chat-container",
          style = "height: 400px; overflow-y: auto; border: 1px solid #ccc; padding: 10px; margin-bottom: 10px;",
          shiny::uiOutput("chat_history")
        ),
        shiny::textInput("user_input", NULL, placeholder = "Type your question...", width = "100%"),
        shiny::actionButton("send", "Send", class = "btn-primary"),
        width = 9
      )
    )
  )
  
  server <- function(input, output, session) {
    chat <- census_chat_create(provider = provider)
    messages <- shiny::reactiveVal(list())
    
    shiny::observeEvent(input$send, {
      req(input$user_input)
      
      question <- input$user_input
      shiny::updateTextInput(session, "user_input", value = "")
      
      # Add user message
      current <- messages()
      current <- c(current, list(list(role = "user", content = question)))
      messages(current)
      
      # Get response
      tryCatch({
        response <- chat$chat(question)
        current <- messages()
        current <- c(current, list(list(role = "assistant", content = response)))
        messages(current)
      }, error = function(e) {
        current <- messages()
        current <- c(current, list(list(role = "assistant", content = paste("Error:", e$message))))
        messages(current)
      })
    })
    
    output$chat_history <- shiny::renderUI({
      msgs <- messages()
      if (length(msgs) == 0) return(shiny::p("Start chatting!", style = "color: #888;"))
      
      shiny::tagList(lapply(msgs, function(msg) {
        if (msg$role == "user") {
          shiny::div(
            style = "background: #e3f2fd; padding: 10px; margin: 5px 0; border-radius: 5px;",
            shiny::strong("You: "), msg$content
          )
        } else {
          shiny::div(
            style = "background: #f5f5f5; padding: 10px; margin: 5px 0; border-radius: 5px;",
            shiny::strong("Census Assistant: "),
            shiny::pre(style = "white-space: pre-wrap; margin: 5px 0;", msg$content)
          )
        }
      }))
    })
  }
  
  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, port = port, launch.browser = launch.browser)
}

# =============================================================================
# MCP Server
# =============================================================================

#' Start Census Chat as an MCP Server
#'
#' Runs censuschat as a Model Context Protocol (MCP) server for use with
#' Claude Desktop, Claude Code, or other MCP-compatible clients.
#'
#' @details
#' This function is **not meant to be run interactively**. Instead, configure
#' your MCP client to launch it. The server exposes Census data tools that
#' Claude can use to answer questions about U.S. Census data.
#'
#' **Advantage over `census_chat()`**: When using the MCP server with Claude
#' Desktop, the LLM inference uses your Claude subscription (Pro, Max, etc.)
#' rather than requiring a pay-per-use API key.
#'
#' ## Setup for Claude Desktop
#'
#' Add to your `claude_desktop_config.json`:
#'
#' ```json
#' {
#'   "mcpServers": {
#'     "censuschat": {
#'       "command": "Rscript",
#'       "args": ["-e", "censuschat::mcp_serve()"]
#'     }
#'   }
#' }
#' ```
#'
#' On macOS, this file is at:
#' `~/Library/Application Support/Claude/claude_desktop_config.json`
#'
#' On Windows, this file is at:
#' `%APPDATA%\Claude\claude_desktop_config.json`
#'
#' ## Setup for Claude Code
#'
#' Run in your terminal:
#'
#' ```bash
#' claude mcp add censuschat -- Rscript -e "censuschat::mcp_serve()"
#' ```
#'
#' ## Environment Variables
#'
#' Make sure `CENSUS_API_KEY` is set in your environment (e.g., in `.Renviron`).
#' Get a free key at <https://api.census.gov/data/key_signup.html>.
#'
#' @return This function does not return; it blocks while serving requests.
#' @export
#' @seealso [census_chat()] for interactive use with pay-per-use API,
#'   [census_tools()] for the underlying tool definitions
mcp_serve <- function() {
  if (!requireNamespace("mcptools", quietly = TRUE)) {
    stop(
      "Package 'mcptools' is required for MCP server functionality.\n",
      "Install with: pak::pak('posit-dev/mcptools')"
    )
  }

  mcptools::mcp_server(tools = census_tools())
}

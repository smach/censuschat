#' Set Census API Key
#'
#' Store your Census API key for use in queries. Get a free key at
#' https://api.census.gov/data/key_signup.html
#'
#' @param key Your Census API key
#' @param install If TRUE, saves to .Renviron for future sessions
#' @return The key, invisibly.
#' @export
#' @examples
#' \dontrun{
#' census_api_key("your_key_here")
#' }
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
#' @return The API key as a string. Throws an error if no key is found.
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

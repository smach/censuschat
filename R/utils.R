#' Resolve state name/abbreviation to FIPS code
#' @param state A state name, abbreviation, or two-digit FIPS code.
#' @return A two-character FIPS code string, or NULL if not found.
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

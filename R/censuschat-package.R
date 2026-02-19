#' @keywords internal
"_PACKAGE"

#' @import ellmer
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom rlang `%||%`
NULL

# Package-level environment for storing the Census API key
.census_env <- new.env(parent = emptyenv())

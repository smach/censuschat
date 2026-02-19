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

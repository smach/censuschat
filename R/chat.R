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
#' \dontrun{
#' chat <- census_chat_create()
#' response <- chat$chat("What's the population of Texas?")
#' cat(response)
#'
#' # Use a different provider/model
#' chat <- census_chat_create("openai/gpt-4o")
#' chat <- census_chat_create("ollama/llama3")
#' }
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
#' @return This function is called for its side effects (interactive chat
#'   session) and does not return a value.
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
#' @return This function is called for its side effects (launches a Shiny app)
#'   and does not return a value.
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
#' @return This function is called for its side effects (launches a Shiny app)
#'   and does not return a value.
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
      shiny::req(input$user_input)

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

#' Create a censuschat solver for vitals evaluation
#'
#' Creates a solver function compatible with vitals that processes Census
#' questions through censuschat's LLM interface with tools.
#'
#' @param provider LLM provider string. If NULL, uses package default.
#' @return A solver function for use with [vitals::Task].
#'
#' @examples
#' \dontrun{
#' library(vitals)
#'
#' task <- Task$new(
#'   dataset = tibble::tibble(
#'     input = "What is the population of Texas?",
#'     target = "Should use population variable for Texas"
#'   ),
#'   solver = census_solver(),
#'   scorer = model_graded_qa()
#' )
#' task$eval()
#' }
#'
#' @export
census_solver <- function(provider = NULL) {
  function(input, ...) {
    chat <- if (is.null(provider)) census_chat_create() else census_chat_create(provider)
    res <- ellmer::parallel_chat(chat, as.list(input), ...)

    list(
      result = vapply(res, function(x) x$last_turn()@text, character(1)),
      solver_chat = res
    )
  }
}


#' Run censuschat evaluation
#'
#' Evaluates censuschat accuracy using vitals framework.
#'
#' @param dataset Path to CSV or tibble with `input` and `target` columns.
#'   If NULL, uses built-in evaluation dataset.
#' @param provider LLM provider to evaluate. If NULL, uses package default.
#' @param epochs Number of times to repeat each sample.
#' @param view Open vitals viewer after evaluation?
#' @param log_dir Directory to save evaluation logs. Defaults to "censuschat_eval_logs"
#'   in current working directory (created if needed). Set to NULL to use vitals default.
#' @return The vitals Task object (invisibly).
#'
#' @examples
#' \dontrun{
#' task <- census_eval()
#' task$get_samples()
#' task$metrics
#' }
#'
#' @export
census_eval <- function(dataset = NULL, provider = NULL, epochs = 1,
                        view = interactive(), log_dir = "censuschat_eval_logs") {
  if (!requireNamespace("vitals", quietly = TRUE)) {
    stop("Package 'vitals' required. Install with: pak::pak('tidyverse/vitals')")
  }

  # Set up log directory so results are never lost

  if (!is.null(log_dir)) {
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    vitals::vitals_log_dir_set(log_dir)
    message("Logs will be saved to: ", normalizePath(log_dir, mustWork = FALSE))
  }

  # Load dataset
  if (is.null(dataset)) {
    path <- system.file("eval/census_questions.csv", package = "censuschat")
    if (path == "") stop("Built-in dataset not found. Provide a custom dataset.")
    dataset <- utils::read.csv(path, stringsAsFactors = FALSE)
  } else if (is.character(dataset)) {
    dataset <- utils::read.csv(dataset, stringsAsFactors = FALSE)
  }
  dataset <- tibble::as_tibble(dataset)

  # Create scorer with Census-specific grading criteria
  # Explicitly pass scorer_chat to avoid auto-detection issues
  scorer <- vitals::model_graded_qa(
    instructions = paste(
      "Grade whether this Census data question was answered correctly.",
      "Check: (1) correct data source (ACS vs Decennial),",
      "(2) appropriate variable codes,",
      "(3) correct geography level.",
      "GRADE: C if correct, I if incorrect.",
      "Do not use any markdown formatting in your response."
    ),
    partial_credit = FALSE,
    scorer_chat = ellmer::chat_anthropic(model = "claude-sonnet-4-5")
  )

  # Run evaluation

  # Create solver chat - use vitals' built-in generate() with our chat
  solver_chat <- if (is.null(provider)) census_chat_create() else census_chat_create(provider)

  task <- vitals::Task$new(
    dataset = dataset,
    solver = vitals::generate(solver_chat),
    scorer = scorer,
    name = "censuschat_accuracy"
  )
  task$eval(epochs = epochs, view = view)

  invisible(task)
}

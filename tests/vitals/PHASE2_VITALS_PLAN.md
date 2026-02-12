# Phase 2: vitals LLM Evaluation Plan for censuschat

## Overview

This document describes how to use the [vitals](https://vitals.tidyverse.org/) package to evaluate the quality of LLM responses when using censuschat tools. Unlike Phase 1 (testthat unit tests), these evaluations test the actual LLM behavior with real API calls.

## What vitals Evaluates

1. **Tool Selection**: Does the LLM choose the correct tool for a given question?
2. **Parameter Accuracy**: Does it pass correct parameters (geography, variables, state)?
3. **Response Quality**: Does it interpret and present Census data accurately?
4. **Code Generation**: Is the generated tidycensus code correct?

## Prerequisites

```r
# Install vitals
install.packages("vitals")

# Or development version
pak::pak("tidyverse/vitals")

# Required environment variables
# ANTHROPIC_API_KEY or OPENAI_API_KEY (for LLM)
# CENSUS_API_KEY (for Census data)
```

## Directory Structure

```
tests/
  vitals/
    PHASE2_VITALS_PLAN.md      # This file
    eval-tool-selection.R       # Tool selection evaluation
    eval-data-accuracy.R        # Data accuracy evaluation
    eval-code-generation.R      # Code generation evaluation
    datasets/
      tool_selection_questions.csv
      data_accuracy_questions.csv
```

---

## Evaluation 1: Tool Selection

Tests whether the LLM selects the correct tool for different types of Census questions.

### Dataset: `datasets/tool_selection_questions.csv`

```csv
input,target,expected_tool
"What is the population of California?","Use tool_fetch_data with decennial census","tool_fetch_data"
"What's the median household income in Boston?","Use tool_fetch_data with ACS data","tool_fetch_data"
"Show me housing data by tract in Framingham, MA","Use tool_get_place_tracts for tract-in-place queries","tool_get_place_tracts"
"What Census datasets are available about housing?","Use tool_list_datasets to search datasets","tool_list_datasets"
"What variable code should I use for median home value?","Use tool_get_variables to look up codes","tool_get_variables"
"What is the FIPS code for Texas?","Use tool_resolve_fips for FIPS lookups","tool_resolve_fips"
"Compare median income in MA vs CA","Use tool_fetch_data for state-level comparison","tool_fetch_data"
"How many housing units are in Cook County, Illinois?","Use tool_fetch_data with decennial or ACS","tool_fetch_data"
"Get education data for all tracts in Seattle","Use tool_get_place_tracts for tract-in-place","tool_get_place_tracts"
"What ACS variables measure poverty?","Use tool_get_variables to find poverty variables","tool_get_variables"
```

### Evaluation Script: `eval-tool-selection.R`

```r
library(vitals)
library(ellmer)
library(tibble)
library(censuschat)

# Set log directory
Sys.setenv(VITALS_LOG_DIR = "tests/vitals/logs")

# Load dataset
tool_selection_data <- tibble(
input = c(
    "What is the population of California?",
    "What's the median household income in Boston?",
    "Show me housing data by tract in Framingham, MA",
    "What Census datasets are available about housing?",
    "What variable code should I use for median home value?",
    "What is the FIPS code for Texas?",
    "Compare median income in MA vs CA",
    "How many housing units are in Cook County, Illinois?",
    "Get education data for all tracts in Seattle",
    "What ACS variables measure poverty?"
  ),
  target = c(
    "Should call tool_fetch_data with decennial census data",
    "Should call tool_fetch_data with ACS data for places",
    "Should call tool_get_place_tracts for tract data within a city",
    "Should call tool_list_datasets to search available datasets",
    "Should call tool_get_variables to look up variable codes",
    "Should call tool_resolve_fips to get FIPS code",
    "Should call tool_fetch_data for state-level comparison",
    "Should call tool_fetch_data with county geography",
    "Should call tool_get_place_tracts for tract-in-city queries",
    "Should call tool_get_variables to find poverty-related variables"
  )
)

# Custom solver using census_chat_create
census_solver <- function(inputs, solver_chat = NULL) {
  results <- character(length(inputs))
  chats <- vector("list", length(inputs))

  for (i in seq_along(inputs)) {
    # Create fresh chat for each question
    chat <- census_chat_create()

    # Get response
    response <- tryCatch(
      chat$chat(inputs[i]),
      error = function(e) paste("Error:", e$message)
    )

    results[i] <- response
    chats[[i]] <- chat
  }

  list(result = results, solver_chat = chats)
}

# Create and run evaluation
task <- Task$new(
  dataset = tool_selection_data,
  solver = census_solver,
  scorer = model_graded_qa(),
  name = "census_tool_selection"
)

# Run evaluation (this makes real API calls!)
task$eval()

# View results
task$get_samples()

# Check metrics
task$metrics
```

---

## Evaluation 2: Data Accuracy

Tests whether the LLM returns accurate Census data and interprets it correctly.

### Dataset Structure

```r
data_accuracy_data <- tibble(
  input = c(
    "What is the median household income in California according to the latest ACS?",
    "What was the total population of Texas in the 2020 Census?",
    "What is the median home value in Massachusetts?",
    "How many housing units are in Florida according to the 2020 Census?"
  ),
  target = c(
    "Should return approximately $85,000-$95,000 (ACS 2022 estimate)",
    "Should return approximately 29,145,505 (2020 Census P1_001N)",
    "Should return approximately $550,000-$600,000 (ACS estimate)",
    "Should return approximately 9,865,350 (2020 Census H1_001N)"
  )
)
```

### Evaluation Script: `eval-data-accuracy.R`

```r
library(vitals)
library(ellmer)
library(tibble)
library(censuschat)

Sys.setenv(VITALS_LOG_DIR = "tests/vitals/logs")

# Questions with verifiable answers
data_accuracy_data <- tibble(
  input = c(
    "What is the median household income in California?",
    "What was the total population of Texas in the 2020 decennial census?",
    "What is the median home value in Massachusetts according to ACS?",
    "What is the population of Wyoming from the 2020 census?"
  ),
  target = c(
    "Median household income in California is approximately $85,000-$95,000 based on recent ACS data. The response should use B19013_001 variable.",
    "The 2020 Census population of Texas was 29,145,505. Should use decennial census with P1_001N.",
    "Massachusetts median home value is approximately $550,000-$600,000 from ACS. Should use B25077_001.",
    "Wyoming 2020 Census population was 576,851. Should use decennial with P1_001N."
  )
)

# Custom scorer that checks for reasonable values
census_accuracy_scorer <- function(samples) {
  scores <- character(nrow(samples))

  for (i in seq_len(nrow(samples))) {
    result <- samples$result[i]
    target <- samples$target[i]

    # Use model grading with specific criteria
    # This is a simplified version - you might want model_graded_qa()
    scores[i] <- "P"  # Placeholder - use model_graded_qa() in practice
  }

  list(score = factor(scores, levels = c("I", "P", "C"), ordered = TRUE))
}

# Use model-graded QA for nuanced evaluation
task <- Task$new(
  dataset = data_accuracy_data,
  solver = census_solver,  # Same solver as above
  scorer = model_graded_qa(),
  name = "census_data_accuracy"
)

task$eval()
```

---

## Evaluation 3: Code Generation Quality

Tests whether the generated tidycensus code is correct and runnable.

### Evaluation Script: `eval-code-generation.R`

```r
library(vitals)
library(ellmer)
library(tibble)
library(censuschat)

Sys.setenv(VITALS_LOG_DIR = "tests/vitals/logs")

code_generation_data <- tibble(
  input = c(
    "Give me the R code to get median income by county in California",
    "Write tidycensus code to get 2020 population by state",
    "Show me how to get housing values for tracts in Boston"
  ),
  target = c(
    "Code should use get_acs() with geography='county', state='CA', variables including B19013_001",
    "Code should use get_decennial() with geography='state', year=2020, sumfile='dhc', variables including P1_001N",
    "Code should use get_acs() with geometry=TRUE and tigris for spatial filtering, or tool_get_place_tracts approach"
  )
)

# Solver that requests code_only mode
code_solver <- function(inputs, solver_chat = NULL) {
  results <- character(length(inputs))
  chats <- vector("list", length(inputs))

  for (i in seq_along(inputs)) {
    chat <- census_chat_create(
      system_prompt = paste(
        "You are a Census data expert. When asked for code,",
        "always use code_only=TRUE in tool calls to return",
        "just the tidycensus R code without executing it."
      )
    )

    response <- tryCatch(
      chat$chat(inputs[i]),
      error = function(e) paste("Error:", e$message)
    )

    results[i] <- response
    chats[[i]] <- chat
  }

  list(result = results, solver_chat = chats)
}

task <- Task$new(
  dataset = code_generation_data,
  solver = code_solver,
  scorer = model_graded_qa(),
  name = "census_code_generation"
)

task$eval()
```

---

## Running Evaluations

### Quick Test (Single Evaluation)

```r
library(vitals)
library(censuschat)

# Set up
Sys.setenv(VITALS_LOG_DIR = tempdir())

# Minimal test
test_data <- tibble::tibble(
  input = "What is the population of California?",
  target = "Should use decennial census data with P1_001N variable"
)

task <- Task$new(
  dataset = test_data,
  solver = function(inputs, ...) {
    chat <- census_chat_create()
    list(
      result = chat$chat(inputs[1]),
      solver_chat = list(chat)
    )
  },
  scorer = model_graded_qa()
)

task$eval()
```

### Full Evaluation Suite

```r
# Run all evaluations
source("tests/vitals/eval-tool-selection.R")
source("tests/vitals/eval-data-accuracy.R")
source("tests/vitals/eval-code-generation.R")

# View all logs
vitals_view()
```

### Comparing Providers

```r
# Test with different LLM providers
providers <- c(
  "anthropic/claude-sonnet-4-5-20250929",
  "openai/gpt-4o",
  "anthropic/claude-haiku"
)

results <- list()

for (provider in providers) {
  solver <- function(inputs, ...) {
    chat <- census_chat_create(provider = provider)
    list(
      result = chat$chat(inputs[1]),
      solver_chat = list(chat)
    )
  }

  task <- Task$new(
    dataset = tool_selection_data,
    solver = solver,
    scorer = model_graded_qa(),
    name = paste0("census_", gsub("/", "_", provider))
  )

  task$eval(view = FALSE)
  results[[provider]] <- task$metrics
}

# Compare results
do.call(rbind, lapply(names(results), function(p) {
  data.frame(provider = p, accuracy = results[[p]]["accuracy"])
}))
```

---

## Interpreting Results

### Metrics

vitals returns scores as ordered factors: `I < P < C`
- **I** (Incorrect): Response is wrong or uses wrong tool
- **P** (Partially Correct): Right approach but some issues
- **C** (Correct): Fully correct response

### Viewing Logs

```r
# Open the Inspect log viewer
vitals_view()

# Or view specific log
vitals_view("tests/vitals/logs/census_tool_selection_*.json")
```

### Cost Tracking

```r
# After running evaluation
task$get_cost()
```

---

## Troubleshooting

### Common Issues

1. **Missing API keys**: Ensure `ANTHROPIC_API_KEY` and `CENSUS_API_KEY` are set
2. **Rate limiting**: Add delays between evaluations if hitting API limits
3. **Timeout errors**: Census API can be slow; increase timeout in httr2 calls

### Debugging

```r
# Test single question manually
chat <- census_chat_create()
response <- chat$chat("What is the population of California?")
cat(response)

# Check what tools were called
chat$get_turns()
```

---

## Cost Estimates

| Evaluation | Questions | Est. Cost (Claude Sonnet) |
|------------|-----------|---------------------------|
| Tool Selection | 10 | ~$0.10-0.20 |
| Data Accuracy | 4 | ~$0.05-0.10 |
| Code Generation | 3 | ~$0.03-0.06 |
| **Total** | 17 | ~$0.20-0.40 |

Costs vary based on response length and tool calls.

---

## Next Steps

1. Run Phase 1 testthat tests first to ensure basic functionality works
2. Start with the tool selection evaluation (most important)
3. Iterate on system prompts based on evaluation results
4. Add more test cases as edge cases are discovered
5. Consider running evaluations in CI with a budget cap

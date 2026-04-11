# ==============================================================================
# 00_report_utils.R — Shared report-writing utilities
# Sourced by every pipeline script to write a markdown report to output/reports/
# ==============================================================================

#' Write a markdown report for a pipeline script
#'
#' @param script_name   filename of the calling script (e.g. "03.1_pooled_data.R")
#' @param description   one-line description of what the script does
#' @param inputs        named list: input file paths (checked for existence)
#' @param outputs       named list: output file paths (checked for existence after run)
#' @param sections      named list of character vectors — each is a report section
#' @param elapsed_sec   numeric: seconds taken
#' @param warnings_vec  character vector of warnings to include
write_report <- function(script_name,
                         description   = "",
                         inputs        = list(),
                         outputs       = list(),
                         sections      = list(),
                         elapsed_sec   = NULL,
                         warnings_vec  = character(0)) {

  reports_dir <- "../output/reports"
  dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

  slug    <- sub("\\.R$|\\.py$", "", script_name)
  outfile <- file.path(reports_dir, paste0(slug, "_report.md"))
  ts      <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")

  lines <- c(
    paste0("# Report: ", script_name),
    "",
    paste0("**Generated:** ", ts),
    if (!is.null(elapsed_sec)) paste0("**Elapsed:** ", round(elapsed_sec, 1), "s"),
    if (nchar(description) > 0) paste0("**Purpose:** ", description),
    "",
    "## Inputs",
    ""
  )

  if (length(inputs) > 0) {
    for (nm in names(inputs)) {
      path   <- inputs[[nm]]
      exists <- if (is.character(path) && length(path) == 1) file.exists(path) else NA
      status <- if (isTRUE(exists)) "✅" else if (isFALSE(exists)) "⚠️ missing" else "—"
      size   <- if (isTRUE(exists)) {
        sz <- file.info(path)$size
        if (!is.na(sz)) paste0(" (", format(sz, big.mark = ","), " B)") else ""
      } else ""
      lines <- c(lines, paste0("- **", nm, "**: `", path, "` ", status, size))
    }
  } else {
    lines <- c(lines, "_No file inputs._")
  }

  lines <- c(lines, "", "## Outputs", "")
  if (length(outputs) > 0) {
    for (nm in names(outputs)) {
      path   <- outputs[[nm]]
      exists <- if (is.character(path) && length(path) == 1) file.exists(path) else NA
      status <- if (isTRUE(exists)) "✅ written" else if (isFALSE(exists)) "❌ NOT written" else "—"
      size   <- if (isTRUE(exists)) {
        sz <- file.info(path)$size
        if (!is.na(sz)) paste0(" (", format(sz, big.mark = ","), " B)") else ""
      } else ""
      lines <- c(lines, paste0("- **", nm, "**: `", path, "` ", status, size))
    }
  } else {
    lines <- c(lines, "_No file outputs._")
  }

  # Custom sections
  for (sec_name in names(sections)) {
    lines <- c(lines, "", paste0("## ", sec_name), "")
    content <- sections[[sec_name]]
    if (length(content) > 0) {
      lines <- c(lines, paste0("```"), content, "```")
    }
  }

  # Warnings
  if (length(warnings_vec) > 0) {
    lines <- c(lines, "", "## Warnings", "")
    for (w in warnings_vec) lines <- c(lines, paste0("- ⚠️ ", w))
  }

  writeLines(lines, outfile)
  invisible(outfile)
}

#' Convenience: capture printed output of an expression as a character vector
capture_output <- function(expr) {
  tryCatch(
    capture.output(expr),
    error = function(e) paste("Error:", conditionMessage(e))
  )
}

#' CI-aware tree count: fewer trees on small synthetic data, more on real data
ci_trees <- function(data, full = 500L, ci = 50L) {
  if (nrow(data) < 2000L) ci else full
}

#' CI-aware CV folds
ci_folds <- function(data, full = 10L, ci = 3L) {
  if (nrow(data) < 2000L) ci else full
}

library(dplyr)
library(stringr)
library(tibble)
# testthat must be loaded so that as.data.frame() dispatches the
# testthat_results S3 method when reading the saved .rds sidecars.
suppressMessages(library(testthat))

# ---------------------------------------------------------------------------
# Compare two loadflex test runs and print high-level, copiable summaries of
# how warnings, failures, and errors differ between them.
#
# Usage: edit OLD_FILE and NEW_FILE below, then source() or Rscript the file.
#
# Two comparison modes, chosen automatically:
#
#   * STRUCTURED (preferred): if an ".rds" sidecar exists next to BOTH .txt
#     files, we read the saved testthat results object. It carries full,
#     untruncated test names and exact per-test counts, so individual tests are
#     matched on a stable (file, test) key - robust against line-number drift
#     and terminal truncation. The harness (temp/test.txt) writes this sidecar
#     automatically for every new run.
#
#   * TEXT (fallback): if either sidecar is missing (e.g. an older run), we
#     scrape the printed summary. Warnings can still be matched by name+message,
#     but failures/errors can only be compared as per-file counts, because their
#     printed names are truncated and their locations drift with edits.
# ---------------------------------------------------------------------------

OLD_FILE <- "temp/test-output-261005-1528.rds"
NEW_FILE <- "temp/test-output-261005-1736.rds"

rds_path <- function(txt) sub("[.]txt$", ".rds", txt)
txt_path <- function(path) sub("[.]rds$", ".txt", path)

# Read the "Status:" note from a run's .txt summary, for labelling OLD/NEW.
# Accepts either the .txt or .rds path (always reads the .txt sidecar).
# Returns a trimmed string, or NA if the file or the Status line is absent.
status_note <- function(path) {
  path <- txt_path(path)
  if (!file.exists(path)) {
    return(NA_character_)
  }
  status_line <- readLines(path) |>
    str_subset("^Status:") |>
    head(1)
  if (length(status_line) == 0) {
    return(NA_character_)
  }
  str_trim(str_remove(status_line, "^Status:\\s*"))
}

# Format "<file> (<status>)" when a status note exists, else just "<file>".
label_with_status <- function(path) {
  note <- status_note(path)
  if (is.na(note)) path else sprintf("%s (%s)", path, note)
}

# Build a one-line run summary from a run's .rds sidecar, e.g.
# "Tests: 89 | Passed: 34 | Failed: 56 | Errors: 25 | Warnings: 17 | Skipped: 24".
# "Passed" counts tests with no failure, error, or skip. Returns NA if the
# sidecar is absent (so callers can omit the line).
totals_line <- function(path) {
  path <- rds_path(path)
  if (!file.exists(path)) {
    return(NA_character_)
  }
  df <- as.data.frame(readRDS(path))
  skipped <- as.logical(df$skipped)
  errored <- as.logical(df$error)
  passed <- df$failed == 0 & !errored & !skipped

  sprintf(
    "Tests: %d | Passed: %d | Failed: %d | Errors: %d | Warnings: %d | Skipped: %d",
    nrow(df), sum(passed), sum(df$failed), sum(errored), sum(df$warning), sum(skipped)
  )
}

# Print "<label>: <file> (<status>)" and, when available, an indented totals line.
print_run_header <- function(label, path) {
  cat(sprintf("%s: %s\n", label, label_with_status(path)))
  totals <- totals_line(path)
  if (!is.na(totals)) {
    cat("    ", totals, "\n", sep = "")
  }
}

# ===========================================================================
# Shared helpers
# ===========================================================================

# Print a labelled, copiable block of wrapped lines (or a "none" note).
print_block <- function(title, items) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  if (length(items) == 0) {
    cat("(none)\n")
  } else {
    items |>
      str_wrap(width = 180, exdent = 2) |>
      cat(sep = "\n\n")
    cat("\n")
  }
}

# ===========================================================================
# STRUCTURED mode: compare two saved testthat results objects
# ===========================================================================

# Read a saved results object into one row per (file, test) with counts for
# each kind. Note two different sources, because testthat represents them
# differently:
#   * failures and warnings are per-expectation outcomes, counted from the
#     `result` list-column (expectation_failure / expectation_warning).
#   * errors are per-test: an error aborts the test before any expectation
#     object is produced, so there is no expectation_error to count. We read the
#     test-level `error` column instead. (A single test can both have completed
#     expectation failures AND then error out, so these are independent.)
per_test_counts <- function(path) {
  res <- readRDS(path)
  df <- as.data.frame(res)

  tibble(
    file = df$file,
    test = df$test,
    failed = vapply(df$result, function(exps) {
      sum(vapply(exps, function(e) inherits(e, "expectation_failure"), logical(1)))
    }, integer(1)),
    warning = vapply(df$result, function(exps) {
      sum(vapply(exps, function(e) inherits(e, "expectation_warning"), logical(1)))
    }, integer(1)),
    error = as.integer(as.logical(df$error))
  )
}

# Compare one outcome kind ("failed" / "error" / "warning") at the (file, test)
# level: which tests gained/lost that outcome, and net count shifts.
compare_kind_structured <- function(old_counts, new_counts, kind, label) {
  cat("\n========================================================\n")
  cat("  ", label, " (structured, matched by file + test)\n", sep = "")
  cat("========================================================\n")

  col <- sym(kind)
  old_k <- old_counts |>
    transmute(file, test, old = !!col) |>
    filter(old > 0)
  new_k <- new_counts |>
    transmute(file, test, new = !!col) |>
    filter(new > 0)

  cat(sprintf(
    "Totals: OLD = %d across %d tests | NEW = %d across %d tests\n",
    sum(old_k$old), nrow(old_k), sum(new_k$new), nrow(new_k)
  ))

  joined <- full_join(old_k, new_k, by = c("file", "test")) |>
    mutate(across(c(old, new), ~ coalesce(.x, 0L)), delta = new - old) |>
    arrange(file, test)

  gone <- joined |>
    filter(new == 0, old > 0) |>
    mutate(line = sprintf("%s - %s (had %d)", file, test, old)) |>
    pull(line)
  print_block(sprintf("IMPROVED - %s fully gone from these tests", label), gone)

  added <- joined |>
    filter(old == 0, new > 0) |>
    mutate(line = sprintf("%s - %s (now %d)", file, test, new)) |>
    pull(line)
  print_block(sprintf("DEGRADED - %s newly present in these tests", label), added)

  rarer <- joined |>
    filter(old > 0, new > 0, delta < 0) |>
    mutate(line = sprintf("%s - %s (%d -> %d)", file, test, old, new)) |>
    pull(line)
  print_block(sprintf("IMPROVED - fewer %s in these tests", label), rarer)

  more <- joined |>
    filter(old > 0, new > 0, delta > 0) |>
    mutate(line = sprintf("%s - %s (%d -> %d)", file, test, old, new)) |>
    pull(line)
  print_block(sprintf("DEGRADED - more %s in these tests", label), more)
}

run_structured <- function(old_txt, new_txt) {
  cat("MODE: structured (.rds sidecars found)\n")
  old_counts <- per_test_counts(rds_path(old_txt))
  new_counts <- per_test_counts(rds_path(new_txt))

  compare_kind_structured(old_counts, new_counts, "failed", "FAILURES")
  compare_kind_structured(old_counts, new_counts, "error", "ERRORS")
  compare_kind_structured(old_counts, new_counts, "warning", "WARNINGS")
}

# ===========================================================================
# TEXT mode: scrape the printed summary (fallback for runs without sidecars)
# ===========================================================================

section_lines <- function(lines, header_regex) {
  start <- which(str_detect(lines, header_regex))
  if (length(start) == 0) {
    return(character(0))
  }
  banners <- which(str_detect(lines, "^══ "))
  end <- banners[banners > start[1]][1]
  if (is.na(end)) end <- length(lines) + 1
  lines[(start[1] + 1):(end - 1)]
}

parse_warnings_txt <- function(path) {
  lines <- readLines(path)
  section <- section_lines(lines, "^══ Warnings")
  numbered <- section[str_detect(section, "^\\d+\\. ")]
  bodies <- str_remove(numbered, "^\\d+\\.\\s*")

  tibble(
    test = str_trim(str_match(bodies, "^(.*?)\\s*\\('")[, 2]),
    file = str_match(bodies, "\\('([^:]*?\\.R)")[, 2],
    message = str_trim(str_match(bodies, "'\\)\\s*-\\s*(.*)$")[, 2])
  ) |>
    mutate(description = sprintf("%s (%s) - %s", test, file, message)) |>
    group_by_all() |>
    summarize(replicates = n(), .groups = "drop")
}

parse_failures_txt <- function(path) {
  lines <- readLines(path)
  section <- section_lines(lines, "^══ Failed")
  headers <- section[str_detect(section, "^── \\d+\\. (Failure|Error)")]
  tibble(
    type = str_match(headers, "^── \\d+\\. (Failure|Error)")[, 2],
    file = str_match(headers, "\\('([^:]*?\\.R):")[, 2]
  )
}

compare_warnings_txt <- function(old, new) {
  cat("\n========================================================\n")
  cat("  WARNINGS (text, matched by name + message)\n")
  cat("========================================================\n")
  cat(sprintf(
    "Totals: OLD = %d distinct (%d incl. replicates) | NEW = %d distinct (%d incl. replicates)\n",
    nrow(old), sum(old$replicates), nrow(new), sum(new$replicates)
  ))

  gone <- anti_join(old, new, by = "description") |> pull(description)
  print_block("IMPROVED - warnings gone in NEW", gone)
  added <- anti_join(new, old, by = "description") |> pull(description)
  print_block("DEGRADED - warnings new in NEW", added)

  shared <- inner_join(old, new, by = c("test", "file", "message"),
    suffix = c("_old", "_new"))
  rarer <- shared |>
    filter(replicates_old > replicates_new) |>
    mutate(line = sprintf("%s  (%d -> %d)", description_new, replicates_old, replicates_new)) |>
    pull(line)
  print_block("IMPROVED - warnings less frequent in NEW", rarer)
  more <- shared |>
    filter(replicates_old < replicates_new) |>
    mutate(line = sprintf("%s  (%d -> %d)", description_new, replicates_old, replicates_new)) |>
    pull(line)
  print_block("DEGRADED - warnings more frequent in NEW", more)
}

compare_type_txt <- function(old, new, which_type) {
  old <- filter(old, type == which_type)
  new <- filter(new, type == which_type)
  label <- paste0(toupper(which_type), "S")

  cat("\n========================================================\n")
  cat("  ", label, " (text, per-file counts only)\n", sep = "")
  cat("========================================================\n")
  cat(sprintf("Totals: OLD = %d | NEW = %d\n", nrow(old), nrow(new)))

  counts <- full_join(
    count(old, file, name = "old"),
    count(new, file, name = "new"),
    by = "file"
  ) |>
    mutate(across(c(old, new), ~ coalesce(.x, 0L)), delta = new - old) |>
    arrange(file)

  cat("\nPer-file counts (file | old -> new | delta)\n")
  cat(strrep("-", 50), "\n", sep = "")
  counts |>
    mutate(line = sprintf("%-32s %3d -> %-3d  %+d", file, old, new, delta)) |>
    pull(line) |>
    cat(sep = "\n")
  cat("\n")

  improved <- counts |>
    filter(delta < 0) |>
    mutate(line = sprintf("%s: %d -> %d (%+d)", file, old, new, delta)) |>
    pull(line)
  print_block(sprintf("IMPROVED - fewer %s in NEW", tolower(label)), improved)
  degraded <- counts |>
    filter(delta > 0) |>
    mutate(line = sprintf("%s: %d -> %d (%+d)", file, old, new, delta)) |>
    pull(line)
  print_block(sprintf("DEGRADED - more %s in NEW", tolower(label)), degraded)
}

run_text <- function(old_txt, new_txt) {
  cat("MODE: text (one or both .rds sidecars missing - run the harness to\n")
  cat("      generate sidecars for precise, name-matched comparison)\n")
  compare_warnings_txt(parse_warnings_txt(old_txt), parse_warnings_txt(new_txt))
  old_f <- parse_failures_txt(old_txt)
  new_f <- parse_failures_txt(new_txt)
  compare_type_txt(old_f, new_f, "Failure")
  compare_type_txt(old_f, new_f, "Error")
}

# ===========================================================================
# Dispatch
# ===========================================================================

# Compare two test runs, choosing structured or text mode automatically.
# old_file / new_file may be either ".txt" or ".rds" paths.
compare_tests <- function(old_file = OLD_FILE, new_file = NEW_FILE) {
  print_run_header("OLD", old_file)
  print_run_header("NEW", new_file)

  if (file.exists(rds_path(old_file)) && file.exists(rds_path(new_file))) {
    run_structured(old_file, new_file)
  } else {
    run_text(old_file, new_file)
  }
}

message(sprintf("compare_tests() is defined. Run with:\ncompare_tests(old_file='%s', new_file='%s')", OLD_FILE, NEW_FILE))

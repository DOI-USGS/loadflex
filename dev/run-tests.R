# Human-readable marker for which remediation status this run reflects.
# Update by hand when a status or tasks are completed, before the verifying harness run.
STATUS_NOTES <- "Edits for PR #264, \"Address issues 259 260 261\""

# Run the full test harness and write a timestamped .txt summary plus a
# machine-readable .rds sidecar (see temp/compare-tests.R). status_notes is
# the human-readable marker recorded in the header's "Status:" line.
run_tests <- function(status_notes = STATUS_NOTES) {
  # Capture a single timestamp so the filename and header stay consistent.
  run_time <- Sys.time()
  out_file <- sprintf(
    "temp/test-output-%s.txt",
    format(run_time, "%y%m%d-%H%M")
  )

  # Load the in-development source before reading the package version so the
  # header reports the version under test (devtools::test() load_all()s too, but
  # only after this point, so packageVersion() would otherwise read the installed
  # version rather than the dev source).
  devtools::load_all(quiet = TRUE)

  # Record which commit this run reflects and whether the tree had uncommitted
  # changes, so a saved output can be traced back to an exact source state.
  # Degrade to "unknown" rather than erroring when git is absent or this is not
  # a repo, since the test run itself does not depend on git.
  git_commit <- tryCatch(
    {
      sha <- system2(
        "git", c("rev-parse", "--short", "HEAD"),
        stdout = TRUE, stderr = FALSE
      )
      dirty <- length(system2(
        "git", c("status", "--porcelain"),
        stdout = TRUE, stderr = FALSE
      )) > 0
      sprintf("%s (%s)", sha, if (dirty) "dirty" else "clean")
    },
    error = function(e) "unknown"
  )

  # Compact header for quick scanning (written first, truncating any prior file).
  header <- sprintf(
    paste(
      "== loadflex test run ==",
      "Status:   %s",
      "Date:     %s",
      "git:      %s",
      "loadflex: %s",
      "testthat: %s",
      "R:        %s",
      "\n",
      sep = "\n"
    ),
    status_notes,
    format(run_time, "%Y-%m-%d %H:%M:%S %Z"),
    git_commit,
    as.character(utils::packageVersion("loadflex")),
    as.character(utils::packageVersion("testthat")),
    R.version.string
  )
  cat(header, file = out_file, append = FALSE)

  # Capture the message stream (message(), warning(), and most testthat/cli
  # progress text) into an in-memory connection rather than co-sinking it live to
  # out_file. Keeping it out of the live stream avoids interleaving with the
  # printed summary, so temp/compare-tests.R's text-mode section parser stays
  # robust. The captured lines are written as a segregated block at the end.
  msg_con <- textConnection("console_messages", open = "w", local = TRUE)

  # Detailed test output, appended to the file via the sink.
  res <- withr::with_options(
    list(
      testthat.progress.max_fails = Inf,
      cli.num_colors = 1  # strip ANSI color codes
    ),
    withr::with_output_sink(out_file, append = TRUE, {
      withr::with_message_sink(msg_con, {
        devtools::test(reporter = testthat::SummaryReporter$new(max_reports = Inf))
      })
    })
  )

  close(msg_con)

  # Machine-readable sidecar for precise run-to-run comparison (see
  # temp/compare-tests.R). The structured results object carries full, untruncated
  # test names and exact per-test counts, so comparisons key on (file, test)
  # rather than on the width-truncated, line-number-dependent summary text.
  saveRDS(res, sub("[.]txt$", ".rds", out_file))

  # Full session info at the end for reproducibility.
  cat("\n== Session info ==\n", file = out_file, append = TRUE)
  cat(
    paste(utils::capture.output(sessioninfo::session_info()), collapse = "\n"),
    "\n",
    file = out_file, append = TRUE
  )

  # Console messages captured during the run, kept in their own block so they
  # never interleave with the printed summary above. The "==" banner style (not
  # "══") is deliberately distinct from testthat's section banners, so the
  # text-mode parser in temp/compare-tests.R does not mistake this for a results
  # section.
  cat("\n== Console messages ==\n", file = out_file, append = TRUE)
  if (length(console_messages) == 0) {
    cat("(none)\n", file = out_file, append = TRUE)
  } else {
    cat(
      paste(console_messages, collapse = "\n"),
      "\n",
      file = out_file, append = TRUE
    )
  }

  # Derive high-level totals from the results object, then add summary to file and stdout
  df <- as.data.frame(res)

  totals <- list(
    tests    = nrow(df),
    failed   = sum(df$failed, na.rm = TRUE),
    errors   = sum(df$error, na.rm = TRUE),
    warnings = sum(df$warning, na.rm = TRUE),
    skipped  = sum(df$skipped, na.rm = TRUE)
  )
  totals$passed <- sum(
    df$failed == 0 & !df$error & !df$skipped,
    na.rm = TRUE
  )

  summary_line <- sprintf(
    "\n== Totals ==\nTests: %d | Passed: %d | Failed: %d | Errors: %d | Warnings: %d | Skipped: %d\n",
    totals$tests, totals$passed, totals$failed,
    totals$errors, totals$warnings, totals$skipped
  )

  cat(summary_line, file = out_file, append = TRUE)
  cat(summary_line)

  invisible(res)
}

message(sprintf("run_tests() is defined. Run with:\nrun_tests(status_notes='%s')", STATUS_NOTES))

# blair_coppock_humphreys_2023/ground_truth/build_ground_truth.R
# Output: ground_truth/blair_coppock_humphreys_2023_ground_truth.csv
# Depends on: original/ (run download_original.R first), maintained/, helpers.R,
#             ground_truth/archive_script_runs.csv
# Description: Runs all 82 maintained figure scripts and writes the comparison table.
#
#   A book companion archive is verified script-level rather than token-level. Its
#   published outputs are figures whose numbers are Monte Carlo diagnosands, so the
#   claim each row carries is "this script runs", not "this number reproduces". The
#   two diagnosis_text rows are the exception: they read a diagnosand out of the
#   deposit's own saved objects, and the book prints no counterpart, so they carry a
#   value and no verdict.
#
#   Each script runs in its own Rscript process, and that run is also how the repo's
#   output is regenerated: this file is the pipeline, not a check bolted on after it.
#   Three reasons the processes are separate. A script that fails must not stop the
#   other 81, which is the whole measurement. The scripts attach packages that mask
#   each other (ggdag masks stats::filter) and set process-level options that do not
#   unset (RGL_USE_NULL), so a shared session makes a script's result depend on which
#   ran before it. And the deposited declarations are sourced into the global
#   environment, where one script's objects are another's silent inputs.

library(here)
library(tidyverse)

here::i_am("ground_truth/build_ground_truth.R")

source(here::here("maintained", "helpers.R"))

paper_id <- "blair_coppock_humphreys_2023"
scripts <- list.files(here::here("maintained"), pattern = "^figure_.*[.]R$")
stopifnot(length(scripts) == 82)

# Packages a script needs but this machine does not have ----
# An uninstalled package is a fact about the machine, not about the book or the rewrite, so
# a script it blocks is recorded as not attempted rather than as a failure. The distinction
# has to be drawn BEFORE the run: after it, "there is no package called X" is just another
# error line, and a machine missing a package would quietly report the rewrite as broken.
# helpers.R attaches what every script needs; the optional ones are attached by the scripts
# that need them, which is why reading each script's own library() calls is the whole list.
needed_packages <- function(script) {
  read_lines(here::here("maintained", script)) |>
    str_subset("^library\\(") |>
    str_extract("(?<=^library\\()[A-Za-z0-9._]+")
}

# purrr::discard is namespaced because helpers.R attaches scales after tidyverse, and
# scales::discard takes the lambda as its `range` argument: the error names base::range and
# says nothing about the mask.
missing_packages <- function(script) {
  needed_packages(script) |>
    purrr::discard(\(p) requireNamespace(p, quietly = TRUE))
}

# Run one maintained script ----
run_script <- function(script) {
  absent <- missing_packages(script)
  if (length(absent) > 0) {
    return(tibble(script, runs = NA_integer_,
                  note = paste0("not attempted: package(s) not installed: ",
                                paste(absent, collapse = ", ")),
                  warns_text = NA_character_))
  }
  log_file <- tempfile(fileext = ".log")
  # Exit status cannot see a warning, so a script that runs is not thereby clean. Classifying
  # on status alone called all 82 scripts Clean-or-broken and silently overwrote 11 accurate
  # hand-written notes; 28 of the 82 emit warnings, among them a substituted apostrophe in a
  # published figure and one figure dropping 8,852 rows. The script is therefore sourced under
  # a calling handler that reports each unique warning, rather than run directly, so this
  # column says what the run emitted rather than what its exit code implies. Messages are
  # flattened to one line each because the marker is read back line by line.
  expr <- sprintf(
    paste0('w <- character(); withCallingHandlers(source("%s"), warning = function(x) ',
           '{ w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }); ',
           'writeLines(paste0("@@WARN@@", gsub("[\r\n]+", " ", unique(w))))'),
    here::here("maintained", script)
  )
  status <- system2("Rscript", c("-e", shQuote(expr)),
                    stdout = log_file, stderr = log_file)
  # A package built under a different R version is a fact about the machine, not the script,
  # which is the same reason there is no absent-package branch above.
  warnings_raised <- read_lines(log_file) |>
    str_subset("^@@WARN@@") |>
    str_remove("^@@WARN@@") |>
    str_squish() |>
    str_subset("built under R version", negate = TRUE)
  if (status == 0) {
    return(tibble(
      script, runs = 1L,
      note = if (length(warnings_raised) == 0) {
        "Clean"
      } else {
        str_trunc(paste0(length(warnings_raised), " warning(s): ",
                         paste(warnings_raised, collapse = "; ")), 300)
      },
      warns_text = if (length(warnings_raised) == 0) NA_character_ else
        paste(warnings_raised, collapse = "; ")
    ))
  }
  # "Fatal error" is Rscript's own startup failure and begins no line the other patterns
  # match, so it is named here rather than left to fall through as "no error line found".
  first_error <- read_lines(log_file) |>
    str_subset(regex("^(Error|Fatal error|.*halted)", ignore_case = TRUE)) |>
    head(1)
  tibble(script, runs = 0L,
         note = if (length(first_error) == 1) str_trunc(first_error, 200) else "no error line found",
         warns_text = if (length(warnings_raised) == 0) NA_character_ else
           paste(warnings_raised, collapse = "; "))
}

rewrite_runs <- map(scripts, run_script) |>
  list_rbind() |>
  mutate(script = str_remove(script, "[.]R$"))

# Blank the PDF clock ----
# Done here rather than in run_all.R because the scripts above are what wrote the files.
walk(list.files(here::here("maintained", "output"), pattern = "[.]pdf$", full.names = TRUE),
     blank_pdf_timestamps)

# The archive side ----
# Whether each DEPOSITED script still runs is a different question from whether the rewrite
# does, measured against a copy of the archive rather than the archive itself, so it lives in
# its own file. That file is still the 2026-03 measurement carried forward by hand;
# ground_truth/run_original_archive.R, which will regenerate it the way coppock_2017a's does,
# is not written yet. Until it is, THIS HALF OF THE TABLE IS NOT REPRODUCED BY THIS SCRIPT,
# and the stop() below is the only thing keeping the two halves in step.
archive <- read_csv(here::here("ground_truth", "archive_script_runs.csv"),
                    col_types = cols(.default = col_character())) |>
  mutate(runs_archive = as.integer(runs), notes_archive = notes, .keep = "unused")

unmeasured <- setdiff(rewrite_runs$script, archive$script)
if (length(unmeasured) > 0) {
  stop("no archive measurement for: ", paste(unmeasured, collapse = ", "))
}

# Assemble ----
# value_paper is 1 for every script: a figure printed in the book is a claim that the script
# behind it ran. The locus for every adverse row is `environment`, and that is the repo's
# whole finding: no figure is wrong, and every failure on either side is an R package that
# moved after the book went to press.
script_rows <- rewrite_runs |>
  left_join(archive, by = "script") |>
  transmute(
    paper_id,
    claim_id = paste0(script, "|script_runs"),
    table_figure = script,
    claim = "script_runs",
    value_script = runs_archive,
    value_paper = 1L,
    match = as.integer(runs_archive == 1L),
    value_rewrite = runs,
    match_rewrite = as.integer(runs == 1L),
    defect_locus = if_else((!is.na(match) & match == 0) |
                             (!is.na(match_rewrite) & match_rewrite == 0),
                           "environment", NA_character_),
    # Both notes where both sides failed. Taking the archive's alone would hide the
    # rewrite's error on the one row where it matters most: figure_18.12 fails on BOTH
    # sides for DIFFERENT reasons, vayr in the archive and fabricatr 2.0 in the rewrite.
    notes = case_when(
      runs_archive == 1L ~ note,
      runs == 1L ~ notes_archive,
      .default = paste0("archive: ", notes_archive, " | rewrite: ", note)
    ),
    # Its own column, because `notes` can only carry one story and on the rows where the
    # archive failed it has to carry that one. Without this the 14 scripts that the rewrite
    # repaired but that still warn would report nothing: `notes` would show the archive's
    # error and the warning would be lost exactly where the rewrite is doing the most work.
    # Kept separate from `notes` also so that build_code_updates.R, which classifies causes
    # by pattern-matching `notes`, cannot match on warning text.
    rewrite_warnings = str_trunc(warns_text, 300)
  ) |>
  arrange(table_figure)

text_rows <- tibble(
  paper_id,
  claim_id = c("diagnosis_text|power_N100_ate0.2", "diagnosis_text|power_N50_unadjusted"),
  table_figure = "diagnosis_text",
  claim = c("power_N100_ate0.2", "power_N50_unadjusted"),
  # CARRIED FORWARD, NOT COMPUTED. Both notes say the value comes from a named deposit
  # object, and neither read reproduces it: diagnosis_10.1.rds is a one-by-one tibble whose
  # single `power` column holds 0.1605, not 0.232, and diagnosis_9.1.rds is not a data frame
  # at all. The book quotes both numbers in its text and prints no table either could be
  # checked against, so where they were read from is an open question rather than a defect.
  # Nothing here derives them; asserting a derivation that does not hold would be worse than
  # saying so.
  value_script = c(0.232, 0.312),
  value_paper = NA_integer_,
  match = NA_integer_,
  value_rewrite = NA_integer_,
  match_rewrite = NA_integer_,
  defect_locus = NA_character_,
  notes = c("From diagnosis_10.1.rds; inline reference in book text",
            "From diagnosis_9.1.rds; power for 50-unit design"),
  # These two rows are carried forward by hand and no script runs for them, so there is
  # nothing that could have warned.
  rewrite_warnings = NA_character_
)

ground_truth <- bind_rows(script_rows, text_rows)

# Gates ----
stopifnot(
  nrow(ground_truth) == 84,
  !anyDuplicated(ground_truth$claim_id),
  # The locus rule, which tools/audit_corpus.R checks from the committed file. Asserting it
  # here as well means a row that breaks it never reaches the file.
  all(!is.na(ground_truth$defect_locus[
    (!is.na(ground_truth$match) & ground_truth$match == 0) |
      (!is.na(ground_truth$match_rewrite) & ground_truth$match_rewrite == 0)])),
  all(is.na(ground_truth$defect_locus[
    !is.na(ground_truth$match) & ground_truth$match == 1 &
      !is.na(ground_truth$match_rewrite) & ground_truth$match_rewrite == 1])),
  # "Clean" is now a claim about warnings, not about exit status, so it may not stand on a
  # row that recorded one. This is the gate that the old exit-status classification had no
  # way to state: it called 28 warning-raising scripts Clean and overwrote 11 accurate notes.
  !any(ground_truth$notes == "Clean" & !is.na(ground_truth$rewrite_warnings), na.rm = TRUE)
)

write_csv(ground_truth,
          here::here("ground_truth", paste0(paper_id, "_ground_truth.csv")), na = "")

print(count(ground_truth, claim, value_script, value_rewrite))

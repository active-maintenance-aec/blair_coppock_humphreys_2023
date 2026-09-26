# blair_coppock_humphreys_2023/run_all.R
# Runs the whole reproduction in order: verify the deposited archive, run all 82 maintained
# figure scripts and rebuild the ground truth from them, then rebuild the code-update notes.
#
# This repo is the corpus's one book, and two of the programme's usual instruments do not
# apply to it, which is why this file is shorter than its siblings and why the README says
# so at length. The published outputs are figures whose numbers are Monte Carlo diagnosands,
# so there is no token-level comparison to make and no in_text_claims.R to run; and there is
# no remastered edition, because a Princeton University Press book is not a candidate for
# re-typesetting.
#
# The repo had no run_all.R at all until 2026-09-25. `Rscript run_all.R` exits 2 on a file
# it cannot open, so the quarterly sweep scored it 3_broke every time it ran and no sweep
# ever measured it. tools/audit_corpus.R had named the gap correctly since 2026-09-23
# ("missing: README.qmd, run_all.R"); the two instruments disagreed and the sweep's row was
# the one that got read.

library(here)
library(withr)
here::i_am("run_all.R")

# Deposited archive ----
# Verify only. The full fetch is 690 MB over 369 files and Harvard answers a sustained burst
# with intermittent 404s, which would fail this pipeline for a reason that is not about the
# pipeline. Set VERIFY_ONLY=FALSE to let it repair what it finds altered.
Sys.setenv(VERIFY_ONLY = Sys.getenv("VERIFY_ONLY", unset = "TRUE"))
source(here::here("download_original.R"))

# Figures and ground truth ----
# build_ground_truth.R runs each of the 82 maintained scripts in its own Rscript process, so
# sourcing it here is what regenerates maintained/output/ as well as the comparison table.
# It also blanks the PDF timestamps, which is the only reason two runs of a deterministic
# script would otherwise produce differing files.
source(here::here("ground_truth", "build_ground_truth.R"))

# Code updates ----
# The five ecosystem changes behind every failure, written as bullets in the style the book's
# own Code updates section uses. Nothing here has been proposed to the book's authors.
source(here::here("code_updates", "build_code_updates.R"))

# Report ----
# The report is a tracked PDF that the README sends a reader to first, and nothing rebuilt it:
# the copy committed on 2026-09-26 had been rendered six weeks earlier from a .qmd that still
# called the rewrite march_2026_rewrite/, a directory this repo does not contain. A build step
# that stops here on a missing quarto is better than one that skips and leaves the old PDF
# looking current.
# SOURCE_DATE_EPOCH and FORCE_SOURCE_DATE are what make the render byte-stable: lualatex
# otherwise stamps a wall-clock date and a time-derived trailer /ID into the PDF, and two
# renders of identical content differ. Without them a run of this pipeline leaves the report
# modified every time, which is the same reason build_ground_truth.R blanks the figures'
# timestamps. Nothing in the document prints a date, so the fixed epoch is metadata only.
report_status <- withr::with_envvar(
  c(SOURCE_DATE_EPOCH = "0", FORCE_SOURCE_DATE = "1"),
  system2("quarto",
          c("render", shQuote(here::here("report", "blair_coppock_humphreys_2023_report.qmd"))))
)
stopifnot(report_status == 0)

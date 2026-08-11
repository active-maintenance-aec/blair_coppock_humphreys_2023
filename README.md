# Reproducibility Report: Blair, Coppock and Humphreys (2023)

*Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign*, Princeton University Press.
Archive DOI: [10.7910/DVN/HYVPO5](https://doi.org/10.7910/DVN/HYVPO5)

The full report is at [`report/blair_coppock_humphreys_2023_report.pdf`](report/blair_coppock_humphreys_2023_report.pdf).

## What this is, and how it differs from the rest of the corpus

This repository is part of an active maintenance programme: take a published
work's deposited replication archive, verify it runs, rewrite it in current
style, and check every published claim against the rewrite.

A book companion archive is not a journal article, and two of the programme's
usual instruments do not apply here.

**There is no errata note, and there will not be one.** The published outputs
are figures, and the numbers behind them are Monte Carlo diagnosands that vary
from draw to draw, so there is no token-level comparison to make against a
printed table. Verification is instead script-level (does it run?) and visual
(does the figure match the deposited one?). The book also already publishes
errata by its own path, at
[book.declaredesign.org](https://book.declaredesign.org), maintained by the
book's three authors.

**There is no remastered edition, and there will not be one.** The programme
produces corrected re-typesettings of journal articles; a Princeton University
Press book is not a candidate.

## What it found

| | |
|---|---|
| figure scripts in the archive | 82 |
| run under current R | 65 |
| fail under current R | 17 |
| reproduced by the maintained rewrite | 82 |

**Every failure is an ecosystem change rather than an error in the book.** No
figure is wrong. Five changes in the R package ecosystem account for all 17,
and the `ggdag::tidy_dagitty()` change alone accounts for 13 DAG figures.

[`code_updates/`](code_updates/) generates those five as bullets in the style
the book's own *Code updates* section already uses, which states the commitment
they discharge: "We are committed to maintaining the code in this book as the R
ecosystem evolves." **Nothing in this repository has been proposed to the book's
authors**; it is a draft for them to accept, revise or reject.

## Layout

- `original/` — the deposited archive. Not redistributed here; fetched by `download_original.R`.
- `maintained/` — the rewrite, one script per figure.
- `ground_truth/` — script-level pass/fail for all 82 figure scripts, in both the archive and the rewrite.
- `report/` — the full reproducibility report.
- `code_updates/` — the generator and its output.

## Known incomplete

**`download_original.R` does not yet complete a clean run.** It fetches from
Harvard Dataverse and verifies all 369 files against their published checksums,
but Harvard answers a sustained burst of requests with intermittent HTTP 404s,
and the retry backoff currently in the script is not always enough to get
through. If it stops, re-run it: it skips anything already present with the
right checksum, so successive runs make progress. Set `verify_only <- TRUE` to
check the deposit without downloading anything.

That verification is worth running on any deposit a script has ever been run
inside. It was written after finding that **67 of the 369 deposited files no
longer matched their published checksums**, all of them `figures/*.pdf`, because
the book's figure scripts write output into `figures/` beside the deposited
copies. Running one in place silently replaces a deposited file. Re-fetching
repairs it.

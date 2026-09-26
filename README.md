# Reproducibility Report: Blair, Coppock and Humphreys (2023)

*Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign*, Princeton University Press. Archive DOI: [10.7910/DVN/HYVPO5](https://doi.org/10.7910/DVN/HYVPO5)

The full report is at [`report/blair_coppock_humphreys_2023_report.pdf`](report/blair_coppock_humphreys_2023_report.pdf).

## What this is, and how it differs from the rest of the corpus

This repository is part of an active maintenance programme: take a published work's deposited replication archive, verify it runs, rewrite it in current style, and check every published claim against the rewrite.

A book companion archive is not a journal article, and two of the programme's usual instruments do not apply here.

**There is no errata note, and there will not be one.** The published outputs are figures, and the numbers behind them are Monte Carlo diagnosands that vary from draw to draw, so there is no token-level comparison to make against a printed table. Verification is instead script-level (does it run?) and visual (does the figure match the deposited one?). The book also already publishes errata by its own path, at [book.declaredesign.org](https://book.declaredesign.org), maintained by the book's three authors.

**There is no remastered edition, and there will not be one.** The programme produces corrected re-typesettings of journal articles; a Princeton University Press book is not a candidate.

## What it found

| | |
|---|---|
| figure scripts in the archive | 82 |
| run under current R | 65 |
| fail under current R | 17 |
| reproduced by the maintained rewrite | 82 |

**Every failure is an ecosystem change rather than an error in the book.** No figure is wrong. Five changes in the R package ecosystem account for all 17, and the `ggdag::tidy_dagitty()` change alone accounts for 13 DAG figures.

[`code_updates/`](code_updates/) generates those five as bullets in the style the book's own *Code updates* section already uses, which states the commitment they discharge: "We are committed to maintaining the code in this book as the R ecosystem evolves." **Nothing in this repository has been proposed to the book's authors**; it is a draft for them to accept, revise or reject.

## Relationship to the deposit

The deposited archive at [10.7910/DVN/HYVPO5](https://doi.org/10.7910/DVN/HYVPO5) is the authoritative object, and this repository is not a copy of it. `original/` is in `.gitignore`: the deposit is 369 files and 690 MB, it has a DOI and a citation, and Harvard Dataverse is the place to get it from. A clone of this repository is about 60 MB and contains no deposited file.

**There is no useful subset to redistribute.** The rewrite reads 48 of the deposit's 62 saved diagnosis objects, and those 48 are 537 MB: 78 percent of the deposit's bytes in 56 of its 369 files. What makes the archive heavy is exactly what the rewrite needs, namely the saved Monte Carlo diagnoses the book's figures are drawn from. There is no small set of inputs that would make this repository self-contained.

So the repository holds the results of the maintenance work rather than its inputs, and it holds all of them:

- all 82 maintained figure scripts, in `maintained/`
- every figure they produce, 84 of them, as PDF and SVG, in `maintained/output/`
- the script-level verdict for all 82 scripts, in the archive and in the rewrite, in `ground_truth/`
- the deposit's own manifest, `original_manifest.csv`: all 369 files with their published MD5 checksums and Dataverse file ids
- the report, and the five code-update bullets

**29 of the 82 figure scripts run from a bare clone**, with no deposit at all: they declare a design and diagnose it on the spot, and between them they write 30 of the 84 figures. The other 53 read a saved diagnosis object or a declaration out of `original/` and stop without it. Each script's `# Depends on:` header says which of the two it is, and the split was measured by running all 29 in a fresh clone with an emptied output directory rather than read off those headers, which until now claimed the deposit in all 82.

To close the gap:

    Rscript download_original.R

It fetches the 369 files into `original/` and checks every one against its published checksum. Re-running is free: a file already present with the right checksum is not fetched again. To check a deposit that is already on disk, without downloading anything:

    VERIFY_ONLY=TRUE Rscript download_original.R

`Rscript run_all.R` runs the whole pipeline: verify the deposit, run all 82 maintained scripts, rebuild the ground truth from them, rebuild the code-update notes, and re-render the report. It verifies the deposit rather than fetching it, so `download_original.R` has to have been run at least once first. A run leaves the tree byte-identical to what is committed, which is why the figures' PDF timestamps and the report's are fixed rather than stamped with the wall clock.

**Verify any deposit that a script has ever been run inside.** The check has caught the damage twice, months apart, and both times it was in `figures/`: the book's figure scripts write their output to `figures/`, beside the deposited copies of the same names, so running one in place silently replaces a deposited file with a freshly rendered one. The first episode left 67 of the 369 files not matching their published checksums. The second left 40 not matching and one missing outright, along with 69 `.svg` files that the deposit never contained: it holds 84 figure PDFs and no SVG at all. No code or data file was ever touched, and nothing detects any of it short of a checksum. Re-fetching repairs it, which is why `download_original.R` is the repair and not only the fetch.

## Layout

- `maintained/`: the rewrite, one script per figure, and the figures they write to `output/`.
- `ground_truth/`: script-level pass/fail for all 82 figure scripts, in both the archive and the rewrite.
- `report/`: the full reproducibility report.
- `code_updates/`: the generator and its output.
- `original_manifest.csv`: the deposit's 369 files, with published checksums and Dataverse file ids.
- `original/`: the deposited archive. Not in this repository; `download_original.R` fetches it.

## Known incomplete

**Two numbers in `ground_truth/` were never compared to the book.** The file's 84 rows are 82 script-level pass-or-fail verdicts and two `diagnosis_text` rows: power for the 50-unit design, and power at N = 100 with an effect of 0.2. Both carry a `value_script` read out of the deposit's saved diagnosis objects and an empty `value_paper`, because nobody has opened the book to the page and checked. They are unverified rather than non-reproducing.

**`download_original.R` has not been run as a cold fetch of all 369 files since it gained its retry loop.** Harvard Dataverse answers a sustained burst of requests with an intermittent 404 that is a server error page rather than a Dataverse "no such file", and the same file id succeeds moments later; two early attempts stopped a third of the way through because of it. The script now retries five times with a widening pause, which carried a 41-file repair without a failure. If a fetch does stop, re-run it: it skips anything already present with the right checksum, so successive runs make progress.

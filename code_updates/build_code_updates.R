# blair_coppock_humphreys_2023/code_updates/build_code_updates.R
# Output: code_updates/blair_coppock_humphreys_2023_code_updates.md
# Depends on: ground_truth/blair_coppock_humphreys_2023_ground_truth.csv
# Description: Group the figure scripts that no longer run under current R by the change
#   that broke them, and write them out as bullets in the style book.declaredesign.org's
#   errata.qmd already uses under "Code updates". That page is the book's own, maintained
#   by its three authors; this file is a proposal for it and publishes nothing.
#
#   The grouping is the point. Twenty scripts fail and five ecosystem changes explain all
#   of them, so a reader is served by five bullets naming what moved rather than twenty
#   naming which figures moved with it.
#
#   A script that fails only because a CRAN package is not installed is not a finding of any
#   kind. Install it and re-run. Three scripts were recorded that way in the March 2026 pass
#   and all three run once rdrobust and grf are present, so the record was describing the
#   machine rather than the archive. There is consequently no absent-package branch here:
#   if one ever appears, the fix is to install the package, not to classify it.

library(here)
library(tidyverse)

here::i_am("code_updates/build_code_updates.R")

options(width = 200)

ground_truth <- read_csv(
  here::here("ground_truth", "blair_coppock_humphreys_2023_ground_truth.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

# The ground truth holds two numeric diagnosand rows alongside the script rows, so the
# denominator is the script rows rather than every row: a first version said "84 figure
# scripts" where the archive has 82.
scripts <- ground_truth |> filter(claim == "script_runs")

failing <- scripts |>
  filter(value_script == "0") |>
  mutate(
    cause = case_when(
      str_detect(notes, "make_dag_df|tidy_dagitty") ~ "ggdag",
      str_detect(notes, "vayr::sunflower") ~ "vayr",
      str_detect(notes, "reframe|summarise no longer") ~ "dplyr",
      str_detect(notes, "interference package not on CRAN") ~ "interference",
      str_detect(notes, "DIDmultiplegt|rgl|RGL_USE_NULL") ~ "rgl",
      .default = "unclassified"
    )
  )

stopifnot(!any(failing$cause == "unclassified"))

# The book numbers its figures by chapter, and the ground truth keys them the same way,
# so the figure label carries the location a bullet needs without a lookup table. A script
# that draws two figures is keyed with both numbers joined by an underscore.
figure_labels <- function(keys) {
  keys |>
    str_remove("^figure_") |>
    str_replace_all("_", " and ") |>
    str_c("Figure ", x = _)
}

# The ground truth is keyed alphabetically, which puts Figure 10.1 ahead of Figure 2.1.
# A reader reaches these bullets from the book, so the figures are listed in the book's
# order: chapter number first, then figure number, both read as numbers.
figures_for <- function(which_cause) {
  keys <- failing |> filter(cause == which_cause) |> pull(table_figure)
  lead <- keys |> str_remove("^figure_") |> str_split("_") |> map_chr(1)
  nums <- str_split_fixed(lead, fixed("."), 2)
  keys[order(as.numeric(nums[, 1]), as.numeric(nums[, 2]))] |>
    figure_labels() |>
    str_c(collapse = ", ")
}

n_for <- function(which_cause) sum(failing$cause == which_cause)

bullets <- c(
  str_glue(
    "- The DAG figures are drawn through a `make_dag_df()` helper that joins ",
    "`ggdag::tidy_dagitty()` output onto a node table. `tidy_dagitty()` now returns its ",
    "own `x` and `y` columns, so that join produces `x.x` and `x.y` and the plot fails ",
    "with `object 'x' not found`. The helper now takes the coordinates from the tidied ",
    "object rather than joining them in. Affects {n_for('ggdag')} figures: ",
    "{figures_for('ggdag')}."
  ),
  str_glue(
    "- {figures_for('dplyr')}: `dplyr::summarise()` no longer returns more rows than ",
    "groups, and the utility calculation returns 2,000 rows per group. It now uses ",
    "`reframe()`, which is the function dplyr introduced for that case."
  ),
  str_glue(
    "- {figures_for('vayr')}: `vayr::sunflower()` renamed its `width` and `height` ",
    "arguments to `density` and `aspect_ratio`."
  ),
  str_glue(
    "- {figures_for('interference')}: the `interference` package is no longer on CRAN and ",
    "installs from GitHub at `szonszein/interference`. Loading `ggspatial` in the same ",
    "session also needs `Sys.setenv(RGL_USE_NULL = TRUE)`."
  ),
  str_glue(
    "- {figures_for('rgl')}: `DIDmultiplegt` pulls in `rgl`, which fails to load without ",
    "system OpenGL, so `Sys.setenv(RGL_USE_NULL = TRUE)` is set before it is loaded. The ",
    "same figure used `..count..`, which ggplot2 has deprecated in favour of ",
    "`after_stat(count)`. The errata page already carries the `RGL_USE_NULL` note for ",
    "pp. 212-213; this is the same fix reached from a second direction."
  )
)

# A sixth bullet, and the only one not derived from a `script_runs` row. The other five
# each explain a script that exits non-zero, so the ground truth records them and the
# classification above reaches them. This one is a declaration-level defect that raises no
# error at all: nothing exits non-zero, so there is no row to classify and the finding has
# to be carried by hand. That is also what makes it the most worth reporting of the six.
silent_bullet <- str_glue(
  "- `code/declarations/declaration_18.10.R` and `code/declarations/declaration_16.3.R` ",
  "declare their potential outcomes inside `cross_levels()`. fabricatr 2.0 renamed that ",
  "function's `by =` argument to `.by`, and R will not partial-match a supplied `by` to a ",
  "formal whose name begins with a dot. So `by =` falls through into `...`, `.by` is left ",
  "unfilled, and R binds the first *unnamed* argument to it positionally. In both these ",
  "files that argument is the `potential_outcomes()` call. fabricatr's deprecation shim ",
  "then reads `by` back out of the dots and returns it, discarding whatever had landed in ",
  "`.by` without a word, so the design runs to completion with no `Y_Z_0` and no `Y_Z_1` ",
  "column. **This is the only entry here that fails silently rather than with an error.** ",
  "Figure 18.12 does raise `object 'Y_Z_0' not found` further down, which is the good case; ",
  "`declaration_16.3.R` is sourced by nothing in the archive, and a reader who adapts it ",
  "and never references the potential outcomes gets no signal at all. Two other deposited ",
  "files, `code/figures/figure_16.6.R` and `code/diagnoses/diagnosis_16.4.R`, use the same ",
  "deprecated `by =` and are unaffected, because their `cross_levels()` has no unnamed ",
  "argument to displace: that contrast is the whole diagnosis. The repair belongs in ",
  "fabricatr and has been made there; until a release carries it, write ",
  "`.by = c(\"units\", \"periods\")` in place of `by = join_using(units, periods)`."
)

bullets <- c(bullets, silent_bullet)

lines <- c(
  "# Proposed additions to the book's Code updates section",
  "",
  str_glue(
    "Generated by `code_updates/build_code_updates.R` from the reproduction ground truth. ",
    "The destination is the `Code updates` section of `errata.qmd` in ",
    "`DeclareDesign/book`, which already states the commitment these entries discharge: ",
    "\"We are committed to maintaining the code in this book as the R ecosystem evolves.\" ",
    "Nothing here has been proposed to the book's authors."
  ),
  "",
  str_glue(
    "{nrow(failing)} of the {nrow(scripts)} figure scripts in the deposited archive ",
    "no longer run under current R. Every one of them is an ecosystem change rather than ",
    "an error in the book: no figure is wrong, and the maintained rewrite reproduces all ",
    "{nrow(scripts)}. The first five bullets below account for those {nrow(failing)} ",
    "scripts. The sixth is a separate case that raises no error, so no script-level record ",
    "reaches it."
  ),
  "",
  "## Bullets",
  "",
  bullets
)

write_lines(lines, here::here("code_updates",
                             "blair_coppock_humphreys_2023_code_updates.md"))

cat("bullets written:", length(bullets), "\n")
cat("scripts explained:", nrow(failing), "\n")

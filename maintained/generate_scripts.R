# blair_coppock_humphreys_2023 — generate_scripts.R
# Description: Record of the first-pass transformations that produced maintained/*.R from
#   the deposited scripts. It has ALREADY RUN, and maintained/ has been hand-edited since,
#   so it no longer reproduces its own output and writes nothing by default.
# Usage (from the paper root):
#   Rscript maintained/generate_scripts.R                              # refuses, reports divergence
#   GENERATE_SCRIPTS_MODE=dry Rscript maintained/generate_scripts.R    # report only
#   GENERATE_SCRIPTS_MODE=write Rscript maintained/generate_scripts.R  # overwrite maintained/*.R

library(here)
library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
here::i_am("maintained/generate_scripts.R")

orig_dir <- here::here("original", "code", "figures")
out_dir  <- here::here("maintained")
diag_dir <- here::here("original", "diagnosis_objects")

scripts <- list.files(orig_dir, pattern = "\\.R$", full.names = TRUE)

# Standard header to prepend
# The dependency line is derived from the transformed code rather than asserted. The fixed
# string it replaced named original/diagnosis_objects/ in all 82 headers, and 29 of the 82
# scripts read nothing out of the deposit at all. That line is where a reader looks to find
# out whether a script needs the 690 MB archive, so in 24 files it answered wrongly.
make_header <- function(script_name, output_names, code) {
  output_str <- paste(output_names, collapse = "\n# Output: ")
  deposit <- c(
    if (str_detect(code, stringr::fixed('here::here("original", "diagnosis_objects"'))) {
      "original/diagnosis_objects/"
    },
    if (str_detect(code, stringr::fixed('here::here("original", "code", "declarations"'))) {
      "original/code/declarations/"
    }
  )
  depends_str <- paste(c(deposit, "helpers.R"), collapse = ", ")
  sprintf(
    "# blair_coppock_humphreys_2023 — %s\n# Output: %s\n# Depends on: %s\n# Description: Maintained rewrite of %s\n\nsource(here::here(\"maintained\", \"helpers.R\"))\n\n",
    script_name, output_str, depends_str, script_name
  )
}

# Standard transformations applied to all scripts
apply_standard_transforms <- function(code) {
  code |>
    # Remove library() calls — packages loaded via helpers.R.
    # (?m) is load-bearing: without it `^` anchors at the start of the whole file and
    # only the first library() line is removed.
    str_replace_all("(?m)^library\\([^)]+\\)[ \\t]*\n", "") |>
    # Fix read_rds paths: "diagnosis_objects/X" -> here::here("original", "diagnosis_objects", "X")
    str_replace_all(
      'read_rds\\("diagnosis_objects/([^"]+)"\\)',
      'read_rds(here::here("original", "diagnosis_objects", "\\1"))'
    ) |>
    # Fix ggsave paths: "figures/X.pdf" -> here::here("maintained", "output", "X.pdf")
    str_replace_all(
      'ggsave\\("figures/([^"]+)"',
      'ggsave(here::here("maintained", "output", "\\1")'
    ) |>
    # The archive writes figure_23.2 through a cairo_pdf device rather than ggsave,
    # because ggsave has some trouble with that one figure. The ggsave rule above does not
    # match a device call, so before this rule the rewritten script still wrote to the
    # archive's own figures/ directory: it produced no PDF in the repo, where no such
    # directory exists, and wrote into the deposit in a tree where one does.
    str_replace_all(
      'cairo_pdf\\("figures/([^"]+)"',
      'cairo_pdf(here::here("maintained", "output", "\\1")'
    ) |>
    # Fix source paths for utilities
    str_replace_all(
      'source\\("code/utilities/make_dag_df.R"\\)',
      'source(here::here("maintained", "utilities", "make_dag_df.R"))'
    ) |>
    # Fix source paths for declarations
    str_replace_all(
      'source\\("code/declarations/([^"]+)"\\)',
      'source(here::here("original", "code", "declarations", "\\1"))'
    ) |>
    # Fix ..count.. -> after_stat(count). One global rule covers both the bare
    # `y = ..count..` and the `..count.. / sum(..count..)` form; the two rules that
    # used to be here spelled the delimiter with three dots and never matched.
    str_replace_all("\\.\\.count\\.\\.", "after_stat(count)") |>
    # size -> linewidth deprecation for line geoms is a warning only (not error).
    # ggplot2 4.x is backward compatible with size in line geoms; leave as-is.
    identity()
}

# Script-specific fixes
apply_script_fixes <- function(code, script_name) {
  # figure_21.2.R: summarise(Utility = prob) returns 2000 rows/group; fix with first()
  if (str_detect(script_name, "figure_21\\.2")) {
    code <- str_replace(
      code,
      "Utility = prob,",
      "Utility = first(prob),"
    )
  }

  # figure_18.12.R: vayr::sunflower API changed: width/height -> density/aspect_ratio
  # Old: sunflower(y = Z, width = 0.12, height = 0.18)
  # New: sunflower(y = Z, density = 0.5, aspect_ratio = 1.5)
  # width = 0.12, height = 0.18 => aspect_ratio = width/height = 0.12/0.18 = 0.667
  # density chosen to give similar visual spread
  if (str_detect(script_name, "figure_18\\.12")) {
    code <- str_replace(
      code,
      "vayr::sunflower\\(y = Z, width = 0\\.12, height = 0\\.18\\)",
      "vayr::sunflower(y = Z, density = 0.5, aspect_ratio = 0.667)"
    )
    code <- str_replace(
      code,
      "vayr::sunflower\\(x = periods, width = 0\\.12, height = 0\\.18\\)",
      "vayr::sunflower(x = periods, density = 0.5, aspect_ratio = 0.667)"
    )
    # Add library call for vayr (not in helpers)
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(vayr)\nlibrary(geomtextpath)"
    )
  }

  # figure_16.6.R: DIDmultiplegt loading — RGL_USE_NULL already set in helpers.R
  # Also fix ..count.. notation (handled by standard transforms)

  # figure_2.1_5.1.R and figure_10.1.R: custom DAG code (not using make_dag_df)
  # Fix: select() comes AFTER as_tibble(), not before
  if (str_detect(script_name, "figure_2\\.1_5\\.1|figure_10\\.1")) {
    # The pattern: tidy_dagitty(dag) |> select(name, direction, to, circular) |> as_tibble()
    # Fix: tidy_dagitty(dag) |> as_tibble() |> select(name, direction, to, circular)
    code <- str_replace_all(
      code,
      "tidy_dagitty\\(dag\\) \\|>\\s*\n\\s*select\\(name, direction, to, circular\\) \\|>\\s*\n\\s*as_tibble\\(\\)",
      "tidy_dagitty(dag) |>\n  as_tibble() |>\n  select(name, direction, to, circular)"
    )
    # Add ggdag library
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(ggdag)\nlibrary(ggraph)\nlibrary(ggforce)\nlibrary(ggtext)\nlibrary(latex2exp)"
    )
  }

  # DAG figures using make_dag_df utility
  dag_figures <- c("figure_6\\.1", "figure_6\\.2", "figure_8\\.1", "figure_8\\.7",
                   "figure_15\\.1", "figure_16\\.1", "figure_16\\.7", "figure_16\\.8",
                   "figure_17\\.1", "figure_18\\.1", "figure_18\\.9")
  if (any(map_lgl(dag_figures, ~ str_detect(script_name, .x)))) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"utilities\", \"make_dag_df\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"utilities\", \"make_dag_df.R\"))"
    )
    # Ensure ggdag and related packages are loaded
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(ggdag)\nlibrary(ggraph)\nlibrary(ggforce)\nlibrary(ggtext)"
    )
  }

  # figure_16.9.R: rdrobust (now installed)
  if (str_detect(script_name, "figure_16\\.9")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(rdrobust)"
    )
  }

  # figure_18.16.R: interference (GitHub) and ggspatial
  if (str_detect(script_name, "figure_18\\.16")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(sf)\nlibrary(spdep)\nlibrary(interference)\nlibrary(ggspatial)"
    )
    # gather() -> still functional but deprecated; keep as is for this figure
  }

  # figure_19.1.R and figure_19.2_19.3.R: grf
  if (str_detect(script_name, "figure_19\\.1|figure_19\\.2_19\\.3")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(grf)"
    )
  }

  # Scripts loading ggridges
  if (str_detect(script_name, "figure_10\\.4")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(ggridges)"
    )
  }

  # Scripts loading geomtextpath
  if (str_detect(script_name, "figure_13\\.1|figure_13\\.3")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"maintained\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"maintained\", \"helpers.R\"))\nlibrary(geomtextpath)"
    )
  }

  code
}

# Generate, in memory ----
generated <- map(scripts, function(f) {
  script_name <- basename(f)

  # Determine output figure names from the script
  code_orig <- read_file(f)
  output_names <- str_extract_all(code_orig, 'ggsave\\("figures/([^"]+)"', simplify = FALSE)[[1]]
  output_names <- str_replace(output_names, 'ggsave\\("figures/', "")
  output_names <- str_remove(output_names, '"')

  code_orig |>
    apply_standard_transforms() |>
    (\(x) paste0(make_header(script_name, output_names, x), x))() |>
    apply_script_fixes(script_name)
}) |>
  set_names(basename(scripts))

# Compare against what is on disk ----
divergence <- tibble(
  script = names(generated),
  on_disk = file.path(out_dir, names(generated))
) |>
  mutate(
    exists = file.exists(on_disk),
    disk_code = map2_chr(on_disk, exists, \(p, e) if (e) read_file(p) else NA_character_),
    gen_seeds = map_int(generated, \(x) str_count(x, stringr::fixed("set.seed("))),
    disk_seeds = map_int(disk_code, \(x) if (is.na(x)) NA_integer_ else str_count(x, stringr::fixed("set.seed("))),
    status = case_when(
      !exists ~ "absent from maintained/",
      map2_lgl(generated, disk_code, identical) ~ "identical",
      TRUE ~ "would be overwritten"
    )
  ) |>
  select(script, status, gen_seeds, disk_seeds)

n_diff <- sum(divergence$status == "would be overwritten")
seeds_lost <- sum(divergence$disk_seeds, na.rm = TRUE) - sum(divergence$gen_seeds)

# Dispatch on mode ----
# This generator is a one-shot: its header says "run once", it did, and the 82 scripts
# in maintained/ have been hand-edited since. It knows nothing about the set.seed(343)
# calls, the figure_18.12 sunflower values or the library() additions those edits added,
# so re-running it silently throws them away. It therefore writes nothing by default.
mode <- Sys.getenv("GENERATE_SCRIPTS_MODE", "")

report <- function() {
  print(count(divergence, status))
  print(filter(divergence, status != "identical"), n = Inf)
  print(str_glue(
    "{n_diff} of {nrow(divergence)} maintained scripts would be overwritten; ",
    "{seeds_lost} set.seed() calls would be lost."
  ))
}

if (mode == "") {
  report()
  stop(str_glue(
    "generate_scripts.R writes nothing by default: maintained/ has diverged from it. ",
    "Set GENERATE_SCRIPTS_MODE=dry to see the divergence, or ",
    "GENERATE_SCRIPTS_MODE=write to overwrite maintained/*.R anyway."
  ), call. = FALSE)
}

if (mode == "dry") {
  report()
  print("Dry run: nothing written.")
} else if (mode == "write") {
  report()
  print("GENERATE_SCRIPTS_MODE=write: overwriting maintained/*.R.")
  iwalk(generated, \(code, nm) write_file(code, file.path(out_dir, nm)))
  print(str_glue("Written: {length(generated)} scripts."))
} else {
  stop(str_glue("Unknown GENERATE_SCRIPTS_MODE: '{mode}'. Use 'dry' or 'write'."), call. = FALSE)
}

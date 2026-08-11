# blair_coppock_humphreys_2023 — generate_scripts.R
# Description: Programmatically generates maintained rewrite scripts from original scripts.
# Apply standard transformations plus targeted bug fixes.
# Run once from the paper root: Rscript march_2026_rewrite/generate_scripts.R

library(here)
library(readr)
library(stringr)
library(purrr)
here::i_am("march_2026_rewrite/generate_scripts.R")

orig_dir <- here::here("original", "code", "figures")
out_dir  <- here::here("march_2026_rewrite")
diag_dir <- here::here("original", "diagnosis_objects")

scripts <- list.files(orig_dir, pattern = "\\.R$", full.names = TRUE)

# Standard header to prepend
make_header <- function(script_name, output_names) {
  output_str <- paste(output_names, collapse = "\n# Output: ")
  sprintf(
    "# blair_coppock_humphreys_2023 — %s\n# Output: %s\n# Depends on: original/diagnosis_objects/, helpers.R\n# Description: Maintained rewrite of %s\n\nsource(here::here(\"march_2026_rewrite\", \"helpers.R\"))\n\n",
    script_name, output_str, script_name
  )
}

# Standard transformations applied to all scripts
apply_standard_transforms <- function(code) {
  code |>
    # Remove library() calls — packages loaded via helpers.R
    str_replace_all("^library\\([^)]+\\)\\s*\n", "") |>
    # Fix read_rds paths: "diagnosis_objects/X" -> here::here("original", "diagnosis_objects", "X")
    str_replace_all(
      'read_rds\\("diagnosis_objects/([^"]+)"\\)',
      'read_rds(here::here("original", "diagnosis_objects", "\\1"))'
    ) |>
    # Fix ggsave paths: "figures/X.pdf" -> here::here("march_2026_rewrite", "output", "X.pdf")
    str_replace_all(
      'ggsave\\("figures/([^"]+)"',
      'ggsave(here::here("march_2026_rewrite", "output", "\\1")'
    ) |>
    # Fix source paths for utilities
    str_replace_all(
      'source\\("code/utilities/make_dag_df.R"\\)',
      'source(here::here("march_2026_rewrite", "utilities", "make_dag_df.R"))'
    ) |>
    # Fix source paths for declarations
    str_replace_all(
      'source\\("code/declarations/([^"]+)"\\)',
      'source(here::here("original", "code", "declarations", "\\1"))'
    ) |>
    # Fix ..count.. -> after_stat(count)
    str_replace_all(
      "\\.\\.\\.count\\.\\.\\. / sum\\(\\.\\.\\.count\\.\\.\\.",
      "after_stat(count) / sum(after_stat(count))"
    ) |>
    str_replace_all(
      "y = \\.\\.\\.count\\.\\.\\.",
      "y = after_stat(count)"
    ) |>
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
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(vayr)\nlibrary(geomtextpath)"
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
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(ggdag)\nlibrary(ggraph)\nlibrary(ggforce)\nlibrary(ggtext)\nlibrary(latex2exp)"
    )
  }

  # DAG figures using make_dag_df utility
  dag_figures <- c("figure_6\\.1", "figure_6\\.2", "figure_8\\.1", "figure_8\\.7",
                   "figure_15\\.1", "figure_16\\.1", "figure_16\\.7", "figure_16\\.8",
                   "figure_17\\.1", "figure_18\\.1", "figure_18\\.9")
  if (any(map_lgl(dag_figures, ~ str_detect(script_name, .x)))) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"utilities\", \"make_dag_df\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"utilities\", \"make_dag_df.R\"))"
    )
    # Ensure ggdag and related packages are loaded
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(ggdag)\nlibrary(ggraph)\nlibrary(ggforce)\nlibrary(ggtext)"
    )
  }

  # figure_16.9.R: rdrobust (now installed)
  if (str_detect(script_name, "figure_16\\.9")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(rdrobust)"
    )
  }

  # figure_18.16.R: interference (GitHub) and ggspatial
  if (str_detect(script_name, "figure_18\\.16")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(sf)\nlibrary(spdep)\nlibrary(interference)\nlibrary(ggspatial)"
    )
    # gather() -> still functional but deprecated; keep as is for this figure
  }

  # figure_19.1.R and figure_19.2_19.3.R: grf
  if (str_detect(script_name, "figure_19\\.1|figure_19\\.2_19\\.3")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(grf)"
    )
  }

  # Scripts loading ggridges
  if (str_detect(script_name, "figure_10\\.4")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(ggridges)"
    )
  }

  # Scripts loading geomtextpath
  if (str_detect(script_name, "figure_13\\.1|figure_13\\.3")) {
    code <- str_replace(
      code,
      "source\\(here::here\\(\"march_2026_rewrite\", \"helpers\\.R\"\\)\\)",
      "source(here::here(\"march_2026_rewrite\", \"helpers.R\"))\nlibrary(geomtextpath)"
    )
  }

  code
}

# Process each script
walk(scripts, function(f) {
  script_name <- basename(f)
  base_name <- str_remove(script_name, "\\.R$")

  # Determine output figure names from the script
  code_orig <- read_file(f)
  output_names <- str_extract_all(code_orig, 'ggsave\\("figures/([^"]+)"', simplify = FALSE)[[1]]
  output_names <- str_replace(output_names, 'ggsave\\("figures/', "")
  output_names <- str_remove(output_names, '"')

  # Apply transforms
  code_new <- code_orig |>
    apply_standard_transforms() |>
    (\(x) paste0(make_header(script_name, output_names), x))() |>
    apply_script_fixes(script_name)

  # Write
  out_path <- file.path(out_dir, script_name)
  write_file(code_new, out_path)
  cat("Written:", script_name, "\n")
})

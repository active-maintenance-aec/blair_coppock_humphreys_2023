# blair_coppock_humphreys_2023/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every file
#   against its published checksum. Run this once before running anything in maintained/.
#   Re-running is free: a file already present with the right checksum is not fetched again.
#
#   THE ARCHIVE IS ALSO THE THING THAT GETS OVERWRITTEN. The book's figure scripts write
#   their output into figures/ beside the deposited copies, so running one inside original/
#   replaces a deposited file with a freshly rendered one. That had happened to 67 of the
#   369 files by the time this script was written, in two separate episodes months apart,
#   and nothing detected it until every file was checked against Dataverse. Every one was a
#   figures/*.pdf; no code or data file was touched. So this script is not only a fetch, it
#   is the repair, and `verify_only = TRUE` is the way to ask whether the deposit is still
#   pristine without downloading anything.
#
#   Five files were ingested by Dataverse into its tabular .tab format. For those the
#   deposited bytes come back only from ?format=original, which is what this fetches, and
#   the file is stored under its original name rather than the .tab one.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/HYVPO5"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

verify_only <- as.logical(Sys.getenv("VERIFY_ONLY", unset = "FALSE"))

# Manifest ----

manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

stopifnot(
  !any(duplicated(manifest$path)),
  !any(duplicated(manifest$file_id)),
  all(nzchar(manifest$md5_published))
)

# What is on disk now ----
# tools::md5sum returns NA for a file that is not there, which is the same answer this
# needs from a missing file and from a wrong one: fetch it.

local_md5 <- function(paths) {
  full <- here::here("original", paths)
  unname(tools::md5sum(full))
}

status <- manifest |>
  mutate(
    md5_local = local_md5(path),
    state = case_when(
      is.na(md5_local) ~ "missing",
      md5_local == md5_published ~ "ok",
      .default = "altered"
    )
  )

print(count(status, state))

altered <- status |> filter(state == "altered")
if (nrow(altered) > 0) {
  message("\nFiles on disk that differ from the deposited bytes:")
  print(altered |> select(path, md5_published, md5_local), n = 100)
}

if (verify_only) {
  stopifnot(all(status$state == "ok"))
  message("\nThe deposit on disk matches Dataverse file for file.")
} else {

  # Fetch ----
  # Only what is missing or altered, so a re-run of a clean tree downloads nothing.

  needed <- status |> filter(state != "ok")

  # ?format=original asks for the bytes behind an ingested file, and Dataverse answers 404
  # for a file it never ingested, so the query is attached only to the five tabular ones.
  pwalk(
    list(needed$file_id, needed$path, needed$tabular),
    function(id, path, tabular) {
      dest <- here::here("original", path)
      dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
      url <- if (tabular) {
        str_glue("{base_url}/{id}?format=original")
      } else {
        str_glue("{base_url}/{id}")
      }
      # Harvard Dataverse answers a burst of requests with an intermittent 404 that is a
      # Payara error page rather than a Dataverse "no such file", and the same id succeeds
      # moments later. Retrying with a widening pause is the difference between a fetch
      # that works and one that stops a third of the way through, which is what the first
      # two attempts did. The pause also keeps this from hammering a public archive.
      for (attempt in 1:5) {
        ok <- tryCatch({
          download.file(url, dest, mode = "wb", quiet = TRUE)
          TRUE
        }, error = function(e) FALSE, warning = function(w) FALSE)
        if (ok) break
        if (file.exists(dest)) file.remove(dest)
        if (attempt == 5) stop(str_glue("could not fetch {path} after five attempts"))
        Sys.sleep(2 * attempt)
      }
      Sys.sleep(0.3)
    }
  )

  # Verify ----
  # Against the manifest rather than against what was just downloaded, so a fetch that
  # silently returned an error page fails here rather than being trusted.

  final <- manifest |> mutate(md5_local = local_md5(path))
  wrong <- final |> filter(is.na(md5_local) | md5_local != md5_published)

  if (nrow(wrong) > 0) {
    print(wrong |> select(path, md5_published, md5_local), n = 100)
    stop(str_glue("{nrow(wrong)} of {nrow(final)} files do not match their published checksum"))
  }

  message(str_glue(
    "\n{nrow(needed)} fetched, {nrow(final)} files verified against {dataset_doi}."))
}

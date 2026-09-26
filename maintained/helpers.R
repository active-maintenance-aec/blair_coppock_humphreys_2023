# blair_coppock_humphreys_2023 — helpers.R
# Loaded by all scripts in maintained/
# Description: shared packages and utilities for maintained rewrite

here::i_am("maintained/helpers.R")

# macOS Tahoe: rgl requires OpenGL (libGLU) which is not available without XQuartz.
# DIDmultiplegt and ggspatial load rgl transitively; setting RGL_USE_NULL before
# their library() calls prevents the fatal dyn.load error.
Sys.setenv(RGL_USE_NULL = "TRUE")

suppressPackageStartupMessages({
  library(here)
  library(DeclareDesign)
  library(rdss)
  library(tidyverse)
  library(scales)
  library(ggtext)
  library(patchwork)
})

# 29 of the deposited scripts end with a bare `g`, which is how you look at a figure in an
# interactive session and is a no-op there. Under Rscript it opens the default pdf() device
# instead and draws into Rplots.pdf in the working directory, so the pipeline was writing a
# 259 KB figure nobody asked for beside the 82 it was asked for. Every figure this repo keeps
# is written by an explicit ggsave() or cairo_pdf() call, so the default device should discard.
pdf(NULL)

# Optional packages — loaded explicitly in scripts that need them:
# library(ggdag)          — DAG figures (2.1_5.1, 6.1, 6.2, 8.1, 8.7, 10.1, 15.x, 16.x, 17.1, 18.x)
# library(ggraph)         — DAG figures
# library(ggforce)        — DAG figures (geom_regon)
# library(latex2exp)      — figures 2.1_5.1, 10.1
# library(ggridges)       — figure 10.4
# library(geomtextpath)   — figures 13.1, 13.3, 18.12
# library(vayr)           — figure 18.12
# library(DIDmultiplegt)  — figure 16.6 (RGL_USE_NULL already set above)
# library(rdrobust)       — figure 16.9 (install from CRAN)
# library(sf)             — figure 18.16
# library(spdep)          — figure 18.16
# library(interference)   — figure 18.16 (install from GitHub: szonszein/interference)
# library(ggspatial)      — figure 18.16
# library(grf)            — figures 19.1, 19.2_19.3 (install from CRAN)

# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, so two runs of an otherwise deterministic script produce differing files. The
# SVG device stamps nothing, which is how we know the difference is only the clock:
# figure_2.2's SVG is byte-identical across runs while its PDF differs in exactly the
# 14 bytes of those two dates. Blanking them lets the sweep's diff cover every file the
# pipeline writes rather than all but the 82 PDFs.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))

  blank_raw <- function(x) {
    hits <- grepRaw("D:[0-9]{14}", x, all = TRUE)
    for (h in hits) x[h:(h + length(epoch) - 1L)] <- epoch
    x
  }

  raw_pdf <- blank_raw(raw_pdf)

  # The 81 ggsave() PDFs carry their dates in the clear, but figure_23.2 is drawn by
  # cairo_pdf, which packs /Info into a compressed object stream, so its clock is invisible
  # to the search above and survived every run until 2026-09-25. Each Flate stream is
  # decompressed, blanked, recompressed and padded back to its original byte length, so
  # /Length and every cross-reference offset stay exactly where they were and no xref
  # rebuild is needed. memCompress("gzip") emits the zlib wrapper FlateDecode expects.
  starts <- grepRaw("stream\r?\n", raw_pdf, all = TRUE)
  for (s in starts) {
    header <- regexpr("stream\r?\n", rawToChar(raw_pdf[s:(s + 7L)]))
    body_start <- s + attr(header, "match.length")
    body_end <- grepRaw("endstream", raw_pdf, offset = body_start)
    if (length(body_end) == 0) next
    body <- raw_pdf[body_start:(body_end - 1L)]
    plain <- tryCatch(memDecompress(body, "gzip"), error = function(e) NULL)
    if (is.null(plain) || length(grepRaw("D:[0-9]{14}", plain, all = TRUE)) == 0) next
    packed <- memCompress(blank_raw(plain), "gzip")
    if (length(packed) > length(body)) next
    raw_pdf[body_start:(body_end - 1L)] <-
      c(packed, rep(charToRaw("\n"), length(body) - length(packed)))
  }

  writeBin(raw_pdf, path)
  invisible(path)
}

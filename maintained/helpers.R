# blair_coppock_humphreys_2023 — helpers.R
# Loaded by all scripts in march_2026_rewrite/
# Description: shared packages and utilities for maintained rewrite

here::i_am("march_2026_rewrite/helpers.R")

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

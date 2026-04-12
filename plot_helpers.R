# ============================================================
# plot_helpers.R
# Purpose:
#   Shared helpers for thesis simulation plots
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(grid)

# -----------------------------
# Label helpers
# -----------------------------
label_rhoY <- function(x) {
  paste0("rho_Y = ", formatC(as.numeric(x), format = "f", digits = 2))
}

label_latentR2 <- function(x) {
  paste0("latent R² = ", formatC(as.numeric(x), format = "f", digits = 2))
}

label_rhoB <- function(x) {
  paste0("rho_betweenX = ", formatC(as.numeric(x), format = "f", digits = 2))
}

label_comp <- function(x) {
  paste0("linear share = ", formatC(as.numeric(x), format = "f", digits = 2))
}

label_rhoX_axis <- function(x) {
  formatC(as.numeric(x), format = "f", digits = 2)
}

# -----------------------------
# Color palettes
# -----------------------------
raw_model_cols <- c(
  "Baseline OLS" = "#6F8FAF",   # stronger muted blue-grey
  "Oracle OLS"   = "#C97C5D",   # stronger muted terracotta
  "XGBoost"      = "#B8B8B8"    # lighter soft sage
)

delta_cols <- c(
  "XGB - Baseline" = "#6F8FAF",
  "XGB - Oracle"   = "#C97C5D"
)

box_outline_col <- "#4A4A4A"

# Heatmap palette:
# low = XGB rarely wins
# mid = around chance
# high = XGB often wins
win_heat_cols <- c(
  low  = "#2166AC",
  mid  = "white",
  high = "#B2182B"
)

# -----------------------------
# Common theme
# -----------------------------
theme_thesis <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      panel.spacing = unit(0.8, "lines"),
      strip.text = element_text(face = "bold", size = base_size * 0.9),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size * 1.05),
      plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

# -----------------------------
# Figure subtitle helper
# -----------------------------
make_fixed_subtitle <- function(comp_linear, rho_betweenX) {
  paste0(
    label_comp(comp_linear),
    " | ",
    label_rhoB(rho_betweenX)
  )
}
# ============================================================
# 09_plots.R
# Purpose:
# Generate ONLY main-text pooled R2 figures
#
# Figures:
# Figure 1: Measurement conditions
# x-axis = rho_X
# facet = rho_Y
# pooled over latent_R2, comp_linear, rho_betweenX
#
# Figure 2: Structural conditions
# x-axis = latent_R2
# facet = comp_linear
# pooled over rho_X, rho_Y, rho_betweenX
#
# Notes:
# - Replication-level boxplots
# - Outcome = test-set R2 only
# - XGBoost shown first / leftmost
# - Horizontal whisker caps added with stat_boxplot()
# ============================================================

rm(list = ls(all.names = TRUE))

source("run_config.R")
source("plot_helpers.R")

library(ggplot2)
library(dplyr)

# -----------------------------
# 1. Load replication-level data
# -----------------------------

results_df <- read.csv(
  file.path(run_dir, "results_replication_level.csv"),
  stringsAsFactors = FALSE
)

fig_dir <- file.path(run_dir, "main_text_figures")

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# -----------------------------
# 2. Main-text y-axis settings
# -----------------------------

# Use -0.15 to bring x tick labels visually closer to the 0 line.
# If any lower whiskers are clipped, change back to c(-0.25, 0.85).
r2_limits <- c(-0.25, 0.90)

r2_breaks <- c(
  -0.25, 0.00, 0.25, 0.50, 0.75
)

# -----------------------------
# 3. Factor levels
# -----------------------------

latent_levels <- c(0.20, 0.50, 0.80)
rhoX_levels   <- c(0.60, 0.80, 1.00)
rhoY_levels   <- c(1.00, 0.80, 0.60)
comp_levels   <- c(0.80, 0.50, 0.20)
rhoB_levels   <- c(0.00, 0.50)

model_order <- c(
  "XGBoost",
  "Baseline OLS",
  "Aligned OLS",
  "Oracle OLS"
)

# -----------------------------
# 4. Helper formatting
# -----------------------------

fmt2 <- function(x) {
  sprintf("%.2f", as.numeric(as.character(x)))
}

label_rhoY_pretty <- function(x) {
  paste0("\u03C1Y = ", fmt2(x))
}

label_comp_pretty <- function(x) {
  paste0("Linear share = ", fmt2(x))
}

label_rhoX_tick <- function(x) {
  paste0("\u03C1X = ", fmt2(x))
}

label_latentR2_pretty <- function(x) {
  paste0("R\u00B2 = ", fmt2(x))
}

# -----------------------------
# 5. Model colours
# -----------------------------

main_model_cols <- c(
  "XGBoost"      = "#74ADD1",
  "Baseline OLS" = "#FFFFBF",
  "Aligned OLS"  = "#FEE080",
  "Oracle OLS"   = "#FDAE61"
)

if (!exists("box_outline_col")) {
  box_outline_col <- "grey50"
}

# -----------------------------
# 6. Theme
# -----------------------------

main_text_theme <- theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    
    strip.text = element_text(
      size = 15,
      face = "bold",
      margin = margin(t = 2, r = 4, b = 4, l = 4)
    ),
    
    axis.title.x = element_blank(),
    
    axis.title.y = element_text(
      size = 14,
      face = "bold",
      margin = margin(r = 8)
    ),
    
    axis.text.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = -4)
    ),
    
    axis.text.y = element_text(
      size = 14
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.spacing = unit(0.85, "lines"),
    
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

# -----------------------------
# 7. Create long R2 data
# -----------------------------

rep_long_r2 <- bind_rows(
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Baseline OLS",
      value = r2_ols_base
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Aligned OLS",
      value = r2_ols_true_interaction
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Oracle OLS",
      value = r2_ols_oracle
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "XGBoost",
      value = r2_xgb
    )
) %>%
  mutate(
    model = factor(model, levels = model_order),
    
    latent_R2 = factor(latent_R2, levels = latent_levels),
    rho_X = factor(rho_X, levels = rhoX_levels),
    rho_Y = factor(rho_Y, levels = rhoY_levels),
    comp_linear = factor(comp_linear, levels = comp_levels),
    rho_betweenX = factor(rho_betweenX, levels = rhoB_levels)
  )

# ============================================================
# 8. Figure 1: Measurement conditions, R2
# ============================================================

make_measurement_r2_plot <- function(df) {
  
  ggplot(
    df,
    aes(x = rho_X, y = value, fill = model)
  ) +
    
    # Horizontal end caps on whiskers
    stat_boxplot(
      geom = "errorbar",
      width = 0.32,
      position = position_dodge(width = 0.78),
      linewidth = 0.30,
      colour = box_outline_col
    ) +
    
    geom_hline(
      yintercept = 0,
      linewidth = 0.30,
      colour = "grey65"
    ) +
    
    geom_boxplot(
      width = 0.70,
      position = position_dodge(width = 0.78),
      outlier.shape = NA,
      linewidth = 0.30,
      median.linewidth = 0.45,
      colour = box_outline_col
    ) +
    
    facet_wrap(
      ~ rho_Y,
      ncol = 1,
      labeller = as_labeller(label_rhoY_pretty)
    ) +
    
    scale_fill_manual(
      values = main_model_cols,
      breaks = model_order,
      drop = FALSE
    ) +
    
    scale_x_discrete(
      labels = label_rhoX_tick
    ) +
    
    scale_y_continuous(
      breaks = r2_breaks
    ) +
    
    coord_cartesian(
      ylim = r2_limits
    ) +
    
    labs(
      x = NULL,
      y = expression("Test-set " * R^2)
    ) +
    
    main_text_theme
}

fig1_r2 <- make_measurement_r2_plot(rep_long_r2)

ggsave(
  filename = file.path(
    fig_dir,
    "Figure1_measurement_conditions_R2_replication_level.png"
  ),
  plot = fig1_r2,
  width = 10.0,
  height = 8.5,
  units = "in",
  dpi = 500,
  bg = "white"
)

# ============================================================
# 9. Figure 2: Structural conditions, R2
# ============================================================

make_structural_r2_plot <- function(df) {
  
  ggplot(
    df,
    aes(x = latent_R2, y = value, fill = model)
  ) +
    
    # Horizontal end caps on whiskers
    stat_boxplot(
      geom = "errorbar",
      width = 0.32,
      position = position_dodge(width = 0.78),
      linewidth = 0.30,
      colour = box_outline_col
    ) +
    
geom_hline(
  yintercept = 0,
  linewidth = 0.30,
  colour = "grey65"
) +
    
    geom_boxplot(
      width = 0.70,
      position = position_dodge(width = 0.78),
      outlier.shape = NA,
      linewidth = 0.30,
      median.linewidth = 0.45,
      colour = box_outline_col
    ) +
    
    facet_wrap(
      ~ comp_linear,
      ncol = 1,
      labeller = as_labeller(label_comp_pretty)
    ) +
    
    scale_fill_manual(
      values = main_model_cols,
      breaks = model_order,
      drop = FALSE
    ) +
    
    scale_x_discrete(
      labels = label_latentR2_pretty
    ) +
    
    scale_y_continuous(
      breaks = r2_breaks
    ) +
    
    coord_cartesian(
      ylim = r2_limits
    ) +
    
    labs(
      x = NULL,
      y = expression("Test-set " * R^2)
    ) +
    
    main_text_theme
}

fig2_r2 <- make_structural_r2_plot(rep_long_r2)

ggsave(
  filename = file.path(
    fig_dir,
    "Figure2_structural_conditions_R2_replication_level.png"
  ),
  plot = fig2_r2,
  width = 10.0,
  height = 8.5,
  units = "in",
  dpi = 500,
  bg = "white"
)

cat("Finished main-text R2 figures.\n")
cat("Saved to:\n")
cat(fig_dir, "\n")

















##################### Appendix - all conditions boxplots ####

# ============================================================
# APPENDIX R2 FIGURES
# Same visual style as main-text R2 figures
# Split by:
#   comp_linear x rho_betweenX x latent_R2
# Panels:
#   rows = rho_Y
# x-axis:
#   rho_X
# ============================================================

appendix_fig_dir <- file.path(run_dir, "appendix_figures_R2_clear")

if (!dir.exists(appendix_fig_dir)) {
  dir.create(appendix_fig_dir, recursive = TRUE)
}

# IMPORTANT:
# Use exactly the same R2 settings as the main-text figures.
r2_limits <- c(-0.10, 0.85)

r2_breaks <- c(0.00, 0.25, 0.50, 0.75
)

make_appendix_r2_plot <- function(df) {
  
  ggplot(
    df,
    aes(x = rho_X, y = value, fill = model)
  ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.30,
      colour = "grey65"
    ) +
    geom_boxplot(
      width = 0.82,
      position = position_dodge(width = 0.88),
      outlier.shape = NA,
      linewidth = 0.35,
      median.linewidth = 0.50,
      colour = box_outline_col
    ) +
    facet_wrap(
      ~ rho_Y,
      ncol = 1,
      labeller = as_labeller(label_rhoY_pretty)
    ) +
    scale_fill_manual(
      values = main_model_cols,
      breaks = model_order,
      drop = FALSE
    ) +
    scale_x_discrete(
      labels = label_rhoX_tick
    ) +
    scale_y_continuous(
      breaks = r2_breaks
    ) +
    coord_cartesian(
      ylim = r2_limits
    ) +
    labs(
      x = NULL,
      y = expression("Test-set " * R^2)
    ) +
    main_text_theme +
    theme(
      panel.spacing = unit(0.45, "lines"),
      
      strip.text = element_text(
        size = 15,
        face = "bold",
        margin = margin(t = 4, r = 4, b = 2, l = 4)
      ),
      
      plot.margin = margin(t = 14, r = 8, b = 5, l = 6),
      
      legend.box.margin = margin(t = 2, r = 0, b = 0, l = 0),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      
      axis.title.y = element_text(
        size = 14,
        face = "bold",
        margin = margin(r = 6)
      )
    )
}

# -----------------------------
# Generate appendix R2 figures
# -----------------------------

for (comp_val in levels(rep_long_r2$comp_linear)) {
  for (rhoB_val in levels(rep_long_r2$rho_betweenX)) {
    for (latent_val in levels(rep_long_r2$latent_R2)) {
      
      plot_df <- rep_long_r2 %>%
        filter(
          comp_linear == comp_val,
          rho_betweenX == rhoB_val,
          latent_R2 == latent_val
        )
      
      p <- make_appendix_r2_plot(plot_df)
      
      file_name <- paste0(
        "Appendix_R2_",
        "comp", gsub("\\.", "p", comp_val),
        "_rhoB", gsub("\\.", "p", rhoB_val),
        "_latentR2", gsub("\\.", "p", latent_val),
        ".png"
      )
      
      ggsave(
        filename = file.path(appendix_fig_dir, file_name),
        plot = p,
        width = 10.0,
        height = 13.0,
        units = "in",
        dpi = 300,
        bg = "white"
      )
    }
  }
}

cat("Finished appendix R2 figures.\n")
cat("Saved to:\n")
cat(appendix_fig_dir, "\n")





library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

# -----------------------------
# 1. Load replication-level data
# -----------------------------

results_df <- read.csv(
  file.path(run_dir, "results_replication_level.csv")
)

# -----------------------------
# 2. Output folders
# -----------------------------

appendix_r2_dir <- file.path(run_dir, "appendix_figures_R2_clear")
appendix_rmse_dir <- file.path(run_dir, "appendix_figures_RMSE_clear")

if (!dir.exists(appendix_r2_dir)) {
  dir.create(appendix_r2_dir, recursive = TRUE)
}

if (!dir.exists(appendix_rmse_dir)) {
  dir.create(appendix_rmse_dir, recursive = TRUE)
}

# -----------------------------
# 3. Condition levels
# -----------------------------
# These are inferred from the data, so the script does not depend
# on external objects such as latent_levels or comp_levels.

latent_levels <- sort(unique(results_df$latent_R2))
rhoX_levels   <- sort(unique(results_df$rho_X))
rhoY_levels   <- sort(unique(results_df$rho_Y), decreasing = TRUE)
comp_levels   <- sort(unique(results_df$comp_linear))
rhoB_levels   <- sort(unique(results_df$rho_betweenX))

# -----------------------------
# 4. Model order
# -----------------------------

model_order <- c(
  "XGBoost",
  "Baseline OLS",
  "Aligned OLS",
  "Oracle OLS"
)

# -----------------------------
# 5. Axis settings
# -----------------------------

# R2: common scale across all appendix R2 figures
appendix_r2_limits <- c(-0.10, 0.90)
appendix_r2_breaks <- c(0.00, 0.25, 0.50, 0.75)

# RMSE: common scale across all appendix RMSE figures
# Adjust only if your RMSE values go outside this range.
appendix_rmse_limits <- c(0.50, 3.25)
appendix_rmse_breaks <- seq(0.50, 3.00, by = 0.50)

# -----------------------------
# 6. Label helpers
# -----------------------------

fmt2 <- function(x) {
  sprintf("%.2f", as.numeric(as.character(x)))
}

label_rhoX_tick <- function(x) {
  paste0("\u03C1X = ", fmt2(x))
}

label_rhoY_pretty <- function(x) {
  paste0("\u03C1Y = ", fmt2(x))
}

label_comp_text <- function(x) {
  paste0("linear share = ", fmt2(x))
}

label_rhoB_text <- function(x) {
  paste0("\u03C1betweenX = ", fmt2(x))
}

label_latentR2_text <- function(x) {
  paste0("latent R\u00B2 = ", fmt2(x))
}

safe_num <- function(x) {
  gsub("\\.", "p", fmt2(x))
}

# -----------------------------
# 7. Create long R2 data
# -----------------------------

rep_long_r2 <- bind_rows(
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Baseline OLS",
      value = r2_ols_base
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Aligned OLS",
      value = r2_ols_true_interaction
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Oracle OLS",
      value = r2_ols_oracle
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "XGBoost",
      value = r2_xgb
    )
) %>%
  mutate(
    model = factor(model, levels = model_order),
    
    latent_R2 = factor(latent_R2, levels = latent_levels),
    rho_X = factor(rho_X, levels = rhoX_levels),
    rho_Y = factor(rho_Y, levels = rhoY_levels),
    comp_linear = factor(comp_linear, levels = comp_levels),
    rho_betweenX = factor(rho_betweenX, levels = rhoB_levels)
  )

# -----------------------------
# 8. Create long RMSE data
# -----------------------------

rep_long_rmse <- bind_rows(
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Baseline OLS",
      value = rmse_ols_base
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Aligned OLS",
      value = rmse_ols_true_interaction
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "Oracle OLS",
      value = rmse_ols_oracle
    ),
  
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      model = "XGBoost",
      value = rmse_xgb
    )
) %>%
  mutate(
    model = factor(model, levels = model_order),
    
    latent_R2 = factor(latent_R2, levels = latent_levels),
    rho_X = factor(rho_X, levels = rhoX_levels),
    rho_Y = factor(rho_Y, levels = rhoY_levels),
    comp_linear = factor(comp_linear, levels = comp_levels),
    rho_betweenX = factor(rho_betweenX, levels = rhoB_levels)
  )

# -----------------------------
# 9. Appendix theme
# -----------------------------

appendix_theme <- theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    
    plot.title = element_blank(),
    
    plot.subtitle = element_text(
      size = 14,
      hjust = 0,
      margin = margin(b = 10)
    ),
    
    strip.text = element_text(
      size = 16,
      face = "bold",
      margin = margin(t = 5, r = 4, b = 5, l = 4)
    ),
    
    axis.title.x = element_blank(),
    
    axis.title.y = element_text(
      size = 15,
      face = "bold",
      margin = margin(r = 8)
    ),
    
    axis.text.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 5)
    ),
    
    axis.text.y = element_text(
      size = 14
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.spacing = unit(0.75, "lines"),
    
    plot.margin = margin(t = 12, r = 12, b = 8, l = 10)
  )

# -----------------------------
# 10. General appendix plot function
# -----------------------------

make_appendix_plot <- function(df,
                               metric = c("R2", "RMSE"),
                               comp_val,
                               rhoB_val,
                               latent_val) {
  
  metric <- match.arg(metric)
  
  if (metric == "R2") {
    y_label <- expression("Test-set " * R^2)
    y_limits <- appendix_r2_limits
    y_breaks <- appendix_r2_breaks
  }
  
  if (metric == "RMSE") {
    y_label <- "Test-set RMSE"
    y_limits <- appendix_rmse_limits
    y_breaks <- appendix_rmse_breaks
  }
  
  condition_subtitle <- paste(
    label_latentR2_text(latent_val),
    label_comp_text(comp_val),
    label_rhoB_text(rhoB_val),
    sep = "; "
  )
  
  ggplot(
    df,
    aes(x = rho_X, y = value, fill = model)
  ) +
    
    stat_boxplot(
      geom = "errorbar",
      width = 0.32,
      position = position_dodge(width = 0.82),
      linewidth = 0.35,
      colour = box_outline_col
    ) +
    
    geom_hline(
      yintercept = ifelse(metric == "R2", 0, NA),
      linewidth = 0.30,
      colour = "grey65",
      na.rm = TRUE
    ) +
    
    geom_boxplot(
      width = 0.74,
      position = position_dodge(width = 0.82),
      outlier.shape = NA,
      linewidth = 0.35,
      median.linewidth = 0.50,
      colour = box_outline_col
    ) +
    
    facet_wrap(
      ~ rho_Y,
      ncol = 1,
      labeller = as_labeller(label_rhoY_pretty)
    ) +
    
    scale_fill_manual(
      values = main_model_cols,
      breaks = model_order,
      drop = FALSE
    ) +
    
    scale_x_discrete(
      labels = label_rhoX_tick,
      drop = FALSE
    ) +
    
    scale_y_continuous(
      breaks = y_breaks
    ) +
    
    coord_cartesian(
      ylim = y_limits
    ) +
    
    labs(
      title = NULL,
      subtitle = condition_subtitle,
      x = NULL,
      y = y_label
    ) +
    
    appendix_theme
}

# -----------------------------
# 11. Generate appendix R2 figures
# -----------------------------

for (comp_val in levels(rep_long_r2$comp_linear)) {
  for (rhoB_val in levels(rep_long_r2$rho_betweenX)) {
    for (latent_val in levels(rep_long_r2$latent_R2)) {
      
      plot_df <- rep_long_r2 %>%
        filter(
          comp_linear == comp_val,
          rho_betweenX == rhoB_val,
          latent_R2 == latent_val
        )
      
      p <- make_appendix_plot(
        df = plot_df,
        metric = "R2",
        comp_val = comp_val,
        rhoB_val = rhoB_val,
        latent_val = latent_val
      )
      
      file_name <- paste0(
        "Appendix_R2_",
        "comp", safe_num(comp_val),
        "_rhoB", safe_num(rhoB_val),
        "_latentR2", safe_num(latent_val),
        ".png"
      )
      
      ggsave(
        filename = file.path(appendix_r2_dir, file_name),
        plot = p,
        width = 10.0,
        height = 12.0,
        units = "in",
        dpi = 400,
        bg = "white"
      )
    }
  }
}

cat("Finished appendix R2 figures.\n")
cat("Saved to:\n")
cat(appendix_r2_dir, "\n")

# -----------------------------
# 12. Generate appendix RMSE figures
# -----------------------------

for (comp_val in levels(rep_long_rmse$comp_linear)) {
  for (rhoB_val in levels(rep_long_rmse$rho_betweenX)) {
    for (latent_val in levels(rep_long_rmse$latent_R2)) {
      
      plot_df <- rep_long_rmse %>%
        filter(
          comp_linear == comp_val,
          rho_betweenX == rhoB_val,
          latent_R2 == latent_val
        )
      
      p <- make_appendix_plot(
        df = plot_df,
        metric = "RMSE",
        comp_val = comp_val,
        rhoB_val = rhoB_val,
        latent_val = latent_val
      )
      
      file_name <- paste0(
        "Appendix_RMSE_",
        "comp", safe_num(comp_val),
        "_rhoB", safe_num(rhoB_val),
        "_latentR2", safe_num(latent_val),
        ".png"
      )
      
      ggsave(
        filename = file.path(appendix_rmse_dir, file_name),
        plot = p,
        width = 10.0,
        height = 12.0,
        units = "in",
        dpi = 400,
        bg = "white"
      )
    }
  }
}

cat("Finished appendix RMSE figures.\n")
cat("Saved to:\n")
cat(appendix_rmse_dir, "\n")




source("run_config.R")

# -----------------------------
# 1. Existing figure folders
# -----------------------------

appendix_r2_dir <- file.path(run_dir, "appendix_figures_R2_clear")
appendix_rmse_dir <- file.path(run_dir, "appendix_figures_RMSE_clear")

# -----------------------------
# 2. New ordered folders
# -----------------------------

ordered_r2_dir <- file.path(run_dir, "appendix_figures_R2_ordered")
ordered_rmse_dir <- file.path(run_dir, "appendix_figures_RMSE_ordered")

if (!dir.exists(ordered_r2_dir)) {
  dir.create(ordered_r2_dir, recursive = TRUE)
}

if (!dir.exists(ordered_rmse_dir)) {
  dir.create(ordered_rmse_dir, recursive = TRUE)
}

# -----------------------------
# 3. Helper functions
# -----------------------------

p_to_num <- function(x) {
  as.numeric(gsub("p", ".", x, fixed = TRUE))
}

num_to_p <- function(x) {
  gsub("\\.", "p", sprintf("%.2f", x))
}

order_appendix_figures <- function(input_dir,
                                   output_dir,
                                   metric,
                                   appendix_prefix) {
  
  files <- list.files(
    input_dir,
    pattern = "\\.png$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    stop("No PNG files found in: ", input_dir)
  }
  
  file_base <- basename(files)
  
  pattern <- paste0(
    "^Appendix_", metric,
    "_comp([0-9]+p[0-9]+)",
    "_rhoB([0-9]+p[0-9]+)",
    "_latentR2([0-9]+p[0-9]+)",
    "\\.png$"
  )
  
  matched <- regexec(pattern, file_base, ignore.case = TRUE)
  extracted <- regmatches(file_base, matched)
  
  ok <- lengths(extracted) == 4
  
  if (any(!ok)) {
    cat("These files did not match the expected naming pattern:\n")
    print(file_base[!ok])
    stop("Fix file names or check the regex pattern.")
  }
  
  fig_index <- data.frame(
    original_file = files,
    original_name = file_base,
    comp_raw = sapply(extracted, function(x) x[2]),
    rhoB_raw = sapply(extracted, function(x) x[3]),
    latent_raw = sapply(extracted, function(x) x[4]),
    stringsAsFactors = FALSE
  )
  
  fig_index$comp_linear <- p_to_num(fig_index$comp_raw)
  fig_index$rho_betweenX <- p_to_num(fig_index$rhoB_raw)
  fig_index$latent_R2 <- p_to_num(fig_index$latent_raw)
  
  fig_index <- fig_index[order(
    fig_index$latent_R2,
    fig_index$comp_linear,
    fig_index$rho_betweenX
  ), ]
  
  fig_index$figure_number <- seq_len(nrow(fig_index))
  fig_index$figure_label <- paste0(appendix_prefix, fig_index$figure_number)
  fig_index$figure_code <- paste0(
    appendix_prefix,
    sprintf("%02d", fig_index$figure_number)
  )
  
  fig_index$new_name <- paste0(
    "Figure_",
    fig_index$figure_code,
    "_", metric,
    "_latentR2_", num_to_p(fig_index$latent_R2),
    "_comp_", num_to_p(fig_index$comp_linear),
    "_rhoB_", num_to_p(fig_index$rho_betweenX),
    ".png"
  )
  
  fig_index$new_file <- file.path(output_dir, fig_index$new_name)
  
  for (i in seq_len(nrow(fig_index))) {
    file.copy(
      from = fig_index$original_file[i],
      to = fig_index$new_file[i],
      overwrite = TRUE
    )
  }
  
  caption_index <- fig_index[, c(
    "figure_label",
    "latent_R2",
    "comp_linear",
    "rho_betweenX",
    "original_name",
    "new_name"
  )]
  
  write.csv(
    caption_index,
    file = file.path(output_dir, paste0("Appendix_", metric, "_figure_order.csv")),
    row.names = FALSE
  )
  
  print(caption_index[, c(
    "figure_label",
    "latent_R2",
    "comp_linear",
    "rho_betweenX",
    "new_name"
  )])
  
  cat("\nFinished ordering", metric, "figures.\n")
  cat("Saved ordered files to:\n")
  cat(output_dir, "\n\n")
}

# -----------------------------
# 4. Order R2 figures as Appendix B
# -----------------------------

order_appendix_figures(
  input_dir = appendix_r2_dir,
  output_dir = ordered_r2_dir,
  metric = "R2",
  appendix_prefix = "B"
)

# -----------------------------
# 5. Order RMSE figures as Appendix C
# -----------------------------

order_appendix_figures(
  input_dir = appendix_rmse_dir,
  output_dir = ordered_rmse_dir,
  metric = "RMSE",
  appendix_prefix = "C"
)









# ============================================================
# selected_16_condition_plots.R
# Purpose:
#   Create clean portrait boxplots for the 16 selected conditions
#   with legend only and no model labels under the boxes.
# ============================================================

library(ggplot2)

# -----------------------------
# 1. Check run directory
# -----------------------------

if (!exists("run_dir")) {
  stop("Object 'run_dir' not found. Run source('run_config.R') first.")
}

results_file <- file.path(run_dir, "results_replication_level.csv")

if (!file.exists(results_file)) {
  stop("Could not find: ", results_file)
}

dat <- read.csv(results_file, stringsAsFactors = FALSE)

# -----------------------------
# 2. Output folder
# -----------------------------

out_dir <- file.path(run_dir, "selected_16_condition_plots_clean")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 3. Model colours
# -----------------------------

main_model_cols <- c(
  "XGBoost"      = "#74ADD1",
  "Baseline OLS" = "#FFFFBF",
  "Aligned OLS"  = "#FEE080",
  "Oracle OLS"   = "#FDAE61"
)

box_outline_col <- "grey50"

model_order <- c(
  "XGBoost",
  "Baseline OLS",
  "Aligned OLS",
  "Oracle OLS"
)

# -----------------------------
# 4. Selected 16 conditions
# -----------------------------
# Order:
# rho_X -> rho_Y -> linear share -> latent R²

selected_conditions <- expand.grid(
  latent_R2    = c(0.20, 0.80),
  comp_linear  = c(0.20, 0.80),
  rho_Y        = c(0.60, 1.00),
  rho_X        = c(0.60, 1.00),
  rho_betweenX = 0.00
)

selected_conditions <- selected_conditions[
  , c("rho_X", "rho_Y", "comp_linear", "latent_R2", "rho_betweenX")
]

selected_conditions$selected_id <- seq_len(nrow(selected_conditions))

# -----------------------------
# 5. Helper functions
# -----------------------------

near <- function(x, value, tol = 1e-8) {
  abs(x - value) < tol
}

fmt <- function(x) {
  sprintf("%.2f", x)
}

make_subtitle <- function(cond_row) {
  paste0(
    "\u03C1X = ", fmt(cond_row$rho_X),
    "   |   \u03C1Y = ", fmt(cond_row$rho_Y),
    "   |   linear share = ", fmt(cond_row$comp_linear),
    "\nlatent R\u00B2 = ", fmt(cond_row$latent_R2),
    "   |   \u03C1 between predictors = ", fmt(cond_row$rho_betweenX)
  )
}

make_long_data <- function(d, metric = "R2") {
  
  if (metric == "R2") {
    value_dat <- rbind(
      data.frame(model = "XGBoost",      value = d$r2_xgb),
      data.frame(model = "Baseline OLS", value = d$r2_ols_base),
      data.frame(model = "Aligned OLS",  value = d$r2_ols_true_interaction),
      data.frame(model = "Oracle OLS",   value = d$r2_ols_oracle)
    )
  }
  
  if (metric == "RMSE") {
    value_dat <- rbind(
      data.frame(model = "XGBoost",      value = d$rmse_xgb),
      data.frame(model = "Baseline OLS", value = d$rmse_ols_base),
      data.frame(model = "Aligned OLS",  value = d$rmse_ols_true_interaction),
      data.frame(model = "Oracle OLS",   value = d$rmse_ols_oracle)
    )
  }
  
  value_dat$model <- factor(value_dat$model, levels = model_order)
  value_dat
}

# -----------------------------
# 6. Plot theme
# -----------------------------

theme_selected_condition <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title.position = "plot",
      
      # Main title: same size as subtitle, but bold
      plot.title = element_text(
        size = 12.5,
        face = "bold",
        hjust = 0,
        margin = margin(b = 6)
      ),
      
      # Condition text under title
      plot.subtitle = element_text(
        size = 12.5,
        hjust = 0,
        lineheight = 1.15,
        margin = margin(b = 26)
      ),
      
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      axis.title.y = element_text(
        size = 15,
        margin = margin(r = 12)
      ),
      axis.text.y = element_text(size = 13, colour = "grey30"),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.5),
      
      # Smaller legend so it does not get cut off
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9.2),
      legend.key.width = unit(0.65, "cm"),
      legend.key.height = unit(0.38, "cm"),
      legend.spacing.x = unit(0.12, "cm"),
      legend.margin = margin(t = 4, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
      
      plot.margin = margin(t = 18, r = 22, b = 18, l = 18)
    )
}
# -----------------------------
# 7. Choose metric
# -----------------------------

metrics_to_plot <- c("R2")
# Use this instead if you also need RMSE:
# metrics_to_plot <- c("R2", "RMSE")

# -----------------------------
# 8. Create plots
# -----------------------------

for (m in metrics_to_plot) {
  
  for (i in seq_len(nrow(selected_conditions))) {
    
    cond <- selected_conditions[i, ]
    
    d_cond <- dat[
      near(dat$rho_X, cond$rho_X) &
        near(dat$rho_Y, cond$rho_Y) &
        near(dat$comp_linear, cond$comp_linear) &
        near(dat$latent_R2, cond$latent_R2) &
        near(dat$rho_betweenX, cond$rho_betweenX),
    ]
    
    if (nrow(d_cond) == 0) {
      warning("No data found for selected condition ", cond$selected_id)
      next
    }
    
    plot_dat <- make_long_data(d_cond, metric = m)
    
    y_lab <- ifelse(m == "R2", "Test-set R\u00B2", "Test-set RMSE")
    title_lab <- ifelse(m == "R2", "Test-set R\u00B2", "Test-set RMSE")
    
    p <- ggplot(plot_dat, aes(x = model, y = value, fill = model)) +
      geom_hline(
        yintercept = ifelse(m == "R2", 0, NA),
        colour = "grey55",
        linewidth = 0.45,
        na.rm = TRUE
      ) +
      stat_boxplot(
        geom = "errorbar",
        width = 0.18,
        colour = box_outline_col,
        linewidth = 0.55
      ) +
      geom_boxplot(
        width = 0.55,
        colour = box_outline_col,
        linewidth = 0.55,
        outlier.shape = NA
      ) +
      scale_fill_manual(values = main_model_cols) +
      labs(
        title = paste0(
          "Selected condition ",
          sprintf("%02d", cond$selected_id),
          ": ",
          title_lab
        ),
        subtitle = make_subtitle(cond),
        x = NULL,
        y = y_lab
      ) +
      theme_selected_condition() +
      guides(
        fill = guide_legend(
          nrow = 1,
          byrow = TRUE,
          override.aes = list(linewidth = 0.40)
        )
      )
    
    if (m == "R2") {
      p <- p +
        coord_cartesian(ylim = c(-0.10, 0.90)) +
        scale_y_continuous(
          breaks = c(0.00, 0.25, 0.50, 0.75)
        )
    }
    
    if (m == "RMSE") {
      p <- p +
        coord_cartesian(ylim = c(0.50, 3.50))
    }
    
    file_name <- paste0(
      "Selected_condition_",
      sprintf("%02d", cond$selected_id),
      "_",
      m,
      ".png"
    )
    
    ggsave(
      filename = file.path(out_dir, file_name),
      plot = p,
      width = 6.6,
      height = 7.9,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }
}

cat("Finished selected 16 condition plots.\n")
cat("Saved to:\n")
cat(out_dir, "\n")









# ============================================================
# 09_heatmaps_dominance.R
# Purpose:
#   Overview heatmaps showing dominance in win frequency:
#
#     dominance = P(XGB wins) - P(comparator wins)
#
#   Interpretation:
#     dominance < 0  : comparator wins more often
#     dominance = 0  : equal win frequency
#     dominance > 0  : XGB wins more often
#
#   Layout:
#     separate file per rho_betweenX
#     facet rows = comp_linear
#     facet cols = latent_R2
#     x-axis     = rho_X
#     y-axis     = rho_Y
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")
source("plot_helpers.R")

library(ggplot2)
library(dplyr)
library(scales)
library(grid)

# -----------------------------
# 1. Load data
# -----------------------------
results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

plot_dir <- file.path(run_dir, "heatmaps_dominance")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 2. Heatmap colors
#    Stronger than boxplot colors on purpose
# -----------------------------
baseline_heat_col <- "#3B6FB6"
oracle_heat_col   <- "#B22222"
xgb_heat_col      <- "#2E8B57"
mid_heat_col      <- "white"

# -----------------------------
# 3. Factor levels
# -----------------------------
latent_levels <- sort(unique(results_df$latent_R2))
rhoX_levels   <- sort(unique(results_df$rho_X))
rhoY_levels   <- sort(unique(results_df$rho_Y))
comp_levels   <- sort(unique(results_df$comp_linear))
rhoB_levels   <- sort(unique(results_df$rho_betweenX))

# -----------------------------
# 4. Explicit win flags
# -----------------------------
tol <- 1e-12

results_flagged <- results_df %>%
  mutate(
    # XGB vs Baseline
    xgb_win_r2_base   = delta_r2_xgb_vs_base   >  tol,
    base_win_r2       = delta_r2_xgb_vs_base   < -tol,
    
    xgb_win_rmse_base = delta_rmse_xgb_vs_base < -tol,
    base_win_rmse     = delta_rmse_xgb_vs_base >  tol,
    
    # XGB vs Oracle
    xgb_win_r2_oracle   = delta_r2_xgb_vs_oracle   >  tol,
    oracle_win_r2       = delta_r2_xgb_vs_oracle   < -tol,
    
    xgb_win_rmse_oracle = delta_rmse_xgb_vs_oracle < -tol,
    oracle_win_rmse     = delta_rmse_xgb_vs_oracle >  tol
  )

# -----------------------------
# 5. Aggregate to win proportions
# -----------------------------
dom_df <- results_flagged %>%
  group_by(latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX) %>%
  summarise(
    p_xgb_r2_base     = mean(xgb_win_r2_base,     na.rm = TRUE),
    p_xgb_rmse_base   = mean(xgb_win_rmse_base,   na.rm = TRUE),
    p_xgb_r2_oracle   = mean(xgb_win_r2_oracle,   na.rm = TRUE),
    p_xgb_rmse_oracle = mean(xgb_win_rmse_oracle, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Symmetric dominance:
    # -1 = comparator always wins
    #  0 = equal win frequency
    # +1 = XGB always wins
    dom_r2_base     = 2 * p_xgb_r2_base     - 1,
    dom_rmse_base   = 2 * p_xgb_rmse_base   - 1,
    dom_r2_oracle   = 2 * p_xgb_r2_oracle   - 1,
    dom_rmse_oracle = 2 * p_xgb_rmse_oracle - 1,
    
    latent_R2    = factor(latent_R2, levels = latent_levels),
    rho_X        = factor(rho_X, levels = rhoX_levels),
    rho_Y        = factor(rho_Y, levels = rhoY_levels),
    comp_linear  = factor(comp_linear, levels = comp_levels),
    rho_betweenX = factor(rho_betweenX, levels = rhoB_levels)
  )

# -----------------------------
# 6. Reshape long
# -----------------------------
dom_long <- bind_rows(
  dom_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "R2",
      contrast = "XGB - Baseline",
      dominance = dom_r2_base
    ),
  dom_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "R2",
      contrast = "XGB - Oracle",
      dominance = dom_r2_oracle
    ),
  dom_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "RMSE",
      contrast = "XGB - Baseline",
      dominance = dom_rmse_base
    ),
  dom_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "RMSE",
      contrast = "XGB - Oracle",
      dominance = dom_rmse_oracle
    )
) %>%
  mutate(
    metric = factor(metric, levels = c("R2", "RMSE")),
    contrast = factor(contrast, levels = c("XGB - Baseline", "XGB - Oracle"))
  )

# -----------------------------
# 7. Theme
# -----------------------------
theme_heatmap_overview <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.spacing = unit(0.8, "lines"),
      strip.text = element_text(face = "bold", size = base_size * 0.9),
      legend.position = "bottom",
      legend.title = element_text(size = base_size * 0.9),
      legend.text = element_text(size = base_size * 0.8),
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size * 1.05),
      plot.subtitle = element_text(hjust = 0.5, size = base_size * 0.9),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

# -----------------------------
# 8. Comparator-specific scale
#    FIXED meaning everywhere:
#      negative = comparator dominates
#      zero     = equal
#      positive = XGB dominates
# -----------------------------
make_dominance_scale <- function(contrast_name) {
  
  if (contrast_name == "XGB - Baseline") {
    
    scale_fill_gradient2(
      low = baseline_heat_col,
      mid = mid_heat_col,
      high = xgb_heat_col,
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("Baseline dominates", "Equal", "XGB dominates")
    )
    
  } else if (contrast_name == "XGB - Oracle") {
    
    scale_fill_gradient2(
      low = oracle_heat_col,
      mid = mid_heat_col,
      high = xgb_heat_col,
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("Oracle dominates", "Equal", "XGB dominates")
    )
    
  } else {
    
    stop("Unknown contrast_name: ", contrast_name)
  }
}

# -----------------------------
# 9. Plot function
# -----------------------------
plot_dominance_heatmap_overview <- function(df_sub,
                                            metric_name,
                                            contrast_name,
                                            rhoB_value,
                                            file_name,
                                            title_text) {
  
  plot_data <- df_sub %>%
    filter(metric == metric_name, contrast == contrast_name)
  
  p <- ggplot(
    plot_data,
    aes(x = rho_X, y = rho_Y, fill = dominance)
  ) +
    geom_tile(color = "white", linewidth = 0.35) +
    geom_text(
      aes(label = number(dominance, accuracy = 0.01)),
      size = 3.1,
      colour = "#303030"
    ) +
    facet_grid(
      rows = vars(comp_linear),
      cols = vars(latent_R2),
      labeller = labeller(
        comp_linear = label_comp,
        latent_R2 = label_latentR2
      )
    ) +
    scale_x_discrete(labels = label_rhoX_axis) +
    scale_y_discrete(labels = label_rhoY) +
    make_dominance_scale(contrast_name) +
    labs(
      title = title_text,
      subtitle = label_rhoB(rhoB_value),
      x = expression(rho[X]),
      y = expression(rho[Y]),
      fill = "Dominance"
    ) +
    theme_heatmap_overview()
  
  ggsave(
    filename = file.path(plot_dir, file_name),
    plot = p,
    width = 13.5,
    height = 10.5,
    dpi = 320
  )
}

# -----------------------------
# 10. Loop over rho_betweenX
# -----------------------------
for (rb in rhoB_levels) {
  
  sub <- dom_long %>%
    filter(rho_betweenX == rb)
  
  plot_dominance_heatmap_overview(
    df_sub = sub,
    metric_name = "R2",
    contrast_name = "XGB - Baseline",
    rhoB_value = rb,
    file_name = paste0("overview_dominance_R2_vs_base_rhoB", rb, ".png"),
    title_text = "Win-frequency dominance: XGBoost vs Baseline OLS (R²)"
  )
  
  plot_dominance_heatmap_overview(
    df_sub = sub,
    metric_name = "R2",
    contrast_name = "XGB - Oracle",
    rhoB_value = rb,
    file_name = paste0("overview_dominance_R2_vs_oracle_rhoB", rb, ".png"),
    title_text = "Win-frequency dominance: XGBoost vs Oracle OLS (R²)"
  )
  
  plot_dominance_heatmap_overview(
    df_sub = sub,
    metric_name = "RMSE",
    contrast_name = "XGB - Baseline",
    rhoB_value = rb,
    file_name = paste0("overview_dominance_RMSE_vs_base_rhoB", rb, ".png"),
    title_text = "Win-frequency dominance: XGBoost vs Baseline OLS (RMSE)"
  )
  
  plot_dominance_heatmap_overview(
    df_sub = sub,
    metric_name = "RMSE",
    contrast_name = "XGB - Oracle",
    rhoB_value = rb,
    file_name = paste0("overview_dominance_RMSE_vs_oracle_rhoB", rb, ".png"),
    title_text = "Win-frequency dominance: XGBoost vs Oracle OLS (RMSE)"
  )
}

cat("Finished. Heatmaps saved to:\n")
cat(plot_dir, "\n")

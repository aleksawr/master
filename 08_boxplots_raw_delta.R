# ============================================================
# 08_boxplots_raw_delta.R
# Purpose:
#   Clean raw and delta boxplots for replication-level results
#   Layout:
#     rows    = rho_Y
#     columns = latent_R2
#     x-axis  = rho_X
#     separate file per comp_linear x rho_betweenX
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")
source("plot_helpers.R")

# -----------------------------
# 1. Load data
# -----------------------------
results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

plot_dir <- file.path(run_dir, "boxplots_raw_delta")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 2. Factor levels
# -----------------------------
latent_levels <- sort(unique(results_df$latent_R2))
rhoX_levels   <- sort(unique(results_df$rho_X))
rhoY_levels   <- sort(unique(results_df$rho_Y))
comp_levels   <- sort(unique(results_df$comp_linear))
rhoB_levels   <- sort(unique(results_df$rho_betweenX))

# -----------------------------
# 3. Reshape raw metrics
# -----------------------------
raw_long <- bind_rows(
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "R2",
      model = "Baseline OLS",
      value = r2_ols_base
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "R2",
      model = "Oracle OLS",
      value = r2_ols_oracle
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "R2",
      model = "XGBoost",
      value = r2_xgb
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "RMSE",
      model = "Baseline OLS",
      value = rmse_ols_base
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "RMSE",
      model = "Oracle OLS",
      value = rmse_ols_oracle
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "RMSE",
      model = "XGBoost",
      value = rmse_xgb
    )
) %>%
  mutate(
    model = factor(model, levels = c("Baseline OLS", "Oracle OLS", "XGBoost")),
    rho_X = factor(rho_X, levels = rhoX_levels),
    rho_Y = factor(rho_Y, levels = rhoY_levels),
    latent_R2 = factor(latent_R2, levels = latent_levels),
    comp_linear = factor(comp_linear, levels = comp_levels),
    rho_betweenX = factor(rho_betweenX, levels = rhoB_levels)
  )

# -----------------------------
# 4. Reshape delta metrics
# -----------------------------
delta_long <- bind_rows(
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "delta_R2",
      contrast = "XGB - Baseline",
      value = delta_r2_xgb_vs_base
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "delta_R2",
      contrast = "XGB - Oracle",
      value = delta_r2_xgb_vs_oracle
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "delta_RMSE",
      contrast = "XGB - Baseline",
      value = delta_rmse_xgb_vs_base
    ),
  results_df %>%
    transmute(
      latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      metric = "delta_RMSE",
      contrast = "XGB - Oracle",
      value = delta_rmse_xgb_vs_oracle
    )
) %>%
  mutate(
    contrast = factor(contrast, levels = c("XGB - Baseline", "XGB - Oracle")),
    rho_X = factor(rho_X, levels = rhoX_levels),
    rho_Y = factor(rho_Y, levels = rhoY_levels),
    latent_R2 = factor(latent_R2, levels = latent_levels),
    comp_linear = factor(comp_linear, levels = comp_levels),
    rho_betweenX = factor(rho_betweenX, levels = rhoB_levels)
  )

# -----------------------------
# 5. Manual y-limits
# -----------------------------
ylims <- list(
  R2         = c(-0.10, 0.90),
  RMSE       = c(0.50, 3.50),
  delta_R2   = c(-0.40, 0.70),
  delta_RMSE = c(-0.50, 0.35)
)

# -----------------------------
# 6. Plot functions
# -----------------------------
plot_raw_metric <- function(df_sub, metric_name, comp_value, rhoB_value,
                            y_limits, file_name, ylab_text) {
  
  p <- ggplot(
    df_sub %>% filter(metric == metric_name),
    aes(x = rho_X, y = value, fill = model)
  ) +
    geom_boxplot(
      width = 0.70,
      position = position_dodge(width = 0.78),
      outlier.shape = NA,
      linewidth = 0.30,
      staplewidth = 0.30,
      median.linewidth = 0.30,
      colour = box_outline_col
    ) +
    facet_grid(
      rows = vars(rho_Y),
      cols = vars(latent_R2),
      labeller = labeller(
        rho_Y = label_rhoY,
        latent_R2 = label_latentR2
      )
    ) +
    scale_fill_manual(values = raw_model_cols, drop = FALSE) +
    scale_x_discrete(labels = label_rhoX_axis) +
    coord_cartesian(ylim = y_limits) +
    labs(
      title = ylab_text,
      subtitle = make_fixed_subtitle(comp_value, rhoB_value),
      x = expression(rho[X]),
      y = ylab_text
    ) +
    theme_thesis()
  
  ggsave(
    filename = file.path(plot_dir, file_name),
    plot = p,
    width = 13.5,
    height = 10.5,
    dpi = 320
  )
}

plot_delta_metric <- function(df_sub, metric_name, comp_value, rhoB_value,
                              y_limits, file_name, ylab_text) {
  
  p <- ggplot(
    df_sub %>% filter(metric == metric_name),
    aes(x = rho_X, y = value, fill = contrast)
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.4
    ) +
    geom_boxplot(
      width = 0.66,
      position = position_dodge(width = 0.72),
      outlier.shape = NA,
      linewidth = 0.30,
      staplewidth = 0.30,
      median.linewidth = 0.30,
      colour = box_outline_col
    ) +
    facet_grid(
      rows = vars(rho_Y),
      cols = vars(latent_R2),
      labeller = labeller(
        rho_Y = label_rhoY,
        latent_R2 = label_latentR2
      )
    ) +
    scale_fill_manual(values = delta_cols, drop = FALSE) +
    scale_x_discrete(labels = label_rhoX_axis) +
    coord_cartesian(ylim = y_limits) +
    labs(
      title = ylab_text,
      subtitle = make_fixed_subtitle(comp_value, rhoB_value),
      x = expression(rho[X]),
      y = ylab_text
    ) +
    theme_thesis()
  
  ggsave(
    filename = file.path(plot_dir, file_name),
    plot = p,
    width = 13.5,
    height = 10.5,
    dpi = 320
  )
}

# -----------------------------
# 7. Loop over comp_linear x rho_betweenX
# -----------------------------
for (cl in comp_levels) {
  for (rb in rhoB_levels) {
    
    raw_sub <- raw_long %>%
      filter(comp_linear == cl, rho_betweenX == rb)
    
    delta_sub <- delta_long %>%
      filter(comp_linear == cl, rho_betweenX == rb)
    
    # Raw R2
    plot_raw_metric(
      df_sub = raw_sub,
      metric_name = "R2",
      comp_value = cl,
      rhoB_value = rb,
      y_limits = ylims$R2,
      file_name = paste0("raw_R2_comp", cl, "_rhoB", rb, ".png"),
      ylab_text = "Test-set R²"
    )
    
    # Raw RMSE
    plot_raw_metric(
      df_sub = raw_sub,
      metric_name = "RMSE",
      comp_value = cl,
      rhoB_value = rb,
      y_limits = ylims$RMSE,
      file_name = paste0("raw_RMSE_comp", cl, "_rhoB", rb, ".png"),
      ylab_text = "Test-set RMSE"
    )
    
    # Delta R2
    plot_delta_metric(
      df_sub = delta_sub,
      metric_name = "delta_R2",
      comp_value = cl,
      rhoB_value = rb,
      y_limits = ylims$delta_R2,
      file_name = paste0("delta_R2_comp", cl, "_rhoB", rb, ".png"),
      ylab_text = expression(Delta * "R²")
    )
    
    # Delta RMSE
    plot_delta_metric(
      df_sub = delta_sub,
      metric_name = "delta_RMSE",
      comp_value = cl,
      rhoB_value = rb,
      y_limits = ylims$delta_RMSE,
      file_name = paste0("delta_RMSE_comp", cl, "_rhoB", rb, ".png"),
      ylab_text = expression(Delta * "RMSE")
    )
  }
}

cat("Finished. Boxplots saved to:\n")
cat(plot_dir, "\n")

# ============================================================
# 09_plots.R
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
      model = "Aligned OLS",
      value = r2_ols_true_interaction
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
      model = "Aligned OLS",
      value = rmse_ols_true_interaction
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
    model = factor(
      model,
      levels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost")
    ),
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
      contrast = "XGB - Aligned",
      value = delta_r2_xgb_vs_true
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
      contrast = "XGB - Aligned",
      value = delta_rmse_xgb_vs_true
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
    contrast = factor(
      contrast,
      levels = c("XGB - Baseline", "XGB - Aligned", "XGB - Oracle")
    ),
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
      ylab_text = "Test-set R^2"
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
      ylab_text = "Delta R^2"
    )
    
    # Delta RMSE
    plot_delta_metric(
      df_sub = delta_sub,
      metric_name = "delta_RMSE",
      comp_value = cl,
      rhoB_value = rb,
      y_limits = ylims$delta_RMSE,
      file_name = paste0("delta_RMSE_comp", cl, "_rhoB", rb, ".png"),
      ylab_text = "Delta RMSE"
    )
  }
}

cat("Finished. Boxplots saved to:\n")
cat(plot_dir, "\n")







# ============================================================
# 8. Add-on: selected-condition plots
# Purpose:
#   Create a small set of main-text plots from the manually
#   selected illustrative conditions
# ============================================================

agg_df <- read.csv(file.path(run_dir, "results_condition_summary.csv"))

selected_file <- file.path(run_dir, "selected_conditions", "main_text_selected_conditions.csv")

if (file.exists(selected_file)) {
  
  main_conditions <- read.csv(selected_file)
  
  pub_plot_dir <- file.path(run_dir, "publication_plots")
  dir.create(pub_plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  # -----------------------------
  # A. Replication-level R2 boxplots for selected conditions
  # -----------------------------
  selected_replications <- results_df %>%
    inner_join(
      main_conditions,
      by = c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
    )
  
  selected_raw_r2 <- bind_rows(
    selected_replications %>%
      transmute(label, model = "Baseline OLS", value = r2_ols_base),
    selected_replications %>%
      transmute(label, model = "Aligned OLS", value = r2_ols_true_interaction),
    selected_replications %>%
      transmute(label, model = "Oracle OLS", value = r2_ols_oracle),
    selected_replications %>%
      transmute(label, model = "XGBoost", value = r2_xgb)
  ) %>%
    mutate(
      model = factor(model,
                     levels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost")),
      label = factor(label, levels = main_conditions$label)
    )
  
  p_selected_box <- ggplot(
    selected_raw_r2,
    aes(x = model, y = value, fill = model)
  ) +
    geom_boxplot(
      width = 0.70,
      outlier.shape = NA,
      linewidth = 0.30,
      colour = box_outline_col
    ) +
    facet_wrap(~ label, nrow = 1) +
    scale_fill_manual(values = raw_model_cols, drop = FALSE) +
    coord_cartesian(ylim = ylims$R2) +
    labs(
      title = "Illustrative selected conditions: replication-level test-set R^2",
      x = NULL,
      y = "Test-set R^2"
    ) +
    theme_thesis() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "none"
    )
  
  ggsave(
    filename = file.path(pub_plot_dir, "selected_conditions_boxplot_R2.png"),
    plot = p_selected_box,
    width = 12.5,
    height = 4.8,
    dpi = 320
  )
  
  # -----------------------------
  # B. Mean R2 line plot for selected conditions
  # -----------------------------
  selected_means <- agg_df %>%
    inner_join(
      main_conditions,
      by = c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
    ) %>%
    transmute(
      label,
      `Baseline OLS` = mean_r2_ols_base,
      `Aligned OLS`  = mean_r2_ols_true_interaction,
      `Oracle OLS`   = mean_r2_ols_oracle,
      `XGBoost`      = mean_r2_xgb
    ) %>%
    tidyr::pivot_longer(
      cols = c(`Baseline OLS`, `Aligned OLS`, `Oracle OLS`, `XGBoost`),
      names_to = "model",
      values_to = "mean_r2"
    ) %>%
    mutate(
      model = factor(model,
                     levels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost")),
      label = factor(label, levels = main_conditions$label)
    )
  
  p_selected_mean <- ggplot(
    selected_means,
    aes(x = model, y = mean_r2, group = 1)
  ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.8) +
    facet_wrap(~ label, nrow = 1) +
    coord_cartesian(ylim = ylims$R2) +
    labs(
      title = "Illustrative selected conditions: mean test-set R^2",
      x = NULL,
      y = "Mean test-set R^2"
    ) +
    theme_thesis() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      legend.position = "none"
    )
  
  ggsave(
    filename = file.path(pub_plot_dir, "selected_conditions_mean_R2.png"),
    plot = p_selected_mean,
    width = 12.5,
    height = 4.8,
    dpi = 320
  )
  
  cat("\nPublication selected-condition plots saved to:\n")
  cat(pub_plot_dir, "\n")
  
} else {
  cat("\nSkipping selected-condition publication plots.\n")
  cat("File not found:\n")
  cat(selected_file, "\n")
}








##############################################################


# ============================================================
# 8. Add-on: main-text selected-condition boxplots only
# Purpose:
#   Create only the small set of publication-ready R2 boxplots
#   for manually selected main-text conditions
# ============================================================

selected_file <- file.path(run_dir, "selected_conditions", "main_text_selected_conditions.csv")

if (file.exists(selected_file)) {
  
  pub_plot_dir <- file.path(run_dir, "publication_plots")
  dir.create(pub_plot_dir, showWarnings = FALSE, recursive = TRUE)
  
  main_conditions <- read.csv(selected_file, stringsAsFactors = FALSE)
  
  # -----------------------------
  # A. Clean types
  # -----------------------------
  cond_cols <- c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
  
  main_conditions[cond_cols] <- lapply(main_conditions[cond_cols], as.numeric)
  results_df[cond_cols]      <- lapply(results_df[cond_cols], as.numeric)
  
  main_conditions$panel_order <- as.integer(main_conditions$panel_order)
  main_conditions$facet_nrow  <- as.integer(main_conditions$facet_nrow)
  
  # -----------------------------
  # B. Join selected conditions to replication-level data
  # -----------------------------
  selected_replications <- results_df %>%
    inner_join(
      main_conditions,
      by = c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
    )
  
  # -----------------------------
  # C. Reshape to long format for R2 only
  # -----------------------------
  selected_raw_r2 <- bind_rows(
    selected_replications %>%
      transmute(
        figure_id, figure_title, panel_order, panel_label, facet_nrow,
        model = "Baseline OLS",
        value = r2_ols_base
      ),
    selected_replications %>%
      transmute(
        figure_id, figure_title, panel_order, panel_label, facet_nrow,
        model = "Aligned OLS",
        value = r2_ols_true_interaction
      ),
    selected_replications %>%
      transmute(
        figure_id, figure_title, panel_order, panel_label, facet_nrow,
        model = "Oracle OLS",
        value = r2_ols_oracle
      ),
    selected_replications %>%
      transmute(
        figure_id, figure_title, panel_order, panel_label, facet_nrow,
        model = "XGBoost",
        value = r2_xgb
      )
  ) %>%
    mutate(
      model = factor(
        model,
        levels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost")
      )
    )
  
  # -----------------------------
  # D. Plot helper
  # -----------------------------
  make_selected_boxplot <- function(df_sub, out_file) {
    
    panel_levels <- df_sub %>%
      distinct(panel_order, panel_label) %>%
      arrange(panel_order) %>%
      pull(panel_label)
    
    df_sub <- df_sub %>%
      mutate(panel_label = factor(panel_label, levels = panel_levels))
    
    figure_title <- unique(df_sub$figure_title)
    facet_nrow   <- unique(df_sub$facet_nrow)
    
    if (length(figure_title) != 1) stop("Each figure_id must have exactly one figure_title.")
    if (length(facet_nrow) != 1)   stop("Each figure_id must have exactly one facet_nrow.")
    
    p <- ggplot(
      df_sub,
      aes(x = model, y = value, fill = model)
    ) +
      geom_boxplot(
        width = 0.70,
        outlier.shape = NA,
        linewidth = 0.30,
        colour = box_outline_col
      ) +
      facet_wrap(~ panel_label, nrow = facet_nrow) +
      scale_fill_manual(values = raw_model_cols, drop = FALSE) +
      coord_cartesian(ylim = ylims$R2) +
      labs(
        title = figure_title,
        x = NULL,
        y = "Test-set R^2"
      ) +
      theme_thesis() +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none"
      )
    
    ggsave(
      filename = file.path(pub_plot_dir, out_file),
      plot = p,
      width = 12.5,
      height = ifelse(facet_nrow == 2, 7.8, 4.8),
      dpi = 320
    )
  }
  
  # -----------------------------
  # E. Make one file per figure_id
  # -----------------------------
  figure_ids <- unique(selected_raw_r2$figure_id)
  
  for (fid in figure_ids) {
    
    df_fig <- selected_raw_r2 %>%
      filter(figure_id == fid)
    
    make_selected_boxplot(
      df_sub = df_fig,
      out_file = paste0("main_text_", fid, "_R2_boxplot.png")
    )
  }
  
  cat("\nSelected main-text R2 boxplots saved to:\n")
  cat(pub_plot_dir, "\n")
  
} else {
  cat("\nSkipping selected-condition publication plots.\n")
  cat("File not found:\n")
  cat(selected_file, "\n")
}
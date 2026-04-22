# ============================================================
# 9. Add-on: collapsed summary plots for main-text interpretation
# Purpose:
#   Summarize raw model performance across selected dimensions
#   without just visually cropping figures.
#
# Logic:
#   1) Compute mean performance within each full design cell
#      (condition x model), based on replication-level results.
#   2) Collapse across selected factors by averaging those
#      cell means with equal weight per design cell.
# ============================================================

collapsed_dir <- file.path(run_dir, "plots_collapsed")
dir.create(collapsed_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# A. Build long raw data from replication-level results
# -----------------------------
collapsed_raw_long <- bind_rows(
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
# B. Compute full-condition cell means
#    (one mean per condition x model)
# -----------------------------
cell_means <- collapsed_raw_long %>%
  group_by(latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX, metric, model) %>%
  summarise(cell_mean = mean(value, na.rm = TRUE), .groups = "drop")

# -----------------------------
# C. Helper: collapse across omitted design factors
# -----------------------------
collapse_cells <- function(cell_df, keep_cols) {
  cell_df %>%
    group_by(across(all_of(c(keep_cols, "model")))) %>%
    summarise(
      collapsed_mean = mean(cell_mean, na.rm = TRUE),
      n_cells = n(),
      .groups = "drop"
    )
}

# -----------------------------
# D. Helper: label mapper
# -----------------------------
make_panel_labels <- function(x, var_name) {
  x_chr <- as.character(x)
  
  if (var_name == "rho_Y") {
    return(label_rhoY(x_chr))
  }
  if (var_name == "latent_R2") {
    return(label_latentR2(x_chr))
  }
  if (var_name == "comp_linear") {
    return(label_comp(x_chr))
  }
  if (var_name == "rho_betweenX") {
    return(label_rhoB(x_chr))
  }
  if (var_name == "rho_X") {
    return(label_rhoX_axis(x_chr))
  }
  
  x_chr
}

# -----------------------------
# E. Plot helper
# -----------------------------
plot_collapsed_panels <- function(sum_df,
                                  x_col,
                                  row_col,
                                  col_col,
                                  y_lab,
                                  main_title,
                                  out_file,
                                  y_limits = NULL) {
  
  row_levels <- levels(sum_df[[row_col]])
  if (is.null(row_levels)) row_levels <- sort(unique(sum_df[[row_col]]))
  
  col_levels <- levels(sum_df[[col_col]])
  if (is.null(col_levels)) col_levels <- sort(unique(sum_df[[col_col]]))
  
  sum_df <- sum_df %>%
    mutate(
      panel_row = factor(
        make_panel_labels(.data[[row_col]], row_col),
        levels = make_panel_labels(row_levels, row_col)
      ),
      panel_col = factor(
        make_panel_labels(.data[[col_col]], col_col),
        levels = make_panel_labels(col_levels, col_col)
      )
    )
  
  p <- ggplot(
    sum_df,
    aes(
      x = .data[[x_col]],
      y = collapsed_mean,
      colour = model,
      group = model
    )
  ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.2) +
    facet_grid(rows = vars(panel_row), cols = vars(panel_col)) +
    scale_colour_manual(values = raw_model_cols, drop = FALSE) +
    scale_x_discrete(labels = label_rhoX_axis) +
    labs(
      title = main_title,
      x = expression(rho[X]),
      y = y_lab,
      colour = NULL
    ) +
    theme_thesis() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  ggsave(
    filename = file.path(collapsed_dir, out_file),
    plot = p,
    width = 13.5,
    height = 10.5,
    dpi = 320
  )
}

# ============================================================
# F. Collapse set A
# Collapse over latent_R2 and rho_betweenX
# Keep: rho_X, rho_Y, comp_linear
# ============================================================

cell_R2 <- cell_means %>% filter(metric == "R2")
cell_RMSE <- cell_means %>% filter(metric == "RMSE")

keep_A <- c("rho_X", "rho_Y", "comp_linear")

sum_R2_A <- collapse_cells(cell_R2, keep_cols = keep_A)
sum_RMSE_A <- collapse_cells(cell_RMSE, keep_cols = keep_A)

plot_collapsed_panels(
  sum_df = sum_R2_A,
  x_col = "rho_X",
  row_col = "rho_Y",
  col_col = "comp_linear",
  y_lab = "Collapsed mean test-set R^2",
  main_title = "R^2 collapsed over latent_R2 and rho_betweenX",
  out_file = "collapsed_R2_keep_rhoX_rhoY_comp.png",
  y_limits = ylims$R2
)

plot_collapsed_panels(
  sum_df = sum_RMSE_A,
  x_col = "rho_X",
  row_col = "rho_Y",
  col_col = "comp_linear",
  y_lab = "Collapsed mean test-set RMSE",
  main_title = "RMSE collapsed over latent_R2 and rho_betweenX",
  out_file = "collapsed_RMSE_keep_rhoX_rhoY_comp.png",
  y_limits = ylims$RMSE
)

write.csv(
  sum_R2_A,
  file.path(collapsed_dir, "collapsed_table_R2_keep_rhoX_rhoY_comp.csv"),
  row.names = FALSE
)

write.csv(
  sum_RMSE_A,
  file.path(collapsed_dir, "collapsed_table_RMSE_keep_rhoX_rhoY_comp.csv"),
  row.names = FALSE
)

# ============================================================
# G. Collapse set B
# Collapse over comp_linear and rho_betweenX
# Keep: rho_X, rho_Y, latent_R2
# ============================================================

keep_B <- c("rho_X", "rho_Y", "latent_R2")

sum_R2_B <- collapse_cells(cell_R2, keep_cols = keep_B)
sum_RMSE_B <- collapse_cells(cell_RMSE, keep_cols = keep_B)

plot_collapsed_panels(
  sum_df = sum_R2_B,
  x_col = "rho_X",
  row_col = "rho_Y",
  col_col = "latent_R2",
  y_lab = "Collapsed mean test-set R^2",
  main_title = "R^2 collapsed over comp_linear and rho_betweenX",
  out_file = "collapsed_R2_keep_rhoX_rhoY_latentR2.png",
  y_limits = ylims$R2
)

plot_collapsed_panels(
  sum_df = sum_RMSE_B,
  x_col = "rho_X",
  row_col = "rho_Y",
  col_col = "latent_R2",
  y_lab = "Collapsed mean test-set RMSE",
  main_title = "RMSE collapsed over comp_linear and rho_betweenX",
  out_file = "collapsed_RMSE_keep_rhoX_rhoY_latentR2.png",
  y_limits = ylims$RMSE
)

write.csv(
  sum_R2_B,
  file.path(collapsed_dir, "collapsed_table_R2_keep_rhoX_rhoY_latentR2.csv"),
  row.names = FALSE
)

write.csv(
  sum_RMSE_B,
  file.path(collapsed_dir, "collapsed_table_RMSE_keep_rhoX_rhoY_latentR2.csv"),
  row.names = FALSE
)

cat("\nCollapsed summary plots saved to:\n")
cat(collapsed_dir, "\n")







# ============================================================
# Add-on: selected raw R2 boxplots from chosen conditions
# Purpose:
#   Show only a few specific conditions as boxplots
#   using the replication-level data you already use.
# ============================================================

pub_plot_dir <- file.path(run_dir, "publication_plots")
dir.create(pub_plot_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1. Manually choose conditions
# -----------------------------
selected_conditions <- data.frame(
  label = c(
    "A: low reliability, interaction-heavy",
    "B: high reliability, interaction-heavy",
    "C: high reliability, linear-dominant"
  ),
  latent_R2 = c(0.50, 0.50, 0.50),
  rho_X = c(0.60, 1.00, 1.00),
  rho_Y = c(0.60, 1.00, 1.00),
  comp_linear = c(0.20, 0.20, 0.80),
  rho_betweenX = c(0.00, 0.00, 0.00),
  stringsAsFactors = FALSE
)

# -----------------------------
# 2. Make sure join columns match type
# -----------------------------
cond_cols <- c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")

selected_conditions[cond_cols] <- lapply(selected_conditions[cond_cols], as.numeric)
results_df[cond_cols] <- lapply(results_df[cond_cols], as.numeric)

# -----------------------------
# 3. Join chosen conditions to replication-level data
# -----------------------------
selected_replications <- results_df %>%
  inner_join(
    selected_conditions,
    by = c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
  )

# -----------------------------
# 4. Reshape to long format for raw R2
# -----------------------------
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
    model = factor(
      model,
      levels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost")
    ),
    label = factor(label, levels = selected_conditions$label)
  )

# -----------------------------
# 5. Plot selected-condition boxplots
# -----------------------------
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
    title = "Selected conditions: replication-level test-set R^2",
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

cat("\nSelected boxplot saved to:\n")
cat(file.path(pub_plot_dir, "selected_conditions_boxplot_R2.png"), "\n")
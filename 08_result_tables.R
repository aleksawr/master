# ============================================================
# 08_result_tables.R
# Purpose:
#   Create compact result tables for writing and appendix
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))
agg_df <- read.csv(file.path(run_dir, "results_condition_summary.csv"))

table_dir <- file.path(run_dir, "result_tables")
dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 1. Implementation check tables
# -----------------------------
tab_latentR2 <- aggregate(mean_realized_latent_R2 ~ latent_R2, data = agg_df, mean)
tab_rhoX     <- aggregate(mean_realized_rho_X ~ rho_X, data = agg_df, mean)
tab_rhoY     <- aggregate(mean_realized_rho_Y ~ rho_Y, data = agg_df, mean)
tab_comp     <- aggregate(mean_realized_linear_share ~ comp_linear, data = agg_df, mean)
tab_rhoB     <- aggregate(mean_realized_mean_cor_X ~ rho_betweenX, data = agg_df, mean)

write.csv(tab_latentR2, file.path(table_dir, "implementation_latent_R2.csv"), row.names = FALSE)
write.csv(tab_rhoX,     file.path(table_dir, "implementation_rho_X.csv"), row.names = FALSE)
write.csv(tab_rhoY,     file.path(table_dir, "implementation_rho_Y.csv"), row.names = FALSE)
write.csv(tab_comp,     file.path(table_dir, "implementation_comp_linear.csv"), row.names = FALSE)
write.csv(tab_rhoB,     file.path(table_dir, "implementation_rho_betweenX.csv"), row.names = FALSE)

# -----------------------------
# 2. Full condition summary table
# -----------------------------
condition_table <- agg_df[, c(
  "condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  "mean_r2_ols_base", "mean_r2_ols_true_interaction", "mean_r2_ols_oracle", "mean_r2_xgb",
  "mean_rmse_ols_base", "mean_rmse_ols_true_interaction", "mean_rmse_ols_oracle", "mean_rmse_xgb"
)]

condition_table <- condition_table[order(
  condition_table$comp_linear,
  condition_table$rho_betweenX,
  condition_table$rho_Y,
  condition_table$rho_X,
  condition_table$latent_R2
), ]

write.csv(condition_table,
          file.path(table_dir, "condition_summary_main_metrics.csv"),
          row.names = FALSE)

# -----------------------------
# 3. Collapsed summary tables
# -----------------------------
perf_by_rhoX <- aggregate(
  cbind(
    mean_r2_ols_base, mean_r2_ols_true_interaction, mean_r2_ols_oracle, mean_r2_xgb,
    mean_rmse_ols_base, mean_rmse_ols_true_interaction, mean_rmse_ols_oracle, mean_rmse_xgb
  ) ~ rho_X,
  data = agg_df,
  mean
)

perf_by_rhoY <- aggregate(
  cbind(
    mean_r2_ols_base, mean_r2_ols_true_interaction, mean_r2_ols_oracle, mean_r2_xgb,
    mean_rmse_ols_base, mean_rmse_ols_true_interaction, mean_rmse_ols_oracle, mean_rmse_xgb
  ) ~ rho_Y,
  data = agg_df,
  mean
)

perf_by_latentR2 <- aggregate(
  cbind(
    mean_r2_ols_base, mean_r2_ols_true_interaction, mean_r2_ols_oracle, mean_r2_xgb,
    mean_rmse_ols_base, mean_rmse_ols_true_interaction, mean_rmse_ols_oracle, mean_rmse_xgb
  ) ~ latent_R2,
  data = agg_df,
  mean
)

perf_by_comp <- aggregate(
  cbind(
    mean_r2_ols_base, mean_r2_ols_true_interaction, mean_r2_ols_oracle, mean_r2_xgb,
    mean_rmse_ols_base, mean_rmse_ols_true_interaction, mean_rmse_ols_oracle, mean_rmse_xgb
  ) ~ comp_linear,
  data = agg_df,
  mean
)

perf_by_rhoB <- aggregate(
  cbind(
    mean_r2_ols_base, mean_r2_ols_true_interaction, mean_r2_ols_oracle, mean_r2_xgb,
    mean_rmse_ols_base, mean_rmse_ols_true_interaction, mean_rmse_ols_oracle, mean_rmse_xgb
  ) ~ rho_betweenX,
  data = agg_df,
  mean
)

write.csv(perf_by_rhoX,     file.path(table_dir, "collapsed_performance_by_rho_X.csv"), row.names = FALSE)
write.csv(perf_by_rhoY,     file.path(table_dir, "collapsed_performance_by_rho_Y.csv"), row.names = FALSE)
write.csv(perf_by_latentR2, file.path(table_dir, "collapsed_performance_by_latent_R2.csv"), row.names = FALSE)
write.csv(perf_by_comp,     file.path(table_dir, "collapsed_performance_by_comp_linear.csv"), row.names = FALSE)
write.csv(perf_by_rhoB,     file.path(table_dir, "collapsed_performance_by_rho_betweenX.csv"), row.names = FALSE)

# -----------------------------
# 4. Optional benchmark table
#    Selected cells only
# -----------------------------
benchmark_table <- subset(
  agg_df,
  latent_R2 %in% c(0.2, 0.8) &
    rho_X %in% c(0.6, 1.0) &
    rho_Y %in% c(0.6, 1.0) &
    comp_linear %in% c(0.2, 0.8) &
    rho_betweenX %in% c(0.0, 0.5)
)

benchmark_table <- benchmark_table[, c(
  "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  "mean_r2_ols_base", "mean_r2_ols_true_interaction", "mean_r2_ols_oracle", "mean_r2_xgb",
  "mean_rmse_ols_base", "mean_rmse_ols_true_interaction", "mean_rmse_ols_oracle", "mean_rmse_xgb"
)]

benchmark_table <- benchmark_table[order(
  benchmark_table$comp_linear,
  benchmark_table$rho_betweenX,
  benchmark_table$rho_Y,
  benchmark_table$rho_X,
  benchmark_table$latent_R2
), ]

write.csv(benchmark_table,
          file.path(table_dir, "benchmark_conditions_table.csv"),
          row.names = FALSE)

cat("Finished. Tables saved to:\n")

cat(table_dir, "\n")




# ============================================================
# 5. Publication add-on tables
# Purpose:
#   Create thesis-facing tables without changing diagnostic tables
# ============================================================

pub_table_dir <- file.path(run_dir, "publication_tables")
dir.create(pub_table_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# A. Compact implementation table
# -----------------------------
implementation_table <- bind_rows(
  tab_latentR2 %>% mutate(design_factor = "latent_R2",
                          target = latent_R2,
                          realized = mean_realized_latent_R2) %>%
    select(design_factor, target, realized),
  
  tab_rhoX %>% mutate(design_factor = "rho_X",
                      target = rho_X,
                      realized = mean_realized_rho_X) %>%
    select(design_factor, target, realized),
  
  tab_rhoY %>% mutate(design_factor = "rho_Y",
                      target = rho_Y,
                      realized = mean_realized_rho_Y) %>%
    select(design_factor, target, realized),
  
  tab_comp %>% mutate(design_factor = "comp_linear",
                      target = comp_linear,
                      realized = mean_realized_linear_share) %>%
    select(design_factor, target, realized),
  
  tab_rhoB %>% mutate(design_factor = "rho_betweenX",
                      target = rho_betweenX,
                      realized = mean_realized_mean_cor_X) %>%
    select(design_factor, target, realized)
)

write.csv(
  implementation_table,
  file.path(pub_table_dir, "table_implementation_check_compact.csv"),
  row.names = FALSE
)

# -----------------------------
# B. Full appendix contrast table
# -----------------------------
appendix_contrast_table <- agg_df %>%
  transmute(
    condition_id,
    latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
    
    mean_r2_base    = mean_r2_ols_base,
    mean_r2_aligned = mean_r2_ols_true_interaction,
    mean_r2_oracle  = mean_r2_ols_oracle,
    mean_r2_xgb     = mean_r2_xgb,
    
    mean_rmse_base    = mean_rmse_ols_base,
    mean_rmse_aligned = mean_rmse_ols_true_interaction,
    mean_rmse_oracle  = mean_rmse_ols_oracle,
    mean_rmse_xgb     = mean_rmse_xgb,
    
    delta_r2_aligned_vs_base   = mean_r2_ols_true_interaction - mean_r2_ols_base,
    delta_r2_oracle_vs_aligned = mean_r2_ols_oracle - mean_r2_ols_true_interaction,
    delta_r2_oracle_vs_base    = mean_r2_ols_oracle - mean_r2_ols_base,
    delta_r2_xgb_vs_base       = mean_r2_xgb - mean_r2_ols_base,
    delta_r2_xgb_vs_aligned    = mean_r2_xgb - mean_r2_ols_true_interaction,
    delta_r2_xgb_vs_oracle     = mean_r2_xgb - mean_r2_ols_oracle
  ) %>%
  arrange(comp_linear, rho_betweenX, rho_Y, rho_X, latent_R2)

write.csv(
  appendix_contrast_table,
  file.path(pub_table_dir, "appendix_full_condition_contrast_table.csv"),
  row.names = FALSE
)

# -----------------------------
# C. Main-text selected-condition table
# -----------------------------
selected_file <- file.path(run_dir, "selected_conditions", "main_text_selected_conditions.csv")

if (file.exists(selected_file)) {
  
  main_conditions <- read.csv(selected_file)
  
  main_text_table <- main_conditions %>%
    left_join(
      appendix_contrast_table,
      by = c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
    ) %>%
    select(
      label, latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
      mean_r2_base, mean_r2_aligned, mean_r2_oracle, mean_r2_xgb,
      delta_r2_aligned_vs_base,
      delta_r2_oracle_vs_aligned,
      delta_r2_oracle_vs_base,
      delta_r2_xgb_vs_base,
      delta_r2_xgb_vs_aligned,
      delta_r2_xgb_vs_oracle
    )
  
  write.csv(
    main_text_table,
    file.path(pub_table_dir, "table_selected_conditions_main_text.csv"),
    row.names = FALSE
  )
  
  cat("\nPublication tables saved to:\n")
  cat(pub_table_dir, "\n")
  
} else {
  cat("\nSkipping selected-condition main-text table.\n")
  cat("File not found:\n")
  cat(selected_file, "\n")
}

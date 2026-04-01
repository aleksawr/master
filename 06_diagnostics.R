# ============================================================
# 06_diagnostics.R
# Purpose:
#   Diagnostics for performance and realized condition values
#   for one selected run
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

diag_dir <- file.path(run_dir, "diagnostic_tables")
dir.create(diag_dir, showWarnings = FALSE)

required_cols <- c(
  "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  "realized_latent_R2", "realized_rho_X", "realized_rho_Y",
  "r2_ols_base", "r2_ols_true_interaction", "r2_ols_oracle", "r2_xgb"
)

missing_cols <- setdiff(required_cols, names(results_df))
if (length(missing_cols) > 0) {
  stop("Missing required columns in results_df: ",
       paste(missing_cols, collapse = ", "))
}

cat("\n==============================\n")
cat("BASIC STRUCTURE CHECK\n")
cat("==============================\n")

str(results_df)
cat("\nRows:", nrow(results_df), "\n")
cat("Unique conditions:", length(unique(results_df$condition_id)), "\n")

# ------------------------------------------------------------
# 1. Realized condition checks
# ------------------------------------------------------------
tab_latentR2 <- aggregate(realized_latent_R2 ~ latent_R2, data = results_df, mean)
tab_rhoX <- aggregate(realized_rho_X ~ rho_X, data = results_df, mean)
tab_rhoY <- aggregate(realized_rho_Y ~ rho_Y, data = results_df, mean)
tab_comp_linear <- aggregate(realized_linear_share ~ comp_linear, data = results_df, mean)
tab_comp_interaction <- aggregate(realized_interaction_share ~ comp_linear, data = results_df, mean)
tab_linint_cor <- aggregate(realized_lin_int_cor ~ comp_linear, data = results_df, mean)
tab_rhoBetweenX <- aggregate(realized_mean_cor_X ~ rho_betweenX, data = results_df, mean)

cat("\n==============================\n")
cat("REALIZED CONDITION CHECKS\n")
cat("==============================\n")

cat("\nTarget vs realized latent R2:\n")
print(tab_latentR2)

cat("\nTarget vs realized rho_X:\n")
print(tab_rhoX)

cat("\nTarget vs realized rho_Y:\n")
print(tab_rhoY)

cat("\nTarget vs realized linear share:\n")
print(tab_comp_linear)

cat("\nTarget vs realized interaction share:\n")
print(tab_comp_interaction)

cat("\nMean correlation between linear and interaction components:\n")
print(tab_linint_cor)

cat("\nTarget vs realized predictor correlation:\n")
print(tab_rhoBetweenX)

# ------------------------------------------------------------
# 2. Performance summaries
# ------------------------------------------------------------
perf_by_latentR2 <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb) ~ latent_R2,
  data = results_df,
  mean
)

perf_by_rhoX <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb) ~ rho_X,
  data = results_df,
  mean
)

perf_by_rhoY <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb) ~ rho_Y,
  data = results_df,
  mean
)

perf_by_rhoBetweenX <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb) ~ rho_betweenX,
  data = results_df,
  mean
)

lm_contrasts_by_comp <- aggregate(
  cbind(delta_r2_true_vs_base,
        delta_r2_oracle_vs_base,
        delta_r2_oracle_vs_true) ~ comp_linear,
  data = results_df,
  mean
)

xgb_contrasts_by_comp <- aggregate(
  cbind(delta_r2_xgb_vs_base,
        delta_r2_xgb_vs_true,
        delta_r2_xgb_vs_oracle) ~ comp_linear,
  data = results_df,
  mean
)

cat("\n==============================\n")
cat("PERFORMANCE PATTERNS\n")
cat("==============================\n")

cat("\nBy latent_R2:\n")
print(perf_by_latentR2)

cat("\nBy rho_X:\n")
print(perf_by_rhoX)

cat("\nBy rho_Y:\n")
print(perf_by_rhoY)

cat("\nBy rho_betweenX:\n")
print(perf_by_rhoBetweenX)

cat("\nLM-family contrasts by comp_linear:\n")
print(lm_contrasts_by_comp)

cat("\nOptional XGB contrasts by comp_linear:\n")
print(xgb_contrasts_by_comp)

# ------------------------------------------------------------
# 3. Recovery check under perfect reliability
# ------------------------------------------------------------
cat("\n==============================\n")
cat("RECOVERY CHECK: PERFECT RELIABILITY\n")
cat("==============================\n")

perfect_df <- subset(results_df, rho_X == 1 & rho_Y == 1)

if (nrow(perfect_df) > 0) {
  recovery_tab <- aggregate(
    cbind(realized_latent_R2,
          r2_ols_base,
          r2_ols_true_interaction,
          r2_ols_oracle,
          r2_xgb) ~ latent_R2 + comp_linear + rho_betweenX,
    data = perfect_df,
    mean
  )
  
  recovery_tab$gap_true_model_vs_realized_latent <-
    recovery_tab$realized_latent_R2 - recovery_tab$r2_ols_true_interaction
  
  recovery_tab$gap_oracle_vs_realized_latent <-
    recovery_tab$realized_latent_R2 - recovery_tab$r2_ols_oracle
  
  print(recovery_tab)
  
  cat("\nGap between realized latent R2 and full true-interaction model:\n")
  print(recovery_tab[, c(
    "latent_R2", "comp_linear", "rho_betweenX",
    "gap_true_model_vs_realized_latent"
  )])
  
  cat("\nGap between realized latent R2 and oracle model:\n")
  print(recovery_tab[, c(
    "latent_R2", "comp_linear", "rho_betweenX",
    "gap_oracle_vs_realized_latent"
  )])
} else {
  recovery_tab <- NULL
  cat("No perfect-reliability cells found.\n")
}

# ------------------------------------------------------------
# 4. Save diagnostic tables
# ------------------------------------------------------------
write.csv(tab_latentR2,
          file.path(diag_dir, "target_vs_realized_latent_R2.csv"),
          row.names = FALSE)

write.csv(tab_rhoX,
          file.path(diag_dir, "target_vs_realized_rho_X.csv"),
          row.names = FALSE)

write.csv(tab_rhoY,
          file.path(diag_dir, "target_vs_realized_rho_Y.csv"),
          row.names = FALSE)

write.csv(tab_comp_linear,
          file.path(diag_dir, "target_vs_realized_linear_share.csv"),
          row.names = FALSE)

write.csv(tab_comp_interaction,
          file.path(diag_dir, "target_vs_realized_interaction_share.csv"),
          row.names = FALSE)

write.csv(tab_linint_cor,
          file.path(diag_dir, "realized_linear_interaction_correlation.csv"),
          row.names = FALSE)

write.csv(tab_rhoBetweenX,
          file.path(diag_dir, "target_vs_realized_predictor_correlation.csv"),
          row.names = FALSE)

write.csv(perf_by_latentR2,
          file.path(diag_dir, "performance_by_latent_R2.csv"),
          row.names = FALSE)

write.csv(perf_by_rhoX,
          file.path(diag_dir, "performance_by_rho_X.csv"),
          row.names = FALSE)

write.csv(perf_by_rhoY,
          file.path(diag_dir, "performance_by_rho_Y.csv"),
          row.names = FALSE)

write.csv(perf_by_rhoBetweenX,
          file.path(diag_dir, "performance_by_rho_betweenX.csv"),
          row.names = FALSE)

write.csv(lm_contrasts_by_comp,
          file.path(diag_dir, "lm_contrasts_by_comp_linear.csv"),
          row.names = FALSE)

write.csv(xgb_contrasts_by_comp,
          file.path(diag_dir, "xgb_contrasts_by_comp_linear.csv"),
          row.names = FALSE)

if (!is.null(recovery_tab)) {
  write.csv(recovery_tab,
            file.path(diag_dir, "recovery_check_perfect_reliability.csv"),
            row.names = FALSE)
}

writeLines(
  c(
    paste("Run directory:", run_dir),
    paste("Rows in replication data:", nrow(results_df)),
    paste("Unique conditions:", length(unique(results_df$condition_id))),
    paste("Diagnostics saved:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ),
  con = file.path(diag_dir, "diagnostics_info.txt")
)

cat("\nSaved diagnostic tables to:\n", diag_dir, "\n")

cat("\n==============================\n")
cat("DIAGNOSTICS COMPLETE\n")
cat("==============================\n")

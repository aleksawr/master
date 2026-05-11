# ============================================================
# 06_diagnostics.R
# Purpose:
#   Diagnostics for realized design values and broad performance
#   patterns for one selected run
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

infile <- file.path(run_dir, "results_replication_level.csv")

if (!file.exists(infile)) {
  stop("Could not find results file: ", infile)
}

results_df <- read.csv(infile)

diag_dir <- file.path(run_dir, "diagnostic_tables")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

required_cols <- c(
  "condition_id",
  "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  "realized_latent_R2", "realized_rho_X", "realized_rho_X_min",
  "realized_rho_X_max", "realized_rho_Y", "realized_linear_share",
  "realized_interaction_share", "realized_lin_int_cor",
  "realized_mean_cor_X",
  "r2_ols_base", "r2_ols_true_interaction", "r2_ols_oracle", "r2_xgb",
  "rmse_ols_base", "rmse_ols_true_interaction", "rmse_ols_oracle", "rmse_xgb",
  "delta_r2_true_vs_base", "delta_r2_oracle_vs_base", "delta_r2_oracle_vs_true",
  "delta_r2_xgb_vs_base", "delta_r2_xgb_vs_true", "delta_r2_xgb_vs_oracle"
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
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb,
        rmse_ols_base, rmse_ols_true_interaction, rmse_ols_oracle, rmse_xgb) ~ latent_R2,
  data = results_df,
  mean
)

perf_by_rhoX <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb,
        rmse_ols_base, rmse_ols_true_interaction, rmse_ols_oracle, rmse_xgb) ~ rho_X,
  data = results_df,
  mean
)

perf_by_rhoY <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb,
        rmse_ols_base, rmse_ols_true_interaction, rmse_ols_oracle, rmse_xgb) ~ rho_Y,
  data = results_df,
  mean
)

perf_by_rhoBetweenX <- aggregate(
  cbind(r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb,
        rmse_ols_base, rmse_ols_true_interaction, rmse_ols_oracle, rmse_xgb) ~ rho_betweenX,
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
  
  recovery_tab <- recovery_tab[order(
    recovery_tab$comp_linear,
    recovery_tab$rho_betweenX,
    recovery_tab$latent_R2
  ), ]
  
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
# 4. Condition-level realized checks
# ------------------------------------------------------------
condition_realized_tab <- aggregate(
  cbind(
    realized_latent_R2,
    realized_rho_X,
    realized_rho_X_min,
    realized_rho_X_max,
    realized_rho_Y,
    realized_linear_share,
    realized_interaction_share,
    realized_lin_int_cor,
    realized_mean_cor_X
  ) ~ condition_id + latent_R2 + rho_X + rho_Y + comp_linear + rho_betweenX,
  data = results_df,
  mean
)

condition_realized_tab <- condition_realized_tab[order(
  condition_realized_tab$comp_linear,
  condition_realized_tab$rho_betweenX,
  condition_realized_tab$rho_Y,
  condition_realized_tab$rho_X,
  condition_realized_tab$latent_R2,
  condition_realized_tab$condition_id
), ]

# ------------------------------------------------------------
# 5. Save diagnostic tables
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

write.csv(condition_realized_tab,
          file.path(diag_dir, "condition_level_realized_checks.csv"),
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
# ------------------------------------------------------------
# 6. Selected 16 corner conditions
# ------------------------------------------------------------

sel_dir <- file.path(run_dir, "selected_conditions")
dir.create(sel_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 6.1 Create condition-level means
# -----------------------------

condition_summary <- aggregate(
  cbind(
    r2_ols_base,
    r2_ols_true_interaction,
    r2_ols_oracle,
    r2_xgb,
    rmse_ols_base,
    rmse_ols_true_interaction,
    rmse_ols_oracle,
    rmse_xgb,
    delta_r2_xgb_vs_base,
    delta_r2_xgb_vs_true,
    delta_r2_xgb_vs_oracle,
    delta_r2_true_vs_base,
    delta_r2_oracle_vs_base,
    delta_r2_oracle_vs_true
  ) ~ condition_id + latent_R2 + rho_X + rho_Y + comp_linear + rho_betweenX,
  data = results_df,
  mean
)

# -----------------------------
# 6.2 Select the 16 corner conditions
# -----------------------------

selected_16 <- subset(
  condition_summary,
  rho_betweenX == 0.00 &
    rho_X %in% c(0.60, 1.00) &
    rho_Y %in% c(0.60, 1.00) &
    comp_linear %in% c(0.20, 0.80) &
    latent_R2 %in% c(0.20, 0.80)
)

selected_16 <- selected_16[order(
  selected_16$rho_X,
  selected_16$rho_Y,
  selected_16$comp_linear,
  selected_16$latent_R2
), ]

selected_16$condition_label <- sprintf(
  "rhoX=%.2f | rhoY=%.2f | linear=%.2f | latentR2=%.2f | rhoBetweenX=%.2f",
  selected_16$rho_X,
  selected_16$rho_Y,
  selected_16$comp_linear,
  selected_16$latent_R2,
  selected_16$rho_betweenX
)

selected_16 <- selected_16[, c(
  "condition_label",
  setdiff(names(selected_16), "condition_label")
)]

# -----------------------------
# 6.3 Save condition-level means
# -----------------------------

write.csv(
  selected_16,
  file = file.path(sel_dir, "selected_16_condition_means.csv"),
  row.names = FALSE
)

# -----------------------------
# 6.4 Create and save replication-level data
#     This is the file you need for boxplots.
# -----------------------------

selected_16_replication <- subset(
  results_df,
  condition_id %in% selected_16$condition_id
)

selected_16_replication$condition_label <- selected_16$condition_label[
  match(selected_16_replication$condition_id, selected_16$condition_id)
]

selected_16_replication <- selected_16_replication[order(
  selected_16_replication$rho_X,
  selected_16_replication$rho_Y,
  selected_16_replication$comp_linear,
  selected_16_replication$latent_R2,
  selected_16_replication$condition_id
), ]

selected_16_replication <- selected_16_replication[, c(
  "condition_label",
  setdiff(names(selected_16_replication), "condition_label")
)]

write.csv(
  selected_16_replication,
  file = file.path(sel_dir, "selected_16_replication_level.csv"),
  row.names = FALSE
)

# -----------------------------
# 6.5 Checks
# -----------------------------

cat("\nSelected 16 condition means:\n")
cat("Rows:", nrow(selected_16), "\n")
cat("Unique condition IDs:", length(unique(selected_16$condition_id)), "\n")
cat("Duplicated condition IDs:", any(duplicated(selected_16$condition_id)), "\n")

cat("\nSelected 16 replication-level data:\n")
cat("Rows:", nrow(selected_16_replication), "\n")
cat("Unique condition IDs:", length(unique(selected_16_replication$condition_id)), "\n")
cat("Replications per condition:\n")
print(table(selected_16_replication$condition_id))

cat("\nSaved selected 16 condition means to:\n")
cat(file.path(sel_dir, "selected_16_condition_means.csv"), "\n")

cat("\nSaved selected 16 replication-level data to:\n")
cat(file.path(sel_dir, "selected_16_replication_level.csv"), "\n")

cat("\nSaved diagnostic tables to:\n")
cat(diag_dir, "\n")

cat("\n==============================\n")
cat("DIAGNOSTICS COMPLETE\n")
cat("==============================\n")
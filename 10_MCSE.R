# ============================================================
# 10_MCSE.R
# Purpose:
#   Compute Monte Carlo standard errors (MCSEs) for two
#   simulation runs using wide replication-level result files.
#
#   Expected wide columns in each file:
#     - condition_id
#     - rep  (or replication)
#     - latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX
#     - r2_ols_base, r2_ols_true_interaction, r2_ols_oracle, r2_xgb
#     - rmse_ols_base, rmse_ols_true_interaction, rmse_ols_oracle, rmse_xgb
# ============================================================

rm(list = ls(all.names = TRUE))

library(dplyr)

# -----------------------------
# 1. File paths
# -----------------------------
file_run1 <- "runs/run_2026-04-20_23-32_nrep200/results_replication_level.csv"
file_run2 <- "runs/run_2026-04-24_13-57_nrep200/results_replication_level.csv"

run1_id <- "2026-04-20"
run2_id <- "2026-04-24"

out_dir <- "runs/mcse_outputs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 2. Read data
# -----------------------------
d1 <- read.csv(file_run1, stringsAsFactors = FALSE)
d2 <- read.csv(file_run2, stringsAsFactors = FALSE)

d1$run_id <- run1_id
d2$run_id <- run2_id

# -----------------------------
# 3. Basic checks
# -----------------------------
cat("Run 1 dim:", dim(d1), "\n")
cat("Run 2 dim:", dim(d2), "\n")

cat("\nRun 1 file:\n")
print(normalizePath(file_run1))
print(file.info(file_run1)[, c("size", "mtime")])

cat("\nRun 2 file:\n")
print(normalizePath(file_run2))
print(file.info(file_run2)[, c("size", "mtime")])

cat("\nMD5 checksums:\n")
print(tools::md5sum(file_run1))
print(tools::md5sum(file_run2))

# -----------------------------
# 4. Harmonize replication column
# -----------------------------
if ("rep" %in% names(d1)) names(d1)[names(d1) == "rep"] <- "replication"
if ("rep" %in% names(d2)) names(d2)[names(d2) == "rep"] <- "replication"

# -----------------------------
# 5. Combine
# -----------------------------
dat <- bind_rows(d1, d2)

# -----------------------------
# 6. Required columns
# -----------------------------
cond_vars <- c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
rep_var   <- "replication"

required_cols <- c(
  "run_id", "condition_id", rep_var, cond_vars,
  "r2_ols_base", "r2_ols_true_interaction", "r2_ols_oracle", "r2_xgb",
  "rmse_ols_base", "rmse_ols_true_interaction", "rmse_ols_oracle", "rmse_xgb"
)

missing_cols <- setdiff(required_cols, names(dat))

if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# -----------------------------
# 7. MCSE helper
# -----------------------------
mcse_mean <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n == 0) {
    return(c(n = 0, mean = NA, sd = NA, mcse = NA))
  }
  
  m <- mean(x)
  s <- sd(x)
  
  if (n == 1 || is.na(s)) s <- NA_real_
  
  mcse <- s / sqrt(n)
  
  c(n = n, mean = m, sd = s, mcse = mcse)
}

# -----------------------------
# 8. Long version for model-specific MCSEs
# -----------------------------
r2_long <- bind_rows(
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "Baseline OLS", metric = "R2", value = r2_ols_base),
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "Aligned OLS", metric = "R2", value = r2_ols_true_interaction),
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "Oracle OLS", metric = "R2", value = r2_ols_oracle),
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "XGBoost", metric = "R2", value = r2_xgb)
)

rmse_long <- bind_rows(
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "Baseline OLS", metric = "RMSE", value = rmse_ols_base),
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "Aligned OLS", metric = "RMSE", value = rmse_ols_true_interaction),
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "Oracle OLS", metric = "RMSE", value = rmse_ols_oracle),
  dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                    model = "XGBoost", metric = "RMSE", value = rmse_xgb)
)

perf_long <- bind_rows(r2_long, rmse_long)

# -----------------------------
# 9. MCSE by run x condition x model x metric
# -----------------------------
mcse_by_model <- perf_long %>%
  group_by(run_id, condition_id, across(all_of(cond_vars)), model, metric) %>%
  summarise(
    n    = sum(is.finite(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    mcse = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(run_id, model, metric, comp_linear, rho_betweenX, rho_Y, rho_X, latent_R2)

# -----------------------------
# 10. Paired contrasts within each run
# -----------------------------
contrast_dat <- dat %>%
  transmute(
    run_id, condition_id, replication, !!!syms(cond_vars),
    
    diff_r2_xgb_vs_base    = r2_xgb - r2_ols_base,
    diff_r2_xgb_vs_aligned = r2_xgb - r2_ols_true_interaction,
    diff_r2_xgb_vs_oracle  = r2_xgb - r2_ols_oracle,
    diff_r2_aligned_vs_base = r2_ols_true_interaction - r2_ols_base,
    diff_r2_oracle_vs_aligned = r2_ols_oracle - r2_ols_true_interaction,
    
    diff_rmse_xgb_vs_base    = rmse_xgb - rmse_ols_base,
    diff_rmse_xgb_vs_aligned = rmse_xgb - rmse_ols_true_interaction,
    diff_rmse_xgb_vs_oracle  = rmse_xgb - rmse_ols_oracle,
    diff_rmse_aligned_vs_base = rmse_ols_true_interaction - rmse_ols_base,
    diff_rmse_oracle_vs_aligned = rmse_ols_oracle - rmse_ols_true_interaction
  )

contrast_long <- bind_rows(
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "XGBoost - Baseline OLS", metric = "delta_R2",
                             value = diff_r2_xgb_vs_base),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "XGBoost - Aligned OLS", metric = "delta_R2",
                             value = diff_r2_xgb_vs_aligned),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "XGBoost - Oracle OLS", metric = "delta_R2",
                             value = diff_r2_xgb_vs_oracle),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "Aligned OLS - Baseline OLS", metric = "delta_R2",
                             value = diff_r2_aligned_vs_base),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "Oracle OLS - Aligned OLS", metric = "delta_R2",
                             value = diff_r2_oracle_vs_aligned),
  
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "XGBoost - Baseline OLS", metric = "delta_RMSE",
                             value = diff_rmse_xgb_vs_base),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "XGBoost - Aligned OLS", metric = "delta_RMSE",
                             value = diff_rmse_xgb_vs_aligned),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "XGBoost - Oracle OLS", metric = "delta_RMSE",
                             value = diff_rmse_xgb_vs_oracle),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "Aligned OLS - Baseline OLS", metric = "delta_RMSE",
                             value = diff_rmse_aligned_vs_base),
  contrast_dat %>% transmute(run_id, condition_id, replication, !!!syms(cond_vars),
                             contrast = "Oracle OLS - Aligned OLS", metric = "delta_RMSE",
                             value = diff_rmse_oracle_vs_aligned)
)

mcse_contrasts <- contrast_long %>%
  group_by(run_id, condition_id, across(all_of(cond_vars)), contrast, metric) %>%
  summarise(
    n    = sum(is.finite(value)),
    mean = mean(value, na.rm = TRUE),
    sd   = sd(value, na.rm = TRUE),
    mcse = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(run_id, contrast, metric, comp_linear, rho_betweenX, rho_Y, rho_X, latent_R2)

# -----------------------------
# 11. Compare condition means across runs
# -----------------------------
run_compare_base <- mcse_by_model %>%
  select(run_id, condition_id, all_of(cond_vars), model, metric, mean, mcse)

rc1 <- run_compare_base %>% filter(run_id == run1_id)
rc2 <- run_compare_base %>% filter(run_id == run2_id)

cmp <- rc1 %>%
  inner_join(
    rc2,
    by = c("condition_id", cond_vars, "model", "metric"),
    suffix = c("_run1", "_run2")
  ) %>%
  mutate(
    abs_diff_mean = abs(mean_run1 - mean_run2),
    pooled_mcse = sqrt(mcse_run1^2 + mcse_run2^2),
    diff_over_pooled_mcse = abs_diff_mean / pooled_mcse
  ) %>%
  arrange(desc(abs_diff_mean))

# -----------------------------
# 12. Compact summaries for thesis writing
# -----------------------------
mcse_summary_by_metric <- mcse_by_model %>%
  group_by(run_id, model, metric) %>%
  summarise(
    mean_mcse = mean(mcse, na.rm = TRUE),
    max_mcse  = max(mcse, na.rm = TRUE),
    min_mcse  = min(mcse, na.rm = TRUE),
    .groups = "drop"
  )

contrast_mcse_summary <- mcse_contrasts %>%
  group_by(run_id, contrast, metric) %>%
  summarise(
    mean_mcse = mean(mcse, na.rm = TRUE),
    max_mcse  = max(mcse, na.rm = TRUE),
    min_mcse  = min(mcse, na.rm = TRUE),
    .groups = "drop"
  )

run_similarity_summary <- cmp %>%
  group_by(model, metric) %>%
  summarise(
    mean_abs_diff = mean(abs_diff_mean, na.rm = TRUE),
    max_abs_diff  = max(abs_diff_mean, na.rm = TRUE),
    mean_ratio_to_pooled_mcse = mean(diff_over_pooled_mcse, na.rm = TRUE),
    max_ratio_to_pooled_mcse  = max(diff_over_pooled_mcse, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 13. Save outputs
# -----------------------------
write.csv(
  mcse_by_model,
  file.path(out_dir, "mcse_by_model_by_run.csv"),
  row.names = FALSE
)

write.csv(
  mcse_contrasts,
  file.path(out_dir, "mcse_model_contrasts_by_run.csv"),
  row.names = FALSE
)

write.csv(
  cmp,
  file.path(out_dir, "mcse_run_comparison.csv"),
  row.names = FALSE
)

write.csv(
  mcse_summary_by_metric,
  file.path(out_dir, "mcse_summary_by_model_metric.csv"),
  row.names = FALSE
)

write.csv(
  contrast_mcse_summary,
  file.path(out_dir, "mcse_summary_by_contrast_metric.csv"),
  row.names = FALSE
)

write.csv(
  run_similarity_summary,
  file.path(out_dir, "mcse_run_similarity_summary.csv"),
  row.names = FALSE
)

# -----------------------------
# 14. Console output
# -----------------------------
cat("\nDone.\n")
cat("Outputs saved to:\n")
cat(out_dir, "\n\n")

cat("Average MCSE by model and metric:\n")
print(mcse_summary_by_metric)

cat("\nAverage MCSE by contrast and metric:\n")
print(contrast_mcse_summary)

cat("\nRun similarity summary:\n")
print(run_similarity_summary)


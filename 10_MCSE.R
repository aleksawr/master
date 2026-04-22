# ============================================================
# 10_MCSE.R
# Purpose:
# MCSE script for two simulation runs
# Assumes each run has a results_replication_level.csv file
# Calculate MCSE
# ============================================================

# ---- paths: change these to your actual folders/files ----
file_run1 <- "runs/run_2026-04-13_00-27_nrep200/results_replication_level.csv"
file_run2 <- "runs/run_2026-04-20_23-32_nrep200/results_replication_level.csv"

# ---- read data ----
d1 <- read.csv(file_run1, stringsAsFactors = FALSE)
d2 <- read.csv(file_run2, stringsAsFactors = FALSE)

d1$run_id <- "2026-04-13"
d2$run_id <- "2026-04-20"

dat <- rbind(d1, d2)

# ---- inspect names ----
names(dat)
str(dat)


file_run1
file_run2

normalizePath(file_run1)
normalizePath(file_run2)

file.info(file_run1)[, c("size", "mtime")]
file.info(file_run2)[, c("size", "mtime")]

tools::md5sum(file_run1)
tools::md5sum(file_run2)

list.files("runs/run_2026-04-13_00-27_nrep200", full.names = TRUE)
list.files("runs/run_2026-04-20_23-32_nrep200", full.names = TRUE)

file.info(list.files("runs/run_2026-04-13_00-27_nrep200", full.names = TRUE))
file.info(list.files("runs/run_2026-04-20_23-32_nrep200", full.names = TRUE))


# =========================================================
# EDIT THESE if your column names differ
# =========================================================

# condition columns
cond_vars <- c("latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")

# model column
model_var <- "model"

# replication column
rep_var <- "replication"

# performance columns
r2_var   <- "test_r2"
rmse_var <- "test_rmse"


d1 <- read.csv(file_run1, stringsAsFactors = FALSE)
d2 <- read.csv(file_run2, stringsAsFactors = FALSE)

dim(d1)
dim(d2)

head(d1[, c("condition_id", "rep", "r2_ols_base", "r2_xgb")])
head(d2[, c("condition_id", "rep", "r2_ols_base", "r2_xgb")])

identical(d1, d2)





# =========================================================
# helper: mean + MCSE
# =========================================================
mcse_mean <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  mcse <- s / sqrt(n)
  out <- c(n = n, mean = m, sd = s, mcse = mcse)
  return(out)
}

# =========================================================
# 1) MCSE for each condition x model within each run
# =========================================================

split_keys <- c("run_id", cond_vars, model_var)

split_list <- split(dat, dat[split_keys], drop = TRUE)

res_list <- lapply(split_list, function(df) {
  r2_out   <- mcse_mean(df[[r2_var]])
  rmse_out <- mcse_mean(df[[rmse_var]])
  
  first_row <- df[1, c("run_id", cond_vars, model_var), drop = FALSE]
  
  cbind(
    first_row,
    n_rep      = r2_out["n"],
    mean_test_r2   = r2_out["mean"],
    sd_test_r2     = r2_out["sd"],
    mcse_test_r2   = r2_out["mcse"],
    mean_test_rmse = rmse_out["mean"],
    sd_test_rmse   = rmse_out["sd"],
    mcse_test_rmse = rmse_out["mcse"]
  )
})

mcse_by_model <- do.call(rbind, res_list)
row.names(mcse_by_model) <- NULL

# =========================================================
# 2) MCSE for paired model differences within each run
#    Example comparisons:
#      XGBoost - Baseline OLS
#      XGBoost - Aligned OLS
#      Aligned OLS - Baseline OLS
# =========================================================

# Change these labels to exactly match your model names
baseline_name <- "Baseline OLS"
aligned_name  <- "Aligned OLS"
xgb_name      <- "XGBoost"

# reshape manually using merge in base R
id_vars <- c("run_id", cond_vars, rep_var)

sub_baseline <- dat[dat[[model_var]] == baseline_name, c(id_vars, r2_var, rmse_var)]
sub_aligned  <- dat[dat[[model_var]] == aligned_name,  c(id_vars, r2_var, rmse_var)]
sub_xgb      <- dat[dat[[model_var]] == xgb_name,      c(id_vars, r2_var, rmse_var)]

names(sub_baseline)[names(sub_baseline) == r2_var]   <- "r2_baseline"
names(sub_baseline)[names(sub_baseline) == rmse_var] <- "rmse_baseline"

names(sub_aligned)[names(sub_aligned) == r2_var]   <- "r2_aligned"
names(sub_aligned)[names(sub_aligned) == rmse_var] <- "rmse_aligned"

names(sub_xgb)[names(sub_xgb) == r2_var]   <- "r2_xgb"
names(sub_xgb)[names(sub_xgb) == rmse_var] <- "rmse_xgb"

tmp1 <- merge(sub_xgb, sub_baseline, by = id_vars, all = FALSE)
tmp2 <- merge(sub_xgb, sub_aligned,  by = id_vars, all = FALSE)
tmp3 <- merge(sub_aligned, sub_baseline, by = id_vars, all = FALSE)

tmp1$contrast <- "XGBoost - Baseline OLS"
tmp1$diff_r2  <- tmp1$r2_xgb - tmp1$r2_baseline
tmp1$diff_rmse <- tmp1$rmse_xgb - tmp1$rmse_baseline

tmp2$contrast <- "XGBoost - Aligned OLS"
tmp2$diff_r2  <- tmp2$r2_xgb - tmp2$r2_aligned
tmp2$diff_rmse <- tmp2$rmse_xgb - tmp2$rmse_aligned

tmp3$contrast <- "Aligned OLS - Baseline OLS"
tmp3$diff_r2  <- tmp3$r2_aligned - tmp3$r2_baseline
tmp3$diff_rmse <- tmp3$rmse_aligned - tmp3$rmse_baseline

contrasts_dat <- rbind(
  tmp1[, c("run_id", cond_vars, rep_var, "contrast", "diff_r2", "diff_rmse")],
  tmp2[, c("run_id", cond_vars, rep_var, "contrast", "diff_r2", "diff_rmse")],
  tmp3[, c("run_id", cond_vars, rep_var, "contrast", "diff_r2", "diff_rmse")]
)

split_keys2 <- c("run_id", cond_vars, "contrast")
split_list2 <- split(contrasts_dat, contrasts_dat[split_keys2], drop = TRUE)

contrast_res <- lapply(split_list2, function(df) {
  r2_out   <- mcse_mean(df$diff_r2)
  rmse_out <- mcse_mean(df$diff_rmse)
  
  first_row <- df[1, c("run_id", cond_vars, "contrast"), drop = FALSE]
  
  cbind(
    first_row,
    n_rep           = r2_out["n"],
    mean_diff_r2    = r2_out["mean"],
    sd_diff_r2      = r2_out["sd"],
    mcse_diff_r2    = r2_out["mcse"],
    mean_diff_rmse  = rmse_out["mean"],
    sd_diff_rmse    = rmse_out["sd"],
    mcse_diff_rmse  = rmse_out["mcse"]
  )
})

mcse_contrasts <- do.call(rbind, contrast_res)
row.names(mcse_contrasts) <- NULL

# =========================================================
# 3) compare the two runs at condition level
#    This checks how similar the estimated condition means are
# =========================================================

# make a compact table for comparison
run_compare <- mcse_by_model[, c("run_id", cond_vars, model_var,
                                 "mean_test_r2", "mcse_test_r2",
                                 "mean_test_rmse", "mcse_test_rmse")]

# split by run
rc1 <- run_compare[run_compare$run_id == "2026-04-13", ]
rc2 <- run_compare[run_compare$run_id == "2026-04-20", ]

merge_keys <- c(cond_vars, model_var)

cmp <- merge(rc1, rc2, by = merge_keys, suffixes = c("_run1", "_run2"))

cmp$abs_diff_mean_r2   <- abs(cmp$mean_test_r2_run1 - cmp$mean_test_r2_run2)
cmp$abs_diff_mean_rmse <- abs(cmp$mean_test_rmse_run1 - cmp$mean_test_rmse_run2)

# =========================================================
# 4) save outputs
# =========================================================
write.csv(mcse_by_model,   "mcse_by_model_by_run.csv", row.names = FALSE)
write.csv(mcse_contrasts,  "mcse_model_contrasts_by_run.csv", row.names = FALSE)
write.csv(cmp,             "mcse_run_comparison.csv", row.names = FALSE)

cat("Done.\n")

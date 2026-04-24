# ============================================================
# run_config.R
# Purpose:
#   Shared run paths for aggregation, diagnostics, and plots
# ============================================================

run_label <- "run_2026-04-24_13-32_nrep200"

run_seed <- 20260424

run_dir <- file.path("runs", run_label)

# run_label <- "old_run"
# run_dir <- "."

if (!dir.exists(run_dir)) {
  stop("Run directory does not exist: ", run_dir)
}

diag_dir <- file.path(run_dir, "diagnostic_tables")
plot_dir <- file.path(run_dir, "plots")

dir.create(diag_dir, showWarnings = FALSE)
dir.create(plot_dir, showWarnings = FALSE)

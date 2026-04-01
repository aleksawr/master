# ============================================================
# 00_design.R
# Purpose:
#   Define the simulation design and condition grid
# Outputs:
#   sim_design, cond_grid
# ============================================================

rm(list = ls(all.names = TRUE))

# -----------------------------
# 1. Core design choices
# -----------------------------
N <- 1000
p <- 20
n_true <- 4
train_prop <- 0.70
n_rep <- 2

# First 4 predictors have true main effects
beta <- c(0.6, 0.5, 0.4, 0.3, rep(0, p - n_true))

# -----------------------------
# 2. Simulation conditions
# -----------------------------
latent_R2_levels <- c(0.20, 0.50, 0.80)
rho_X_levels <- c(0.60, 0.80, 1.00)
rho_Y_levels <- c(0.60, 0.80, 1.00)

# Interpreted as target proportion of signal variance
# coming from the linear component
comp_linear_levels <- c(0.80, 0.50, 0.20)

# New: latent predictor intercorrelation
rho_betweenX_levels <- c(0.00, 0.30)

cond_grid <- expand.grid(
  latent_R2 = latent_R2_levels,
  rho_X = rho_X_levels,
  rho_Y = rho_Y_levels,
  comp_linear = comp_linear_levels,
  rho_betweenX = rho_betweenX_levels
)

cond_grid$condition_id <- seq_len(nrow(cond_grid))
cond_grid <- cond_grid[, c(
  "condition_id",
  "latent_R2",
  "rho_X",
  "rho_Y",
  "comp_linear",
  "rho_betweenX"
)]

# -----------------------------
# 3. Collect design
# -----------------------------
sim_design <- list(
  N = N,
  p = p,
  n_true = n_true,
  beta = beta,
  train_prop = train_prop,
  n_rep = n_rep,
  latent_R2_levels = latent_R2_levels,
  rho_X_levels = rho_X_levels,
  rho_Y_levels = rho_Y_levels,
  comp_linear_levels = comp_linear_levels,
  rho_betweenX_levels = rho_betweenX_levels,
  cond_grid = cond_grid
)

# -----------------------------
# 4. Quick check
# -----------------------------
cat("Number of conditions:", nrow(cond_grid), "\n")
cat("Target linear variance-share levels:",
    paste(comp_linear_levels, collapse = ", "), "\n")
cat("Predictor correlation levels:",
    paste(rho_betweenX_levels, collapse = ", "), "\n")
print(cond_grid)


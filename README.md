# Measurement Error and Structural Complexity in Prediction

This repository contains the R code for the master's thesis simulation study on measurement quality, structural data-generating conditions, and XGBoost predictive performance.

## Abstract

Machine-learning models such as XGBoost are increasingly used for prediction in the social, behavioural, and educational sciences, but their performance may be constrained by measurement quality. This Monte Carlo simulation examined how predictor reliability, outcome reliability, latent explained variance, and structural composition shaped XGBoost’s out-of-sample performance relative to three ordinary least squares (OLS) benchmarks. Lower reliability reduced predictive performance and compressed model differences. XGBoost showed the largest positive differences relative to main-effects OLS when the structure was interaction-dominant and measurement quality was high. These differences were reduced or reversed when the relevant interaction was specified in regression. The findings suggest that apparent gains from flexible machine-learning models should be interpreted in relation to measurement quality, recoverable structure, and benchmark specification.

## Repository Structure

The core simulation pipeline is organised as follows:

- `run_config.R`: defines shared paths, run identifiers, random seeds, and computational settings.
- `00_design.R`: defines the simulation design, including manipulated factor levels and the full factorial condition grid.
- `01_simulation.R`: implements the data-generating process, including latent predictors, structural composition, latent outcome generation, predictor intercorrelation, and measurement error.
- `02_fit_models.R`: fits the prediction models used in the study: XGBoost, baseline OLS, aligned OLS, and oracle OLS.
- `03_evaluate.R`: computes predictive performance metrics, including test-set R² and RMSE.
- `04_run_all.R`: runs the full simulation pipeline by sourcing the design, simulation, model-fitting, and evaluation scripts.

Additional scripts may be included for aggregation, diagnostics, Monte Carlo standard errors, tables, figures, and exported summaries.

## Reproducibility

The analysis was conducted in R version 4.4.3.

To run the main simulation pipeline:

```r
source("run_config.R")
source("04_run_all.R")


# ============================================================
# Canadian Bank Market Risk VaR Engine
# Dynamic volatility models: EWMA and GARCH(1,1)
# ============================================================

# Load project setup if needed
if (!exists("required_packages")) {
  source(
    here::here(
      "R",
      "00_project_setup.R"
    )
  )
}

# Load portfolio definition if needed
if (!exists("portfolio_definition")) {
  source(
    here::here(
      "R",
      "01_define_portfolio.R"
    )
  )
}

# Load portfolio returns if needed
if (!exists("portfolio_returns")) {
  source(
    here::here(
      "R",
      "03_prepare_returns.R"
    )
  )
}

# Check that rugarch is installed
if (
  !requireNamespace(
    "rugarch",
    quietly = TRUE
  )
) {
  stop(
    paste(
      "The rugarch package is required.",
      "Install it using renv::install('rugarch')."
    )
  )
}

library(rugarch)
# ============================================================
# Output paths
# ============================================================

volatility_results_path <- here::here(
  "data",
  "processed",
  "dynamic_volatility.rds"
)

volatility_results_csv_path <- here::here(
  "data",
  "processed",
  "dynamic_volatility.csv"
)

garch_parameters_path <- here::here(
  "output",
  "tables",
  "garch_parameters.csv"
)

volatility_summary_path <- here::here(
  "output",
  "tables",
  "volatility_model_summary.csv"
)

# ============================================================
# Input return series
# ============================================================

returns <- portfolio_returns |>
  dplyr::arrange(date) |>
  dplyr::select(
    date,
    portfolio_simple_return
  )

if (
  anyNA(
    returns$portfolio_simple_return
  )
) {
  stop(
    "Missing portfolio returns were detected."
  )
}

if (
  any(
    !is.finite(
      returns$portfolio_simple_return
    )
  )
) {
  stop(
    "Non-finite portfolio returns were detected."
  )
}

# ============================================================
# EWMA volatility model
# ============================================================

ewma_lambda <- 0.94

if (
  ewma_lambda <= 0 ||
  ewma_lambda >= 1
) {
  stop(
    "EWMA lambda must lie strictly between 0 and 1."
  )
}

number_of_returns <- nrow(returns)

ewma_variance <- numeric(
  number_of_returns
)

# Initialize variance using the full-sample variance.
# Later, rolling backtesting will use only prior information.

ewma_variance[1] <-
  stats::var(
    returns$portfolio_simple_return
  )

for (
  t in 2:number_of_returns
) {
  
  ewma_variance[t] <-
    ewma_lambda *
    ewma_variance[t - 1] +
    (1 - ewma_lambda) *
    returns$portfolio_simple_return[t - 1]^2
}

ewma_volatility <-
  sqrt(
    ewma_variance
  )

# ============================================================
# GARCH(1,1) specification
# ============================================================

# Fit using percentage returns for numerical stability.
# Volatility is converted back to decimal-return units below.

garch_returns_percent <-
  returns$portfolio_simple_return * 100

garch_specification <-
  rugarch::ugarchspec(
    
    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),
    
    mean.model = list(
      armaOrder = c(0, 0),
      include.mean = TRUE
    ),
    
    distribution.model = "norm"
  )

# ============================================================
# Estimate GARCH(1,1)
# ============================================================

garch_fit <-
  rugarch::ugarchfit(
    spec = garch_specification,
    data = garch_returns_percent,
    solver = "hybrid"
  )

# Check convergence
garch_convergence <-
  convergence(
    garch_fit
  )

if (
  garch_convergence != 0
) {
  warning(
    paste(
      "GARCH estimation did not return",
      "convergence code 0.",
      "Convergence code:",
      garch_convergence
    )
  )
}

# ============================================================
# Extract GARCH conditional volatility
# ============================================================

garch_volatility <-
  as.numeric(
    sigma(
      garch_fit
    )
  ) / 100

garch_residuals <-
  as.numeric(
    residuals(
      garch_fit,
      standardize = FALSE
    )
  ) / 100

garch_standardized_residuals <-
  as.numeric(
    residuals(
      garch_fit,
      standardize = TRUE
    )
  )

# ============================================================
# Extract GARCH coefficients
# ============================================================

garch_coefficients <-
  coef(
    garch_fit
  )

garch_parameters <- tibble::tibble(
  parameter =
    names(
      garch_coefficients
    ),
  
  estimate =
    as.numeric(
      garch_coefficients
    )
)

# Extract key parameters
omega <-
  unname(
    garch_coefficients["omega"]
  )

alpha1 <-
  unname(
    garch_coefficients["alpha1"]
  )

beta1 <-
  unname(
    garch_coefficients["beta1"]
  )

garch_persistence <-
  alpha1 + beta1

# ============================================================
# Long-run GARCH volatility
# ============================================================

if (
  is.finite(garch_persistence) &&
  garch_persistence < 1
) {
  
  unconditional_variance_percent <-
    omega /
    (1 - garch_persistence)
  
  unconditional_volatility <-
    sqrt(
      unconditional_variance_percent
    ) / 100
  
} else {
  
  unconditional_volatility <-
    NA_real_
}

# ============================================================
# Combine volatility models
# ============================================================

dynamic_volatility <- returns |>
  dplyr::mutate(
    
    ewma_daily_volatility =
      ewma_volatility,
    
    garch_daily_volatility =
      garch_volatility,
    
    ewma_annualized_volatility =
      ewma_daily_volatility *
      sqrt(
        trading_days_per_year
      ),
    
    garch_annualized_volatility =
      garch_daily_volatility *
      sqrt(
        trading_days_per_year
      ),
    
    garch_residual =
      garch_residuals,
    
    garch_standardized_residual =
      garch_standardized_residuals
  )

# ============================================================
# Model summary
# ============================================================

volatility_model_summary <- tibble::tibble(
  
  statistic = c(
    "EWMA lambda",
    "GARCH omega",
    "GARCH alpha",
    "GARCH beta",
    "GARCH persistence",
    "GARCH unconditional daily volatility",
    "Average EWMA daily volatility",
    "Average GARCH daily volatility",
    "Latest EWMA daily volatility",
    "Latest GARCH daily volatility"
  ),
  
  value = c(
    ewma_lambda,
    omega,
    alpha1,
    beta1,
    garch_persistence,
    unconditional_volatility,
    mean(
      ewma_volatility,
      na.rm = TRUE
    ),
    mean(
      garch_volatility,
      na.rm = TRUE
    ),
    tail(
      ewma_volatility,
      1
    ),
    tail(
      garch_volatility,
      1
    )
  )
)

# ============================================================
# Validation checks
# ============================================================

if (
  any(
    dynamic_volatility$ewma_daily_volatility <= 0
  )
) {
  stop(
    "Non-positive EWMA volatility detected."
  )
}

if (
  any(
    dynamic_volatility$garch_daily_volatility <= 0
  )
) {
  stop(
    "Non-positive GARCH volatility detected."
  )
}

if (
  length(garch_volatility) !=
  nrow(returns)
) {
  stop(
    "GARCH volatility series does not align with return dates."
  )
}

# ============================================================
# Save results
# ============================================================

saveRDS(
  dynamic_volatility,
  volatility_results_path
)

readr::write_csv(
  dynamic_volatility,
  volatility_results_csv_path
)

readr::write_csv(
  garch_parameters,
  garch_parameters_path
)

readr::write_csv(
  volatility_model_summary,
  volatility_summary_path
)

# ============================================================
# Display key results
# ============================================================

cat(
  "\nGARCH convergence code:",
  garch_convergence,
  "\n"
)

cat(
  "GARCH alpha:",
  round(
    alpha1,
    4
  ),
  "\n"
)

cat(
  "GARCH beta:",
  round(
    beta1,
    4
  ),
  "\n"
)

cat(
  "GARCH persistence (alpha + beta):",
  round(
    garch_persistence,
    4
  ),
  "\n"
)

cat(
  "Latest EWMA annualized volatility:",
  scales::percent(
    tail(
      dynamic_volatility$ewma_annualized_volatility,
      1
    ),
    accuracy = 0.01
  ),
  "\n"
)

cat(
  "Latest GARCH annualized volatility:",
  scales::percent(
    tail(
      dynamic_volatility$garch_annualized_volatility,
      1
    ),
    accuracy = 0.01
  ),
  "\n"
)

message(
  "Dynamic volatility modelling completed successfully."
)
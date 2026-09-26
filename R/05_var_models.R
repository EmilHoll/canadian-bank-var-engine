# ============================================================
# Canadian Bank Market Risk VaR Engine
# Historical and Parametric VaR
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

# Load prepared returns if needed
if (!exists("portfolio_returns")) {
  source(
    here::here(
      "R",
      "03_prepare_returns.R"
    )
  )
}

# ============================================================
# Output paths
# ============================================================

var_results_path <- here::here(
  "output",
  "tables",
  "var_estimates.csv"
)

parametric_inputs_path <- here::here(
  "output",
  "tables",
  "parametric_var_inputs.csv"
)

covariance_matrix_path <- here::here(
  "output",
  "tables",
  "asset_covariance_matrix.csv"
)

# ============================================================
# Basic inputs
# ============================================================

portfolio_return_series <-
  portfolio_returns$portfolio_simple_return

tail_probabilities <-
  1 - confidence_levels

# ============================================================
# Historical Simulation VaR
# ============================================================

historical_var_returns <- vapply(
  tail_probabilities,
  function(alpha) {
    
    empirical_quantile <- stats::quantile(
      portfolio_return_series,
      probs = alpha,
      na.rm = TRUE,
      type = 1
    )
    
    -as.numeric(empirical_quantile)
  },
  FUN.VALUE = numeric(1)
)

historical_var_results <- tibble::tibble(
  model = "Historical Simulation",
  confidence_level = confidence_levels,
  tail_probability = tail_probabilities,
  var_return = historical_var_returns,
  var_dollar =
    portfolio_value *
    historical_var_returns
)

# ============================================================
# Build asset return matrix
# ============================================================

asset_returns_wide_var <- asset_returns |>
  dplyr::select(
    date,
    ticker,
    simple_return
  ) |>
  tidyr::pivot_wider(
    names_from = ticker,
    values_from = simple_return
  ) |>
  dplyr::arrange(date) |>
  dplyr::select(
    date,
    dplyr::all_of(
      bank_information$ticker
    )
  )

asset_return_matrix_var <- asset_returns_wide_var |>
  dplyr::select(
    -date
  ) |>
  as.matrix()

# ============================================================
# Align portfolio weights
# ============================================================

weights_var <- portfolio_definition$weight[
  match(
    bank_information$ticker,
    portfolio_definition$ticker
  )
]

names(weights_var) <-
  bank_information$ticker

if (anyNA(weights_var)) {
  stop(
    "Portfolio weights could not be aligned with asset returns."
  )
}

if (
  abs(
    sum(weights_var) - 1
  ) > 1e-10
) {
  stop(
    "Portfolio weights do not sum to 1."
  )
}

# ============================================================
# Asset means and covariance matrix
# ============================================================

asset_mean_vector <-
  colMeans(
    asset_return_matrix_var
  )

asset_covariance_matrix <-
  stats::cov(
    asset_return_matrix_var
  )

# ============================================================
# Portfolio mean and volatility using
# variance-covariance method
# ============================================================

portfolio_mean_vcv <-
  sum(
    weights_var *
      asset_mean_vector
  )

portfolio_variance_vcv <-
  as.numeric(
    t(weights_var) %*%
      asset_covariance_matrix %*%
      weights_var
  )

portfolio_volatility_vcv <-
  sqrt(
    portfolio_variance_vcv
  )

# ============================================================
# Direct portfolio statistics for validation
# ============================================================

portfolio_mean_direct <-
  mean(
    portfolio_return_series
  )

portfolio_volatility_direct <-
  stats::sd(
    portfolio_return_series
  )

mean_difference <-
  abs(
    portfolio_mean_vcv -
      portfolio_mean_direct
  )

volatility_difference <-
  abs(
    portfolio_volatility_vcv -
      portfolio_volatility_direct
  )

# Allow only very small floating-point differences
if (mean_difference > 1e-10) {
  stop(
    "Portfolio mean does not reconcile with asset-level calculation."
  )
}

if (volatility_difference > 1e-10) {
  stop(
    "Portfolio volatility does not reconcile with covariance calculation."
  )
}

# ============================================================
# Parametric Gaussian VaR
# ============================================================

parametric_var_returns <- vapply(
  tail_probabilities,
  function(alpha) {
    
    left_tail_threshold <-
      portfolio_mean_vcv +
      stats::qnorm(alpha) *
      portfolio_volatility_vcv
    
    -left_tail_threshold
  },
  FUN.VALUE = numeric(1)
)

parametric_var_results <- tibble::tibble(
  model = "Parametric Gaussian",
  confidence_level = confidence_levels,
  tail_probability = tail_probabilities,
  var_return = parametric_var_returns,
  var_dollar =
    portfolio_value *
    parametric_var_returns
)

# ============================================================
# Combine VaR estimates
# ============================================================

var_results <- dplyr::bind_rows(
  historical_var_results,
  parametric_var_results
) |>
  dplyr::arrange(
    confidence_level,
    model
  )

# ============================================================
# Parametric model inputs
# ============================================================

parametric_var_inputs <- tibble::tibble(
  statistic = c(
    "Daily portfolio mean",
    "Daily portfolio volatility",
    "Annualized portfolio volatility",
    "Portfolio value"
  ),
  
  value = c(
    portfolio_mean_vcv,
    portfolio_volatility_vcv,
    portfolio_volatility_vcv *
      sqrt(trading_days_per_year),
    portfolio_value
  )
)

# ============================================================
# Save covariance matrix
# ============================================================

covariance_table <-
  asset_covariance_matrix |>
  as.data.frame() |>
  tibble::rownames_to_column(
    var = "ticker"
  )

# ============================================================
# Save outputs
# ============================================================

readr::write_csv(
  var_results,
  var_results_path
)

readr::write_csv(
  parametric_var_inputs,
  parametric_inputs_path
)

readr::write_csv(
  covariance_table,
  covariance_matrix_path
)

# ============================================================
# Display results
# ============================================================

print(
  var_results |>
    dplyr::mutate(
      confidence_level =
        scales::percent(
          confidence_level,
          accuracy = 1
        ),
      
      var_return =
        scales::percent(
          var_return,
          accuracy = 0.01
        ),
      
      var_dollar =
        scales::dollar(
          var_dollar,
          prefix = "CAD $"
        )
    )
)

cat(
  "\nDaily portfolio mean:",
  scales::percent(
    portfolio_mean_vcv,
    accuracy = 0.001
  ),
  "\n"
)

cat(
  "Daily portfolio volatility:",
  scales::percent(
    portfolio_volatility_vcv,
    accuracy = 0.001
  ),
  "\n"
)

cat(
  "Mean reconciliation difference:",
  format(
    mean_difference,
    scientific = TRUE
  ),
  "\n"
)

cat(
  "Volatility reconciliation difference:",
  format(
    volatility_difference,
    scientific = TRUE
  ),
  "\n"
)

message(
  "Historical and Parametric VaR estimation completed successfully."
)
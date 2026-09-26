# ============================================================
# Canadian Bank Market Risk VaR Engine
# Descriptive risk analysis
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

# Load returns if needed
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

portfolio_risk_summary_path <- here::here(
  "output",
  "tables",
  "portfolio_risk_summary.csv"
)

asset_risk_summary_path <- here::here(
  "output",
  "tables",
  "asset_risk_summary.csv"
)

correlation_matrix_path <- here::here(
  "output",
  "tables",
  "asset_return_correlations.csv"
)

worst_days_path <- here::here(
  "output",
  "tables",
  "worst_portfolio_days.csv"
)

rolling_volatility_path <- here::here(
  "data",
  "processed",
  "rolling_volatility.rds"
)

# ============================================================
# Helper functions
# ============================================================

calculate_skewness <- function(x) {
  
  x <- x[is.finite(x)]
  
  n <- length(x)
  
  if (n < 3) {
    return(NA_real_)
  }
  
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  
  if (sd_x == 0) {
    return(NA_real_)
  }
  
  (
    n / ((n - 1) * (n - 2))
  ) *
    sum(
      ((x - mean_x) / sd_x)^3
    )
}


calculate_excess_kurtosis <- function(x) {
  
  x <- x[is.finite(x)]
  
  n <- length(x)
  
  if (n < 4) {
    return(NA_real_)
  }
  
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  
  if (sd_x == 0) {
    return(NA_real_)
  }
  
  standardized_fourth_moment <-
    sum(
      ((x - mean_x) / sd_x)^4
    )
  
  (
    n * (n + 1) /
      ((n - 1) * (n - 2) * (n - 3))
  ) *
    standardized_fourth_moment -
    (
      3 * (n - 1)^2 /
        ((n - 2) * (n - 3))
    )
}


calculate_max_drawdown <- function(simple_returns) {
  
  wealth_index <- cumprod(
    1 + simple_returns
  )
  
  running_peak <- cummax(
    wealth_index
  )
  
  drawdown <- (
    wealth_index /
      running_peak
  ) - 1
  
  min(drawdown)
}

# ============================================================
# Portfolio descriptive statistics
# ============================================================

portfolio_simple_returns <-
  portfolio_returns$portfolio_simple_return

number_of_portfolio_returns <-
  length(portfolio_simple_returns)

annualized_geometric_return <-
  prod(
    1 + portfolio_simple_returns
  )^(
    trading_days_per_year /
      number_of_portfolio_returns
  ) - 1

annualized_volatility <-
  stats::sd(
    portfolio_simple_returns
  ) *
  sqrt(
    trading_days_per_year
  )

portfolio_risk_summary <- tibble::tibble(
  
  observations =
    number_of_portfolio_returns,
  
  start_date =
    min(portfolio_returns$date),
  
  end_date =
    max(portfolio_returns$date),
  
  mean_daily_return =
    mean(portfolio_simple_returns),
  
  daily_volatility =
    stats::sd(portfolio_simple_returns),
  
  annualized_return =
    annualized_geometric_return,
  
  annualized_volatility =
    annualized_volatility,
  
  skewness =
    calculate_skewness(
      portfolio_simple_returns
    ),
  
  excess_kurtosis =
    calculate_excess_kurtosis(
      portfolio_simple_returns
    ),
  
  minimum_daily_return =
    min(portfolio_simple_returns),
  
  maximum_daily_return =
    max(portfolio_simple_returns),
  
  maximum_drawdown =
    calculate_max_drawdown(
      portfolio_simple_returns
    )
)

# ============================================================
# Asset-level risk statistics
# ============================================================

asset_risk_summary <- asset_returns |>
  dplyr::group_by(
    ticker,
    bank_name
  ) |>
  dplyr::summarise(
    
    observations =
      dplyr::n(),
    
    mean_daily_return =
      mean(simple_return),
    
    daily_volatility =
      stats::sd(simple_return),
    
    annualized_volatility =
      stats::sd(simple_return) *
      sqrt(trading_days_per_year),
    
    skewness =
      calculate_skewness(simple_return),
    
    excess_kurtosis =
      calculate_excess_kurtosis(
        simple_return
      ),
    
    minimum_daily_return =
      min(simple_return),
    
    maximum_daily_return =
      max(simple_return),
    
    .groups = "drop"
  )

# ============================================================
# Correlation matrix
# ============================================================

asset_returns_wide <- asset_returns |>
  dplyr::select(
    date,
    ticker,
    simple_return
  ) |>
  tidyr::pivot_wider(
    names_from = ticker,
    values_from = simple_return
  ) |>
  dplyr::arrange(date)

correlation_matrix <-
  stats::cor(
    asset_returns_wide |>
      dplyr::select(
        dplyr::all_of(
          bank_information$ticker
        )
      ),
    use = "complete.obs"
  )

correlation_table <-
  correlation_matrix |>
  as.data.frame() |>
  tibble::rownames_to_column(
    var = "ticker"
  )

# ============================================================
# Rolling volatility
# ============================================================

rolling_volatility <- portfolio_returns |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    
    rolling_21d_volatility =
      zoo::rollapplyr(
        portfolio_simple_return,
        width = 21,
        FUN = stats::sd,
        fill = NA,
        na.rm = TRUE
      ) *
      sqrt(trading_days_per_year),
    
    rolling_63d_volatility =
      zoo::rollapplyr(
        portfolio_simple_return,
        width = 63,
        FUN = stats::sd,
        fill = NA,
        na.rm = TRUE
      ) *
      sqrt(trading_days_per_year)
  )

# ============================================================
# Identify worst portfolio days
# ============================================================

worst_portfolio_days <- portfolio_returns |>
  dplyr::arrange(
    portfolio_simple_return
  ) |>
  dplyr::slice_head(
    n = 10
  ) |>
  dplyr::select(
    date,
    portfolio_simple_return,
    portfolio_pnl
  )

best_portfolio_days <- portfolio_returns |>
  dplyr::arrange(
    dplyr::desc(
      portfolio_simple_return
    )
  ) |>
  dplyr::slice_head(
    n = 10
  ) |>
  dplyr::select(
    date,
    portfolio_simple_return,
    portfolio_pnl
  )

# ============================================================
# Save outputs
# ============================================================

readr::write_csv(
  portfolio_risk_summary,
  portfolio_risk_summary_path
)

readr::write_csv(
  asset_risk_summary,
  asset_risk_summary_path
)

readr::write_csv(
  correlation_table,
  correlation_matrix_path
)

readr::write_csv(
  worst_portfolio_days,
  worst_days_path
)

saveRDS(
  rolling_volatility,
  rolling_volatility_path
)

# ============================================================
# Display results
# ============================================================

print(
  portfolio_risk_summary
)

print(
  asset_risk_summary
)

cat(
  "\nAverage pairwise asset correlation:",
  round(
    mean(
      correlation_matrix[
        upper.tri(
          correlation_matrix
        )
      ]
    ),
    3
  ),
  "\n"
)

message(
  "Descriptive risk analysis completed successfully."
)
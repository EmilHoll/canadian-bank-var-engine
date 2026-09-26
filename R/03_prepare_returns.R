# ============================================================
# Canadian Bank Market Risk VaR Engine
# Prepare individual and portfolio returns
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

# Load market data if needed
if (!exists("raw_bank_prices")) {
  source(
    here::here(
      "R",
      "02_download_data.R"
    )
  )
}

# ============================================================
# Output paths
# ============================================================

asset_returns_rds_path <- here::here(
  "data",
  "processed",
  "asset_returns.rds"
)

asset_returns_csv_path <- here::here(
  "data",
  "processed",
  "asset_returns.csv"
)

portfolio_returns_rds_path <- here::here(
  "data",
  "processed",
  "portfolio_returns.rds"
)

portfolio_returns_csv_path <- here::here(
  "data",
  "processed",
  "portfolio_returns.csv"
)

return_contributions_rds_path <- here::here(
  "data",
  "processed",
  "portfolio_return_contributions.rds"
)

return_contributions_csv_path <- here::here(
  "data",
  "processed",
  "portfolio_return_contributions.csv"
)

return_quality_path <- here::here(
  "output",
  "tables",
  "return_data_quality.csv"
)

return_alignment_path <- here::here(
  "output",
  "tables",
  "return_alignment_summary.csv"
)

# ============================================================
# Validate price data
# ============================================================

required_price_columns <- c(
  "ticker",
  "bank_name",
  "date",
  "adjusted"
)

missing_price_columns <- setdiff(
  required_price_columns,
  names(raw_bank_prices)
)

if (length(missing_price_columns) > 0) {
  stop(
    paste(
      "The following required price columns are missing:",
      paste(missing_price_columns, collapse = ", ")
    )
  )
}

# ============================================================
# Calculate individual stock returns
# ============================================================

individual_returns_unaligned <- raw_bank_prices |>
  dplyr::select(
    ticker,
    bank_name,
    date,
    adjusted
  ) |>
  dplyr::arrange(
    ticker,
    date
  ) |>
  dplyr::group_by(
    ticker,
    bank_name
  ) |>
  dplyr::mutate(
    previous_adjusted = dplyr::lag(adjusted),
    
    simple_return =
      adjusted / previous_adjusted - 1,
    
    log_return =
      log1p(simple_return)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    !is.na(previous_adjusted)
  )

# ============================================================
# Validate individual returns
# ============================================================

if (any(!is.finite(individual_returns_unaligned$simple_return))) {
  stop(
    "Non-finite simple returns were detected."
  )
}

if (any(!is.finite(individual_returns_unaligned$log_return))) {
  stop(
    "Non-finite log returns were detected."
  )
}

if (any(individual_returns_unaligned$simple_return <= -1)) {
  stop(
    paste(
      "A simple return less than or equal to -100%",
      "was detected."
    )
  )
}

# ============================================================
# Reshape returns into a common-date panel
# ============================================================

wide_simple_returns <- individual_returns_unaligned |>
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

# Ensure all expected ticker columns exist
missing_return_columns <- setdiff(
  bank_information$ticker,
  names(wide_simple_returns)
)

if (length(missing_return_columns) > 0) {
  stop(
    paste(
      "Return columns are missing for:",
      paste(missing_return_columns, collapse = ", ")
    )
  )
}

# Keep only dates on which all six stocks have returns
wide_simple_returns <- wide_simple_returns |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(bank_information$ticker),
      ~ !is.na(.x)
    )
  ) |>
  dplyr::select(
    date,
    dplyr::all_of(bank_information$ticker)
  )

if (nrow(wide_simple_returns) == 0) {
  stop(
    "No common return dates were found."
  )
}

# ============================================================
# Create aligned long-format asset returns
# ============================================================

common_return_dates <- wide_simple_returns |>
  dplyr::select(date)

asset_returns <- individual_returns_unaligned |>
  dplyr::semi_join(
    common_return_dates,
    by = "date"
  ) |>
  dplyr::arrange(
    date,
    ticker
  )

# Every common date should have one observation per asset
observations_per_date <- asset_returns |>
  dplyr::count(
    date,
    name = "number_of_assets"
  )

if (
  any(
    observations_per_date$number_of_assets !=
    number_of_assets
  )
) {
  stop(
    "Some aligned dates do not contain all portfolio assets."
  )
}

# ============================================================
# Align portfolio weights with return columns
# ============================================================

portfolio_weights_aligned <- portfolio_definition$weight[
  match(
    bank_information$ticker,
    portfolio_definition$ticker
  )
]

names(portfolio_weights_aligned) <-
  bank_information$ticker

if (anyNA(portfolio_weights_aligned)) {
  stop(
    "Portfolio weights could not be matched to all tickers."
  )
}

if (
  abs(
    sum(portfolio_weights_aligned) - 1
  ) > 1e-10
) {
  stop(
    "Aligned portfolio weights do not sum to 1."
  )
}

# ============================================================
# Calculate portfolio returns
# ============================================================

asset_return_matrix <- wide_simple_returns |>
  dplyr::select(
    dplyr::all_of(bank_information$ticker)
  ) |>
  as.matrix()

portfolio_simple_return <- as.numeric(
  asset_return_matrix %*%
    portfolio_weights_aligned
)

portfolio_returns <- tibble::tibble(
  date = wide_simple_returns$date,
  
  portfolio_simple_return =
    portfolio_simple_return,
  
  portfolio_log_return =
    log1p(portfolio_simple_return),
  
  portfolio_pnl =
    portfolio_value *
    portfolio_simple_return
) |>
  dplyr::mutate(
    cumulative_wealth_index =
      cumprod(
        1 + portfolio_simple_return
      ),
    
    cumulative_return =
      cumulative_wealth_index - 1
  )

# ============================================================
# Calculate asset contributions to portfolio return
# ============================================================

portfolio_return_contributions <- asset_returns |>
  dplyr::left_join(
    portfolio_definition |>
      dplyr::select(
        ticker,
        weight
      ),
    by = "ticker"
  ) |>
  dplyr::mutate(
    weighted_return_contribution =
      weight * simple_return,
    
    dollar_pnl_contribution =
      portfolio_value *
      weighted_return_contribution
  ) |>
  dplyr::select(
    date,
    ticker,
    bank_name,
    weight,
    simple_return,
    log_return,
    weighted_return_contribution,
    dollar_pnl_contribution
  )

# ============================================================
# Validate portfolio-return calculations
# ============================================================

if (
  any(
    !is.finite(
      portfolio_returns$portfolio_simple_return
    )
  )
) {
  stop(
    "Non-finite portfolio returns were detected."
  )
}

if (
  any(
    portfolio_returns$portfolio_simple_return <= -1
  )
) {
  stop(
    "A portfolio return less than or equal to -100% was detected."
  )
}

contribution_check <- portfolio_return_contributions |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    contribution_sum =
      sum(weighted_return_contribution),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    portfolio_returns |>
      dplyr::select(
        date,
        portfolio_simple_return
      ),
    by = "date"
  ) |>
  dplyr::mutate(
    calculation_difference =
      contribution_sum -
      portfolio_simple_return
  )

maximum_contribution_difference <- max(
  abs(
    contribution_check$calculation_difference
  )
)

if (maximum_contribution_difference > 1e-12) {
  stop(
    paste(
      "Portfolio-return contributions do not reconcile.",
      "Maximum difference:",
      maximum_contribution_difference
    )
  )
}

# ============================================================
# Return data-quality summary
# ============================================================

return_quality_summary <- asset_returns |>
  dplyr::group_by(
    ticker,
    bank_name
  ) |>
  dplyr::summarise(
    first_return_date = min(date),
    last_return_date = max(date),
    number_of_returns = dplyr::n(),
    missing_simple_returns =
      sum(is.na(simple_return)),
    missing_log_returns =
      sum(is.na(log_return)),
    minimum_simple_return =
      min(simple_return),
    maximum_simple_return =
      max(simple_return),
    .groups = "drop"
  )

number_of_candidate_return_dates <-
  dplyr::n_distinct(
    individual_returns_unaligned$date
  )

number_of_common_return_dates <-
  nrow(wide_simple_returns)

return_alignment_summary <- tibble::tibble(
  number_of_assets =
    number_of_assets,
  
  candidate_return_dates =
    number_of_candidate_return_dates,
  
  common_return_dates =
    number_of_common_return_dates,
  
  dates_removed_during_alignment =
    number_of_candidate_return_dates -
    number_of_common_return_dates
)

# ============================================================
# Save processed datasets
# ============================================================

saveRDS(
  asset_returns,
  asset_returns_rds_path
)

readr::write_csv(
  asset_returns,
  asset_returns_csv_path
)

saveRDS(
  portfolio_returns,
  portfolio_returns_rds_path
)

readr::write_csv(
  portfolio_returns,
  portfolio_returns_csv_path
)

saveRDS(
  portfolio_return_contributions,
  return_contributions_rds_path
)

readr::write_csv(
  portfolio_return_contributions,
  return_contributions_csv_path
)

readr::write_csv(
  return_quality_summary,
  return_quality_path
)

readr::write_csv(
  return_alignment_summary,
  return_alignment_path
)

# ============================================================
# Display results
# ============================================================

print(return_alignment_summary)

print(return_quality_summary)

cat(
  "\nPortfolio return observations:",
  scales::comma(
    nrow(portfolio_returns)
  ),
  "\n"
)

cat(
  "First portfolio return date:",
  format(
    min(portfolio_returns$date)
  ),
  "\n"
)

cat(
  "Last portfolio return date:",
  format(
    max(portfolio_returns$date)
  ),
  "\n"
)

cat(
  "Minimum daily portfolio return:",
  scales::percent(
    min(
      portfolio_returns$portfolio_simple_return
    ),
    accuracy = 0.01
  ),
  "\n"
)

cat(
  "Maximum daily portfolio return:",
  scales::percent(
    max(
      portfolio_returns$portfolio_simple_return
    ),
    accuracy = 0.01
  ),
  "\n"
)

cat(
  "Maximum contribution reconciliation difference:",
  format(
    maximum_contribution_difference,
    scientific = TRUE
  ),
  "\n"
)

message(
  "Return preparation completed successfully."
)
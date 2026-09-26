# ============================================================
# Canadian Bank Market Risk VaR Engine
# Portfolio definition and risk parameters
# ============================================================

# Canadian bank universe
bank_information <- tibble::tibble(
  ticker = c(
    "RY.TO",
    "TD.TO",
    "BMO.TO",
    "BNS.TO",
    "CM.TO",
    "NA.TO"
  ),
  bank_name = c(
    "Royal Bank of Canada",
    "Toronto-Dominion Bank",
    "Bank of Montreal",
    "Bank of Nova Scotia",
    "Canadian Imperial Bank of Commerce",
    "National Bank of Canada"
  )
)

# Number of securities
number_of_assets <- nrow(bank_information)

# Equal portfolio weights
portfolio_weights <- rep(
  1 / number_of_assets,
  number_of_assets
)

names(portfolio_weights) <- bank_information$ticker

# Main portfolio assumptions
portfolio_value <- 1000000

confidence_levels <- c(
  0.95,
  0.99
)

var_horizon_days <- 1

trading_days_per_year <- 252

data_start_date <- as.Date("2015-01-01")

data_end_date <- Sys.Date()

return_method <- "log"

# Add portfolio information to the bank table
portfolio_definition <- bank_information |>
  dplyr::mutate(
    weight = portfolio_weights[ticker],
    position_value = portfolio_value * weight
  )

# Store model assumptions in one object
risk_parameters <- list(
  portfolio_value = portfolio_value,
  confidence_levels = confidence_levels,
  var_horizon_days = var_horizon_days,
  trading_days_per_year = trading_days_per_year,
  data_start_date = data_start_date,
  data_end_date = data_end_date,
  return_method = return_method
)

# ============================================================
# Validation checks
# ============================================================

if (anyDuplicated(portfolio_definition$ticker) > 0) {
  stop("Duplicate ticker symbols were detected.")
}

if (any(portfolio_definition$weight < 0)) {
  stop("Portfolio weights cannot be negative in the baseline model.")
}

if (abs(sum(portfolio_definition$weight) - 1) > 1e-10) {
  stop("Portfolio weights must sum to 1.")
}

if (portfolio_value <= 0) {
  stop("Portfolio value must be positive.")
}

if (any(confidence_levels <= 0 | confidence_levels >= 1)) {
  stop("Confidence levels must be between 0 and 1.")
}

if (data_start_date >= data_end_date) {
  stop("The data start date must be earlier than the end date.")
}

message("Portfolio definition loaded successfully.")

print(portfolio_definition)

cat(
  "\nTotal portfolio weight:",
  sum(portfolio_definition$weight),
  "\n"
)

cat(
  "Total portfolio value:",
  scales::dollar(
    sum(portfolio_definition$position_value),
    prefix = "CAD $"
  ),
  "\n"
)
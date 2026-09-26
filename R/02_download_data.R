# ============================================================
# Canadian Bank Market Risk VaR Engine
# Download and validate historical market data
# ============================================================

# Load project configuration if it is not already available
if (!exists("required_packages")) {
  source(
    here::here(
      "R",
      "00_project_setup.R"
    )
  )
}

# Load portfolio definition if it is not already available
if (!exists("portfolio_definition")) {
  source(
    here::here(
      "R",
      "01_define_portfolio.R"
    )
  )
}

# ============================================================
# File paths
# ============================================================

raw_rds_path <- here::here(
  "data",
  "raw",
  "canadian_bank_prices.rds"
)

raw_csv_path <- here::here(
  "data",
  "raw",
  "canadian_bank_prices.csv"
)

quality_summary_path <- here::here(
  "output",
  "tables",
  "price_data_quality.csv"
)

metadata_path <- here::here(
  "data",
  "raw",
  "download_metadata.csv"
)

# Set this to TRUE only when you want to download fresh data
refresh_market_data <- FALSE

# ============================================================
# Function for downloading one stock
# ============================================================

download_stock_data <- function(
    symbol,
    start_date,
    end_date
) {
  
  message("Downloading ", symbol, "...")
  
  stock_data <- tryCatch(
    {
      tidyquant::tq_get(
        x = symbol,
        get = "stock.prices",
        from = as.character(start_date),
        to = as.character(end_date),
        complete_cases = FALSE
      )
    },
    error = function(error_message) {
      stop(
        paste0(
          "Download failed for ",
          symbol,
          ": ",
          conditionMessage(error_message)
        ),
        call. = FALSE
      )
    }
  )
  
  if (!is.data.frame(stock_data) || nrow(stock_data) == 0) {
    stop(
      paste0(
        "No observations were returned for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  # tq_get may omit the symbol column when one ticker is requested
  if ("symbol" %in% names(stock_data)) {
    stock_data <- stock_data |>
      dplyr::rename(
        ticker = symbol
      )
  } else {
    stock_data <- stock_data |>
      dplyr::mutate(
        ticker = symbol,
        .before = 1
      )
  }
  
  stock_data
}

# ============================================================
# Download or load cached market data
# ============================================================

if (
  file.exists(raw_rds_path) &&
  !refresh_market_data
) {
  
  message("Loading previously downloaded market data.")
  
  raw_bank_prices <- readRDS(
    raw_rds_path
  )
  
} else {
  
  message("Downloading market data from Yahoo Finance.")
  
  raw_bank_prices <- purrr::map_dfr(
    bank_information$ticker,
    download_stock_data,
    start_date = data_start_date,
    end_date = data_end_date
  )
  
  raw_bank_prices <- raw_bank_prices |>
    dplyr::left_join(
      bank_information,
      by = "ticker"
    ) |>
    dplyr::select(
      ticker,
      bank_name,
      date,
      open,
      high,
      low,
      close,
      volume,
      adjusted
    ) |>
    dplyr::arrange(
      ticker,
      date
    )
  
  # Save the raw data in two formats
  saveRDS(
    raw_bank_prices,
    raw_rds_path
  )
  
  readr::write_csv(
    raw_bank_prices,
    raw_csv_path
  )
  
  # Record information about the download
  download_metadata <- tibble::tibble(
    download_timestamp_utc = format(
      Sys.time(),
      tz = "UTC",
      usetz = TRUE
    ),
    data_source = "Yahoo Finance via tidyquant::tq_get",
    requested_start_date = data_start_date,
    requested_end_date = data_end_date,
    number_of_tickers = length(bank_information$ticker)
  )
  
  readr::write_csv(
    download_metadata,
    metadata_path
  )
}

# ============================================================
# Validate the downloaded data
# ============================================================

expected_columns <- c(
  "ticker",
  "bank_name",
  "date",
  "open",
  "high",
  "low",
  "close",
  "volume",
  "adjusted"
)

missing_columns <- setdiff(
  expected_columns,
  names(raw_bank_prices)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "The following required columns are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Ensure date is stored as a Date object
raw_bank_prices <- raw_bank_prices |>
  dplyr::mutate(
    date = as.Date(date)
  ) |>
  dplyr::arrange(
    ticker,
    date
  )

# Check that every requested ticker was downloaded
downloaded_tickers <- unique(
  raw_bank_prices$ticker
)

missing_tickers <- setdiff(
  bank_information$ticker,
  downloaded_tickers
)

if (length(missing_tickers) > 0) {
  stop(
    paste(
      "No data were obtained for:",
      paste(missing_tickers, collapse = ", ")
    )
  )
}

# Check for duplicate ticker-date observations
duplicate_observations <- raw_bank_prices |>
  dplyr::count(
    ticker,
    date,
    name = "number_of_rows"
  ) |>
  dplyr::filter(
    number_of_rows > 1
  )

if (nrow(duplicate_observations) > 0) {
  stop(
    "Duplicate ticker-date observations were detected."
  )
}

# Adjusted prices are required for return calculations
if (anyNA(raw_bank_prices$adjusted)) {
  stop(
    "Missing adjusted prices were detected."
  )
}

if (any(raw_bank_prices$adjusted <= 0)) {
  stop(
    "Non-positive adjusted prices were detected."
  )
}

# ============================================================
# Create data-quality summary
# ============================================================

data_quality_summary <- raw_bank_prices |>
  dplyr::group_by(
    ticker,
    bank_name
  ) |>
  dplyr::summarise(
    first_observation = min(date),
    last_observation = max(date),
    number_of_observations = dplyr::n(),
    missing_adjusted_prices = sum(is.na(adjusted)),
    duplicate_dates = dplyr::n() -
      dplyr::n_distinct(date),
    .groups = "drop"
  )

readr::write_csv(
  data_quality_summary,
  quality_summary_path
)

# ============================================================
# Check whether all stocks share the same dates
# ============================================================

daily_coverage <- raw_bank_prices |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    number_of_available_assets =
      dplyr::n_distinct(ticker),
    .groups = "drop"
  )

incomplete_market_dates <- daily_coverage |>
  dplyr::filter(
    number_of_available_assets < number_of_assets
  )

if (nrow(incomplete_market_dates) > 0) {
  warning(
    paste(
      nrow(incomplete_market_dates),
      "dates do not contain observations for all portfolio assets.",
      "These dates will be handled during return preparation."
    )
  )
}

# ============================================================
# Final output
# ============================================================

print(data_quality_summary)

cat(
  "\nTotal observations:",
  scales::comma(nrow(raw_bank_prices)),
  "\n"
)

cat(
  "Overall date range:",
  format(min(raw_bank_prices$date)),
  "to",
  format(max(raw_bank_prices$date)),
  "\n"
)

cat(
  "Incomplete market dates:",
  nrow(incomplete_market_dates),
  "\n"
)

message(
  "Market-data download and validation completed successfully."
)
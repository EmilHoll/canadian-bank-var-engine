# ============================================================
# Canadian Bank Market Risk VaR Engine
# Historical and Hypothetical Stress Testing
# ============================================================


# ============================================================
# Load project setup if needed
# ============================================================

if (!exists("required_packages")) {
  source(
    here::here(
      "R",
      "00_project_setup.R"
    )
  )
}


# ============================================================
# Load portfolio definition if needed
# ============================================================

if (!exists("portfolio_definition")) {
  source(
    here::here(
      "R",
      "01_define_portfolio.R"
    )
  )
}


# ============================================================
# Load prepared returns if needed
# ============================================================

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

historical_stress_path <- here::here(
  "output",
  "tables",
  "historical_stress_results.csv"
)

hypothetical_stress_path <- here::here(
  "output",
  "tables",
  "hypothetical_stress_results.csv"
)

hypothetical_contributions_path <- here::here(
  "output",
  "tables",
  "hypothetical_stress_contributions.csv"
)

extreme_event_path <- here::here(
  "output",
  "tables",
  "empirical_extreme_events.csv"
)

historical_stress_series_path <- here::here(
  "data",
  "processed",
  "historical_stress_series.rds"
)


# ============================================================
# Helper function: maximum drawdown
# ============================================================

stress_max_drawdown <- function(
    simple_returns
) {
  
  wealth_index <-
    cumprod(
      1 + simple_returns
    )
  
  running_peak <-
    cummax(
      c(
        1,
        wealth_index
      )
    )[-1]
  
  drawdowns <-
    wealth_index /
    running_peak -
    1
  
  min(
    drawdowns
  )
}


# ============================================================
# Historical stress-scenario definitions
# ============================================================

historical_scenarios <- tibble::tribble(
  
  ~scenario,
  ~requested_start_date,
  ~requested_end_date,
  
  "COVID-19 Market Shock",
  as.Date("2020-02-19"),
  as.Date("2020-03-23"),
  
  "2022 Tightening Selloff",
  as.Date("2022-02-09"),
  as.Date("2022-06-16"),
  
  "2023 Banking Stress",
  as.Date("2023-03-08"),
  as.Date("2023-03-24")
)


# ============================================================
# Historical stress calculations
# ============================================================

historical_stress_results <-
  purrr::map_dfr(
    seq_len(
      nrow(
        historical_scenarios
      )
    ),
    function(i) {
      
      scenario_name <-
        historical_scenarios$scenario[i]
      
      start_date <-
        historical_scenarios$
        requested_start_date[i]
      
      end_date <-
        historical_scenarios$
        requested_end_date[i]
      
      scenario_data <-
        portfolio_returns |>
        dplyr::filter(
          date >= start_date,
          date <= end_date
        ) |>
        dplyr::arrange(
          date
        )
      
      if (
        nrow(
          scenario_data
        ) == 0
      ) {
        stop(
          paste(
            "No observations were found for:",
            scenario_name
          )
        )
      }
      
      cumulative_return <-
        prod(
          1 +
            scenario_data$
            portfolio_simple_return
        ) - 1
      
      scenario_pnl <-
        portfolio_value *
        cumulative_return
      
      maximum_drawdown <-
        stress_max_drawdown(
          scenario_data$
            portfolio_simple_return
        )
      
      realized_volatility <-
        stats::sd(
          scenario_data$
            portfolio_simple_return
        ) *
        sqrt(
          trading_days_per_year
        )
      
      tibble::tibble(
        
        scenario =
          scenario_name,
        
        start_date =
          min(
            scenario_data$date
          ),
        
        end_date =
          max(
            scenario_data$date
          ),
        
        trading_days =
          nrow(
            scenario_data
          ),
        
        cumulative_return =
          cumulative_return,
        
        portfolio_pnl =
          scenario_pnl,
        
        stress_loss =
          pmax(
            -scenario_pnl,
            0
          ),
        
        worst_daily_return =
          min(
            scenario_data$
              portfolio_simple_return
          ),
        
        best_daily_return =
          max(
            scenario_data$
              portfolio_simple_return
          ),
        
        maximum_drawdown =
          maximum_drawdown,
        
        annualized_realized_volatility =
          realized_volatility
      )
    }
  )


# ============================================================
# Build historical stress-series dataset
# ============================================================

historical_stress_series <-
  purrr::map_dfr(
    seq_len(
      nrow(
        historical_scenarios
      )
    ),
    function(i) {
      
      portfolio_returns |>
        dplyr::filter(
          
          date >=
            historical_scenarios$
            requested_start_date[i],
          
          date <=
            historical_scenarios$
            requested_end_date[i]
        ) |>
        dplyr::arrange(
          date
        ) |>
        dplyr::mutate(
          
          scenario =
            historical_scenarios$
            scenario[i],
          
          scenario_wealth_index =
            cumprod(
              1 +
                portfolio_simple_return
            ),
          
          scenario_cumulative_return =
            scenario_wealth_index -
            1
        )
    }
  )


# ============================================================
# Empirical extreme-event analysis
# ============================================================

# Worst single day

worst_one_day <-
  portfolio_returns |>
  dplyr::slice_min(
    order_by =
      portfolio_simple_return,
    n = 1,
    with_ties = FALSE
  )

# Rolling 5-day compounded return

five_day_stress <-
  portfolio_returns |>
  dplyr::arrange(
    date
  ) |>
  dplyr::mutate(
    
    rolling_5d_return =
      zoo::rollapplyr(
        
        portfolio_simple_return,
        
        width = 5,
        
        FUN = function(x) {
          prod(
            1 + x
          ) - 1
        },
        
        fill = NA_real_
      )
  )

worst_five_day <-
  five_day_stress |>
  dplyr::filter(
    !is.na(
      rolling_5d_return
    )
  ) |>
  dplyr::slice_min(
    order_by =
      rolling_5d_return,
    n = 1,
    with_ties = FALSE
  )


empirical_extreme_events <-
  tibble::tibble(
    
    scenario = c(
      "Worst Historical 1-Day Loss",
      "Worst Historical 5-Day Loss"
    ),
    
    end_date = c(
      worst_one_day$date,
      worst_five_day$date
    ),
    
    stress_return = c(
      worst_one_day$
        portfolio_simple_return,
      
      worst_five_day$
        rolling_5d_return
    )
  ) |>
  dplyr::mutate(
    
    stress_pnl =
      portfolio_value *
      stress_return,
    
    stress_loss =
      pmax(
        -stress_pnl,
        0
      )
  )


# ============================================================
# Hypothetical stress scenarios
# ============================================================

hypothetical_shocks <-
  tibble::tribble(
    
    ~scenario,
    ~ticker,
    ~shock_return,
    
    # ------------------------------------------
    # Scenario 1: broad banking-sector correction
    # ------------------------------------------
    
    "Broad Bank Selloff",
    "RY.TO",
    -0.10,
    
    "Broad Bank Selloff",
    "TD.TO",
    -0.10,
    
    "Broad Bank Selloff",
    "BMO.TO",
    -0.10,
    
    "Broad Bank Selloff",
    "BNS.TO",
    -0.10,
    
    "Broad Bank Selloff",
    "CM.TO",
    -0.10,
    
    "Broad Bank Selloff",
    "NA.TO",
    -0.10,
    
    
    # ------------------------------------------
    # Scenario 2: recession / credit stress
    # ------------------------------------------
    
    "Recession and Credit Stress",
    "RY.TO",
    -0.12,
    
    "Recession and Credit Stress",
    "TD.TO",
    -0.14,
    
    "Recession and Credit Stress",
    "BMO.TO",
    -0.11,
    
    "Recession and Credit Stress",
    "BNS.TO",
    -0.16,
    
    "Recession and Credit Stress",
    "CM.TO",
    -0.13,
    
    "Recession and Credit Stress",
    "NA.TO",
    -0.10,
    
    
    # ------------------------------------------
    # Scenario 3: severe systemic banking shock
    # ------------------------------------------
    
    "Severe Systemic Bank Shock",
    "RY.TO",
    -0.20,
    
    "Severe Systemic Bank Shock",
    "TD.TO",
    -0.20,
    
    "Severe Systemic Bank Shock",
    "BMO.TO",
    -0.20,
    
    "Severe Systemic Bank Shock",
    "BNS.TO",
    -0.20,
    
    "Severe Systemic Bank Shock",
    "CM.TO",
    -0.20,
    
    "Severe Systemic Bank Shock",
    "NA.TO",
    -0.20
  )


# ============================================================
# Validate hypothetical scenarios
# ============================================================

scenario_asset_counts <-
  hypothetical_shocks |>
  dplyr::group_by(
    scenario
  ) |>
  dplyr::summarise(
    
    number_of_assets =
      dplyr::n_distinct(
        ticker
      ),
    
    .groups = "drop"
  )

if (
  any(
    scenario_asset_counts$
    number_of_assets !=
    number_of_assets
  )
) {
  stop(
    paste(
      "Each hypothetical scenario must",
      "contain one shock for every portfolio asset."
    )
  )
}


missing_hypothetical_tickers <-
  setdiff(
    bank_information$ticker,
    unique(
      hypothetical_shocks$ticker
    )
  )

if (
  length(
    missing_hypothetical_tickers
  ) > 0
) {
  stop(
    paste(
      "Missing hypothetical shocks for:",
      paste(
        missing_hypothetical_tickers,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# Calculate hypothetical return contributions
# ============================================================

hypothetical_stress_contributions <-
  hypothetical_shocks |>
  dplyr::left_join(
    
    portfolio_definition |>
      dplyr::select(
        ticker,
        bank_name,
        weight
      ),
    
    by = "ticker"
  ) |>
  dplyr::mutate(
    
    portfolio_return_contribution =
      weight *
      shock_return,
    
    dollar_pnl_contribution =
      portfolio_value *
      portfolio_return_contribution
  ) |>
  dplyr::arrange(
    scenario,
    ticker
  )


# ============================================================
# Aggregate hypothetical stress results
# ============================================================

hypothetical_stress_results <-
  hypothetical_stress_contributions |>
  dplyr::group_by(
    scenario
  ) |>
  dplyr::summarise(
    
    portfolio_stress_return =
      sum(
        portfolio_return_contribution
      ),
    
    portfolio_stress_pnl =
      sum(
        dollar_pnl_contribution
      ),
    
    stress_loss =
      pmax(
        -portfolio_stress_pnl,
        0
      ),
    
    largest_single_asset_loss =
      min(
        dollar_pnl_contribution
      ),
    
    .groups =
      "drop"
  )


# ============================================================
# Validation checks
# ============================================================

if (
  anyNA(
    historical_stress_results
  )
) {
  stop(
    "Missing values were detected in historical stress results."
  )
}


if (
  anyNA(
    hypothetical_stress_results
  )
) {
  stop(
    "Missing values were detected in hypothetical stress results."
  )
}


# Verify contributions reconcile

hypothetical_reconciliation <-
  hypothetical_stress_contributions |>
  dplyr::group_by(
    scenario
  ) |>
  dplyr::summarise(
    
    contribution_total =
      sum(
        portfolio_return_contribution
      ),
    
    .groups = "drop"
  ) |>
  dplyr::left_join(
    
    hypothetical_stress_results |>
      dplyr::select(
        scenario,
        portfolio_stress_return
      ),
    
    by = "scenario"
  ) |>
  dplyr::mutate(
    
    difference =
      contribution_total -
      portfolio_stress_return
  )


maximum_stress_reconciliation_difference <-
  max(
    abs(
      hypothetical_reconciliation$
        difference
    )
  )


if (
  maximum_stress_reconciliation_difference >
  1e-12
) {
  stop(
    "Hypothetical stress contributions do not reconcile."
  )
}


# ============================================================
# Save outputs
# ============================================================

readr::write_csv(
  historical_stress_results,
  historical_stress_path
)

readr::write_csv(
  hypothetical_stress_results,
  hypothetical_stress_path
)

readr::write_csv(
  hypothetical_stress_contributions,
  hypothetical_contributions_path
)

readr::write_csv(
  empirical_extreme_events,
  extreme_event_path
)

saveRDS(
  historical_stress_series,
  historical_stress_series_path
)


# ============================================================
# Display results
# ============================================================

cat(
  "\nHistorical Stress Tests\n"
)

print(
  historical_stress_results |>
    dplyr::mutate(
      
      cumulative_return =
        scales::percent(
          cumulative_return,
          accuracy = 0.01
        ),
      
      portfolio_pnl =
        scales::dollar(
          portfolio_pnl,
          prefix = "CAD $"
        ),
      
      maximum_drawdown =
        scales::percent(
          maximum_drawdown,
          accuracy = 0.01
        )
    )
)


cat(
  "\nEmpirical Extreme Events\n"
)

print(
  empirical_extreme_events |>
    dplyr::mutate(
      
      stress_return =
        scales::percent(
          stress_return,
          accuracy = 0.01
        ),
      
      stress_loss =
        scales::dollar(
          stress_loss,
          prefix = "CAD $"
        )
    )
)


cat(
  "\nHypothetical Stress Tests\n"
)

print(
  hypothetical_stress_results |>
    dplyr::mutate(
      
      portfolio_stress_return =
        scales::percent(
          portfolio_stress_return,
          accuracy = 0.01
        ),
      
      stress_loss =
        scales::dollar(
          stress_loss,
          prefix = "CAD $"
        )
    )
)


message(
  "Stress testing completed successfully."
)
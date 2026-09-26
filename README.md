# Canadian Bank Market Risk VaR Engine

## Overview

This project develops a reproducible market-risk engine in R for an
equal-weight portfolio of six major Canadian banks.

The framework integrates Value at Risk (VaR), Expected Shortfall,
dynamic volatility modelling, out-of-sample VaR backtesting,
statistical model validation, and stress testing.

The project was designed to demonstrate practical quantitative risk
modelling, financial econometrics, model validation, and reproducible
analysis in R.

## Portfolio

The baseline CAD 1 million portfolio contains:

- Royal Bank of Canada (`RY.TO`)
- Toronto-Dominion Bank (`TD.TO`)
- Bank of Montreal (`BMO.TO`)
- Bank of Nova Scotia (`BNS.TO`)
- Canadian Imperial Bank of Commerce (`CM.TO`)
- National Bank of Canada (`NA.TO`)

Each stock initially receives an equal weight.

## Risk Models

The engine implements:

**Value at Risk**
- Historical Simulation
- Parametric Gaussian VaR
- Rolling Gaussian VaR
- EWMA Gaussian VaR
- GARCH(1,1) Gaussian VaR

**Expected Shortfall**
- Historical Expected Shortfall
- Gaussian Expected Shortfall

**Volatility Models**
- EWMA
- GARCH(1,1)

## Model Validation

One-day-ahead VaR forecasts are generated using a rolling
500-trading-day estimation window.

Model calibration is evaluated using:

- VaR violation rates
- Kupiec unconditional-coverage test
- Christoffersen independence test
- Christoffersen conditional-coverage test

This allows the project to evaluate both the frequency and clustering
of VaR exceedances.

## Stress Testing

The engine includes:

- historical stress scenarios
- worst historical one-day loss
- worst historical five-day loss
- hypothetical banking-sector shocks
- security-level stress contributions

## Key Risk-Management Insights

The analysis illustrates that:

- volatility changes materially through time;
- VaR estimates depend on modelling assumptions;
- Expected Shortfall provides information unavailable from VaR alone;
- successful VaR validation requires more than counting violations;
- dynamic volatility models improve responsiveness to changing markets;
- stress losses can substantially exceed ordinary VaR thresholds;
- diversification across individual banks does not eliminate sector concentration;
- failure to reject a model statistically does not prove that the model is correct.

## Repository Structure

```text
R/          Analysis and model scripts
data/       Raw and processed market data
output/     Model outputs and tables
reports/    Final R Markdown risk report

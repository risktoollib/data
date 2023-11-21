
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data Files

<!-- badges: start -->
<!-- badges: end -->

The goal is to provide a centralized marketplace for data in the context
of educational and training material in Energy, Commodities and Finance.

The main data format for dataframes is `feather` to support both R and
Python.

## Finance

- `sp500_desc` - S&P 500 constituents with sector and industry
  classification.
- `sp500_prices` - S&P 500 constituents daily prices from 2010-01-01 to
  2020-12-31.

## Energy

- `esso` - Scraped data from the [Esso
  website](https://essocardlocks.ca/en/sites/) on carlock stations in
  Canada, including location, address, amenities, and distance to city
  centers using [nomatim](https://nominatim.org/) for OpenStreetMap.
- `flyingj` - Scraped data from the [Flying J
  website](https://locations.pilotflyingj.com/search?) on carlock
  stations in Noth America, including location, address, amenities, and
  distance to city centers using [nomatim](https://nominatim.org/) for
  OpenStreetMap.

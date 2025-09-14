library(tidyverse)
library(tidyquant)
tick <- c("XLE", "XOM","QQQ","SPY","GLD","TLT")
prices <- tick %>%
  tidyquant::tq_get(get = "stock.prices", from = "2005-01-01") %>%
  stats::na.omit()

stocks <- list(prices = prices %>% 
                 dplyr::transmute(date, series = symbol, value = close),
               returns = prices %>%
                 dplyr::group_by(symbol) %>% 
                 dplyr::transmute(date,
                                  series = symbol,
                                  value = log(adjusted) - log(dplyr::lag(adjusted))) %>%
                 tidyr::drop_na())

jsonlite::write_json(stocks, "stocks.json", pretty = TRUE)

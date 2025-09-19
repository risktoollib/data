library(tidyverse)
library(tidyquant)

tick <- c("XLE", "XOM","QQQ","SPY","GLD","TLT","COW","EEM")
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

###

sp400_desc <- tidyquant::tq_index("SP400") #%>% dplyr::filter(!stringr::str_detect(symbol,"BRK.B|BF.B"))

sp400_prices <- tidyquant::tq_get(sort(sp400_desc$symbol),
                                  #sort(grep(sp400_desc$symbol,pattern = "BRK.B|BF.B|DD", value = TRUE, invert = TRUE)),
                                  get  = "stock.prices",
                                  from = "2015-01-01",
                                  to = Sys.Date()) %>%
  stats::na.omit() %>%
  dplyr::group_by(symbol) %>%
  dplyr::select(symbol, date, close = adjusted) %>%
  tidyquant::tq_transmute(select = close, mutate_fun = to.monthly, indexAt = "lastof") %>%
  dplyr::select(date,series = symbol, value = close)

sp400_joined <- sp400_prices %>%
  dplyr::left_join(sp400_desc %>% dplyr::select(symbol, weight, sector),
                   by = join_by(series == symbol)) %>%
  dplyr::mutate(ret = log(value / dplyr::lag(value))) %>%
  stats::na.omit()

feather::write_feather(sp400_desc,"sp400_desc.feather")
feather::write_feather(sp400_prices,"sp400_prices.feather")
feather::write_feather(sp400_joined,"sp400_joined.feather")

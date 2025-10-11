install.packages(c("feather","rvest","readxl","devtools"))
devtools::install_github("risktoollib/RTL")
library(tidyverse)
library(tidyquant)
library(rvest)
library(readxl)
library(arrow)
library(RTL)
                 
# STOCKS
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

# POWER
url = "https://www.ercot.com/misapp/GetReports.do?reportTypeId=13060&reportTitle=Historical%20DAM%20Load%20Zone%20and%20Hub%20Prices&showHTMLView=&mimicKey"
urls <- rvest::read_html(url) %>%
  rvest::html_elements("body > form > table") %>%
  rvest::html_elements("a") %>%
  rvest::html_attr("href") %>%
  paste0("https://www.ercot.com",.)

for (i in 1:length(urls)) {

  url <- urls[i]
  destfile <- paste0("ercot.zip")
  curl::curl_download(url, destfile)
  utils::unzip(destfile)
  ff <- list.files(pattern = "rpt",)
  if (i == 1) {
    ercot <- lapply(readxl::excel_sheets(ff), read_excel, path = ff) %>% do.call(rbind, .)
  } else {
    tmp <- lapply(readxl::excel_sheets(ff), read_excel, path = ff) %>% do.call(rbind, .)
    ercot <- rbind(ercot, tmp)
  }
  file.remove(ff)
  file.remove(destfile)
}

ercot <- ercot %>%
  dplyr::as_tibble(.name_repair = "universal") %>%
  dplyr::mutate(Delivery.Date = as.Date(Delivery.Date, "%m/%d/%Y"),
                Hour.Ending = lubridate::hm(Hour.Ending),
                Date = lubridate::as_datetime(paste(Delivery.Date, Hour.Ending))) %>%
  dplyr::select(Date, everything())
feather::write_feather(ercot,"ercot.feather")

## Regressions
reg1 <- dplyr::tibble(x = 1:100, y = x + x^2 + x^5)
reg2 <- dplyr::tibble(x = seq(from =0,4*pi,,100),
                      y = 2 * sin(2 * x) + x * 0.75)
reg3 <- tidyquant::tq_get(x = c("ICLN","XLE"), get = "stock.prices",
                          from = lubridate::rollback(Sys.Date() - months(120)), to = lubridate::rollback(Sys.Date())) %>%
  dplyr::transmute(date, series = symbol, value = adjusted) %>%
  dplyr::group_by(series) %>%
  dplyr::mutate(value = log(value/dplyr::lag(value))) %>%
  tidyr::drop_na() %>%
  tidyr::pivot_wider(names_from = series,values_from = value)
feather::write_feather(reg1,"reg1.feather")
feather::write_feather(reg2,"reg2.feather")
feather::write_feather(reg3,"reg3.feather")

## multivariates

fromDate <- "2014-01-01"
tick <- c("CVX", "SPY")

df.stock <- tick %>%
  tidyquant::tq_get(get = "stock.prices", from = fromDate) %>%
  dplyr::select(date, series = symbol, value = adjusted) %>% 
  tidyr::pivot_wider(names_from = series, values_from = value)

df.oil <- RTL::dfwide %>% 
  dplyr::select(date, CL01, HO01, RB01) %>% 
  dplyr::filter(date >= fromDate)

df_long <- dplyr::inner_join(df.stock, df.oil, by = c("date")) %>%
  stats::na.omit() %>% 
  dplyr::mutate(CX = (2/3 * RB01 + 1/3 * HO01) * 42 - CL01) %>%
  tidyr::pivot_longer(-date, names_to = "symbol", values_to = "value") %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(value = log(value / dplyr::lag(value))) %>%
  stats::na.omit()

feather::write_feather(df_long,"cvx.feather")

## nonlin reg IR

tickers <- c("DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", 
             "DGS10", "DGS20", "DGS30")

yields <- tickers %>%
  tidyquant::tq_get(get = "economic.data", from = "2023-01-01") %>%
  stats::na.omit()

maturity_map <- c("DGS3MO" = 0.25, "DGS6MO" = 0.5, "DGS1" = 1, 
                  "DGS2" = 2, "DGS5" = 5, "DGS10" = 10, 
                  "DGS20" = 20, "DGS30" = 30)

df_yields <- yields %>%
  dplyr::mutate(maturity = maturity_map[symbol]) %>%
  dplyr::select(date, maturity, yield = price) %>%
  dplyr::filter(date == max(date))

feather::write_feather(df_yields,"usd_cmt_yc.feather")


# parsing exercises
quantmod::getSymbols('MSFT', src = 'yahoo')
microsoft <- MSFT %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "date")
feather::write_feather(microsoft,"microsoft.feather")

quantmod::getSymbols('AAPL', src = 'yahoo')
apple <- AAPL %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "date")
feather::write_feather(apple,"apple.feather")

quantmod::getSymbols('CVX', src = 'yahoo')
chevron <- CVX %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "date")
feather::write_feather(chevron,"chevron.feather")

quantmod::getSymbols('CAT', src = 'yahoo')
caterpillar <- CAT %>% timetk::tk_tbl(preserve_index = TRUE, rename_index = "date")
feather::write_feather(caterpillar,"caterpillar.feather")







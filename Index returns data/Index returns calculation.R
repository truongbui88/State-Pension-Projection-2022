library(tidyverse)
library(lubridate)
library(quantmod)
library(rio)

#Get MSCI ACWI ex US Index
acwi_exUS <- import("./Index returns data/acwi_exUS_price_net.xls", skip = 6)

acwi_exUS_price <- acwi_exUS %>%  
  slice(1:253) %>% # last few rows are not numbers
  rename(acwi_exUS = `ACWI ex USA Standard (Large+Mid Cap)`,
         date = Date) %>% 
  mutate(acwi_exUS = as.numeric(acwi_exUS),
         date = mdy(date),
         month = month(date),
         fy = year(date)) %>% 
  select(-date)

#Get iShares Russell 3000 ETF (IWV) and Vanguard Total Bond Market Index Fund Institutional Shares (VBTIX) prices
symbols <- c("IWV", "VBTIX")

prices <- getSymbols(symbols,
                     src = "yahoo",
                     from = "1998-12-31",
                     to = "2022-01-01",
                     auto.assign = T,
                     warnings = F) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices_monthly <- to.monthly(prices, OHLC = F)    #get monthly prices

#Add MSCI ACWI ex-US prices
index_price <- prices_monthly %>% 
  data.frame(date = index(.)) %>% 
  remove_rownames() %>% 
  mutate(fy = year(date), month = month(date)) %>% 
  left_join(acwi_exUS_price) %>% 
  select(fy, month, acwi_exUS, IWV, VBTIX)

#Calculate annual returns for the individual securities
index_returns <- index_price %>% 
  pivot_longer(cols = 3:5,
               names_to = "index",
               values_to = "price") %>% 
  arrange(index, month, fy) %>% 
  group_by(index, month) %>% 
  mutate(index_returns = price/lag(price) - 1) %>% 
  select(-price) %>% 
  pivot_wider(names_from = index, values_from = index_returns) %>% 
  ungroup()

export(index_returns, "./Index returns data/index_returns.csv")


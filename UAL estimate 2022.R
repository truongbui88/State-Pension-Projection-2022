rm(list = ls())
library(tidyverse)
library(readxl)
library(plotly)
library(formattable)
library(scales)
library(lubridate)
library(quantmod)
library(quadprog)
library(rio)

#Import pension data
ppd_full <- read.csv("ppd-data-latest-05-2022.csv") 
# reason_data <- read_excel("StateDataFiltered_2001-2021.xlsx")

#Specify fiscal years
pre_fy <- 2020
current_fy <- 2021

#Filter and clean PPD data
ppd <- ppd_full %>% 
  mutate(PlanFullName = gsub("\x92", "'", PlanFullName),    #clean plan names and full names
         PlanName = gsub("\x92", "'", PlanName)) %>% 
  filter(AdministeringGovt == 0, fy > 2000, !(PlanName %in% c("Colorado State and School", 
                                                              "Oklahoma Municipal Employees"))) %>%    #select state plans only and get data after 2000. Filter out two plans that don't have enough data.
  select(fy, fye, PlanName, PlanFullName, StateName, ActLiabilities_GASB, MktAssets_net,
         InvestmentReturnAssumption_GASB, InvestmentReturn_1yr, payroll,
         PayrollGrowthAssumption, NormCostRate_tot, ReqContRate_tot, 
         expense_TotBenefits) %>% 
  rename(plan_name = PlanName,            #rename columns for easier reference
         plan_full_name = PlanFullName,
         state = StateName,             
         aal = ActLiabilities_GASB,
         mva = MktAssets_net,
         arr = InvestmentReturnAssumption_GASB,
         return = InvestmentReturn_1yr,
         payroll_growth = PayrollGrowthAssumption,
         nc = NormCostRate_tot,
         cont_rate = ReqContRate_tot,
         ben_pay = expense_TotBenefits) %>% 
  mutate(fye = ymd(fye),
         month = month(fye), .after = fye)

#Impute missing normal costs for New York State Teachers, NY State & Local ERS, and NY State & Local Police & Fire
ppd <- ppd %>% 
  mutate(nc = replace(nc, plan_name == "New York State Teachers" & fy == 2020, 0.1),
         nc = replace(nc, plan_name == "NY State & Local ERS" & fy == 2020, 0.17),
         nc = replace(nc, plan_name == "NY State & Local Police & Fire" & fy == 2020, 0.3))


#Identify plans that have the same AAL for current fy and previous fy
# ppd_same_aal <- ppd %>%        
#   filter(fy %in% c(pre_fy, current_fy)) %>% 
#   select(plan_name, fy, aal) %>% 
#   pivot_wider(names_from = "fy",
#               values_from = "aal") %>% 
#   rename(pre_year = 2,
#          current_year = 3) %>% 
#   mutate(diff = pre_year - current_year) %>% 
#   filter(diff == 0)


#Identify plans with missing data for current fy
# ppd_missing_current <- ppd %>% 
#   filter(fy == current_fy & (is.na(aal) | is.na(mva)))
# 
# check2 <- ppd %>% 
#   filter(fy == current_fy, is.na(mva))
# 
# check3 <- ppd %>% 
#   group_by(plan_name) %>% 
#   summarise(across(fy:ben_pay, ~ sum(is.na(.x)), .names = "{.col}_na_count"))
# 
# 
# test <- ppd %>% 
#   semi_join(check3 %>% filter(payroll_growth_na_count == 21), by = "plan_name")

# Funding Projection ------------------------------------------------------

#Add a 2022 row to each plan
ppd <- ppd %>% 
  group_by(plan_name) %>% 
  group_modify(~ add_row(.x)) %>% 
  ungroup()

#Calculate 5-year average payroll growth rates and benefit payment growth rates
ppd <- ppd %>% 
  group_by(plan_name) %>% 
  mutate(payroll_growth_avg = (payroll/lag(payroll, n = 5))^(1/5)-1,
         ben_pay_growth_avg = (ben_pay/lag(ben_pay, n = 5))^(1/5)-1) %>% 
  ungroup()

#Identify years that have missing AAL/MVA data or have duplicated AAL data
ppd <- ppd %>% 
  mutate(real_vs_estimate = case_when(is.na(aal) ~ "estimate",
                                      is.na(mva) ~ "estimate",
                                      aal == lag(aal) ~ "estimate", 
                                      TRUE ~ "real"), .before = aal)

#Custom functions to project some key variables
fy_f <- function(fy){       
  for(i in 2:length(fy)) {
    if(is.na(fy[i])){
      fy[i] <- fy[i-1] + 1
    }
  }
  return(fy)
}


fye_f <- function(fye){       
  for(i in 2:length(fye)) {
    if(is.na(fye[i])){
      fye[i] <- fye[i-1] + years(1)
    }
  }
  return(fye)
}


get_last_f <- function(x) {
  for(i in 2:length(x)) {
    if(is.na(x[i])) {
      x[i] <- x[i-1]
    }
  }
  return(x)
}


payroll_growth_f <- function(payroll_growth, payroll_growth_avg) {    #if the payroll growth assumption isn't available in any year, use the average payroll growth rate. Otherwise, use the latest available payroll growth assumption
  if(sum(!is.na(payroll_growth)) == 0) {   
    payroll_growth <- payroll_growth_avg
  } else {
    payroll_growth <- get_last_f(payroll_growth)
  }
  return(payroll_growth)
}


growth_f <- function(x, g) {
  for(i in 2:length(x)) {
    if(is.na(x[i])) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}


#Initial projection
ppd_project <- ppd %>% 
  group_by(plan_name) %>% 
  mutate(fy = fy_f(fy),
         fye = fye_f(fye),
         across(.cols = c(month,
                          plan_full_name,
                          state,
                          arr,
                          payroll_growth_avg,
                          ben_pay_growth_avg,
                          nc,
                          cont_rate), 
                .fns = get_last_f),
         payroll_growth = payroll_growth_f(payroll_growth, payroll_growth_avg),
         payroll = growth_f(x = payroll, g = payroll_growth),
         ben_pay = growth_f(x = ben_pay, g = ben_pay_growth_avg)
         ) %>% 
  ungroup()


#Create synthetic benchmark portfolios to predict returns in missing years with 3 indexes
#Get MSCI ACWI ex US Index
acwi_exUS <- read_excel("acwi_exUS_price_net.xls", skip = 6)

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


#Function to create a synthetic benchmark portfolio using quadratic programming (to find the "best fit" benchmark portfolio)
#See examples in the two links below:
#https://henrywang.nl/quadratic-programming-with-r/
#https://henrywang.nl/another-quadratic-programming-example-with-r/

benchmark_portfolio <- function(return, x1, x2, x3) {

end_pos <- max(which(!is.na(return)))
y_actual <- return[(end_pos - 9):end_pos]
x1 <- x1[(end_pos - 9):end_pos]
x2 <- x2[(end_pos - 9):end_pos]
x3 <- x3[(end_pos - 9):end_pos]
x = cbind(x1, x2, x3)

Dmat <- crossprod(x)
dvec <- crossprod(y_actual, x)     # vector to be minimized: product:transpose y_actual and x
Amat <- cbind(rep(1,3), diag(3))   # vector defining constraint
bvec <- c(1,0,0,0)                 # vector of b coefficient; meq = 1 is equality constraint: coefs sum to 1  

result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 

return(list(result$solution))
}


#Join pension data with index returns and solve the benchmark portfolio for each plan
ppd_benchmark <- ppd_project %>% 
  left_join(index_returns) %>% 
  group_by(plan_name) %>% 
  mutate(benchmark = benchmark_portfolio(return, acwi_exUS, IWV, VBTIX),
         benchmark_return = benchmark[[1]][1]*acwi_exUS + benchmark[[1]][2]*IWV + benchmark[[1]][3]*VBTIX) %>% 
  ungroup()


#Custom functions to project return, aal, and mva
#return function
return_f <- function(return, benchmark_return, fy, proj_return) {  
  for (i in 2:length(return)) {
    if (is.na(return[i]) && fy[i] <= current_fy) {   #if returns are missing in current or previous fy, use the benchmark returns
      return[i] <- benchmark_return[i]
    } else if (fy[i] > current_fy) {
      return[i] <- proj_return
    }
  }
  return(return)
}

#AAL function
aal_f <- function(aal, arr, payroll, nc, ben_pay) {
  for(i in 2:length(aal)) {
    if(is.na(aal[i]) || isTRUE(aal[i] == aal[i-1])) {
      aal[i] <- aal[i-1]*(1 + arr[i]) + (nc[i]*payroll[i] + ben_pay[i])*(1 + arr[i])^0.5
    }
  }
  return(aal)
}

#MVA function
mva_f <- function(mva, return, payroll, cont_rate, ben_pay) {
  for(i in 2:length(mva)) {
    if(is.na(mva[i])) {
      mva[i] <- mva[i-1]*(1 + return[i]) + (cont_rate[i]*payroll[i] + ben_pay[i])*(1 + return[i])^0.5
    }
  }
  return(mva)
}


#Final projection with 2022 return input
return_2022 <- 0   #input the expected 2022 return here (currently default to 0%)

ppd_project_final <- ppd_project %>% 
  left_join(ppd_benchmark) %>% 
  group_by(plan_name) %>% 
  mutate(return = return_f(return, benchmark_return, fy, proj_return = return_2022),
         aal = aal_f(aal, arr, payroll, nc, ben_pay),
         mva = mva_f(mva, return, payroll, cont_rate, ben_pay)) %>% 
  mutate(ual = aal - mva, 
         funded_ratio = mva/aal,
         .after = mva) %>% 
  ungroup()


# check4 <- ppd_project_final %>%
#   filter(fy > 2018) %>%
#   group_by(plan_name) %>%
#   summarise(across(fy:ben_pay, ~ sum(is.na(.x)), .names = "{.col}_na_count"))
# export(ppd_project_final, "ppd_project_final.xlsx")

#Calculate state and national funding metrics
ppd_project_state <- ppd_project_final %>% 
  group_by(state, fy) %>% 
  summarise(state_aal = sum(aal),
            state_mva = sum(mva),
            state_ual = state_aal - state_mva,
            state_funded_ratio = state_mva/state_aal) %>% 
  ungroup()

ppd_project_us <- ppd_project_final %>% 
  group_by(fy) %>% 
  summarise(us_aal = sum(aal, na.rm = T),
            us_mva = sum(mva, na.rm = T),
            us_ual = us_aal - us_mva,
            us_funded_ratio = us_mva/us_aal)



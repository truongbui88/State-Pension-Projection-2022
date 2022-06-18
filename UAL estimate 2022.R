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

#Import index returns data
index_returns <- read.csv("./Index returns data/index_returns.csv")

#Import population data
uspop <- import("State population.xlsx")

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

#Fix wrong data:
ppd <- ppd %>% 
  mutate(aal = replace(aal, plan_name == "Nebraska Schools" & fy == 2001, 4576863))

#Missouri Local Government Employees Retirement System
#Arkansas Public Employees Retirement System
#Nebraska Public Employees Retirement System - School Employees Plan
#North Carolina Local Governmental Employees' Retirement System
#Employees' Retirement System of the State of Hawaii
#Massachusetts State Retirement System
#State Employees' Retirement System of Illinois
#Massachusetts Teachers' Retirement System
#Teachers' and State Employees' Retirement System of North Carolina



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


#Create synthetic benchmark portfolios to estimate returns in missing years with 3 indexes

#Function to create a synthetic benchmark portfolio using quadratic programming (to find the "best fit" benchmark portfolio)
#See examples in the two links below:
#https://henrywang.nl/quadratic-programming-with-r/
#https://henrywang.nl/another-quadratic-programming-example-with-r/

benchmark_portfolio <- function(return, x1, x2, x3) {
  
  end_pos <- max(which(!is.na(return)))       #find the latest year with available return data
  y_actual <- return[(end_pos - 9):end_pos]   #use only the last 10 years (with available return data) to create the benchmark
  x0 <- 1                                     #for the intercept (alpha)
  x1 <- x1[(end_pos - 9):end_pos]
  x2 <- x2[(end_pos - 9):end_pos]
  x3 <- x3[(end_pos - 9):end_pos]
  x = cbind(x0, x1, x2, x3)
  
  Dmat <- crossprod(x)
  dvec <- crossprod(y_actual, x)                  # vector to be minimized: product:transpose y_actual and x
  # Amat <- cbind(rep(1,3), diag(3))   
  Amat <- t(cbind(0, rbind(rep(1,3), diag(3))))   # matrix defining the constraints
  bvec <- c(1,0,0,0)                              # vector of b coefficient; meq = 1 is equality constraint: coefs sum to 1  
  
  result <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1) 
  
  return(list(result$solution))
}


#Join pension data with index returns and solve the benchmark portfolio for each plan, then use the benchmark portfolio to estimate the pension funds' returns
ppd_benchmark <- ppd_project %>% 
  left_join(index_returns) %>% 
  group_by(plan_name) %>% 
  mutate(benchmark = benchmark_portfolio(return, acwi_exUS, IWV, VBTIX),
         benchmark_return = benchmark[[1]][2]*acwi_exUS + benchmark[[1]][3]*IWV + benchmark[[1]][4]*VBTIX,
         predict_return = benchmark[[1]][1] + benchmark[[1]][2]*acwi_exUS + benchmark[[1]][3]*IWV + benchmark[[1]][4]*VBTIX) %>% 
  ungroup()


#Custom functions to project return, aal, and mva
#return function
return_f <- function(return, predict_return, fy, proj_return) {  
  for (i in 2:length(return)) {
    if (is.na(return[i]) && fy[i] <= current_fy) {   #if returns are missing in current or previous fy, use the returns estimated by the benchmark portfolio
      return[i] <- predict_return[i]
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
  mutate(return = return_f(return, predict_return, fy, proj_return = return_2022),
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
ppd_project_state <- ppd_project_final %>%     #state
  group_by(state, fy) %>% 
  summarise(state_aal = sum(aal),
            state_mva = sum(mva),
            state_ual = state_aal - state_mva,
            state_funded_ratio = state_mva/state_aal) %>% 
  ungroup()

ppd_project_us <- ppd_project_final %>%        #national
  group_by(fy) %>% 
  summarise(us_aal = sum(aal, na.rm = T),
            us_mva = sum(mva, na.rm = T),
            us_ual = us_aal - us_mva,
            us_funded_ratio = us_mva/us_aal)

#Calculate unfunded liability per capita by state
uspop_long <- uspop %>% 
  mutate(`2022` = `2021`) %>% 
  pivot_longer(cols = 2:24, names_to = "fy", values_to = "pop") %>% 
  mutate(fy = as.numeric(fy)) %>% 
  rename(state = State) %>% 
  arrange(state, fy)

ppd_project_state_capita <- ppd_project_state %>% 
  left_join(uspop_long) %>% 
  mutate(state_ual_capita = state_ual/pop * 1000)

# library(echarts4r)

# ppd_project_us %>% 
#   mutate(us_ual_real = ifelse(fy <= 2020, us_ual, NA),
#          fy = as.character(fy)) %>% 
#   e_charts(fy) %>% 
#   e_line(us_ual, symbolSize = 7, lineStyle = list(width = 3, type = "dotted")) |>     #plot the dotted line first and the solid line later so that the solid line will block the dots
#   e_line(us_ual_real, symbolSize = 7, lineStyle = list(width = 3)) |>
#   # e_theme_custom("echarts_theme.json") |>
#   e_title("Unfunded Liability") |>
#   e_tooltip(trigger = "item",
#             formatter = e_tooltip_item_formatter("currency", digits = 0)) |>
#   e_legend(show = F) |>
#   e_color("orange")



# 
# chosen_plan <- c("Pennsylvania Municipal", "Alabama ERS")
# ppd_project_final %>% 
#   filter(plan_name %in% chosen_plan) %>% 
#   group_by(plan_name) %>% 
#   mutate(ual_real = ifelse(real_vs_estimate == "real", ual, NA)) |>
#   # mutate(ual_estimate = ifelse(real_vs_estimate == "estimate", ual, NA)) |>
#   mutate(fy = as.character(fy)) |>
#   e_charts(fy) |>
#   e_line(ual, symbolSize = 7, lineStyle = list(width = 3, color = "gray", type = "dotted")) |>
#   e_line(ual_real, symbolSize = 7, lineStyle = list(width = 3)) |>
#   # e_theme_custom("echarts_theme.json") |>
#   e_title("UAL") |>
#   e_tooltip(trigger = "item",
#             formatter = e_tooltip_item_formatter("currency", digits = 0))
# 


  


library(readxl)
library(tidyverse)
budget <- read_excel("data/Assignment-Budget.xlsx", skip=4) %>%  
  janitor::clean_names() %>% 
  select(-c(x,a)) 

# (1) cleaning data: 
#lets break up cases & $ columns
col_dollar <- budget %>% names() %>% tail(12)


budget_price <- budget %>%  
  select(customer_code:item_group, matches(col_dollar)) 

budget_cases <- budget %>%  
  select(-c(matches(col_dollar))) 

#edit column names
colnames(budget_price) <- gsub("(21).*","\\1",colnames(budget_price))
colnames(budget_cases) <- gsub("(21).*","\\1",colnames(budget_cases))

budget_cases_final  <- budget_cases %>% 
  gather(month, volume, c(matches('21'))) %>% 
  mutate(
    month=paste0(1,month),
    month = as.Date(month, '%d%b_%y')
    ) 

budget_price_final  <- budget_price %>% 
  gather(month, budget_prices, c(matches('21'))) %>% 
  mutate(
    month=paste0(1,month),
    month = as.Date(month, '%d%b_%y')
  ) 

#generate excel~ cleaned
budget_edited <- budget_cases_final %>% 
  bind_cols(
    budget_price_final %>% select(-c(customer_code:item_group, month))
  ) %>% 
  mutate(
    budget_prices=as.character(budget_prices),
    volume = as.character(volume)
)

write_csv2(budget_edited, 'data/budget_data.csv')
##########################################
wghts <- read_excel("data/Assignment-CaseWeights.xlsx") %>%  
  janitor::clean_names() %>% 
  rename(
    item_code = item_no
  )
#ok so for basically all item codes there is only 12 records corrsponding to 12 months, exception of 1 time of item
budget_price_final %>% 
  group_by( 
    customer_code, channel, branded_pl, item_group,item_code
  ) %>% 
  summarize(
    n=n() ) %>%
  ungroup() %>% 
  group_by(n) %>% 
  summarize(count=n())
#this confirms that further
budget_price_final %>% 
  group_by( 
    customer_code, channel, branded_pl, item_group,item_code,month
  ) %>% 
  summarize(
    n=n() ) %>%
  ungroup() %>% 
  group_by(n) %>% 
  summarize(count=n())


budget_edited %>%
  filter(customer_code =='761644',
    item_code== '53672',branded_pl=='Private Label', channel=='Retail') 
 
#budget_price_final %>% 
budget_edited %>%
  rename(case_vol = volume) %>% 
  left_join(wghts, by ='item_code') %>% 
  mutate(
    across(c(case_vol, budget_prices),~as.numeric(.)),
    total_lbs = case_vol * case_weight_lb,
    price_per_lb = budget_prices/total_lbs
  ) %>% 
  group_by( 
    customer_code, channel, branded_pl, item_group,item_code
  ) %>% 
  summarize(
    across(c(budget_prices, total_lbs, case_vol,price_per_lb), list(mean = mean, sum = sum), .names = "{.fn}_{.col}")

  ) %>% 
  select(-c(sum_price_per_lb))

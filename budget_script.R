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
avg_year_budget <- budget_edited %>%
  rename(case_vol = volume) %>% 
  left_join(wghts, by ='item_code') %>% 
  mutate(
    year = lubridate::floor_date(month, 'year'),
    across(c(case_vol, budget_prices),~as.numeric(.)),
    total_lbs = case_vol * case_weight_lb,
    price_per_lb = budget_prices/total_lbs
  ) %>% 
  rename(
    total_amt = budget_prices, 
    lbs = total_lbs
  ) %>% 
  group_by( 
    customer_code, channel, branded_pl, item_group,item_code,year
  ) %>% 
  summarize(
    across(c(total_amt, lbs, case_vol,price_per_lb), list(mean = mean, sum = sum), .names = "{.fn}_{.col}")

  ) %>% 
  select(-c(sum_price_per_lb))

#okay so quantity = cases 
avg_year_sales <- sales %>%
  rename(
    case_vol=quantity,
    lbs_per_case=   lb_sper_case
  ) %>% 
  mutate(
    customer_code=as.character(customer_code),
    item_code = as.character(item_code),
    
    year = as.Date(year,"%Y"),
    year=lubridate::floor_date(year,'year'),
    cases= lbs/lbs_per_case,
    price_per_lb = total_amt/lbs
  )  %>% 
  group_by( 
    customer_code, channel, branded_pl, item_group,item_code,year
  ) %>% 
  summarize(
    across(c(total_amt, lbs, case_vol,price_per_lb), list(mean = mean, sum = sum), .names = "{.fn}_{.col}")
    
  ) %>% 
  select(-c(sum_price_per_lb))

all_time_data <- avg_year_budget %>% 
  bind_rows(avg_year_sales) %>% 
  arrange(customer_code,item_code,year) 

all_time_data %>% 
  #filter(item_group =='Crackers') %>% 
  ggplot(aes(x = year, y= mean_total_amt, color=as.factor(item_code), fill=as.factor(item_code),group=customer_code
             ))+
  geom_point()+
  facet_wrap(~item_group, scales="free_y", nrow=1)

all_time_data %>% 
  filter(item_group =='Crackers') %>% 
  ggplot(aes(x = year, y= mean_lbs, color=as.factor(item_code), fill=as.factor(item_code),group=customer_code
  ))+
  geom_point()

# data load: The goal of this document is to stack the datasources to generate 1 datasource. Currently, there is not method to stack this easily in tableau. 
# this is possible, but the goal here is efficiency leveraging currently skills i already have.


source('budget_script.R')
#edit budget data to combine with all time sales data 
budget_final <- budget_edited %>% 
  rename(
    total_amt = budget_prices,
    case_vol=volume,
    month_date = month,
    business_line=business
  ) %>%
  mutate(
    month = stringr::str_remove(format(month_date, '%m'),'0'),
    month=as.numeric(month),
    year= lubridate::floor_date(month_date, 'year'),
    data_source = 'forecast'
  )

################### pull in data #######################
# wghts <- read_excel("data/Assignment-CaseWeights.xlsx") %>%  
#   janitor::clean_names() %>% 
#   rename(
#     item_code = item_no
#   )

sales_data <- read_excel("data/Assignment-Historical Sales.xlsx") %>%  
  janitor::clean_names() %>% 
  rename(
    states = state_s ,
    case_vol=quantity,
    lbs_per_case=   lb_sper_case
  ) %>% 
  mutate(
    customer_code=as.character(customer_code),
    item_code = as.character(item_code),
    month_date=ifelse(stringr::str_count(month)<2,paste0('0',month),month),
    month_date=paste0(year,'-',month_date,'-01'),
    month_date = as.Date(month_date, format='%Y-%m-%d'),
    year = as.Date(year,"%Y"),
    year=lubridate::floor_date(year,'year')#,
    # case_vol= lbs/lbs_per_case,
    # price_per_lb = total_amt/lbs
  ) %>% 
  mutate(
    data_source = 'sales'
  )
# generate combined data -- do other editing in tableau
all_year_data <- sales_data %>% 
  bind_rows(budget_final) 

if(save_data ==TRUE){
  all_year_data %>% 
    mutate(
      total_amt=as.character(total_amt),
      case_vol = as.character(case_vol)
    )  %>% 
    write_csv2( 'data/data_combined.csv')
}

  # left_join(wghts, by='item_code') %>% 
  # mutate(
  #   lbs_wghts = case_vol * case_weight_lb
  # )

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
avg_year_sales <- sales_data %>%
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

# data load: The goal of this document is to stack the data sources to generate 1 data source. Currently, there is not method to stack this easily in tableau. 
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


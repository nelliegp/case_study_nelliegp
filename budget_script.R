library(readxl)
library(tidyverse)
budget <- read_excel("Assignment-Budget.xlsx", skip=4) %>%  
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
# 
# colnames(budget_price) <-  stringr::str_replace(colnames(budget_price) ,"(21).*","price")
# colnames(budget_cases) <- stringr::str_replace(colnames(budget_cases) ,"(21).*","case")

#make data long:
# budget_cases %>% 
#   summarize(
#     n=n(),
#     across(c(customer_code,item_code, business, channel, branded_pl,item_group),
#     ~n_distinct(.)
#     ) 
#   )

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


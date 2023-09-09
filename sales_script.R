
sales <- read_excel("data/Assignment-Historical Sales.xlsx") %>%  
  janitor::clean_names() 

sales %>% 
  glimpse()

sales %>% 
  group_by(item_group) %>% 
  summarize(
    sum(total_amt)
  )

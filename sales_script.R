library(reactable)
library(knitr)
sales <- read_excel("data/Assignment-Historical Sales.xlsx") %>%  
  janitor::clean_names() %>% 
  rename(states = state_s )

sales %>% 
  glimpse()


sales %>% summarize(n=n_distinct(customer_code))

#distribution of revenue by year
sales_year <- sales %>% 
  mutate(
    sum_sales= sum(total_amt),
    sum_vol = sum(quantity)
  ) %>% 
  group_by(customer_code,year) %>% 
  summarize(
   sales= sum(total_amt),
   vol= sum(quantity),
   perc_sales = sum(total_amt,na.rm=T)/mean(sum_sales),
   perc_vol = sum(quantity,na.rm=T)/mean(sum_vol),
   no_states = n_distinct(states),
   no_items = n_distinct(item_code),
   no_itemgroup = n_distinct(item_group),
   group = paste0(unique(item_group), collapse='; ')
  ) %>% 
  arrange(desc(year),desc(perc_sales))

check <- sales_year  %>%
  filter(year =='2020') %>% 
  arrange(desc(perc_sales)) %>% 
  head(10) %>% 
  select(customer_code) %>% pull()

sales_year  %>%
  group_by(year) %>% 
  arrange(year, desc(perc_sales)) %>% 
  mutate(
    rank = row_number()
  ) %>% 
  ungroup() %>% 
  filter(customer_code %in% check) %>% 
  arrange(customer_code, year)


sales_year  %>%

  reactable(
    filterable = TRUE,
    searchable = TRUE,
    resizable = TRUE,
    sortable=TRUE,
    wrap = TRUE,
    bordered = TRUE,
    highlight = TRUE,
    pagination = FALSE,
    showPageSizeOptions=TRUE,
    fullWidth = TRUE,
    height = 470,
    # width = 600,
    style = list(fontSize = "12px"),
    defaultColDef = colDef(
      headerClass = "wordwrap",
      minWidth = 88,
      na = "",
      align = "left",
      headerStyle = list(background = "#f7f7f8")
    ),
    
    columns = c(
      define_columns(sales_year)
    )
  )

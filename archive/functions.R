#functions


define_columns <- function(dataset){
  
  names(dataset) -> col_names
  x<-c()
  
  for(i in 1: length(col_names)){
    if(stringr::str_detect(col_names[i],'Volume|#|% pop|Size|Vol|Node')==TRUE){
      y <- def_cell(col_names[i],'n')
    }else if( stringr::str_detect(col_names[i],'%|Rate|Delta|rmse|RMSE|std_err|stderr|perc|MAE|RSQ')==TRUE){
      y <- def_cell(col_names[i],'perc')
    }else if( stringr::str_detect(col_names[i],'cost_complexity|Cost Complexity')==TRUE){
      y <- def_cell(col_names[i],'scientific')
    }else{
      y <-x
    }
    x<-c(x,  y)
  }
  x
}

def_cell <- function(name,type){
  if(type=='n'){
    test <-  list(
      name = colDef(
        aggregate = "sum",
        format = colFormat(separators = TRUE, digits = 0)
      )
    )
  }else if(type=='perc'){
    test <-  list(
      name = colDef(
        format = colFormat(percent = TRUE, digits = 2)
      )
    )
  }else if(type=='scientific'){
    test <- list(
      name =colDef(cell = function(value){format(value,digits=3, scientific=TRUE)})
    )
  }else{
    test <-  list(
      name = colDef(
      )
    )
  }
  
  names(test) <- name
  test
}





comp_tbl_input %>%
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
      # list(
      #   'Key' = colDef(
      #     style = sticky_style(),
      #     headerStyle = sticky_style()
      #   ),
      #  
      #   "(+) [10%, +) % Diff" = colDef(
      #     defaultSortOrder = "desc",
      #     style = function(value) {
      #       if(is.na(value)){
      #         list(background = '#f7f7f8')
      #       }else if(value==1){
      #         color <-'#feb24c'
      #         list(background = color)
      #       }else{ list(background = '#f7f7f8')}
      #     }
      #   ),
      #   "(-) [10%, +) % Diff" = colDef(
      #     style = function(value) {
      #       if(is.na(value)){
      #         list(background = '#f7f7f8')
      #       }else if(value==1){
      #         color <-'#ffeda0'
      #         list(background = color)
      #       }else{list(background = '#f7f7f8')}
      #       
      #     }              )
      # ),
      define_columns2(comp_tbl_input)
    )
  )

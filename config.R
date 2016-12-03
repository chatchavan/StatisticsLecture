kDefaultDatatableOptions <- 

displayTable <- function(df, height = 100) {
  datatable(df, class="compact", height = height, 
            options = list(
              searching = F,
              paging = F,
              scrollY = paste0(height,"px"), 
              scrollCollapse = T, 
              info = F)) 
}
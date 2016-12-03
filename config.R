kDefaultDatatableOptions <- 

displayTable <- function(df, height = 200) {
  datatable(df, class="compact", height = height+50, 
            options = list(
              searching = F,
              paging = F,
              scrollY = paste0(height,"px"), 
              scrollCollapse = T, 
              info = F)) 
}
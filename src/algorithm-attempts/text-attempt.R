
filings <- lapply(file_list, parse_filing_html)

holdings_tbls <- filings[[1]]

clean_table <- function(tbl) {
  tbl <- as.data.table(lapply(tbl, 
                              function(x) {
                                if(is.factor(x)) {
                                  as.character(x) 
                                  } else {
                                    x
                                  }
                                }))
  remove_na_rows(remove_na_cols(replace_empty(tbl)))
}

for(i in 1:length(cleaned)){
  print(i)
  get_main_data(cleaned[[i]])
  
}

cleaned <- lapply(holdings_tbls, clean_table) %>%
  lapply(remove_header) %>% 
  lapply(make_names) %>%  
  rbindlist(use.names = T,fill = T)

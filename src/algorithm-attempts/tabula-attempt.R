

tmp <- tabulizer::extract_tables("test/tabulatest.pdf",
                                 output = "matrix") %>%
  lapply(as.data.frame, stringsAsFactors = F) %>% 
  lapply(replace_empty) %>% 
  lapply(remove_almost_na_cols)


rbindlist(tmp)

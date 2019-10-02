# preliminary attempt to use some OCR tools to pull tables
# so far this seems to work ok, but it needs to be customized to
# accomodate table gaps that are common to these filings

library(tabulizer)

# grab tables from a filing converted to PDF, clean the tables after
tmp <- tabulizer::extract_tables("test/tabulatest.pdf",
                                 output = "matrix") %>%
  lapply(as.data.frame, stringsAsFactors = F) %>% 
  lapply(replace_empty) %>% 
  lapply(remove_almost_na_cols)

# combine the holdings back together
rbindlist(tmp)

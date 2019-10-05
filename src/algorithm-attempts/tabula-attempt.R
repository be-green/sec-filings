# preliminary attempt to use some OCR tools to pull tables
# so far this seems to work ok, but it needs to be customized to
# accomodate table gaps that are common to these filings

library(tabulizer)
library(magrittr)
library(stringr)
library(data.table)

# source relevant scripts first
allscripts <- list.files("src/helpers/", full.names = T)

## don't want the output, source the helpers
invisible(lapply(allscripts, source))

# grab tables from a filing converted to PDF, clean the tables after
# this gets close but still misses by more than I'd like
# visual separators like ------- seem to mess it up
visual_tables <- 
  tabulizer::extract_tables("tests/test-files/tabulatest.pdf", 
                            output = "matrix") %>%
  lapply(as.data.frame, stringsAsFactors = F) %>% 
  lapply(replace_empty) %>% 
  lapply(remove_almost_na_cols)

x <- 
  tabulizer::extract_tables("tests/test-files/alger-funds.pdf", 
                            output = "character") %>% 
  lapply(parse_tables)






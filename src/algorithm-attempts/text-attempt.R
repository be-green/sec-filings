
# source relevant scripts first
allscripts <- list.files("src/helpers/", full.names = T)

## don't want the output
invisible(lapply(allscripts, source))
invisible(source("src/build/parse-filing.R"))

# create a list of test filings

## attach relevant packages
library(magrittr)
library(xml2)
library(rvest)

## single year to start
filing_year <- 2012

# list the filings for the year, combine them into one data table
all_filings <-
  lapply(list_filings(filing_year), fread) %>% 
  rbindlist(idcol = T) %>% 
  .[FilingType %like% "N-Q"] # subset for just N-Q filings

# text document version of the filing
# otherwise it links to the parent site on the SEC website
# not the document
all_filings[,FilingText := str_replace(FilingLink, 
                                       pattern = "-index.htm",
                                       ".txt")]


# use a specific filing to test our parser
# this should return 10 html tables
# thanks to xbrl, html is embedded INSIDE an XML schema
# normal xml parsers don't recognize it
file_list <- get_filing_html(all_filings$FilingText[10])

# grab the tables out of the html
filings <- lapply(file_list, parse_filing_html)

# test our text parser on a specific table in the list
holdings_tbls <- filings[[1]]

# function that converts factors back to strings and removes
# na rows, replacing "" with NA values to properly identify
# NULL values
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

# still doesn't really work yet since there's no standard format
cleaned <- 
  lapply(holdings_tbls, clean_table) %>% # fix some NA rows
  lapply(remove_header) %>% # remove the header
  # lapply(make_names) %>%  # construct column names
  rbindlist(use.names = T,fill = T) # merge the table back together


for(i in 1:length(cleaned)){
  print(i)
  get_main_data(cleaned[[i]]) # get the central data, not the headers
  
}

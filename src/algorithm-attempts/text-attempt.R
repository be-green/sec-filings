
# source relevant scripts first
allhelpers <- list.files("src/helpers/", full.names = T)
allparsers <- list.files("src/parsers/", full.names = T)

## don't want the output
invisible(lapply(allhelpers, source))
invisible(lapply(allparsers, source))
invisible(source("src/build/parse-filing.R"))

# create a list of test filings

## attach relevant packages
library(magrittr)
library(xml2)
library(rvest)
library(data.table)

## single year to start
filing_year <- 2008

# list the filings for the year, combine them into one data table
all_filings <-
  lapply(list_filings(filing_year), fread) %>% 
  rbindlist(idcol = T) %>% 
  .[FilingType %like% "N-Q|N-30D"] # subset for just N-Q filings

# text document version of the filing
# otherwise it links to the parent site on the SEC website
# not the document
all_filings[,FilingText := str_replace(FilingLink, 
                                       pattern = "-index.htm",
                                       ".txt")]

### test cases ###

test_cases <- c("https://www.sec.gov/Archives/edgar/data/832566/0001104659-08-072666.txt",
                "https://www.sec.gov/Archives/edgar/data/1233310/000093506908000486/0000935069-08-000486.txt",
                "https://www.sec.gov/Archives/edgar/data/354603/0000935069-08-002855.txt",
                "https://www.sec.gov/Archives/edgar/data/810598/0000900092-08-000052.txt")

# use a specific filing to test our parser
# this should return 10 html tables
# thanks to xbrl, html is embedded INSIDE an XML schema
# normal xml parsers don't recognize it

### HTML example ###
# gets the raw filing text
filing <- get_filing(test_cases[1])

# pulls out the relevant html
filing_html <- get_filing_html(filing)

# pulls out the tables in the html, converts to text
# for cleanup
filing_html_tables <- lapply(filing_html, parse_filing_html)

# dispatch relevant parser
parsed_tables <- lapply(filing_html_tables[[1]], parse_tables) %>% 
  get_relevant_tables %>% 
  rbindlist

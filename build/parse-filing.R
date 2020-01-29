
# source relevant scripts first
helper_scripts <- list.files("helpers/", full.names = T)
parser_scripts <- list.files("parsers/", full.names = T)

## don't want the output
invisible(lapply(helper_scripts, source))
invisible(lapply(parser_scripts, source))

# create a list of test filings

## attach relevant packages
library(magrittr)
library(xml2)
library(rvest)
library(data.table)
library(stringr)

### test cases ###
test_cases <- c("https://www.sec.gov/Archives/edgar/data/832566/0001104659-08-072666.txt",
                "https://www.sec.gov/Archives/edgar/data/810598/0000900092-08-000052.txt",
                "https://www.sec.gov/Archives/edgar/data/709364/0000949377-08-000097.txt",
                "https://www.sec.gov/Archives/edgar/data/99188/0001104659-08-012285.txt")

# these american funds work great!
gfa <- 
  c("https://www.sec.gov/Archives/edgar/data/44201/0000051931-08-000025.txt", 
    "https://www.sec.gov/Archives/edgar/data/44201/0000051931-08-000430.txt"
  )

amcap <- c("https://www.sec.gov/Archives/edgar/data/4405/0000051931-08-000024.txt", 
           "https://www.sec.gov/Archives/edgar/data/4405/0000051931-08-000429.txt")

scwf <- c("https://www.sec.gov/Archives/edgar/data/858744/0000051931-08-000071.txt", 
          "https://www.sec.gov/Archives/edgar/data/858744/0000051931-08-000590.txt")

# text filing example


# use a specific filing to test our parser
# thanks to xbrl, html is embedded INSIDE an XML schema
# normal xml parsers don't recognize it

### HTML example ###
get_filing_tables(scwf[1])

# dispatch relevant parser
# parsed_tables <- combine_all_tables_from_filing(filing_html_tables)

### text example ###

text_examples <- c(
  "https://www.sec.gov/Archives/edgar/data/1352280/0000950124-08-001579.txt",
  "https://www.sec.gov/Archives/edgar/data/1329992/0000935069-08-000485.txt"
)

text_filing <- get_filing(text_examples[2])


# pulls out the relevant html
filing_text_tables <- get_text_filing_tables(text_filing)

# generic version

# same as html example above
get_filing_tables(scwf[1])
get_filing_tables(text_examples[2])

# random sample tests

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

n_tests <- 50

# randomly sample filings
test_filings <- sample(all_filings$FilingText, n_tests)

tests <- list()

# for 100 tests this took about 12 minutes
# when I ran it last
for(i in 1:n_tests) {
  print(i)
  # safely handle errors and save in a string
  tests[[i]] <- tryCatch({
    get_filing_tables(test_filings[i])
  }, 
  error = function(e) e)
}

# I've done a few tests
# this one said that 87/100 of these parsed filings
# return at least some tables without error
# other smaller samples 
tests_with_returns <- 
  Filter(function(x) length(x) > 0 & 
           !"simpleError" %in% class(x) & !"message" %in% names(x), 
         tests)

num_long_tables <- function(x, len = 10) {
  sapply(x, nrow) %>% 
    subset(., . > len) %>% 
    length
}

tests_with_returns %>% 
  length  %>% 
  paste0("At least ", ., " out of ", 
         n_tests, " tests returned some tables.") %>% 
  message

# some of these still didn't work though
sapply(tests_with_returns, num_long_tables) %>% 
  subset(., . > 0) %>% 
  length %>% 
  paste0("At least ", ., " out of ", 
         n_tests, " tests returned tables with holdings.") %>% 
  message


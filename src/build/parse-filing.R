
# source relevant scripts first
helper_scripts <- list.files("src/helpers/", full.names = T)
parser_scripts <- list.files("src/parsers/", full.names = T)


## don't want the output
invisible(lapply(helper_scripts, source))
invisible(lapply(parser_scripts, source))


# create a list of test filings

## attach relevant packages
library(magrittr)
library(xml2)
library(rvest)
library(data.table)

### test cases ###
test_cases <- c("https://www.sec.gov/Archives/edgar/data/832566/0001104659-08-072666.txt",
                "https://www.sec.gov/Archives/edgar/data/1233310/000093506908000486/0000935069-08-000486.txt",
                "https://www.sec.gov/Archives/edgar/data/354603/0000935069-08-002855.txt",
                "https://www.sec.gov/Archives/edgar/data/810598/0000900092-08-000052.txt")

# use a specific filing to test our parser
# thanks to xbrl, html is embedded INSIDE an XML schema
# normal xml parsers don't recognize it

### HTML example ###
# gets the raw filing text
filing <- get_filing(test_cases[1])

# pulls out the relevant html
filing_html <- get_filing_html(filing)

# pulls out the tables in the html, converts to text
# for cleanup
filing_html_tables <- lapply(filing_html, parse_filing_html) %>% 
  Filter(x = ., function(x) length(x) > 0)

# dispatch relevant parser
parsed_tables <- lapply(filing_html_tables, combine_all_tables_from_filing)


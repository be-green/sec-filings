
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

# growth fund of american in 2008

gfa <- 
  c("https://www.sec.gov/Archives/edgar/data/44201/0000051931-08-000025.txt", 
    "https://www.sec.gov/Archives/edgar/data/44201/0000051931-08-000430.txt"
  )

# use a specific filing to test our parser
# thanks to xbrl, html is embedded INSIDE an XML schema
# normal xml parsers don't recognize it

### HTML example ###
# gets the raw filing text
filing <- get_filing(gfa[1])

# get header data from filing
header <- get_sec_header(filing)

# funds <- header$fund_data
# fundids <- funds$`series-id` %>% unique

# pulls out the relevant html
filing_html <- get_filing_html(filing)

# pulls out the tables in the html, converts to text
# for cleanup
filing_html_tables <- lapply(filing_html, 
                             parse_filing_html,
                             html_header = header) %>% 
  # unlist(recursive = F) %>% # hack for now
  unlist(recursive = F) %>% 
  Filter(x = ., function(x) length(x) > 0)

# dispatch relevant parser
parsed_tables <- combine_all_tables_from_filing(filing_html_tables)


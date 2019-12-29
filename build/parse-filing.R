
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

html_filing <- get_filing(scwf[1])

### HTML example ###
get_html_filing_tables(scwf[1])

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


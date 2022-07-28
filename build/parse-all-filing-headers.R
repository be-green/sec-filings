# parse all filings
if(!dir.exists("data/processed/1999/")) {
  source("build/build-crawler-dataset.R")
}

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
library(parallel)

all_filings <- purrr::map_dfr(1993:2019,
  function(filing_year) {
      lapply(list_filings(filing_year), fread)
  })

# text document version of the filing
# otherwise it links to the parent site on the SEC website
# not the document
all_filings[,FilingText := str_replace(FilingLink,
                                 pattern = "-index.htm",
                                 ".txt")]

filing_headers <- purrr::map(all_filings$FilingText, function(x) {
  filing <- get_filing(x)
  header <- get_sec_header(filing)
  header
})


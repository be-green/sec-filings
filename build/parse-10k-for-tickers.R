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
library(quanteda) # NLP basics
library(parallel)

# comp_names <- fread("data/external/compustat_names_with_fixed_tickers.csv")

for(j in 1993:2020) {

  ## single year to start
  filing_year <- j

  # list the filings for the year, combine them into one data table
  # this will have every 10k since 1993
  tenKs <-
    lapply(list_filings(filing_year), fread) %>%
    rbindlist(idcol = T) %>%
    .[FilingType == "10-K"] # subset for just 10kfilings

  # text document version of the filing
  # otherwise it links to the parent site on the SEC website
  # not the document
  tenKs[,FilingText := str_replace(FilingLink,
                                         pattern = "-index.htm",
                                         ".txt")]

  all_filings <- tenKs$FilingText


  get_ticker_from_10k <- function(filing_text_link) {
    ## attach relevant packages
    library(magrittr)
    library(xml2)
    library(rvest)
    library(data.table)
    library(stringr)
    library(quanteda) # NLP basics
    library(parallel)

    filing <- get_filing(filing_text_link)
    header <- get_sec_header(filing)

    if(guess_text_html(filing) == "html") {
      # there are several html documents embedded in
      # each xbrl filing
      filing_html <- get_filing_html(filing)
      text <-
        lapply(filing_html, html_text) %>%
        paste0(collapse = " ")
    } else {
      text <- filing
    }

    # remove all non-alpha numeric characters
    text <- str_replace_all(text,"[^0-9A-Za-z]", " ")

    # deduplicate spaces
    text <- str_replace_all(text, "[ \t\n\r]+", " ")

    tokenized <- tokens(text)
    tokenized <- quanteda::tokens_remove(tokenized,
                                         c(stopwords("english")))

    tokenized

    ticker <- tokens_select(tokenized, pattern = "symbol|symbols",
                  valuetype = "regex",
                  selection = "keep",
                  window = 5) %>%
      unlist %>%
      subset(., . %in% comp_names$noDotsTic) %>%
      unique

    if(length(ticker) > 1) {
      ticker <- paste0(ticker, collapse = "; ")
    }

    header$parent_data$ticker <- ticker
    header$parent_data
  }

  # do in parallel
  cl <- makeCluster(2)
  clusterExport(cl, ls())

  filing_list <- parallel::parLapply(cl, all_filings[c(1, 4)], get_ticker_from_10k)

  cik_with_ticker <-
    rbindlist(filing_list, fill = T, use.names = T)

  if(!dir.exists(paste0("data/10k/", filing_year))) {
    dir.create(paste0("data/10k/", filing_year))
  }

  fwrite(cik_with_ticker, paste0("data/10k/", filing_year, "/10ktickers.csv"))

}

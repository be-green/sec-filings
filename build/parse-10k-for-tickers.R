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

## single year to start
filing_year <- 2008

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

filing_list <- list()

for(i in 1:1000) {
  filing <- get_filing(all_filings[i])
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
  text <- str_replace_all(text,"[^0-9A-Za-z ]", " ")
  
  # deduplicate spaces
  text <- str_replace_all(text, "[ \t\n\r]+", " ")
  
  tokenized <- tokens(text)
  tokenized <- quanteda::tokens_remove(tokenized, stopwords("english"))
  
  
  ticker <- tokens_select(tokenized, pattern = "symbol", 
                selection = "keep", window = 1) %>% 
    unlist %>% 
    tail(1)
  
  
  header$parent_data$ticker <- ticker
  filing_list[[i]] <- header$parent_data
}

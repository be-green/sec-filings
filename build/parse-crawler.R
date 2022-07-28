library(iotools)
library(data.table)
library(stringr)
library(httr)

headers = c(
  `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
)


get_crawler <- function(year, quarter){

  if(!dir.exists(paste0("data/"))) {
    dir.create("data")
    dir.create("data/raw")
    dir.create("data/processed")
  }

  if (!dir.exists(paste0("data/raw/",year))){
    dir.create(paste0("data/raw/",year))
  }

  if (!dir.exists(paste0("data/processed/",year))){
    dir.create(paste0("data/processed/",year))
  }

  if(!dir.exists(paste0("data/raw/",year,"/",quarter))){
    dir.create(paste0("data/raw/",year,"/",quarter))
  }

  if (!dir.exists(paste0("data/processed/",year,"/",quarter))){
    dir.create(paste0("data/processed/",year,"/",quarter))
  }

  # now the sec requires user-agent permissions
  # can't just use download.file
  httr::GET(paste0("https://www.sec.gov/Archives/edgar/full-index/",year,
                   "/QTR",quarter,"/crawler.idx"),
            httr::add_headers(.headers=headers),
            httr::write_disk(path=paste0("data/raw/",year,"/",
                                         quarter,"/crawler.idx"),
                       overwrite = TRUE))


  crawler <-
    input.file(paste0("data/raw/",year,"/",quarter,"/crawler.idx"),
               formatter = dstrfw,
               skip = 9,
               col_types = c("character",
                             "character",
                             "numeric",
                             "character",
                             "character"),
               widths = c(62, 12, 12,
                          12, 86))


  crawler <- data.table(crawler)

  crawler[,colnames(crawler) := lapply(.SD, str_trim)]

  setnames(crawler, colnames(crawler), c("CompanyName","FilingType",
                                         "CIK","Date","FilingLink"))

  fwrite(crawler, paste0("data/processed/",year,"/",quarter,"/crawler.csv"))

}

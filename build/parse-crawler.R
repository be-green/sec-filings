library(iotools)
library(data.table)
library(stringr)

get_crawler <- function(year, quarter){
  
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
  
  download.file(paste0("https://www.sec.gov/Archives/edgar/full-index/",year,
                      "/QTR",quarter,"/crawler.idx"),
                paste0("data/raw/",year,"/",quarter,"/crawler.idx"))
  
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

#' source crawler parsing script
source("build/parse-crawler.R")

#' for each year in the SEC database, get the parsers locally
for(i in 1993:year(Sys.Date())) {
  for(j in 1:4) {
    tryCatch({
      get_crawler(i, j)
    }, error = function(e) print(e))
  }
}

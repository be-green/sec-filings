# numeric parsing


##  check column format
check_pattern <- function(x, pattern, threshold = 0.3) {
  sum(str_count(x, pattern), na.rm = T)/length(x) > threshold
}

check_comma <- function(x, threshold = 0.3) {
  check_pattern(x, 
                pattern = "[0-9],[0-9]{3}",
                threshold = threshold)
} 

check_percent <- function(x, threshold = 0.3) {
  check_pattern(x, pattern = "[0-9]\\%",
                threshold = threshold) &
    !check_pattern(x, pattern = paste0("[A-Za-z, \\.]{",sapply(nchar(x)/2, round),"}"))
}

check_parens <- function(x, threshold = 0.05) {
  check_pattern(x, pattern = "\\([0-9,\\.]+\\)",
                threshold = threshold)
}

check_numeric <- function(x) {
  
  suppressWarnings({
    if(is.factor(x)){
      x <- as.character(x)
    }
    if(length(x[which(!is.na(x))]) == 0){
      F
    }
    !anyNA(as.numeric(x[which(!is.na(x))]))
  })

}



try_date <- function(x, format) {
  
  suppressWarnings({
    
    dat <- as.Date(x, format)
    
  })
  
  if(all_na(dat)) {
    x
  } else {
    dat
  }
}
# 
# check_date <- function(x) {
#   
#   orders <- expand.grid(c("%m","%d","%Y"),c("%m","%d","%Y"),c("%m","%d","%Y"), stringsAsFactors = F)
#   
#   seperators <- c("/","-",".")
#   
#   attempts <- list()
#   
#   for(i in 1:length(orders)){
#     for(j in 1:length(seperators)){
#       try_date(x, format = paste(orders[i], sep = seperators[j]))
#     }
#   }
#   
#   
#   
# }

check_character <- function(x) {
  charcheck <- str_replace_all(x, "[0-9,\\.\\$\\%,]","")
  median(sapply(charcheck, nchar), na.rm = T) > 5
}

check_format <- function(x) {
  if (length(x[which(!is.na(x))]) == 0) {
    NA
  } else if (check_character(x)) {
    "character"
  }else if (check_numeric(x)) {
    "numeric"
  } else if (check_comma(x)) {
    "comma"
  } else if (check_percent(x)) {
    "percent"
  } else if (check_parens(x)) {
    "parens"
  } else {
    "character"
  }
}

convert_numeric <- function(x) {
  as.numeric(x)
}

convert_percent <- function(x) {
  paste0(str_extract_all(x, "[0-9\\.]", simplify = T), collapse = "")
}

convert_format <- function(x) {
  format <- check_format(x)
  
  do.call(paste0("convert_", format), x)
}

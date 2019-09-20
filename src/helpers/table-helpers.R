# table helper functions

## nas
any_values <- function(x) {
  length(which(!is.na(x))) > 0
}

all_na <- function(x) {
  !any_values(unique(x))
}

## replace empty strings in tables
replace_empty <- function(x) {
  UseMethod("replace_empty")
}

replace_empty.character <- function(x) {
  x[which(x == "")] <- NA
  x
}

replace_empty.data.frame <- function(x) {
  as.data.frame(lapply(x,
                       function(x) {
                         if (is.character(x)) { 
                           replace_empty(x) } 
                         else {x}
                         })
  )
}

replace_empty.data.table <- function(x) {
  as.data.table(lapply(x,
                       function(x) {
                         if (is.character(x)) { 
                           replace_empty(x) } 
                         else {x}
                       })
  )
}

# check column format
check_pattern <- function(x, pattern, threshold = 0.3) {
  sum(str_count(x, pattern))/length(x) > threshold
}

check_comma <- function(x, threshold = 0.3) {
  check_pattern(x, pattern = "[0-9],[0-9]{3}")
} 

check_percent <- function(x) {
  check_pattern(x, pattern = "[0-9]\\%")
}

check_numeric <- function(x) {
  !all_na(as.numeric(x))
}



## column functions
na_cols <- function(df) {
  sapply(df, all_na)
}

remove_na_cols <- function(df) {
  UseMethod("remove_na_cols")
}

remove_na_cols.data.frame <- function(df) {
  cols <- which(!na_cols(df))
  df[,cols]
}

remove_na_cols.data.table <- function(df) {
  cols <- which(!na_cols(df))
  df[,(cols), with = F]
} 

## row functions





# table helper functions

## nas
any_values <- function(x) {
  length(which(!is.na(x))) > 0
}

all_na <- function(x) {
  !any_values(unique(x))
}

almost_all_na <- function(x){
  length(which(is.na(x)))/length(x) > 0.8
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

## column functions
na_cols <- function(df) {
  sapply(df, all_na)
}

almost_na_cols <- function(df) {
  sapply(df, almost_all_na)
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


remove_almost_na_cols <- function(df) {
  UseMethod("remove_almost_na_cols")
}

remove_almost_na_cols.data.frame <- function(df) {
  cols <- which(!almost_na_cols(df))
  df[,cols]
}

remove_almost_na_cols.data.table <- function(df) {
  cols <- which(!almost_na_cols(df))
  df[,(cols), with = F]
} 


## row functions

na_rows <- function(df) {
  apply(df, all_na, MARGIN = 1)
}


remove_na_rows <- function(df) {
  UseMethod("remove_na_rows")
}

remove_na_rows.data.frame <- function(df) {
  rows <- which(!na_rows(df))
  df[rows,]
}

remove_na_rows.data.table <- function(df) {
  rows <- which(!na_rows(df))
  df[rows,]
} 




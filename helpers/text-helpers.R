# function for finding key word in context in a string
# useful for debugging
str_context <- function(string, pattern, window = 10) {
  locations <- str_locate(string, pattern)
  str_sub(string, locations[,1] - window, locations[,2] + window)
}

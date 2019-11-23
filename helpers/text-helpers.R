# function for finding key word in context in a string
# useful for debugging
str_context <- function(string, pattern, window = 10) {
  locations <- str_locate_all(string, pattern)[[1]]
  kwic_vector <- vector()
  for(i in 1:nrow(locations)) {
    kwic_vector[i] <-str_sub(string, locations[i,1] - window, locations[i,2] + window)
  }
  kwic_vector
}

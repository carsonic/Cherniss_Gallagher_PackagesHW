# part c code
col_means <- function(data_frame) {
  means <- numeric(ncol(data_frame))  
  for (i in seq_along(data_frame)) {
    means[i] <- mean(data_frame[[i]], na.rm = TRUE)
  }
  return(means)
}

# part d code
count_na <- function(vector) {
  count <- 0
  for (value in vector) {
    if (is.na(value)) {
      count <- count + 1
    }
  }
  return(count)
}

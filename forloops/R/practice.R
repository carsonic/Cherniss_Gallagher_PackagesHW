# part c code
#' @title Column Means Function
#' @details
#' It allows you to compute the means of colums in a given data frame
#'  
#' @param data_frame a data frame
#' @return a vector of numeric column means
#' @export
#' 
#' @examples
#' ex_data1 <- data.frame(a = c(1, 2, 3, 4), b = c(5, 6, 7, 8))
#' col_means(ex_data1)
#' 
#' ex_data2 <- data.frame(a = c(2, 4, 6, NA), b = c(8, 10, 12))
#' col_means(ex_data2)
col_means <- function(data_frame) {
  means <- numeric(ncol(data_frame))  
  for (i in seq_along(data_frame)) {
    means[i] <- mean(data_frame[[i]], na.rm = TRUE)
  }
  return(means)
}

# part d code
#' @title NA Count Function
#' @details
#' It allows you to count the number of NA values that appears in a given vector
#'  
#' @param vector a vector
#' @return an integer count of NA values
#' @export
#' 
#' @examples
count_na <- function(vector) {
  count <- 0
  for (value in vector) {
    if (is.na(value)) {
      count <- count + 1
    }
  }
  return(count)
}

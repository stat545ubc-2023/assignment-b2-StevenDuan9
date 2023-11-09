#' @title Function stat_info
#' @description This function output some statistics information about the input data.
#' @param data is the input data to be summarised.
#' @return the mean, median, range and variance of input data.
#' @export
#' @examples stat_info(c(1,2,3))
stat_info <- function(data) {
  if(!is.numeric(data)) {
    stop("Error. This function only works for numeric input data as input.\n")
  }
  if(length(data) < 1){
    stop("Error. This function only works for input with length greater than 0.\n")
  }
  Mean = mean(data, na.rm=T)
  Median = stats::median(data, na.rm=T)
  Range =  diff(range(data), na.rm=T)
  Variance = stats::var(data, na.rm=T)
  cat("mean:", Mean, "\n")
  cat("median:", Median, "\n")
  cat("range:", Range, "\n")
  cat("variance:", Variance, "\n")
  result = c(Mean, Median, Range, Variance)
  return(result)
}

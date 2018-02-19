#' @title compute_mean
#' @name compute_mean
#' @param arraydata: array of data 
#' @description Estimate the mean of the array


compute_mean <- function(arraydata){
  array_mean=plyr::aaply(arraydata,c(1,2),mean)
  return(array_mean)
}

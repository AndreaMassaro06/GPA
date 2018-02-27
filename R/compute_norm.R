#' @title compute_norm
#' @name compute_norm
#' @param arraydata: array of data
#' @description Estimate the normalized mean of the array


compute_norm <- function(arraydata){
  datanorm=arraydata/norm(arraydata,type="F")
  return(datanorm)
}

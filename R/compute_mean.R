#' @title compute_mean
#' @name compute_mean
#' @param arraydata: array of data 
#' @description Estimate the mean of the array


compute_mean <- function(arraydata)
  {
    somma<-arraydata[,,1]
    for(i in 2:dim(arraydata)[3])
    {
      somma<-somma+arraydata[,,i]
    }
    array_mean<-somma/dim(arraydata)[3]
    return(array_mean)
    
  }


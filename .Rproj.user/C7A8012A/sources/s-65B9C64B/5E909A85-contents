#'@name GPA
#'@title Generilazed Procustes Analysis
#'@param arraydata: array of data whit nrow,ncolum and the number of the matrix.
#'@param method: select a method for calculate GPA. "mean" used the matrix of mean for centrate the array of date.
#'"norm" used a normalized matrix of array, selected with the parameter n, for centrate the array of date.
#'@param maxit: maximum number of iteration inside the GPA funtion (default maxit=10).
#'@param mindist: select the minimum distance between the matrix (default mindist=1).
#'@param n: select the matrix to normalized for method "norm". It must be used into method "norm" (default n=NULL).
#'@param verbose TRUE
#'@description The function that calculate the new ruotate arrays
#'@export
#'@examples
#'arraydata=array(rnorm(10*50*4),c(10,50,4))
#'res=GPA(arraydata)
#'plot(res$epsilons,type="l")

GPA<-function(arraydata,method=c("mean","norm"),maxit=10,mindist=1/100,n=NULL,verbose=TRUE)
{
  if(!is.array(arraydata))
  {
    warning("Insert an array of matrix.")
  
    }else if(method=="mean" && (is.null(n) || is.numeric(n)) && (maxit>0 && is.numeric(maxit))&& (mindist>0 && is.numeric(mindist)))
  {
      n<-NULL
      GPAmean(arraydata,mindist,maxit,verbose=verbose)
      
    } else if(method=="norm" && (!is.null(n) && n>0 && n<=(dim(arraydata)[3])) && (maxit>0 && is.numeric(maxit))) {
      
      mindist<-NULL
      GPAnorm(arraydata,n,maxit=10)
      
    } else if(method=="norm" && (is.null(n)|| n<=0  || !is.numeric(n)||n>=(dim(arraydata)[3]))){
      
      warning("Insert an acceptable n value for method norm.")
      
    }
  
    else warning("Invalid mathod")
}


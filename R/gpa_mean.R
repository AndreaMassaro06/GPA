#'@title GPAmean
#'@name GPAmean
#'@param arraydata :initial array where i want to apply gpa
#'@param mindist: the minimal distace beetwen the matrix inside the array
#'@param maxit: maximum number of iteration
#'@description The function that calculate the new ruotate arrays using the mean


GPAmean <- function(arraydata,mindist,maxit){
  distanzainiz<-0
  cont <- 1
  dist2<-mindist+1
  distanze<-0 #creo una variabile che mi tiene le misure delle distanze
  
  mean_start<-compute_mean(arraydata)
  
  while(dist2>mindist && cont<=maxit)
  {
    tabruotate<-plyr::aaply(arraydata,3, function(A)procustemean(A,mean_start)$A)
    tabruotate=aperm(tabruotate,c(2,3,1))
    #str(tabruotate)
    distanzainiz[cont]<-min(aaply(tabruotate,3,function(x)norm(x-mean_start,type="F")))
    #distanzainiz[cont]
    #str(tabruotate)
    mean_new <- compute_mean(tabruotate)
    #str(mean_start2)
    #mean_start2=as.matrix(mean_start2)
    #str(mean_start2)
    
    #calcolo la nuova distanza con la norma di Frobenius
    dist2=norm(mean_new-mean_start,type="F")
    distanze[cont]<-dist2
    
    
    cat("\n fine ciclo di iterazione numero:",cont,"\n distanza iniziale:",distanzainiz[cont],"\n nuova distanza:",dist2,"\n")
    
    cont=cont+1
    mean_start=mean_new
    
    #rm(dist2)
    gc()
  }
  list(Aarray=tabruotate,mean=mean_start_new)
}

#'@title GPAnorm
#'@name GPAnorm
#'@param arraydata : array with the matrix of dates
#'@param n: selection of the matrix for calculate the norm
#'@param maxit: maximum number of the iteration
#'@description The function that calculate the new ruotate arrays whit the normalizzed matrix


GPAnorm<-function(arraydata,n,maxit){
  distanzainiz<-0
  cont <- 1
  #dist2<-mindist+1
  distanze<-0 #creo una variabile che mi tiene le misure delle distanze
  matrice<-arraydata[,,n]
  mean_start<- compute_norm(matrice)
  mean_start_0<-mean_start
  while(cont<=maxit)
  {
    tabruotate<-plyr::aaply(arraydata,3, function(A)procustenorm(A,mean_start)$A)
    tabruotate=aperm(tabruotate,c(2,3,1))
    #str(tabruotate)
    distanzainiz[cont]<-min(aaply(tabruotate,3,function(x)norm(x-mean_start,type="F")^2))
    #distanzainiz[cont]
    #str(tabruotate)
    mean_new <- compute_mean(tabruotate)
    #str(mean_start2)
    #mean_start2=as.matrix(mean_start2)
    #str(mean_start2)
    
    #Calcolo gamma di mean_start2
    
    mean_start_new<-procustenorm(mean_new,mean_start_0)$A
    
    #calcolo la nuova distanza con la norma di Frobenius
    dist2=norm(mean_start_new-mean_start_0,type="F")^2
    distanze[cont]<-dist2
    
    
    cat("\n fine ciclo di iterazione numero:",cont,"\n distanza iniziale:",distanzainiz[cont],"\n nuova distanza:",dist2,"\n")
    
    cont=cont+1
    mean_start=compute_norm(mean_start_new)
    
    #rm(dist2)
    gc()
  }
  list(Aarray=tabruotate,mean=mean_start_new)
}
GPAmatr<-function(arraydata,n,maxit,mindist,verbose=TRUE,normalization=FALSE){
  cont <- 1
  dist2<-Inf
  distanze<-0 #creo una variabile che mi tiene le misure delle distanze
  matrice<-arraydata[,,n]
  if(normalization)
  {
  arraydata<-plyr::aaply(arraydata,2,function(x)scale(x,scale = FALSE))
  arraydata<-aperm(arraydata,c(2,1,3))  
  mean_new<- compute_norm(matrice)
  mean_start_0<-mean_new
  }else mean_new<-matrice
  mean_start_0<-mean_new
  
  distanzainiz<-sum(plyr::aaply(arraydata,3,function(x)norm(x-mean_new,type="F")^2))
  
  if(verbose) cat("\n distanza iniziale:",distanzainiz[cont],"\n")
  
  while(dist2>mindist && cont<=maxit)
  {
    cont=cont+1
    mean_start<-mean_new
    rm(mean_new)
    if(normalization)
    {
      tabruotate<-plyr::aaply(arraydata,3, function(A)procustenorm(A,mean_start)$A)
    }
    else tabruotate<-plyr::aaply(arraydata,3, function(A)procustemean(A,mean_start)$A)
    tabruotate=aperm(tabruotate,c(2,3,1))
    distanzainiz[cont]<-sum(plyr::aaply(tabruotate,3,function(x)norm(x-mean_start,type="F")^2))
    #str(tabruotate)
    #distanzainiz[cont]
    #str(tabruotate)
    mean_new <- compute_mean(tabruotate)
    #str(mean_start2)
    #mean_start2=as.matrix(mean_start2)
    #str(mean_start2)
    
    #Calcolo gamma di mean_start2
    if(normalization)
    {
    mean_start_new<-procustenorm(mean_new,mean_start_0)$A
    }else mean_start_new<-procustemean(mean_new,mean_start_0)$A
    #calcolo la nuova distanza con la norma di Frobenius
    #dist2=norm(mean_start_new-mean_start_0,type="F")^2
    #distanze[cont]<-dist2
    
    dist2=(distanzainiz[cont-1]-distanzainiz[cont])/distanzainiz[cont]
    
    
    if(verbose) cat("\n fine ciclo di iterazione numero:",cont-1,"\n nuova distanza:",distanzainiz[cont],"\n")
    
    
    if(normalization)
      {
    mean_new=compute_norm(mean_start_new)
    rm(mean_start_new)
    }else mean_new<-mean_start_new
    rm(mean_start_new)
    #rm(dist2)
    gc()
  }
  distanzainiz<-distanzainiz[-1]
  list(Aarray=tabruotate,mean=mean_start,distance=distanzainiz)
}

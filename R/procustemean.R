procustemean<-function(A,B)
{
  n<-min(dim(A)[1:2])-1
  dati2<-A #inizializzo i dati
  gam<-t(B)%*%dati2 
  sv<-irlba::irlba(gam,nv = n,nu=n) #eseguo svd (meglio di svd)
  # sv<-svd(gam)
  # rm(gam)
  u=as.matrix(sv$u)
  v=t(as.matrix(sv$v))
  R<-u%*%v #calcolo la matrice R
  #str(R)
  rm(sv)
  tabruotate<-as.matrix(dati2%*%t(R)) #calcolo le nuove matrici ruotate
  rm(gam,R)
  #distprova[i]<-norm(out-xhat,type="F")^2
  rm(dati2)
  #str(distprova)
  # str(tabruotate)
  gc()
  return(list(A=tabruotate))
}


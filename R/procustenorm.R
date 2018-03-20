procustenorm<-function(A,B)
{
  n<-min(dim(A)[1:2])-1
  dati2<-A #inizializzo i dati
  gam<-t(B)%*%dati2 
  sv<-rARPACK::svds(gam,n) #eseguo svd (meglio di svd)
  # sv<-svd(gam)
  # rm(gam)
  u=as.matrix(sv$u)
  v=t(as.matrix(sv$v))
  R<-u%*%v #calcolo la matrice R
  #str(R)
  rm(sv)
  beta<-compute_beta(R,gam,dati2)
  tabruotate<-as.matrix(beta*dati2%*%t(R)) #calcolo le nuove matrici ruotate
  rm(gam,R,beta)
  #distprova[i]<-norm(out-xhat,type="F")^2
  rm(dati2)
  #str(distprova)
  #str(tabruotate)
  # rm(beta)
  gc()
  return(list(A=tabruotate))
}

compute_beta<-function(R,gam,dati)
{
  numerator<-crossprod(as.vector(R),as.vector(gam))
  denominator<-norm(dati,type="F")^2
  beta<-numerator/denominator
  beta<-as.numeric(beta)
}





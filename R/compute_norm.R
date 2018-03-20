compute_norm <- function(arraydata){
  datanorm=arraydata/norm(arraydata,type="F")
  return(datanorm)
}

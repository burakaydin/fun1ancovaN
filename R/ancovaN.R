#' Ancova n Function
#'
#' This function allows you to find n.
#' @param simple parats.
#' @keywords ancova
#' @export
#' @examples
#' ancovaN()

ancovaN=function(
  pow=.9,
  del=.4,
  r2=.5,
  alp=.05){
  ns=seq(5,15000,by=1)
  ncps=ns*(del^2/(4*(1-r2)))
  tpow=matrix(0,nrow=14995)
  for(i in 1:14995){
    tpow[i,]=1-pf(qf((1-alp),1,ns[i]-3),1,ns[i]-3,ncp=ncps[i])
  }
  out=ns[min(which(tpow>pow))]
  return(out)}


#' Title prevision and prevision error acording to AgregHistunif.err
#'
#' @param xx data vector
#' @param grille grid for density evaluation
#' @param nbr breaks number
#' @param B number of histograms to aggregate
#' @param dobs observations
#' @export
AgregHistunif.err = function(xx,grille=aa,nbr = 50, B= 10,dobs) {
  fin = 0
  err00=NULL
  zz = hist(xx,breaks=mybreaks(xx,nbr),plot=F,warn.unused = F)$breaks
  z=diff(zz)
  h=z[1]
  mx = min(xx)
  Mx = max(xx)
  for(i in 1:B)
  {
    newb = zz + runif(length(zz),0,h)
    hs2=hist(xx,breaks=c(mx,newb,Mx),plot=F,warn.unused = F)
    fin= fin + predict.hist(hs2,grille)
    previ=fin/i
    err00=rbind(err00,error(dobs,previ))
  }
  list(prev=fin/B,erreur=err00)
}

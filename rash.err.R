#' Title Title prevision and prevision error acording to rash.err
#'
#' @param xx data vector
#' @param grille grid for density evaluation
#' @param nbr breaks number
#' @param B number of histograms to aggregate
#' @param dobs observations

#' @export
rash.err = function(xx,grille=aa,nbr = 50, B= 10,dobs) {
  fin = 0
  err00=NULL
  zz = hist(xx,breaks=mybreaks(xx,nbr),plot=F,warn.unused = F)$breaks
  mx = min(xx)
  Mx = max(xx)
  for(i in 1:B)
  {
    blabla=sqrt(abs(min(diff(zz))))
    newb = zz + rnorm(1,0,blabla)
    newb=sort(newb)
    if(min(newb) > mx) newb= c(mx,newb)
    if(max(newb) < Mx) newb= c(newb, Mx)
    hs2=hist(xx,breaks=newb,plot=F,warn.unused = F)
    fin= fin + predict.hist(hs2,grille)
    previ=fin/i
    err00=rbind(err00,error(dobs,previ))
  }
  list(prev=fin/B,erreur=err00)
}

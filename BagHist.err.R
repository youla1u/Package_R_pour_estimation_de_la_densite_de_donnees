#' Title prevision and prevision error acording to  BagHist.err
#'
#' @param xx data vector
#' @param grille grid for density evaluation
#' @param B number of histograms to aggregate
#' @param dobs observations
#'
#' @return prevision and prevision error
#' @export
BagHist.err = function(xx,grille=aa,B= 10,dobs) {
  n = length(xx)
  err00=NULL
  fin = 0
  mx = min(xx)
  Mx = max(xx)
  for(i in 1:B)    {
    xb = xx[sample(n,replace=T)]
    nbr=bropt(xb)$opt
    hs2=hist(xb,breaks=mybreaks(xb,nbr),plot=F,warn.unused = F)
    fin= fin + predict.hist(hs2,grille)
    previ=fin/i
    err00=rbind(err00,error(dobs,previ))
  }
}

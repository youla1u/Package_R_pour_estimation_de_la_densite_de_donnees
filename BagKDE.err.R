#' Title prevision and prevision error acording BagKDE
#'
#' @param xx  data vector
#' @param grille grid for density evaluation
#' @param B number of histograms to aggregate
#' @param dobs  observation
#'
#' @return prevision and prevision error
#' @export
BagKDE.err = function(xx,grille=aa,B= 10,dobs) {
  fin = 0
  err00=NULL
  n = length(xx)
  for(i in 1:B) {
    xb = xx[sample(n,replace=T)]
    kk = kde(xb,h=bw.ucv(xb),eval.points=grille)
    fin = fin + kk$estimate
    previ=fin/i
    err00=rbind(err00,error(dobs,previ))
  }
  list(prev=fin/B,erreur=err00)
}

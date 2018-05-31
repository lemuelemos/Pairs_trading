library(RcppArmadillo)
library(Rcpp)

### sign for trading
cppFunction(
  'CharacterMatrix sncalc(int ncol, int nrow, NumericMatrix Zm, NumericVector tr,CharacterMatrix sinal){
    for(int j=0;j < ncol; j++){
      for(int i=1;i < nrow;i++){
    if(Zm(i,j) > tr(0) && sinal(i-1,j) != "OpenLeft" || sinal(i-1,j) == "OpenRight" && Zm(i,j) > -tr(1)) sinal(i,j) = "OpenRight";
    else if(Zm(i,j) < -tr(0) && sinal(i-1,j) != "OpenRight" || sinal(i-1,j) == "OpenLeft" && Zm(i,j) < tr(1)) sinal(i,j) = "OpenLeft";
    else if(Zm(i,j) < -tr(1) && sinal(i-1,j) == "OpenRight") sinal(i,j) = "OutRight";
    else if(Zm(i,j) > tr(1) && sinal(i-1,j) == "OpenLeft") sinal(i,j) = "OutLeft";
    else sinal(i,j) = "Fora";
    }
  }
  return sinal;
 } '
)
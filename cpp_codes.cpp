#include <Rcpp.h> 
using namespace Rcpp;

// [[Rcpp::export]]

CharacterMatrix sncalc(int ncol, int nrow, NumericMatrix Zm, NumericVector tr,CharacterMatrix sinal){
  for(int j=0;j < ncol; j++){
    for(int i=1;i < nrow;i++){
      if((Zm(i,j) > tr(0) && sinal(i-1,j) != "OpenLeft") || (sinal(i-1,j) == "OpenRight" && Zm(i,j) > -tr(1))) sinal(i,j) = "OpenRight";
      else if((Zm(i,j) < -tr(0) && sinal(i-1,j) != "OpenRight") || (sinal(i-1,j) == "OpenLeft" && Zm(i,j) < tr(1))) sinal(i,j) = "OpenLeft";
      else if(Zm(i,j) < -tr(1) && sinal(i-1,j) == "OpenRight") sinal(i,j) = "OutRight";
      else if(Zm(i,j) > tr(1) && sinal(i-1,j) == "OpenLeft") sinal(i,j) = "OutLeft";
      else sinal(i,j) = "Fora";
    }
  }
  return sinal;
}

// [[Rcpp::export]]

  NumericMatrix returcalc(CharacterMatrix sinal, NumericMatrix par){
  NumericMatrix invest;
  NumericMatrix retorno;
  CharacterMatrix t_oper;
  int r = sinal.nrow(); 
  int c = sinal.ncol();
  for(int j=0; j < c; j++){
    for(int i=1; i < r; i++){
      invest(i,j) = invest(i-1,j);
    }
  }
}
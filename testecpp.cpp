#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

double returcalct(CharacterMatrix sinal, NumericMatrix par, NumericVector betas){
  int r = sinal.nrow(); 
  int c = sinal.ncol();
  NumericMatrix invest(r,c);
  NumericMatrix retorno(r,c);
  CharacterMatrix t_oper(r,c);
  double plongri = 0;
  double pshortri = 0;
  double plongrf = 0;
  double pshortrf = 0;
  double plongl = 0;
  double pshortl = 0;
  double portli = 0;
  double portsi = 0;
  double portlf = 0;
  double portsf = 0;
  double porti = 0;
  double portf = 0;
  for(int j=0; j < c; j++){
    for(int i=1; i < r; i++){
      //invest(i,j) = invest(i-1,j);
      if((sinal(i,j) == "OpenRight" && sinal(i-1,j) == "Fora" && i != r)
           || (sinal(i,j) == "OpenRight" && sinal(i-1,j) == "OutLeft" && i != r)
           || (sinal(i,j) == "OpenRight" && sinal(i-1,j) == "OutRight" && i != r)){
           plongri = par(i,1);
      }
    }
  }
  return plongri;
}

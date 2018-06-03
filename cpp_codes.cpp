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

NumericVector returcalc(CharacterVector sinal, NumericMatrix par, double betas,
                        NumericVector invest){
  int r = sinal.size(); 
  //int c = sinal.ncol();
  NumericVector t_oper(r);
  NumericVector retorno(r);
  int k = 0;
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
      for(int i = 1; i < r; i++){
      if(i>k){
        invest(i) = invest(i-1);
      if((sinal(i) == "OpenRight" && sinal(i-1) == "Fora" && i != r)
           || (sinal(i) == "OpenRight" && sinal(i-1) == "OutLeft" && i != r)
           || (sinal(i) == "OpenRight" && sinal(i-1) == "OutRight" && i != r)){
           plongri = par(i,1);
           pshortri = par(i,0);
        if((plongri*betas/pshortri) < 1){
          portli = -((plongri*betas/pshortri)*invest(i-1));
          portsi = invest(i-1);
          porti = portli+portsi;
          //t_oper(i,j) = "Abriu";
          for(k = i; k < r; k++){
            plongrf = par(k,1);
            pshortrf = par(k,0);
            portlf = (plongrf/plongri)*portli;
            portsf = (pshortrf/pshortri)*portsi;
            portf = (portli-portlf)+(portsi-portsf);
            //retorno(k) = portf/invest(i);
            invest(k) = ((portf/invest(i))+1)*invest(i);
            if((sinal(k) == "OutRight" && sinal(k-1) == "OpenRight")
                 || (k == r && sinal(k-1) == "OpenRight")){
              plongrf = par(k,1);
              pshortrf = par(k,0);
              portlf = (plongrf/plongri)*portli;
              portsf = (pshortrf/pshortri)*portsi;
              portf = (portli-portlf)+(portsi-portsf);
              retorno(k) = portf/invest(i);
              invest(k) = (retorno(k)+1)*invest(i);
              break;
            }
          }
        } else{
          
        }
      }
     }
    }
  return invest;
}


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

List returcalc_for(NumericVector sinal, NumericMatrix par, double betas,
                   NumericVector invest, NumericVector tr){
  int r = sinal.size(); 
  //int c = sinal.ncol();
  //NumericVector t_oper(r);
  NumericVector retorno(r);
  int k = 0;
  double plongi = 0;
  double pshorti = 0;
  double plongf = 0;
  double pshortf = 0;
  double portli = 0;
  double portsi = 0;
  double portlf = 0;
  double portsf = 0;
  NumericVector porti(r);
  //double portf = 0;
  CharacterVector t_oper(r);
  t_oper(0) = "Fora";
  for(int i = 1; i < r; i++){
    if(i>k){
      invest(i) = invest(i-1);
      //retorno(i) = 1;
    }
    if((sinal(i) > tr(0) && sinal(i-1) > -tr(1) && i != r && i > k)){
      plongi = par(i,1);
      pshorti = par(i,0);
      if(((plongi*betas)/pshorti) < 1){
        portli = ((betas*plongi)/pshorti)*invest(i-1);
        portsi = invest(i-1);
        porti(i) = portsi-portli;
        t_oper(i) = "Abriu_R1";
        for(k = i; k < r; k++){
          plongf = par(k,1);
          pshortf = par(k,0);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) < -tr(1) && sinal(k-1) > -tr(1))
               || (k == r)){
            plongf = par(k,1);
            pshortf = par(k,0);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_R1";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      } else{
        portli = invest(i-1);
        portsi = (pshorti/(betas*plongi))*invest(i-1);
        t_oper(i) = "Abriu_R2";
        porti(i) = portsi-portli;
        for(k = i; k < r; k++){
          plongf = par(k,1);
          pshortf = par(k,0);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) < -tr(1) && sinal(k-1) > -tr(1))
               || (k == r)){
            plongf = par(k,1);
            pshortf = par(k,0);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_R2";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      }
    } else if((sinal(i) < -tr(0) && sinal(i-1) < tr(1) && i != r && i > k)){
      plongi = par(i,0);
      pshorti = par(i,1);
      if(((pshorti*betas)/plongi) < 1){
        portli = invest(i-1);
        portsi = (((pshorti*betas)/plongi)*invest(i-1));
        porti(i) = portsi-portli;
        t_oper(i) = "Abriu_L1";
        for(k = i; k < r; k++){
          plongf = par(k,0);
          pshortf = par(k,1);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) > tr(1) && sinal(k-1) < tr(1))
               || (k == r)){
            plongf = par(k,0);
            pshortf = par(k,1);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_L1";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      } else {
        portli = ((plongi/(betas*pshorti))*invest(i-1));
        portsi = (invest(i-1));
        porti(i) = portsi-portli;
        t_oper(i) = "Abriu_L2";
        for(k = i; k < r; k++){
          plongf = par(k,0);
          pshortf = par(k,1);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) > tr(1) && sinal(k-1) < tr(1))
               || (k == r)){
            plongf = par(k,0);
            pshortf = par(k,1);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_L2";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      }
    } else{
      if(i < k){
        t_oper(i) = "Aberto";
      } else if (i>k){
        t_oper(i) = "Fora";
      } else if(t_oper(i-1) == "Fora"){
        invest(i) = invest(i-1);
      }
    }
  }
  List results;
  results["invest"] = invest;
  results["retorno"] = retorno;
  results["tt"] = t_oper;
  results["portfolio"] = porti;
  return results;
}


// [[Rcpp::export]]

List returcalc_trad(NumericVector sinal, NumericMatrix par, double betas,
                   NumericVector invest, NumericVector tr,double lmt_perca){
  int r = sinal.size(); 
  //int c = sinal.ncol();
  //NumericVector t_oper(r);
  NumericVector retorno(r);
  int k = 0;
  double plongi = 0;
  double pshorti = 0;
  double plongf = 0;
  double pshortf = 0;
  double portli = 0;
  double portsi = 0;
  double portlf = 0;
  double portsf = 0;
  NumericVector porti(r);
  //double portf = 0;
  CharacterVector t_oper(r);
  t_oper(0) = "Fora";
  for(int i = 1; i < r; i++){
    if(i>k){
      invest(i) = invest(i-1);
      //retorno(i) = 1;
    }
    if((sinal(i) > tr(0) && sinal(i-1) > -tr(1) 
          && i != r && i > k) 
          && invest(i-1) >= (lmt_perca*invest(0))){
      plongi = par(i,1);
      pshorti = par(i,0);
      if(((plongi*betas)/pshorti) < 1){
        portli = ((betas*plongi)/pshorti)*invest(i-1);
        portsi = invest(i-1);
        t_oper(i) = "Abriu_R1";
        porti(i) = portsi-portli;
        for(k = i; k < r; k++){
          plongf = par(k,1);
          pshortf = par(k,0);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) < -tr(1) && sinal(k-1) > -tr(1) 
                && invest(k) >= (lmt_perca*invest(0)))
               || (k == r)
               || invest(k) < lmt_perca){
            plongf = par(k,1);
            pshortf = par(k,0);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_R1";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      } else{
        portli = invest(i-1);
        portsi = (pshorti/(betas*plongi))*invest(i-1);
        t_oper(i) = "Abriu_R2";
        porti(i) = portsi-portli;
        for(k = i; k < r; k++){
          plongf = par(k,1);
          pshortf = par(k,0);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) < -tr(1) && sinal(k-1) > -tr(1) 
                && invest(k) >= (lmt_perca*invest(0)))
               || (k == r)
               || invest(k) < lmt_perca){
            plongf = par(k,1);
            pshortf = par(k,0);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_R2";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      }
    } else if((sinal(i) < -tr(0) && sinal(i-1) < tr(1) 
                 && i != r && i > k) 
                && invest(i-1) >= (lmt_perca*invest(0))){
      plongi = par(i,0);
      pshorti = par(i,1);
      if(((pshorti*betas)/plongi) < 1){
        portli = invest(i-1);
        portsi = (((pshorti*betas)/plongi)*invest(i-1));
        porti(i) = portsi-portli;
        t_oper(i) = "Abriu_L1";
        for(k = i; k < r; k++){
          plongf = par(k,0);
          pshortf = par(k,1);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) > tr(1) && sinal(k-1) < tr(1) && (lmt_perca*invest(0)))
               || (k == r)
               || invest(k) < lmt_perca){
            plongf = par(k,0);
            pshortf = par(k,1);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_L1";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      } else {
        portli = ((plongi/(betas*pshorti))*invest(i-1));
        portsi = (invest(i-1));
        porti(i) = portsi-portli;
        t_oper(i) = "Abriu_L2";
        for(k = i; k < r; k++){
          plongf = par(k,0);
          pshortf = par(k,1);
          portlf = (plongf/plongi)*portli;
          portsf = (pshortf/pshorti)*portsi;
          invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
          if((sinal(k) > tr(1) && sinal(k-1) < tr(1) && 
             invest(k) >= (lmt_perca*invest(0)))
               || (k == r)
               || invest(k) < lmt_perca){
               plongf = par(k,0);
            pshortf = par(k,1);
            portlf = (plongf/plongi)*portli;
            portsf = (pshortf/pshorti)*portsi;
            invest(k) = invest(i-1)+((portsi-portsf)-(portli-portlf));
            t_oper(k) = "Saiu_L2";
            retorno(k) = invest(k)/invest(i);
            break;
          }
        }
      }
    } else{
      if(i < k){
        t_oper(i) = "Aberto";
      } else if (i>k){
        t_oper(i) = "Fora";
      } else if(t_oper(i-1) == "Fora"){
        invest(i) = invest(i-1);
      }
    }
  }
  List results;
  results["invest"] = invest;
  results["retorno"] = retorno;
  results["tt"] = t_oper;
  results["portfolio"] = porti;
  return results;
}
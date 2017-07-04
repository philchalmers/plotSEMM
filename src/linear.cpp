#include <Rcpp.h>
using namespace Rcpp;

// hack fix
void R_init_mirt(DllInfo* info) {
    R_registerRoutines(info, NULL, NULL, NULL, NULL);
    R_useDynamicSymbols(info, TRUE);
}

RcppExport SEXP linear(SEXP RlbCE, SEXP RubCE, SEXP Rx) 
{
    BEGIN_RCPP

    /* Linearity function for computing the crazy intense, patent-pending 'Pek test'
    
    */

    const NumericVector lb(RlbCE);
    const NumericVector ub(RubCE);
    const NumericVector x(Rx);

    const int points = x.length();
    NumericMatrix slope(points, points);
    NumericMatrix Int(points, points);
    NumericMatrix lines(points*points, points);
    
    //slopes and intercepts
    for(int j = 0; j < points; j++){
        for(int i = 0; i < points; i++){
            if(i != j){
                slope(i, j) = (ub(j) - lb(i)) / (x(j) - x(i));
                Int(i, j) = ub(j) - slope(i, j) * x(j);
            }
        } 
    }

    //compute expected lines
    int k = 0;
    for(int j = 0; j < points; j++){
        for(int i = 0; i < points; i++){
            if(i != j){
                for(int m = 0; m < points; m++){
                    double tmp = Int(i,j) + slope(i,j) * x(m);
                    lines(k,m) = tmp;
                }
                k++;
            }
        }
    }

    //set to 1 for debugging
    if(0){
        List Debug;
        Debug["slope"] = slope;
        Debug["Int"] = Int;
        Debug["lines"] = lines;
        return(Debug);
    }    

    return(lines);

    END_RCPP
}


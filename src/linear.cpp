#include <Rcpp.h>
using namespace Rcpp;

RcppExport SEXP linear(SEXP RlbCE, SEXP RubCE, SEXP Rx) 
{
    BEGIN_RCPP

    /* Linearity function for computing the crazy intense, patent-pending 'Pek test'
    
    */

    NumericMatrix lbCE(RlbCE);
    NumericMatrix ubCE(RubCE);
    NumericVector x(Rx);
    int reps = lbCE.nrow();

    int i, j, r, k;    
    int points = lbCE.nrow(); //collect nrow attribute from lbCE; pretty snazzy eh?
    double tmp;
    NumericMatrix slope(points, points);
    NumericMatrix Int(points, points);
    IntegerVector linearity(reps);
    NumericMatrix lines(points*points, points);
    NumericMatrix inside(points*points, points);
    NumericVector lb, ub;
    IntegerVector count(points*points);
    List ret;
    
    for(r = 0; r < reps; r++){ 
        lb = lbCE(_,r);
        ub = ubCE(_,r);
        for(j = 0; j < points; j++){
            for(i = 0; i < points; i++){
                slope(i, j) = (ub(j) - lb(i)) / (x(j) - x(i));
                Int(i, j) = ub(j) - slope(i, j) * x(j);
            } 
        }

        //compute lines
        k = 0;
        for(j = 0; j < points; j++){
            for(i = 0; i < points; i++){
                for(int m = 0; m < points; m++){
                    tmp = Int(i,j) + slope(i,j) * x(m);
                    tmp = std::floor(tmp*1000000 + 0.5)/1000000; // round the hard way...
                    lines(k,m) = tmp;
                }                    
                k++;
            }
        }
        // round values of lb, ub, and lines to 6 decimals....how?  
		for(i = 0; i < points; i++){
			lb(i) = std::floor(lb(i) * 1000000 + 0.5)/1000000;
			ub(i) = std::floor(ub(i) * 1000000 + 0.5)/1000000;		
		}
        
        k = 0;
        for(j = 0; j < points*points; j++){
            for(i = 0; i < points; i++)
               inside(k,i) = lb(i) <= lines(j,i) && ub(i) >= lines(j,i);                           
            k++;
        }
        
        // loop until inside counts == points, and assign 1 to linearity if true
        for(i = 0; i < points*points; i++){
            count(i) = 0;
            for(j = 0; j < points; j++)
                count(i) += inside(i, j);
            if(count(i) == points){
                ret["linearity"] = 1;
                ret["y1"] = lines(i, 0);
                ret["y2"] = lines(i, points-1);
                return(ret);
            }
        }
    }

    //set to 1 for debugging
    if(0){
        List Debug;
        Debug["lb"] = lb;
        Debug["ub"] = ub;
        Debug["slope"] = slope;
        Debug["Int"] = Int;
        Debug["lines"] = lines;
        Debug["inside"] = inside;
        return(Debug);
    }    

    ret["linearity"] = 0;
    return(ret);

    END_RCPP
}


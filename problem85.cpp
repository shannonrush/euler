#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

int FitRectangles(int a, int b) {
    int count = 0;
    for (int i=1; i<=a; i++) {
        for (int j=1; j<=b; j++) {
            for (int x=0; x<a; x++) {
                for (int y=0; y<b; y++) {
                    if (x+i<=a && y+j<=b) {
                        count++;
                    }
                }
            }
        }
    }
    return count;
}



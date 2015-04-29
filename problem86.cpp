#include <Rcpp.h>
using namespace Rcpp;

float ShortestRoute(float a, float b, float c) {
    float x = sqrt(pow(b+a,2)+pow(c,2));
    float y = sqrt(pow(c+a,2)+pow(b,2));
    float z = sqrt(pow(c+b,2)+pow(a,2));
    return std::min(std::min(x, y), z);
}

// [[Rcpp::export]]

int CountRoutes(int m) {
    int count = 0;
    int a = m;
    int b = 1;
    int c = 1;
    while (!(a==m && b==m && c==m)) {
        if (fmod(ShortestRoute(a,b,c),1)==0) count++;
        if (b==c) {
            b++;
            c = 1;
        } else {
            c++;
        }
    }
    return count;
} 

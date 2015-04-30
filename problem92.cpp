#include <Rcpp.h>
using namespace Rcpp;

std::vector<int> IntToDigits(int i) {
    std::vector<int> digits;
    while (i!=0) {
        int d = i%10;
        digits.push_back(d);
        i = (i-d)/10;
    }
    return digits;
}

int DigitsToSquare(std::vector<int> digits) {
    int sum = 0;
    for (int i=0; i<digits.size(); i++) {
        sum += pow(digits[i],2);
    }    
    return sum;
}

// [[Rcpp::export]]
int CountChains() {
    int count = 0;
    std::vector<int> digits;
    for (int i=1; i<10000000; i++) {
        int s = i;
        while (!(s==1||s==89)) {
            digits = IntToDigits(s);
            s = DigitsToSquare(digits);
        }
        if (s==89) count++;
    }
    return count;
}


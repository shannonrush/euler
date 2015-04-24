#include <iostream>
#include <stdio.h>
#include <gmp.h>
#include <gmpxx.h>
#include <vector>
using namespace std;

int Pent(int n) {
    return (n*(3*n-1))/2;
}

void P() {
    vector<int> k;
    k.reserve(1000);
    for (int j=1; j<=500; j++) {
        k.push_back(Pent(j));
        k.push_back(Pent(-j));
    }
    static const int sign[] = {1,1,-1,-1};
    int m = 1000000;
    vector<mpz_class> p;
    p.reserve(75000);
    p.push_back(1);
    p.push_back(1);
    p.push_back(2);
    int n = 3;
    while (p.back()%m!=0) {
        mpz_class sum = 0;
        int i = 0;
        while (n>=k[i]) {
            sum = sum + p[n-k[i]] * sign[i%4];
            i++;
        }
        p.push_back(sum);
        n++;
    }
    cout << "answer is " << n-1;
}

int main() {
    P();
}

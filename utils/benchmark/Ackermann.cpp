// -*- mode: c++ -*-
// $Id: ackermann.g++,v 1.3 2001/06/20 03:20:02 doug Exp $
// http://www.bagley.org/~doug/shootout/

#include <iostream>
#include <stdlib.h>

using namespace std;

int Ack(int M, int N) {
    if (M == 0) return( N + 1 );
    if (N == 0) return( Ack(M - 1, 1) );
    return( Ack(M - 1, Ack(M, (N - 1))) );
}

int main(int argc, char *argv[]) {
    int n = 13; 

    cout << "Ack(3," << n << "): " << Ack(3, n) << endl;
    return(0);
}

// -*- mode: c++ -*-
// $Id: ackermann.g++,v 1.3 2001/06/20 03:20:02 doug Exp $
// http://www.bagley.org/~doug/shootout/
//
// With some modifications to improve timing.

#include <iostream>
#include <cstdlib>
#include <cstdint>

extern "C" {
#include <mach/mach_time.h>
}

using namespace std;

int Ack(int M, int N) {
    if (M == 0) return( N + 1 );
    if (N == 0) return( Ack(M - 1, 1) );
    return( Ack(M - 1, Ack(M, (N - 1))) );
}

int main(int argc, char *argv[]) {
  // Only time Ack not buffering or c++ or anything like that.
  uint64_t start = mach_absolute_time();
  volatile int result = Ack(3, 13);
  uint64_t end = mach_absolute_time() - start;

  printf("%llu nanoseconds.\n", end);
  return result;
}

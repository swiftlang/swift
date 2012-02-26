#include <inttypes.h>
#include <stdio.h>

extern "C" void _T3fib5printFT1iNSs5int64_T_(int64_t l) {
  printf("%lld\n", l);
}

extern "C" void _T3fib5printFT1iNS_6double_T_(double l) {
  printf("%f\n", l);
}

extern "C" void _T5nbody5printFT1iNSs6double_T_(double l) {
  printf("%f\n", l);
}

// This cannot be implemented in swift.swift until we have pattern matching.
extern "C" bool _TSs19convertToLogicValueFT1vNSs4bool_i1(bool b) {
  return b;
}

extern "C"
double
_TNSs6double26convert_from_float_literalFT3valf64_S_(double x)
{
    return x;
}

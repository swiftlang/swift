#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_RETURNS_LARGE_CLASS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_RETURNS_LARGE_CLASS_H

struct LargeClass {
  long long a1 = 0;
  long long a2 = 0;
  long long a3 = 0;
  long long a4 = 0;
  long long a5 = 0;
  long long a6 = 0;
  long long a7 = 0;
  long long a8 = 0;
};

LargeClass funcReturnsLargeClass() {
  LargeClass l;
  l.a2 = 2;
  l.a6 = 6;
  return l;
}

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_RETURNS_LARGE_CLASS_H

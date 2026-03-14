
#include "sensitive.h"

#include <stdio.h>

struct SmallCStruct getSmallStruct(int x) {
  printf("x = %d\n", x);

  struct SmallCStruct s;
  s.a = 0xdeadbeaf;
  s.b = 0xdeadbeaf;
  s.c = 0xdeadbeaf;
  return s;
}

struct LargeCStruct getLargeStruct(int x) {
  printf("x = %d\n", x);

  struct LargeCStruct s;
  s.a = 0xdeadbeaf;
  s.b = 0xdeadbeaf;
  s.c = 0xdeadbeaf;
  s.d = 0xdeadbeaf;
  s.e = 0xdeadbeaf;
  s.f = 0xdeadbeaf;
  s.g = 0xdeadbeaf;
  return s;
}

void printSmallStruct(int x, struct SmallCStruct s, int y) {
  printf("x = %d, y = %d\n", x, y);
  printf("s = (%u, %u, %u)\n", s.a, s.b, s.c);
}

struct SmallCStruct forwardSmallStruct(struct SmallCStruct s) {
  return s;
}

void printLargeStruct(int x, struct LargeCStruct s, int y) {
  printf("x = %d, y = %d\n", x, y);
  printf("s = (%u, %u, %u, %u, %u, %u, %u)\n", s.a, s.b, s.c, s.e, s.e, s.f, s.g);
}

struct LargeCStruct forwardLargeStruct(struct LargeCStruct s) {
  return s;
}


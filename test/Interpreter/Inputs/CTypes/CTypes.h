#ifndef SWIFT_TEST_CTYPES_H
#define SWIFT_TEST_CTYPES_H

struct BigAlignment {
  _Alignas(16) float foo[4];
  char b;
};

#endif

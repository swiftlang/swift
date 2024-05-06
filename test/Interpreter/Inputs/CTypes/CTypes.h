#ifndef SWIFT_TEST_CTYPES_H
#define SWIFT_TEST_CTYPES_H

#include <stdint.h>

struct BigAlignment {
  _Alignas(16) float foo[4];
  char b;
};

#pragma pack(push, 4)
struct UnderAligned {
  int64_t bar;
};
#pragma pack(pop)

#endif

#ifndef CxxHeader_h
#define CxxHeader_h

#ifndef __swift_seal_cxx_interop__
#error "This file should only be used with __swift_seal_cxx_interop__"
#endif

#include <stdint.h>

struct SimpleStruct {
  int32_t x;
  uint32_t y;
};

#endif // CxxHeader_h

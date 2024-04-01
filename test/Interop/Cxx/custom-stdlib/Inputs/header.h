#ifndef CxxHeader_h
#define CxxHeader_h

#ifndef __swift_use_custom_cxx_stdlib__
#error "This file should only be used with __swift_use_custom_cxx_stdlib__"
#endif

#include <cstdint>
#include <string>

struct SimpleStruct {
  std::int32_t x;
  std::uint32_t y;
};

#endif // CxxHeader_h

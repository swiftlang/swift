#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MEMORY_LAYOUT_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MEMORY_LAYOUT_H

#include <stddef.h>
#include <stdint.h>

class PrivateMemberLayout {
  uint32_t a;

public:
  uint32_t b;
};

inline size_t sizeOfPrivateMemberLayout() {
  return sizeof(PrivateMemberLayout);
}

inline size_t offsetOfPrivateMemberLayout_b() {
  return offsetof(PrivateMemberLayout, b);
}

#endif

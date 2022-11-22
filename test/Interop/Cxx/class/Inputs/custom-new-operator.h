#ifndef TEST_INTEROP_CXX_CLASS_CUSTOM_NEW_OPERATOR_H
#define TEST_INTEROP_CXX_CLASS_CUSTOM_NEW_OPERATOR_H

#include <cstddef>
#include <cstdint>

struct container_new_t {};

inline void *operator new(size_t, void *p, container_new_t) { return p; }

struct MakeMe {
  int x;
};

inline MakeMe *callsCustomNew() {
  char buffer[8];
  return new (buffer, container_new_t()) MakeMe;
}

#endif // TEST_INTEROP_CXX_CLASS_CUSTOM_NEW_OPERATOR_H

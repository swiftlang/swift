#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H

struct IntBox {
  int value;
  IntBox operator-(IntBox rhs) { return IntBox{.value = value - rhs.value}; }
};

#endif

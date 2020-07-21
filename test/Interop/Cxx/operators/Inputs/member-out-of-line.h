#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_OUT_OF_LINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_OUT_OF_LINE_H

struct IntBox {
  int value;
  IntBox operator+(IntBox rhs) const;
};

#endif

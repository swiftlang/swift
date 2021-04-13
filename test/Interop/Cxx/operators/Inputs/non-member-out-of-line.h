#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_NON_MEMBER_OUT_OF_LINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_NON_MEMBER_OUT_OF_LINE_H

struct LoadableIntWrapper {
  int value;
};

LoadableIntWrapper operator+(LoadableIntWrapper lhs, LoadableIntWrapper rhs);

#endif

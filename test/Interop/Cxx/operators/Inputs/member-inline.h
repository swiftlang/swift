#ifndef TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H
#define TEST_INTEROP_CXX_OPERATORS_INPUTS_MEMBER_INLINE_H

struct LoadableIntWrapper {
  int value;
  LoadableIntWrapper operator-(LoadableIntWrapper rhs) {
    return LoadableIntWrapper{.value = value - rhs.value};
  }
};

#endif

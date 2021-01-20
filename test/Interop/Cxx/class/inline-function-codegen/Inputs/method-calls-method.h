#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_METHOD_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_METHOD_H

struct Incrementor {
  int increment(int t) { return t + 1; }
};

struct IncrementUser {
  int callIncrement(int value) { return Incrementor().increment(value); }
};

inline int callMethod(int value) {
  return IncrementUser().callIncrement(value);
}

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_METHOD_H

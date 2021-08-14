#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_FUNCTION_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_FUNCTION_H

inline int increment(int t) { return t + 1; }

struct Incrementor {
  int callIncrement(int value) { return increment(value); }
};

inline int callMethod(int value) { return Incrementor().callIncrement(value); }

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_FUNCTION_H

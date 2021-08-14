#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_METHOD_FROM_NESTED_STRUCT_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_METHOD_FROM_NESTED_STRUCT_H

struct IncrementUser {
  struct Incrementor {
    int increment(int t) { return t + 1; }
  };
  int callIncrement(int value) { return Incrementor().increment(value); }
};

inline int callMethod(int value) {
  return IncrementUser().callIncrement(value);
}

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_METHOD_CALLS_METHOD_FROM_NESTED_STRUCT_H

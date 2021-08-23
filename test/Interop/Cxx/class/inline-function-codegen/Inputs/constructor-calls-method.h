#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_METHOD_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_METHOD_H

struct Incrementor {
  int increment(int t) { return t + 1; }
};

struct IncrementUser {
  int incrementee;
  IncrementUser(int value) { incrementee = Incrementor().increment(value); }
};

inline int callConstructor(int value) {
  return IncrementUser(value).incrementee;
}

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_CONSTRUCTOR_CALLS_METHOD_H

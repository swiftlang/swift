#ifndef TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_FIELD_INIT_CALLS_FUNCTION_H
#define TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_FIELD_INIT_CALLS_FUNCTION_H

inline int increment(int t) { return t + 1; }

struct Incrementor {
  int incrementee = increment(41);
};

inline int initializeField() { return Incrementor().incrementee; }

#endif // TEST_INTEROP_CXX_CLASS_INLINE_FUNCTION_THROUGH_MEMBER_INPUTS_FIELD_INIT_CALLS_FUNCTION_H

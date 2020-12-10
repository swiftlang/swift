#ifndef TEST_INTEROP_CXX_MEMBERS_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_H
#define TEST_INTEROP_CXX_MEMBERS_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_H

inline int increment(int t) { return t + 1; }

struct Incrementor {
  int incrementee;
  Incrementor(int value) : incrementee(increment(value)) {}
};

inline int callConstructor(int value) { return Incrementor(value).incrementee; }

#endif // TEST_INTEROP_CXX_MEMBERS_INPUTS_CONSTRUCTOR_CALLS_FUNCTION_H

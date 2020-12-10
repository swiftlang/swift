#ifndef TEST_INTEROP_CXX_MEMBERS_INPUTS_CONSTRUCTOR_CALLS_METHOD_H
#define TEST_INTEROP_CXX_MEMBERS_INPUTS_CONSTRUCTOR_CALLS_METHOD_H

struct Increment {
  int increment(int t) { return t + 1; }
};

struct IncrementUser {
  int incrementee;
  IncrementUser(int value) { incrementee = Increment().increment(value); }
};

inline int callConstructor(int value) { return IncrementUser(value).incrementee; }

#endif // TEST_INTEROP_CXX_MEMBERS_INPUTS_CONSTRUCTOR_CALLS_METHOD_H

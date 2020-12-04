#ifndef TEST_INTEROP_CXX_MEMBERS_INPUTS_CALL_CONSTRUCTOR_H
#define TEST_INTEROP_CXX_MEMBERS_INPUTS_CALL_CONSTRUCTOR_H

struct Incrementor;

inline int increment(int t) {
  return t + 1;
}

struct Incrementor {
  int incrementee;
  Incrementor(int value) : incrementee(increment(value)) {}
};

inline int useIncrementor() {
  return Incrementor(41).incrementee;
}

#endif // TEST_INTEROP_CXX_MEMBERS_INPUTS_CALL_CONSTRUCTOR_H

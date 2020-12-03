#ifndef TEST_INTEROP_CXX_MEMBERS_INPUTS_EMIT_CALLED_CONSTRUCTOR_H
#define TEST_INTEROP_CXX_MEMBERS_INPUTS_EMIT_CALLED_CONSTRUCTOR_H

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

#endif // TEST_INTEROP_CXX_MEMBERS_INPUTS_EMIT_CALLED_CONSTRUCTOR_H

#ifndef TEST_INTEROP_CXX_MEMBERS_INPUTS_CALL_METHOD_H
#define TEST_INTEROP_CXX_MEMBERS_INPUTS_CALL_METHOD_H

inline int increment(int t) {
  return t + 1;
}

struct Incrementor {
  int incrementee;
  Incrementor(int value) : incrementee(value) {}
  int callIncrement() {
    return increment(41);
  }
};

inline int callMethod() {
  return Incrementor(41).callIncrement();
}

#endif // TEST_INTEROP_CXX_MEMBERS_INPUTS_CALL_METHOD_H

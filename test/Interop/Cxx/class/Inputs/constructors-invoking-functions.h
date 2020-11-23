#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_INVOKING_FUNCTIONS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_INVOKING_FUNCTIONS_H

inline int increment(int t) {
  return t + 1;
}

struct Incrementor {
  int incrementee;
  Incrementor(int value) : incrementee(increment(value)) {}
};

inline int badIncrement() {
  return Incrementor(41).incrementee;
}

inline int goodIncrement() {
  return increment(41);
}

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_ASDF_H

#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_INVOKING_FUNCTIONS_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_INVOKING_FUNCTIONS_H

inline int increment(int t) {
  return t + 1;
}

struct Incrementor {
  int incrementee;
  Incrementor() {
    incrementee = 41;
  }
  Incrementor(int value) : incrementee(increment(value)) {}
  void memberIncrement() {
    incrementee = increment(incrementee);
  }
};

inline int badIncrement() {
  return Incrementor(41).incrementee;
}

inline int badMemberIncrement() {
  Incrementor myIncrementor;
  myIncrementor.memberIncrement();
  return myIncrementor.incrementee;

}

inline int goodIncrement() {
  return increment(41);
}

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_CONSTRUCTORS_ASDF_H

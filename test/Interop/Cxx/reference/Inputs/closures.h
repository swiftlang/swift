#ifndef TEST_INTEROP_CXX_CLOSURES_INPUTS_REFERENCE_H
#define TEST_INTEROP_CXX_CLOSURES_INPUTS_REFERENCE_H

inline void invokeWith42ConstRef(void (^fn)(const int&)) {
  int i = 42;
  fn(i);
}

inline void invokeWith42Ref(void (^fn)(int&)) {
  int i = 42;
  fn(i);
}

#endif // TEST_INTEROP_CXX_CLOSURES_INPUTS_REFERENCE_H

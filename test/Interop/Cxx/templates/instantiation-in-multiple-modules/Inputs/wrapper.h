#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_MULTIPLE_MODULES_WRAPPER_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_MULTIPLE_MODULES_WRAPPER_H

template <typename T>
struct Wrapper {
  T t;
  int i = 0;

  void foo() { i++; }

  Wrapper() { foo(); }
};

typedef Wrapper<int> WrapperInt;

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_MULTIPLE_MODULES_WRAPPER_H

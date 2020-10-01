#ifndef TEST_INTEROP_CXX_STATIC_INPUTS_STATIC_LOCAL_VAR_H
#define TEST_INTEROP_CXX_STATIC_INPUTS_STATIC_LOCAL_VAR_H

int counterWrapper();

inline int counter() {
  static int a = 0;
  return a++;
}

#endif

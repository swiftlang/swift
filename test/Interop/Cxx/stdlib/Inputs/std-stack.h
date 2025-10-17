#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_STACK_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_STACK_H

#include <stack>

using StackOfInt = std::stack<int>;

StackOfInt initStackOfCInt() {
  StackOfInt s;
  s.push(1);
  s.push(2);
  s.push(3);
  return s;
}

StackOfInt initEmptyStackOfCInt() {
  StackOfInt s;
  return s;
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_STACK_H

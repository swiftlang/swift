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

struct NonCopyable {
  NonCopyable() = default;
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}
};

using StackOfNonCopyable = std::stack<NonCopyable>;

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_STACK_H

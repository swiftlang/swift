#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_LIST_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_LIST_H

#include <list>

using List = std::list<int>;

inline List makeListInt() { return {1, 2, 3}; }

struct NonCopyable {
  NonCopyable() = default;
  NonCopyable(int x) : number(x) {}
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}

  int number = 0;
};

using ListOfNonCopyable = std::list<NonCopyable>;

inline ListOfNonCopyable makeListOfNonCopyable(List init = {1, 2, 3}) {
  ListOfNonCopyable list;
  for (int i: init) {
    list.emplace_back(i);
  }
  return list;
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_LIST_H

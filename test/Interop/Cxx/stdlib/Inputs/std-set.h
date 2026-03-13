#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H

#include <set>
#include <unordered_set>

using SetOfCInt = std::set<int>;
using UnorderedSetOfCInt = std::unordered_set<int>;
using MultisetOfCInt = std::multiset<int>;

inline SetOfCInt initSetOfCInt() { return {1, 5, 3}; }
inline UnorderedSetOfCInt initUnorderedSetOfCInt() { return {2, 4, 6}; }
inline MultisetOfCInt initMultisetOfCInt() { return {2, 2, 4, 6}; }

struct NonCopyable {
  NonCopyable(int n) : number(n) {}
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}

  int number;
};

using SetOfNonCopyable = std::set<NonCopyable>;

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H

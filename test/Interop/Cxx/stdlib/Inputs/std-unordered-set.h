#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNORDERED_SET_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNORDERED_SET_H

#include <unordered_set>

using UnorderedSet = std::unordered_set<int>;

inline void unambiguousInsert(UnorderedSet &u, int a) {
  u.insert(a);
}

inline int getValueFromInterator(UnorderedSet::const_iterator I) {
  return *I;
}

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_UNORDERED_SET_H

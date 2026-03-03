#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H

#include <set>
#include <unordered_set>

using SetOfCInt = std::set<int>;
using UnorderedSetOfCInt = std::unordered_set<int>;
using MultisetOfCInt = std::multiset<int>;

inline SetOfCInt initSetOfCInt() { return {1, 5, 3}; }
inline SetOfCInt initSetOfCInt2() { return {3, 7}; }
inline SetOfCInt initSetOfCIntEmpty() { return {}; }
inline SetOfCInt initSetOfCIntSubset() { return {1, 5}; }
inline SetOfCInt initSetOfCIntSuperset() { return {1, 3, 5, 7}; }
inline SetOfCInt initSetOfCIntHasIntersection() { return {1, 5, 7}; }
inline SetOfCInt initSetOfCIntDisjoint() { return {2, 4, 6}; }

inline UnorderedSetOfCInt initUnorderedSetOfCInt() { return {2, 4, 6}; }
inline UnorderedSetOfCInt initUnorderedSetOfCInt2() { return {4, 8}; }
inline UnorderedSetOfCInt initUnorderedSetOfCIntEmpty() { return {}; }
inline UnorderedSetOfCInt initUnorderedSetOfCIntSubset() { return {2, 4}; }
inline UnorderedSetOfCInt initUnorderedSetOfCIntSuperset() {
  return {2, 4, 6, 8};
}
inline UnorderedSetOfCInt initUnorderedSetOfCIntHasIntersection() {
  return {2, 4, 8};
}
inline UnorderedSetOfCInt initUnorderedSetOfCIntDisjoint() { return {1, 3, 5}; }
inline MultisetOfCInt initMultisetOfCInt() { return {2, 2, 4, 6}; }

struct NonCopyable {
  NonCopyable() = default;
  NonCopyable(const NonCopyable &other) = delete;
  NonCopyable(NonCopyable &&other) = default;
  ~NonCopyable() {}
};

using SetOfNonCopyable = std::set<NonCopyable>;

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H

#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H

#include <set>

using SetOfCInt = std::set<int>;

inline SetOfCInt initSetOfCInt() { return {1, 5, 3}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SET_H

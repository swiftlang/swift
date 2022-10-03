#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H

#include <map>

using Map = std::map<int, int>;

inline Map initMap() { return {{1, 3}, {2, 2}, {3, 3}}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H

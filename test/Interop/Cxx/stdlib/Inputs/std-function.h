#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H

#include <functional>

using Func = std::function<int(int)>;
inline int invokeWith42(Func fn) { return fn(42); }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_MAP_H

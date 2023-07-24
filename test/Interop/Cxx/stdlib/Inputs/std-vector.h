#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H

#include <vector>
#include <string>

using Vector = std::vector<int>;
using VectorOfString = std::vector<std::string>;

inline Vector initVector() { return {}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_VECTOR_H
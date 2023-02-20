#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H

#include <optional>

using CxxOptional = std::optional<int>;

inline CxxOptional getNonNilOptional() { return {123}; }

inline CxxOptional getNilOptional() { return {std::nullopt}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H

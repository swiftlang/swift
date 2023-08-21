#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H

#include <optional>
#include <string>

using StdOptionalInt = std::optional<int>;
using StdOptionalString = std::optional<std::string>;

inline StdOptionalInt getNonNilOptional() { return {123}; }

inline StdOptionalInt getNilOptional() { return {std::nullopt}; }

inline bool takesOptionalInt(std::optional<int> arg) { return (bool)arg; }
inline bool takesOptionalString(std::optional<std::string> arg) { return (bool)arg; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_OPTIONAL_H

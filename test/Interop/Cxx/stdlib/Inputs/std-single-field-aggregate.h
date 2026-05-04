#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SINGLE_FIELD_AGGREGATE_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_SINGLE_FIELD_AGGREGATE_H

#include <array>
#include <optional>

struct SingleFieldArray {
  std::array<std::optional<int>, 2> elements;
};

inline std::optional<SingleFieldArray> makeOptionalSingleFieldArray() {
  SingleFieldArray s;
  s.elements[0] = 42;
  return s;
}

#endif

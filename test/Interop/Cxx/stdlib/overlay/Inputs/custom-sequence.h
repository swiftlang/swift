#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_SEQUENCE_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_SEQUENCE_H

#include "custom-iterator.h"
#include <iterator>

struct SimpleSequence {
  ConstIterator begin() const { return ConstIterator(1); }
  ConstIterator end() const { return ConstIterator(5); }
};

struct SimpleSequenceWithOutOfLineEqualEqual {
  ConstIteratorOutOfLineEq begin() const { return ConstIteratorOutOfLineEq(1); }
  ConstIteratorOutOfLineEq end() const { return ConstIteratorOutOfLineEq(5); }
};

struct SimpleArrayWrapper {
private:
  int a[5] = {10, 20, 30, 40, 50};

public:
  const int *begin() const __attribute__((returns_nonnull)) { return &a[0]; }
  const int *end() const __attribute__((returns_nonnull)) { return &a[5]; }
};

struct SimpleArrayWrapperNullableIterators {
private:
  int a[5] = {10, 20, 30, 40, 50};

public:
  const int *begin() const { return &a[0]; }
  const int *end() const { return &a[5]; }
};

struct SimpleEmptySequence {
  const int *begin() const { return nullptr; }
  const int *end() const { return nullptr; }
};

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_SEQUENCE_H
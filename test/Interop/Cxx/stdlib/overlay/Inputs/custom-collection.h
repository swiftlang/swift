#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_COLLECTION_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_COLLECTION_H

#include "custom-iterator.h"
#include <iterator>

struct SimpleCollectionNoSubscript {
private:
  int x[5] = {1, 2, 3, 4, 5};

public:
  using iterator = ConstRACIterator;

  iterator begin() const { return iterator(*x); }
  iterator end() const { return iterator(*x + 5); }
};

struct SimpleCollectionReadOnly {
private:
  int x[5] = {1, 2, 3, 4, 5};

public:
  using iterator = ConstRACIteratorRefPlusEq;

  iterator begin() const { return iterator(*x); }
  iterator end() const { return iterator(*x + 5); }

  const int& operator[](int index) const { return x[index]; }
};

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_CUSTOM_COLLECTION_H

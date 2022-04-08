#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_ITERATOR_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_ITERATOR_H

#include <iterator>
#include <vector>

using Iterator = std::vector<int>::iterator;

inline Iterator initIterator() { return {}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_ITERATOR_H

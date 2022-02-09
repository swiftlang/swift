#ifndef TEST_INTEROP_CXX_STDLIB_INPUTS_STD_ITERATORTRAITS_H
#define TEST_INTEROP_CXX_STDLIB_INPUTS_STD_ITERATORTRAITS_H

#include <iterator>

using IteratorTraits = std::iterator_traits<int>;

inline IteratorTraits initIterator() { return {}; }

#endif // TEST_INTEROP_CXX_STDLIB_INPUTS_STD_ITERATOR_H
//===- IndexedViewRange.h - Iterators for indexed projections ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines IndexedViewRange, a template class which makes it
//  easy to define a range for a "collection" that is normally just vended
//  with an indexed accessor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_INDEXEDVIEWRANGE_H
#define SWIFT_BASIC_INDEXEDVIEWRANGE_H

#include <iterator>
#include <memory>
#include "llvm/ADT/iterator_range.h"

namespace swift {

/// An iterator over a range of values provided by an indexed accessor
/// on a base type.
template <class BaseType, class ProjectedType,
          ProjectedType (&Project)(BaseType, size_t)>
class IndexedViewIterator {
public:
  using value_type = ProjectedType;
  using reference = ProjectedType;
  using pointer = void;
  using difference_type = ptrdiff_t;
  using iterator_category = std::random_access_iterator_tag;
private:
  BaseType Base;
  size_t Index;
public:
  IndexedViewIterator(BaseType base, size_t index)
    : Base(base), Index(index) {}
public:
  ProjectedType operator*() const { return Project(Base, Index); }
  ProjectedType operator->() const { return Project(Base, Index); }
  IndexedViewIterator &operator++() { Index++; return *this; }
  IndexedViewIterator operator++(int) { return iterator(Base, Index++); }
  IndexedViewIterator &operator--() { Index--; return *this; }
  IndexedViewIterator operator--(int) { return iterator(Base, Index--); }
  bool operator==(IndexedViewIterator rhs) const { return Index == rhs.Index; }
  bool operator!=(IndexedViewIterator rhs) const { return Index != rhs.Index; }

  IndexedViewIterator &operator+=(difference_type i) {
    Index += i;
    return *this;
  }
  IndexedViewIterator operator+(difference_type i) const {
    return IndexedViewIterator(Base, Index + i);
  }
  friend IndexedViewIterator operator+(difference_type i,
                                       IndexedViewIterator rhs) {
    return IndexedViewIterator(rhs.Base, rhs.Index + i);
  }
  IndexedViewIterator &operator-=(difference_type i) {
    Index -= i;
    return *this;
  }
  IndexedViewIterator operator-(difference_type i) const {
    return IndexedViewIterator(Base, Index - i);
  }
  difference_type operator-(IndexedViewIterator rhs) const {
    return Index - rhs.Index;
  }
  ProjectedType operator[](difference_type i) const {
    return Project(Base, Index + i);
  }
  bool operator<(IndexedViewIterator rhs) const {
    return Index < rhs.Index;
  }
  bool operator<=(IndexedViewIterator rhs) const {
    return Index <= rhs.Index;
  }
  bool operator>(IndexedViewIterator rhs) const {
    return Index > rhs.Index;
  }
  bool operator>=(IndexedViewIterator rhs) const {
    return Index >= rhs.Index;
  }
};

template <class BaseType, class ProjectedType,
          ProjectedType (&Project)(BaseType, size_t)>
using IndexedViewRange =
  llvm::iterator_range<IndexedViewIterator<BaseType, ProjectedType, Project>>;

} // end namespace swift

#endif // SWIFT_BASIC_INDEXEDVIEWRANGE_H

//===- ArrayRefView.h - Adapter for iterating over an ArrayRef --*- C++ -*-===//
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
//  This file defines ArrayRefView, a template class which provides a
//  proxied view of the elements of an array.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ARRAYREFVIEW_H
#define SWIFT_BASIC_ARRAYREFVIEW_H

#include "llvm/ADT/ArrayRef.h"

namespace swift {

/// An adapter for iterating over a range of values as a range of
/// values of a different type.
template <class Orig, class Projected, Projected (&Project)(const Orig &)>
class ArrayRefView {
  llvm::ArrayRef<Orig> Array;
public:
  ArrayRefView(llvm::ArrayRef<Orig> array) : Array(array) {}

  class iterator {
    friend class ArrayRefView<Orig,Projected,Project>;
    const Orig *Ptr;
    iterator(const Orig *ptr) : Ptr(ptr) {}
  public:
    typedef Projected value_type;
    typedef Projected reference;
    typedef void pointer;
    typedef ptrdiff_t difference_type;
    typedef std::random_access_iterator_tag iterator_category;

    Projected operator*() const { return Project(*Ptr); }
    iterator &operator++() { Ptr++; return *this; }
    iterator operator++(int) { return iterator(Ptr++); }
    bool operator==(iterator rhs) const { return Ptr == rhs.Ptr; }
    bool operator!=(iterator rhs) const { return Ptr != rhs.Ptr; }

    iterator &operator+=(difference_type i) {
      Ptr += i;
      return *this;
    }
    iterator operator+(difference_type i) const {
      return iterator(Ptr + i);
    }
    friend iterator operator+(difference_type i, iterator base) {
      return iterator(base.Ptr + i);
    }
    iterator &operator-=(difference_type i) {
      Ptr -= i;
      return *this;
    }
    iterator operator-(difference_type i) const {
      return iterator(Ptr - i);
    }
    difference_type operator-(iterator rhs) const {
      return Ptr - rhs.Ptr;
    }
    Projected operator[](difference_type i) const {
      return Project(Ptr[i]);
    }
    bool operator<(iterator rhs) const {
      return Ptr < rhs.Ptr;
    }
    bool operator<=(iterator rhs) const {
      return Ptr <= rhs.Ptr;
    }
    bool operator>(iterator rhs) const {
      return Ptr > rhs.Ptr;
    }
    bool operator>=(iterator rhs) const {
      return Ptr >= rhs.Ptr;
    }
  };
  iterator begin() const { return iterator(Array.begin()); }
  iterator end() const { return iterator(Array.end()); }

  bool empty() const { return Array.empty(); }
  size_t size() const { return Array.size(); }
  Projected operator[](unsigned i) const { return Project(Array[i]); }
  Projected front() const { return Project(Array.front()); }
  Projected back() const { return Project(Array.back()); }

  ArrayRefView slice(unsigned start) const {
    return ArrayRefView(Array.slice(start));
  }
  ArrayRefView slice(unsigned start, unsigned length) const {
    return ArrayRefView(Array.slice(start, length));
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_ARRAYREFVIEW_H

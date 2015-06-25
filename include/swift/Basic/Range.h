//===- Range.h - Classes for conveniently working with ranges ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file provides classes and functions for conveniently working
//  with ranges, 
//
//  reversed returns an iterator_range out of the reverse iterators of a type.
//
//  map creates an iterator_range which applies a function to all the elements
//  in another iterator_range.
//
//  IntRange is a template class for iterating over a range of
//  integers.
//
//  indices returns the range of indices from [0..size()) on a
//  subscriptable type.
//
//  Note that this is kept in Swift because it's really only useful in
//  C++11, and there aren't any major open-source subprojects of LLVM
//  that can use C++11 yet.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_RANGE_H
#define SWIFT_BASIC_RANGE_H

#include <algorithm>
#include <type_traits>
#include <utility>
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {
  using llvm::make_range;
  using llvm::iterator_range;

  template<typename T>
  inline auto reversed(T &&container)
  -> decltype(llvm::make_range(container.rbegin(), container.rend())) {
    return llvm::make_range(container.rbegin(), container.rend());
  }
  
  // Wrapper for std::transform that creates a new back-insertable container
  // and transforms a range into it.
  template<typename T, typename InputRange, typename MapFn>
  T map(InputRange &&range, MapFn &&mapFn) {
    T result;
    std::transform(std::begin(range), std::end(range),
                   std::back_inserter(result),
                   std::forward<MapFn>(mapFn));
    return result;
  }

/// A range of integers.  This type behaves roughly like an ArrayRef.
template <class T> class IntRange {
  static_assert(std::is_integral<T>::value, "T must be an integer type");
  T Begin;
  T End;
public:
  IntRange() : Begin(0), End(0) {}
  IntRange(T begin, T end) : Begin(begin), End(end) {
    assert(begin <= end);
  }

  class iterator {
    friend class IntRange<T>;
    T Value;
    iterator(T value) : Value(value) {}
  public:
    typedef T value_type;
    typedef T reference;
    typedef void pointer;
    typedef typename std::make_signed<T>::type difference_type;
    typedef std::random_access_iterator_tag iterator_category;

    T operator*() const { return Value; }
    iterator &operator++() { Value++; return *this; }
    iterator operator++(int) { return iterator(Value++); }
    bool operator==(iterator rhs) { return Value == rhs.Value; }
    bool operator!=(iterator rhs) { return Value != rhs.Value; }

    iterator &operator+=(difference_type i) {
      Value += T(i);
      return *this;
    }
    iterator operator+(difference_type i) const {
      return iterator(Value + T(i));
    }
    friend iterator operator+(difference_type i, iterator base) {
      return iterator(base.Value + T(i));
    }
    iterator &operator-=(difference_type i) {
      Value -= T(i);
      return *this;
    }
    iterator operator-(difference_type i) const {
      return iterator(Value - T(i));
    }
    difference_type operator-(iterator rhs) const {
      return difference_type(Value - rhs.Value);
    }
    T operator[](difference_type i) const { return Value + T(i);       }
    bool operator<(iterator rhs) const {    return Value <  rhs.Value; }
    bool operator<=(iterator rhs) const {   return Value <= rhs.Value; }
    bool operator>(iterator rhs) const {    return Value >  rhs.Value; }
    bool operator>=(iterator rhs) const {   return Value >= rhs.Value; }
  };
  iterator begin() const { return iterator(Begin); }
  iterator end() const { return iterator(End); }

  std::reverse_iterator<iterator> rbegin() const {
    return std::reverse_iterator<iterator>(end());
  }
  std::reverse_iterator<iterator> rend() const {
    return std::reverse_iterator<iterator>(begin());
  }

  bool empty() const { return Begin == End; }
  size_t size() const { return End - Begin; }
  T operator[](size_t i) const {
    assert(i < size());
    return Begin + i;
  }
  T front() const { assert(!empty()); return Begin; }
  T back() const { assert(!empty()); return End - 1; }

  IntRange slice(size_t start) const {
    assert(start <= size());
    return IntRange(Begin + start, End);
  }
  IntRange slice(size_t start, size_t length) const {
    assert(start <= size());
    return IntRange(Begin + start,
                    Begin + start + std::min(length, End - (Begin + start)));
  }

  bool operator==(IntRange other) const {
    return Begin == other.Begin && End == other.End;
  }
  bool operator!=(IntRange other) const {
    return !(operator==(other));
  }
};

/// indices - Given a type that's subscriptable with integers, return
/// an IntRange consisting of the valid subscripts.
template <class T>
typename std::enable_if<sizeof(std::declval<T>()[size_t(1)]) != 0,
                        IntRange<decltype(std::declval<T>().size())>>::type
indices(const T &collection) {
  return IntRange<decltype(std::declval<T>().size())>(0, collection.size());
}
} // namespace swift

#endif

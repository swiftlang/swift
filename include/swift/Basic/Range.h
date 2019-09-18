//===--- Range.h - Classes for conveniently working with ranges -*- C++ -*-===//
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
///
///  \file
///
///  This file provides classes and functions for conveniently working
///  with ranges,
///
///  reversed returns an iterator_range out of the reverse iterators of a type.
///
///  map creates an iterator_range which applies a function to all the elements
///  in another iterator_range.
///
///  IntRange is a template class for iterating over a range of
///  integers.
///
///  indices returns the range of indices from [0..size()) on a
///  subscriptable type.
///
///  Note that this is kept in Swift because it's really only useful in
///  C++11, and there aren't any major open-source subprojects of LLVM
///  that can use C++11 yet.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_RANGE_H
#define SWIFT_BASIC_RANGE_H

#include <algorithm>
#include <type_traits>
#include <utility>
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/STLExtras.h"

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

template <class T, bool IsEnum = std::is_enum<T>::value>
struct IntRangeTraits;

template <class T>
struct IntRangeTraits<T, /*is enum*/ false> {
  static_assert(std::is_integral<T>::value,
                "argument type of IntRange is either an integer nor an enum");
  using int_type = T;
  using difference_type = typename std::make_signed<int_type>::type;

  static T addOffset(T value, difference_type quantity) {
    return T(difference_type(value) + quantity);
  }
  static difference_type distance(T begin, T end) {
    return difference_type(end) - difference_type(begin);
  }
};

template <class T>
struct IntRangeTraits<T, /*is enum*/ true> {
  using int_type = typename std::underlying_type<T>::type;
  using difference_type = typename std::make_signed<int_type>::type;

  static T addOffset(T value, difference_type quantity) {
    return T(difference_type(value) + quantity);
  }
  static difference_type distance(T begin, T end) {
    return difference_type(end) - difference_type(begin);
  }
};

/// A range of integers or enum values.  This type behaves roughly
/// like an ArrayRef.
template <class T = unsigned, class Traits = IntRangeTraits<T>>
class IntRange {
  T Begin;
  T End;

  using int_type = typename Traits::int_type;
  using difference_type = typename Traits::difference_type;

public:
  IntRange() : Begin(0), End(0) {}
  IntRange(T end) : Begin(0), End(end) {}
  IntRange(T begin, T end) : Begin(begin), End(end) {
    assert(begin <= end);
  }

  class iterator {
    friend class IntRange<T>;
    T Value;
    iterator(T value) : Value(value) {}
  public:
    using value_type = T;
    using reference = T;
    using pointer = void;
    using difference_type = typename std::make_signed<T>::type;
    using iterator_category = std::random_access_iterator_tag;

    T operator*() const { return Value; }
    iterator &operator++() {
      return *this += 1;
    }
    iterator operator++(int) {
      auto copy = *this;
      *this += 1;
      return copy;
    }
    iterator &operator--() {
      return *this -= 1;
    }
    iterator operator--(int) {
      auto copy = *this;
      *this -= 1;
      return copy;
    }
    bool operator==(iterator rhs) { return Value == rhs.Value; }
    bool operator!=(iterator rhs) { return Value != rhs.Value; }

    iterator &operator+=(difference_type i) {
      Value = Traits::addOffset(Value, i);
      return *this;
    }
    iterator operator+(difference_type i) const {
      return iterator(Traits::adddOfset(Value, i));
    }
    friend iterator operator+(difference_type i, iterator base) {
      return iterator(Traits::addOffset(base.Value, i));
    }
    iterator &operator-=(difference_type i) {
      Value = Traits::addOffset(Value, -i);
      return *this;
    }
    iterator operator-(difference_type i) const {
      return iterator(Traits::addOffset(Value, -i));
    }
    difference_type operator-(iterator rhs) const {
      return Traits::distance(rhs.Value, Value);
    }
    T operator[](difference_type i) const {
      return Traits::addOffset(Value, i);
    }
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
  size_t size() const { return size_t(Traits::distance(Begin, End)); }
  T operator[](size_t i) const {
    assert(i < size());
    return Traits::addOffset(Begin, i);
  }
  T front() const { assert(!empty()); return Begin; }
  T back() const { assert(!empty()); return Traits::addOffset(End, -1); }
  IntRange drop_back(size_t length = 1) const {
    assert(length <= size());
    return IntRange(Begin, Traits::addOffset(End, -length));
  }

  IntRange slice(size_t start) const {
    assert(start <= size());
    return IntRange(Traits::addOffset(Begin, start), End);
  }
  IntRange slice(size_t start, size_t length) const {
    assert(start <= size());
    auto newBegin = Traits::addOffset(Begin, start);
    auto newSize = std::min(length, size_t(Traits::distance(newBegin, End)));
    return IntRange(newBegin, Traits::addOffset(newBegin, newSize));
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

/// Returns an Int range [start, end).
static inline IntRange<unsigned> range(unsigned start, unsigned end) {
  assert(start <= end && "Invalid integral range");
  return IntRange<unsigned>(start, end);
}

/// Returns an Int range [0, end).
static inline IntRange<unsigned> range(unsigned end) {
  return range(0, end);
}

/// Returns a reverse Int range (start, end].
static inline auto reverse_range(unsigned start, unsigned end) ->
  decltype(reversed(range(start+1, end+1))) {
  assert(start <= end && "Invalid integral range");
  return reversed(range(start+1, end+1));
}

} // end namespace swift

#endif // SWIFT_BASIC_RANGE_H

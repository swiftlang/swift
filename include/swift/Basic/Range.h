//===--- Range.h - Classes for conveniently working with ranges -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
template <class T=unsigned> class IntRange {
  static_assert(std::is_integral<T>::value, "T must be an integer type");
  T Begin;
  T End;
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
    typedef T value_type;
    typedef T reference;
    typedef void pointer;
    typedef typename std::make_signed<T>::type difference_type;
    typedef std::random_access_iterator_tag iterator_category;

    T operator*() const { return Value; }
    iterator &operator++() { Value++; return *this; }
    iterator operator++(int) { return iterator(Value++); }
    iterator &operator--() {
      Value--;
      return *this;
    }
    iterator operator--(int) { return iterator(Value--); }
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

/// Returns an Int range [start, end).
static inline IntRange<unsigned> range(unsigned start, unsigned end) {
  assert(start <= end && "Invalid integral range");
  return IntRange<unsigned>(start, end);
}

/// Returns an Int range [0, end).
static inline IntRange<unsigned> range(unsigned end) {
  return range(0, end);
}

/// A random access range that provides iterators that can be used to iterate
/// over the (element, index) pairs of a collection.
template <typename IterTy> class EnumeratorRange {
public:
  using IterTraitsTy = typename std::iterator_traits<IterTy>;
  static_assert(std::is_same<typename IterTraitsTy::iterator_category,
                             std::random_access_iterator_tag>::value,
                "Expected a random access iterator");

private:
  IterTy Begin;
  IterTy End;

public:
  EnumeratorRange(IterTy begin, IterTy end) : Begin(begin), End(end) {}

  class iterator {
    friend class EnumeratorRange;
    IterTy Begin;
    IterTy Iter;

    iterator(IterTy begin, IterTy iter) : Begin(begin), Iter(iter) {}

  public:
    using value_type =
        std::pair<typename std::iterator_traits<IterTy>::value_type, int>;
    using reference =
        std::pair<typename std::iterator_traits<IterTy>::value_type, int>;
    using pointer = void;
    using iterator_category = std::random_access_iterator_tag;
    using difference_type = int;

    value_type operator*() const { return {*Iter, std::distance(Begin, Iter)}; }
    iterator &operator++() {
      Iter++;
      return *this;
    }
    iterator operator++(int) { return iterator(Begin, Iter++); }
    iterator &operator--() {
      Iter--;
      return *this;
    }
    iterator operator--(int) { return iterator(Begin, Iter--); }
    bool operator==(iterator rhs) { return Iter == rhs.Iter; }
    bool operator!=(iterator rhs) { return !(*this == rhs); }

    iterator &operator+=(difference_type i) {
      std::advance(Iter, i);
      return *this;
    }
    iterator operator+(difference_type i) const {
      auto IterCopy = Iter;
      std::advance(IterCopy, i);
      return iterator(Begin, IterCopy);
    }
    friend iterator operator+(difference_type i, iterator base) {
      std::advance(base.Iter, i);
      return base;
    }
    iterator &operator-=(difference_type i) {
      *this += -i;
      return *this;
    }
    iterator operator-(difference_type i) const {
      auto NewIter = *this;
      return NewIter -= i;
    }
    difference_type operator-(iterator rhs) const {
      return difference_type(std::distance(Iter, rhs.Iter));
    }
  };

  iterator begin() const { return iterator(Begin, Begin); }
  iterator end() const { return iterator(Begin, End); }

  using reverse_iterator = std::reverse_iterator<iterator>;
  reverse_iterator rbegin() const { return reverse_iterator(end()); }
  reverse_iterator rend() const { return reverse_iterator(begin()); }
};

/// enumerate - Given a type that's subscriptable with integers, return an
/// IntEnumerateRange consisting of the valid subscripts.
template <class T>
EnumeratorRange<typename T::iterator> enumerate(T &collection) {
  return EnumeratorRange<typename T::iterator>(collection.begin(),
                                               collection.end());
}
template <class T> EnumeratorRange<T> enumerate(T Begin, T End) {
  return EnumeratorRange<T>(Begin, End);
}

/// An adaptor of std::none_of for ranges.
template <class Range, class Predicate>
inline bool none_of(const Range &R, Predicate &&P) {
  return std::none_of(R.begin(), R.end(), std::forward<Predicate>(P));
}

/// An adaptor of std::count for ranges.
///
/// We use std::result_of on Range::begin since llvm::iterator_range does not
/// have a public typedef set to what is the underlying iterator.
//typename std::iterator_traits<decltype(&Range::begin())>::difference_type
template <class Range, class Value>
inline auto count(const Range &R, Value V)
  -> typename std::iterator_traits<decltype(R.begin())>::difference_type {
  return std::count(R.begin(), R.end(), V);
}

/// An adaptor of std::count_if for ranges.
///
/// We use std::result_of on Range::begin since llvm::iterator_range does not
/// have a public typedef set to what is the underlying iterator.
template <class Range, class Predicate>
inline auto count_if(const Range &R, Predicate &&P)
  -> typename std::iterator_traits<decltype(R.begin())>::difference_type {
  return std::count_if(R.begin(), R.end(), std::forward<Predicate>(P));
}

} // end namespace swift

#endif // SWIFT_BASIC_RANGE_H

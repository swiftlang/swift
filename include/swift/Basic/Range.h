//===- Range.h - A lightweight pair of iterators ----------------*- C++ -*-===//
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
//  This file provides Range, a template class which makes it easy to
//  turn a pair of iterators into a "collection" suitable for C++11's
//  for loop.
//
//  Note that this is kept in Swift because it's really only useful in
//  C++11, and there aren't any major open-source subprojects of LLVM
//  that can use C++11 yet.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_RANGE_H
#define SWIFT_BASIC_RANGE_H

#include <algorithm>
#include <utility>

namespace swift {
  /// A pair of iterators which can be used as the collection of a
  /// C++11 for-loop.
  template <typename T> class Range {
    T Begin;
    T End;
  public:
    Range(const T &begin, const T &end) : Begin(begin), End(end) {}
    T begin() { return Begin; }
    T end() { return End; }
    bool empty() const { return Begin == End; }
  };
  
  template<typename T>
  inline Range<T> make_range(const T &begin, const T &end) {
    return {begin, end};
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
}

#endif

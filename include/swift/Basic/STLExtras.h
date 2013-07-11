//===- STLExtras.h - additions to the STL -----------------------*- C++ -*-===//
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
/// \file Provides STL-style algorithms for convenience.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_INTERLEAVE_H
#define SWIFT_BASIC_INTERLEAVE_H

#include <cassert>

namespace swift {

/// @{

/// An STL-style algorithm similar to std::for_each that applies a second
/// functor between every pair of elements.
///
/// This provides the control flow logic to, for example, print a
/// comma-separated list:
/// \code
///   interleave(names.begin(), names.end(),
///              [&](StringRef name) { OS << name; },
///              [&] { OS << ", "; });
/// \endcode
template <typename ForwardIterator, typename UnaryFunctor,
          typename NullaryFunctor>
inline void interleave(ForwardIterator begin, ForwardIterator end,
                       UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  if (begin == end)
    return;
  each_fn(*begin);
  ++begin;
  for (; begin != end; ++begin) {
    between_fn();
    each_fn(*begin);
  }
}

template <typename Container, typename UnaryFunctor, typename NullaryFunctor>
inline void interleave(const Container &c, UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  interleave(c.begin(), c.end(), each_fn, between_fn);
}

/// @}
/// @{

/// The equivalent of std::for_each, but for two lists at once.
template <typename InputIt1, typename InputIt2, typename BinaryFunction>
inline void for_each(InputIt1 I1, InputIt1 E1, InputIt2 I2, BinaryFunction f) {
  while (I1 != E1) {
    f(*I1, *I2);
    ++I1, ++I2;
  }
}

template <typename Container1, typename Container2, typename BinaryFunction>
inline void for_each(const Container1 &c1, const Container2 &c2,
                     BinaryFunction f) {
  assert(c1.size() == c2.size());
  for_each(c1.begin(), c1.end(), c2.begin(), f);
}

/// @}

} // end namespace swift

#endif

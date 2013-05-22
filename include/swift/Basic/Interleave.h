//===- Interleave.h - for_each between sequence elements --------*- C++ -*-===//
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
//  This file provides interleave, an STL-style algorithm similar to
//  std::for_each that applies a second functor between every pair of elements.
//  This provides the control flow logic to, for example, print a
//  comma-separated list:
//
//    interleave(names.begin(), names.end(),
//               [&](StringRef name) { OS << name; },
//               [&] { OS << ", "; });
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_INTERLEAVE_H
#define SWIFT_BASIC_INTERLEAVE_H

namespace swift {

template <typename ForwardIterator,
          typename UnaryFunctor,
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

template <typename Container,
typename UnaryFunctor,
typename NullaryFunctor>
inline void interleave(const Container &c, UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  interleave(c.begin(), c.end(), each_fn, between_fn);
}


} // end namespace swift

#endif
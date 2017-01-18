//===--- Algorithm.h - ------------------------------------------*- C++ -*-===//
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
//  This file defines helper algorithms, some of which are ported from C++14,
//  which may not be available on all platforms yet.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ALGORITHM_H
#define SWIFT_BASIC_ALGORITHM_H

namespace swift {
  /// Returns the minimum of `a` and `b`, or `a` if they are equivalent.
  template <typename T>
  constexpr const T &min(const T &a, const T &b) {
    return !(b < a) ? a : b;
  }
  
  /// Returns the maximum of `a` and `b`, or `a` if they are equivalent.
  template <typename T>
  constexpr const T &max(const T &a, const T &b) {
    return (a < b) ? b : a;
  }
} // end namespace swift

#endif // SWIFT_BASIC_ALGORITHM_H

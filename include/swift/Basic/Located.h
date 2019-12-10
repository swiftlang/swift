//===--- Located.h - Source Location and Associated Value ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Provides a currency data type Located<T> that should be used instead
// of std::pair<T, SourceLoc>.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_BASIC_LOCATED_H
#define SWIFT_BASIC_LOCATED_H
#include "swift/Basic/SourceLoc.h"

namespace swift {

/// A currency type for keeping track of items which were found in the source code.
/// Several parts of the compiler need to keep track of a `SourceLoc` corresponding
/// to an item, in case they need to report some diagnostics later. For example,
/// the ClangImporter needs to keep track of where imports were originally written.
/// Located makes it easy to do so while making the code more readable, compared to
/// using `std::pair`.

template<typename T>
struct Located {

  /// The main item whose source location is being tracked.
  T item;

  /// The original source location from which the item was parsed.
  SourceLoc loc;

  Located() {}

  Located(T item, SourceLoc loc): item(item), loc(loc) {}

  template<typename U>
  friend bool operator ==(const Located<U>& lhs, const Located<U>& rhs) {
    return lhs.item == rhs.item && lhs.loc == rhs.loc;
  }
};
}

#endif // SWIFT_BASIC_LOCATED_H

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
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

namespace swift {

/// A currency type for keeping track of items which were found in the source code.
/// Several parts of the compiler need to keep track of a `SourceLoc` corresponding
/// to an item, in case they need to report some diagnostics later. For example,
/// the ClangImporter needs to keep track of where imports were originally written.
/// Located makes it easy to do so while making the code more readable, compared to
/// using `std::pair`.
template <typename T>
struct Located {
  /// The main item whose source location is being tracked.
  T Item;

  /// The original source location from which the item was parsed.
  SourceLoc Loc;

  Located() : Item(), Loc() {}

  Located(T Item, SourceLoc loc) : Item(Item), Loc(loc) {}

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &os) const;
};

template <typename T>
bool operator ==(const Located<T> &lhs, const Located<T> &rhs) {
  return lhs.Item == rhs.Item && lhs.Loc == rhs.Loc;
}

} // end namespace swift

namespace llvm {

template <typename T> struct DenseMapInfo;

template<typename T>
struct DenseMapInfo<swift::Located<T>> {

  static inline swift::Located<T> getEmptyKey() {
    return swift::Located<T>(DenseMapInfo<T>::getEmptyKey(),
                             DenseMapInfo<swift::SourceLoc>::getEmptyKey());
  }

  static inline swift::Located<T> getTombstoneKey() {
    return swift::Located<T>(DenseMapInfo<T>::getTombstoneKey(),
                             DenseMapInfo<swift::SourceLoc>::getTombstoneKey());
  }

  static unsigned getHashValue(const swift::Located<T> &LocatedVal) {
    return combineHashValue(DenseMapInfo<T>::getHashValue(LocatedVal.Item),
                            DenseMapInfo<swift::SourceLoc>::getHashValue(LocatedVal.Loc));
  }

  static bool isEqual(const swift::Located<T> &LHS, const swift::Located<T> &RHS) {
    return DenseMapInfo<T>::isEqual(LHS.Item, RHS.Item) &&
           DenseMapInfo<T>::isEqual(LHS.Loc, RHS.Loc);
  }
};
} // namespace llvm

#endif // SWIFT_BASIC_LOCATED_H

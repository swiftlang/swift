//===--- ScopedTracking.h - Utilities for scoped tracking -------*- C++ -*-===//
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
// This file defines some miscellaneous utilities that are useful when
// working with tracked values that can be saved and restored in a scoped
// fashion.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SCOPEDTRACKING_H
#define SWIFT_BASIC_SCOPEDTRACKING_H

namespace llvm {
template <class K, class V, class T, class A>
class ScopedHashTable;
template <class K, class V, class T, class A>
class ScopedHashTableScope;
}

namespace swift {

/// Must declare a nested type scope_type which can be initialized
/// with an l-value reference to the tracker type.
template <class Tracker>
struct ScopedTrackingTraits;

template <class K, class V, class T, class A>
struct ScopedTrackingTraits<llvm::ScopedHashTable<K,V,T,A>> {
  using scope_type = llvm::ScopedHashTableScope<K,V,T,A>;
};

/// A class which stores scopes for multiple trackers.  Can be
/// initialized with a pack of l-value references to the trackers.
template <class... Trackers>
class TrackingScopes;

template <>
class TrackingScopes<> {
public:
  TrackingScopes() {}
};

template <class Tracker, class... OtherTrackers>
class TrackingScopes<Tracker, OtherTrackers...> {
  typename ScopedTrackingTraits<Tracker>::scope_type Scope;
  TrackingScopes<OtherTrackers...> OtherScopes;
public:
  TrackingScopes(Tracker &tracker, OtherTrackers &...otherTrackers)
    : Scope(tracker), OtherScopes(otherTrackers...) {}
};

} // end namespace swift

#endif

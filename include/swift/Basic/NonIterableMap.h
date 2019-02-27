//===--- NonIterableMap.h ---------------------------------*- C++ -*-------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// This file contains an adapter class that is a wrapper around a Map that does
/// not allow for iteration without asserts enabled. This allows an API designer
/// to specify that (for example) a DenseMap with pointer keys can not be
/// iterated over. This prevents a common source of non-determinism in
/// compilers.
///
/// NOTE: We still allow for iteration when asserts are enabled for dumping
/// purposes.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_NONITERABLEMAP_H
#define SWIFT_BASIC_NONITERABLEMAP_H

namespace swift {

/// A map adapter that type erases the usage of begin(), end() from a map to
/// prevent iteration.
///
/// Often times in compiler data structures, we use unordered maps to map
/// pointers to other state in the program. Iterating over such unordered maps
/// causes the compiler to potentially become non-deterministic. The intention
/// of this adapter is to provide the compiler writer with a way use the type
/// system to eliminate this bug by banning iteration over an underlying map
/// type.
///
/// NOTE: When asserts are enabled, we still provide a set of debugBegin(),
/// debugEnd() entry points that allow for iteration over the underlying
/// map. The reason for this is to still allow for dump() methods to be
/// implemented on top of the map.
///
/// NOTE: This does violate the Liskov Substitution principle. *BUT* The worst
/// that could happen is that we miss a case if c++ calls MapTy::begin() instead
/// of erroring. This is better than the status quo when /all/ uses of the
/// underlying map can lead to non-determinism.
template <typename MapTy>
struct NonIterableMap : MapTy {
#ifndef NDEBUG
  typename MapTy::iterator debugBegin() { return MapTy::begin(); }
  typename MapTy::iterator debugEnd() { return MapTy::end(); }
  typename MapTy::const_iterator debugBegin() const { return MapTy::begin(); }
  typename MapTy::const_iterator debugEnd() const { return MapTy::end(); }
#endif

private:
  typename MapTy::iterator begin() { return MapTy::begin(); }
  typename MapTy::iterator end() { return MapTy::end(); }
  typename MapTy::const_iterator begin() const { return MapTy::begin(); }
  typename MapTy::const_iterator end() const { return MapTy::end(); }
};

} // namespace swift

#endif

//===--- ConformanceCache.h - Caching conformance lookups -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This is a utility used for caching conformance lookups in the constraint
// solver
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONFORMANCE_CACHE_H
#define SWIFT_SEMA_CONFORMANCE_CACHE_H

#include "swift/Basic/OptionSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class ProtocolConformanceRef;
class ProtocolDecl;
class Type;
class TypeBase;

namespace constraints {

// Subtyping.h
enum class ConversionBehavior : unsigned;

/// To avoid overhead from repeated conformance queries while exploring the
/// disjunction search space, the code in Subtyping.cpp indirects global
/// conformance lookups through this type. Other clients of Subtyping.cpp
/// entry points can just create a ConformanceCache when needed to make a
/// few calls, since there's probably no benefit to caching this information
/// most of the time.
struct ConformanceCache {
  ConformanceCache() = default;

  ConformanceCache(const ConformanceCache &) = delete;
  ConformanceCache(ConformanceCache &&other);

  ConformanceCache &operator=(const ConformanceCache &) = delete;
  ConformanceCache &operator=(ConformanceCache &&) = delete;

  /// A dictionary of all conformances that have been looked up by the solver.
  llvm::DenseMap<std::pair<TypeBase *, ProtocolDecl *>, ProtocolConformanceRef>
      Conformances;

  /// We memoize the computation in the below.
  llvm::DenseMap<std::pair<ConversionBehavior, ProtocolDecl *>, bool>
      ConformanceTransitiveForSupertypeCache;

  /// We memoize the computation in the below.
  llvm::DenseMap<std::pair<ConversionBehavior, ProtocolDecl *>, bool>
      ConformanceTransitiveForSubtypeCache;

  /// Check whether the given type conforms to the given protocol and if
  /// so return a valid conformance reference.
  ProtocolConformanceRef lookupConformance(Type type, ProtocolDecl *P);

  /// Suppose we are given a type T and a protocol P, and U conv T for
  /// some type U; if U conforms to P, does it follow that T conforms to P?
  bool checkTransitiveSubtypeConformance(Type type, ProtocolDecl *proto);

  /// Suppose we are given a type T with the given conversion behavior,
  /// and a protocol P, with the following setup:
  /// - T conv $T0
  /// - $T0 conforms P
  /// The question is, does this imply that T must conform to P? This
  /// returns true if so, false otherwise.
  bool isConformanceTransitiveForSupertype(ConversionBehavior behavior,
                                           ProtocolDecl *proto);

  /// Suppose we are given a type T and a protocol P, and T conv U for
  /// some type U; if U conforms to P, does it follow that T conforms to P?
  bool checkTransitiveSupertypeConformance(Type type, ProtocolDecl *proto);

  /// Suppose we are given a type T with the given conversion behavior,
  /// and a protocol P, with the following setup:
  /// - $T0 conv T
  /// - $T0 conforms P
  /// The question is, does this imply that T must conform to P? This
  /// returns true if so, false otherwise.
  bool isConformanceTransitiveForSubtype(ConversionBehavior behavior,
                                         ProtocolDecl *proto);
};

}  // end namespace constraints

}  // end namespace swift

#endif  // SWIFT_SEMA_SUBTYPING_H

//===--- FormalLinkage.h - Formal linkage of types and decls ----*- C++ -*-===//
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

#ifndef SWIFT_SIL_FORMALLINKAGE_H
#define SWIFT_SIL_FORMALLINKAGE_H

namespace swift {

class CanType;
class NormalProtocolConformance;
class ValueDecl;
enum class SILLinkage : unsigned char;
enum ForDefinition_t : bool;

// Bits useful in defining the below.
enum {
  // Bottom bit is uniqueness.
  FormalLinkage_Unique    = 0x0,
  FormalLinkage_NonUnique = 0x1,

  // Higher bits are visibility, with greater values being more
  // restrictive.
  FormalLinkage_Public    = 0 << 1,
  FormalLinkage_Hidden    = 1 << 1,
  FormalLinkage_Private   = 2 << 1,
};

/// Formal linkage is a property of types and declarations that
/// informs, but is not completely equivalent to, the linkage of
/// symbols corresponding to those types and declarations.
///
/// Forms a semilattice with ^ as the meet operator.
enum class FormalLinkage {
  /// This entity is visible in multiple Swift modules and has a
  /// unique file that is known to define it.
  PublicUnique     = FormalLinkage_Public | FormalLinkage_Unique,

  /// This entity is visible in multiple Swift modules, but does not
  /// have a unique file that is known to define it.
  PublicNonUnique  = FormalLinkage_Public | FormalLinkage_NonUnique,

  /// This entity is visible in only a single Swift module and has a
  /// unique file that is known to define it.
  HiddenUnique     = FormalLinkage_Hidden | FormalLinkage_Unique,

  /// This entity is visible in only a single Swift module but does not
  /// have a unique file that is known to define it.
  HiddenNonUnique  = FormalLinkage_Hidden | FormalLinkage_NonUnique,

  /// This entity is visible in only a single Swift file.
  //
  // In reality, these are by definition unique, but we use the
  // non-unique flag to make merging more efficient.
  Private          = FormalLinkage_Private | FormalLinkage_NonUnique,

  /// The top of the semilattice: (a ^ Top) == a.
  Top = PublicUnique,

  /// The bottom of the semilattice: (a ^ Bottom) == Bottom.
  Bottom = Private,
};

/// Merge two linkages to get the more restrictive.
inline FormalLinkage operator^(FormalLinkage lhs, FormalLinkage rhs) {
  // Semantically, we want the more restrictive visibility; if that's
  // private, it's unique, and otherwise it's non-unique if either is
  // non-unique.  This is more efficient if we define away the
  // special case for private by representing Private as non-unique.
  if (lhs < rhs) {
    return FormalLinkage(unsigned(rhs) | 
                         (unsigned(lhs) & FormalLinkage_NonUnique));
  } else {
    return FormalLinkage(unsigned(lhs) | 
                         (unsigned(rhs) & FormalLinkage_NonUnique));
  }
}
inline FormalLinkage &operator^=(FormalLinkage &lhs, FormalLinkage rhs) {
  return (lhs = lhs ^ rhs);
}

FormalLinkage getTypeLinkage(CanType type);
FormalLinkage getDeclLinkage(const ValueDecl *decl);
SILLinkage getSILLinkage(FormalLinkage linkage,
                         ForDefinition_t forDefinition);
SILLinkage
getLinkageForProtocolConformance(const NormalProtocolConformance *C,
                                 ForDefinition_t definition);

} // end swift namespace

#endif

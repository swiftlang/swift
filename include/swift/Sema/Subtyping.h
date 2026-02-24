//===--- Subtyping.h - Swift subtyping and conversion rules -----*- C++ -*-===//
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
// This file implements various utilities for reasoning about the Swift
// subtyping relation.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_SUBTYPING_H
#define SWIFT_SEMA_SUBTYPING_H

#include "swift/Basic/OptionSet.h"

namespace swift {

class GenericSignature;
class ProtocolDecl;
class Type;

namespace constraints {

class ConstraintSystem;

/// Checks if two types can unify if we record a bind constraint between them.
///
/// Returns:
/// - true if there is some indication that the bind may succeed.
/// - false if the bind will definitely fail.
/// - std::nullopt if unknown.
std::optional<bool> isLikelyExactMatch(Type first, Type second);

enum class ConversionBehavior : unsigned {
  /// Most nominal types, archetypes, empty tuple.
  None,

  /// String has no proper subtypes, but it has pointers as supertypes.
  String,

  /// Classes can have subclasses and superclasses.
  Class,

  /// AnyHashable is a supertype to all Hashable types.
  AnyHashable,

  /// CGFloat and Double.
  Double,

  /// We fold all pointer types into one for now. These have arrays and
  /// strings as subtypes.
  Pointer,

  /// Arrays convert to arrays, and they have pointers as supertypes.
  Array,

  /// Dictionaries convert to dictionaries.
  Dictionary,

  /// Sets convert to sets.
  Set,

  /// Optionals convert to Optionals. Every type has an Optional of itself
  /// as a supertype.
  Optional,

  /// Function types and metatypes.
  Structural,

  /// InOut types have Pointer types as supertypes.
  InOut,

  /// LValue types convert to InOut types. Every type has a subtype that is
  /// the lvalue of itself.
  LValue,

  /// Everything else that we don't reason about for now, like
  /// existentials and tuples.
  Unknown
};

/// Classify the possible conversions having this type as result type.
ConversionBehavior getConversionBehavior(Type type);

/// Suppose we are given a type T and a protocol P, and T conv U for
/// some type U; if U conforms to P, does it follow that T conforms to P?
bool checkTransitiveSupertypeConformance(ConstraintSystem &cs,
                                         Type type, ProtocolDecl *proto);

/// Suppose we are given a type T and a protocol P, and U conv T for
/// some type U; if U conforms to P, does it follow that T conforms to P?
bool checkTransitiveSubtypeConformance(ConstraintSystem &cs,
                                       Type type, ProtocolDecl *proto);

/// Check if there exist any subtypes of the given type, other than
/// the type itself. If the type contains type variables, this will
/// give a conservative approximation.
bool hasProperSubtypes(Type type);

/// Check if there exist any supertypes of the given type, other than
/// the type itself, and those that every type T has, that is,
/// Optional<T> and existentials.
bool hasProperSupertypes(Type type);

enum ConflictFlag : unsigned {
  Category = 1 << 0,
  Exact = 1 << 1,
  Class = 1 << 2,
  Structural = 1 << 3,
  Array = 1 << 4,
  DictionaryKey = 1 << 5,
  DictionaryValue = 1 << 6,
  Set = 1 << 7,
  Optional = 1 << 8,
  Double = 1 << 9,
  Conformance = 1 << 10
};
using ConflictReason = OptionSet<ConflictFlag>;

/// Check whether lhs, as a type with type variables or unopened type
/// parameters, might be a subtype of rhs, which again is a type with
/// type variables or unopened type parameters.
///
/// The type parameters are interpreted with respect to sig, whereas
/// type variables are just assumed opaque.
///
/// The answer is conservative, so we err on the side of saying that
/// a convesion _can_ happen. We only return a non-empty ConflictReason
/// if the conversion will definitely fail.
///
/// Even if the types do not contain type variables or type parameters,
/// this does not give a completely accurate answer, yet.
ConflictReason canPossiblyConvertTo(
    ConstraintSystem &cs,
    Type lhs, Type rhs,
    GenericSignature sig);

}  // end namespace constraints

}  // end namespace swift

#endif  // SWIFT_SEMA_SUBTYPING_H
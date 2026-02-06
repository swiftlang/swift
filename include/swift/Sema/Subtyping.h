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
  None,
  Class,
  AnyHashable,
  Double,
  Pointer,
  Array,
  Dictionary,
  Set,
  Optional,
  Structural,
  Unknown
};

/// Classify the possible conversions having this type as result type.
ConversionBehavior getConversionBehavior(Type type);

/// Check whether there exists a type that could be implicitly converted
/// to a given type i.e. is the given type is Double or Optional<..> this
/// function is going to return true because CGFloat could be converted
/// to a Double and non-optional value could be injected into an optional.
bool hasConversions(Type type);

enum ConflictFlag : unsigned {
  Category = 1 << 0,
  Exact = 1 << 1,
  Class = 1 << 2,
  Structural = 1 << 3,
  Array = 1 << 4,
  Dictionary = 1 << 5,
  Set = 1 << 6,
  Optional = 1 << 7,
  Double = 1 << 8,
  Conformance = 1 << 9,
  Mutability = 1 << 10
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
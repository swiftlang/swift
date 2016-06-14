//===--- Requirement.h - Swift Requirement ASTs -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Requirement class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_REQUIREMENT_H
#define SWIFT_AST_REQUIREMENT_H

#include "swift/AST/Type.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

/// Describes the kind of a requirement that occurs within a requirements
/// clause.
enum class RequirementKind : unsigned {
  /// A conformance requirement T : P, where T is a type that depends
  /// on a generic parameter and P is a protocol to which T must conform.
  Conformance,
  /// A superclass requirement T : C, where T is a type that depends
  /// on a generic parameter and C is a concrete class type which T must
  /// equal or be a subclass of.
  Superclass,
  /// A same-type requirement T == U, where T and U are types that shall be
  /// equivalent.
  SameType,
  /// A marker that indicates where the witness for the given (first)
  /// type should be located.
  ///
  /// FIXME: This is a crutch used to help us eliminate various walks over
  /// "all archetypes".
  WitnessMarker

  // Note: there is code that packs this enum in a 2-bit bitfield.  Audit users
  // when adding enumerators.
};

/// \brief A single requirement placed on the type parameters (or associated
/// types thereof) of a
class Requirement {
  llvm::PointerIntPair<Type, 2, RequirementKind> FirstTypeAndKind;
  Type SecondType;

public:
  /// Create a conformance or same-type requirement.
  Requirement(RequirementKind kind, Type first, Type second)
    : FirstTypeAndKind(first, kind), SecondType(second) {
    if (kind != RequirementKind::WitnessMarker) {
      assert(first);
      assert(second);
    }
  }

  /// \brief Determine the kind of requirement.
  RequirementKind getKind() const { return FirstTypeAndKind.getInt(); }

  /// \brief Retrieve the first type.
  Type getFirstType() const {
    return FirstTypeAndKind.getPointer();
  }

  /// \brief Retrieve the second type.
  Type getSecondType() const {
    return SecondType;
  }
};

} // end namespace swift

#endif

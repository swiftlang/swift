//===--- RequirementBase.h - Swift Requirement ASTs -------------*- C++ -*-===//
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
// This file defines the Requirement class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_REQUIREMENTBASE_H
#define SWIFT_AST_REQUIREMENTBASE_H

#include "llvm/ADT/Hashing.h"

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
  /// A layout bound T : L, where T is a type that depends on a generic
  /// parameter and L is some layout specification that should bound T.
  Layout,

  // Note: there is code that packs this enum in a 2-bit bitfield.  Audit users
  // when adding enumerators.
};

/// Abstract base class for Requirement.
template <typename TypeType, typename PointerIntPairType,
          typename LayoutConstraintType>
class RequirementBase {
protected:
  PointerIntPairType FirstTypeAndKind;
  /// The second element of the requirement. Its content is dependent
  /// on the requirement kind.
  /// The payload of the following enum should always match the kind!
  /// Any access to the fields of this enum should first check if the
  /// requested access matches the kind of the requirement.
  union {
    TypeType SecondType;
    LayoutConstraintType SecondLayout;
  };

public:
  /// Create a conformance or same-type requirement.
  RequirementBase(RequirementKind kind, TypeType first, TypeType second)
      : FirstTypeAndKind(first, kind), SecondType(second) {
    assert(first);
    assert(second);
    assert(kind != RequirementKind::Layout);
  }

  /// Create a layout constraint requirement.
  RequirementBase(RequirementKind kind, TypeType first,
                  LayoutConstraintType second)
      : FirstTypeAndKind(first, kind), SecondLayout(second) {
    assert(first);
    assert(second);
    assert(kind == RequirementKind::Layout);
  }

  /// Determine the kind of requirement.
  RequirementKind getKind() const { return FirstTypeAndKind.getInt(); }

  /// Retrieve the first type.
  TypeType getFirstType() const {
    return FirstTypeAndKind.getPointer();
  }

  /// Retrieve the second type.
  TypeType getSecondType() const {
    assert(getKind() != RequirementKind::Layout);
    return SecondType;
  }

  /// Retrieve the layout constraint.
  LayoutConstraintType getLayoutConstraint() const {
    assert(getKind() == RequirementKind::Layout);
    return SecondLayout;
  }

  friend llvm::hash_code hash_value(const RequirementBase &requirement) {
    using llvm::hash_value;

    llvm::hash_code first =
        hash_value(requirement.FirstTypeAndKind.getOpaqueValue());
    llvm::hash_code second;
    switch (requirement.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::SameType:
      second = hash_value(requirement.getSecondType());
      break;

    case RequirementKind::Layout:
      second = hash_value(requirement.getLayoutConstraint());
      break;
    }

    return llvm::hash_combine(first, second);
  }

  friend bool operator==(const RequirementBase &lhs,
                         const RequirementBase &rhs) {
    if (lhs.FirstTypeAndKind.getOpaqueValue()
          != rhs.FirstTypeAndKind.getOpaqueValue())
      return false;

    switch (lhs.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::SameType:
      return lhs.getSecondType().getPointer() ==
          rhs.getSecondType().getPointer();

    case RequirementKind::Layout:
      return lhs.getLayoutConstraint() == rhs.getLayoutConstraint();
    }
    llvm_unreachable("Unhandled RequirementKind in switch");
  }
};
} // namespace swift
#endif

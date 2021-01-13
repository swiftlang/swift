//===--- ConstraintGraph.h - Constraint Graph -------------------*- C++ -*-===//
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
// This file defines the \c PotentialBindings class and its auxilary types
// such as \c PotentialBinding, that are used to descibe bindings which
// a particular type variable could be bound to.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CSBINDINGS_H
#define SWIFT_SEMA_CSBINDINGS_H

#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Sema/Constraint.h"
#include "llvm/ADT/PointerUnion.h"

namespace swift {

class ProtocolDecl;

namespace constraints {

class ConstraintLocator;

namespace inference {

/// The kind of bindings that are permitted.
enum class AllowedBindingKind : uint8_t {
  /// Only the exact type.
  Exact,
  /// Supertypes of the specified type.
  Supertypes,
  /// Subtypes of the specified type.
  Subtypes
};

/// The kind of literal binding found.
enum class LiteralBindingKind : uint8_t {
  None,
  Collection,
  Float,
  Atom,
};

/// A potential binding from the type variable to a particular type,
/// along with information that can be used to construct related
/// bindings, e.g., the supertypes of a given type.
struct PotentialBinding {
  /// The type to which the type variable can be bound.
  Type BindingType;

  /// The kind of bindings permitted.
  AllowedBindingKind Kind;

protected:
  /// The source of the type information.
  ///
  /// Determines whether this binding represents a "hole" in
  /// constraint system. Such bindings have no originating constraint
  /// because they are synthetic, they have a locator instead.
  PointerUnion<Constraint *, ConstraintLocator *> BindingSource;

  PotentialBinding(Type type, AllowedBindingKind kind,
                   PointerUnion<Constraint *, ConstraintLocator *> source)
      : BindingType(type), Kind(kind), BindingSource(source) {}

public:
  PotentialBinding(Type type, AllowedBindingKind kind, Constraint *source)
      : PotentialBinding(
            type->getWithoutParens(), kind,
            PointerUnion<Constraint *, ConstraintLocator *>(source)) {}

  bool isDefaultableBinding() const {
    if (auto *constraint = BindingSource.dyn_cast<Constraint *>())
      return constraint->getKind() == ConstraintKind::Defaultable;
    // If binding source is not constraint - it's a hole, which is
    // a last resort default binding for a type variable.
    return true;
  }

  bool hasDefaultedLiteralProtocol() const {
    return bool(getDefaultedLiteralProtocol());
  }

  ProtocolDecl *getDefaultedLiteralProtocol() const {
    auto *constraint = BindingSource.dyn_cast<Constraint *>();
    if (!constraint)
      return nullptr;

    return constraint->getKind() == ConstraintKind::LiteralConformsTo
               ? constraint->getProtocol()
               : nullptr;
  }

  ConstraintLocator *getLocator() const {
    if (auto *constraint = BindingSource.dyn_cast<Constraint *>())
      return constraint->getLocator();
    return BindingSource.get<ConstraintLocator *>();
  }

  Constraint *getSource() const { return BindingSource.get<Constraint *>(); }

  PotentialBinding withType(Type type) const {
    return {type, Kind, BindingSource};
  }

  PotentialBinding withSameSource(Type type, AllowedBindingKind kind) const {
    return {type, kind, BindingSource};
  }

  /// Determine whether this binding could be a viable candidate
  /// to be "joined" with some other binding. It has to be at least
  /// a non-default r-value supertype binding with no type variables.
  bool isViableForJoin() const;

  static PotentialBinding forHole(TypeVariableType *typeVar,
                                  ConstraintLocator *locator) {
    return {HoleType::get(typeVar->getASTContext(), typeVar),
            AllowedBindingKind::Exact,
            /*source=*/locator};
  }

  static PotentialBinding forPlaceholder(Type placeholderTy) {
    return {placeholderTy, AllowedBindingKind::Exact,
            PointerUnion<Constraint *, ConstraintLocator *>()};
  }
};

} // end namespace inference

} // end namespace constraints

} // end namespace swift

namespace llvm {

template <>
struct DenseMapInfo<swift::constraints::inference::PotentialBinding> {
  using Binding = swift::constraints::inference::PotentialBinding;

  static Binding getEmptyKey() {
    return placeholderKey(llvm::DenseMapInfo<swift::TypeBase *>::getEmptyKey());
  }

  static Binding getTombstoneKey() {
    return placeholderKey(
        llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey());
  }

  static unsigned getHashValue(const Binding &Val) {
    return DenseMapInfo<swift::Type>::getHashValue(
        Val.BindingType->getCanonicalType());
  }

  static bool isEqual(const Binding &LHS, const Binding &RHS) {
    auto lhsTy = LHS.BindingType.getPointer();
    auto rhsTy = RHS.BindingType.getPointer();

    // Fast path: pointer equality.
    if (DenseMapInfo<swift::TypeBase *>::isEqual(lhsTy, rhsTy))
      return true;

    // If either side is empty or tombstone, let's use pointer equality.
    {
      auto emptyTy = llvm::DenseMapInfo<swift::TypeBase *>::getEmptyKey();
      auto tombstoneTy =
          llvm::DenseMapInfo<swift::TypeBase *>::getTombstoneKey();

      if (lhsTy == emptyTy || lhsTy == tombstoneTy)
        return lhsTy == rhsTy;

      if (rhsTy == emptyTy || rhsTy == tombstoneTy)
        return lhsTy == rhsTy;
    }

    // Otherwise let's drop the sugar and check.
    return LHS.BindingType->isEqual(RHS.BindingType);
  }

private:
  static Binding placeholderKey(swift::Type type) {
    return Binding::forPlaceholder(type);
  }
};

} // end namespace llvm

#endif // SWIFT_SEMA_CSBINDINGS_H

//===--- Requirement.h - Swift Requirement ASTs -----------------*- C++ -*-===//
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

#ifndef SWIFT_AST_REQUIREMENT_H
#define SWIFT_AST_REQUIREMENT_H

#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/RequirementBase.h"
#include "swift/AST/Type.h"
#include "swift/Basic/Debug.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {

/// A single requirement placed on the type parameters (or associated
/// types thereof) of a
class Requirement
    : public RequirementBase<Type,
                             llvm::PointerIntPair<Type, 3, RequirementKind>,
                             LayoutConstraint> {
public:
  /// Create a conformance or same-type requirement.
  Requirement(RequirementKind kind, Type first, Type second)
      : RequirementBase(kind, first, second) {}

  /// Create a layout constraint requirement.
  Requirement(RequirementKind kind, Type first, LayoutConstraint second)
    : RequirementBase(kind, first, second) {}

  /// Whether this requirement is in its canonical form.
  bool isCanonical() const;

  /// Get the canonical form of this requirement.
  Requirement getCanonical() const;

  /// Subst the types involved in this requirement.
  ///
  /// The \c args arguments are passed through to Type::subst. This doesn't
  /// touch the superclasses, protocols or layout constraints.
  template <typename ...Args>
  llvm::Optional<Requirement> subst(Args &&...args) const {
    auto newFirst = getFirstType().subst(std::forward<Args>(args)...);
    if (newFirst->hasError())
      return None;

    switch (getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::SameType: {
      auto newSecond = getSecondType().subst(std::forward<Args>(args)...);
      if (newSecond->hasError())
        return None;
      return Requirement(getKind(), newFirst, newSecond);
    }
    case RequirementKind::Layout:
      return Requirement(getKind(), newFirst, getLayoutConstraint());
    }

    llvm_unreachable("Unhandled RequirementKind in switch.");
  }

  ProtocolDecl *getProtocolDecl() const;

  /// Determines if this substituted requirement is satisfied.
  ///
  /// \param conditionalRequirements An out parameter initialized to an
  /// array of requirements that the caller must check to ensure this
  /// requirement is completely satisfied.
  bool isSatisfied(ArrayRef<Requirement> &conditionalRequirements,
                   bool allowMissing = false) const;

  /// Determines if this substituted requirement can ever be satisfied,
  /// possibly with additional substitutions.
  ///
  /// For example, if 'T' is unconstrained, then a superclass requirement
  /// 'T : C' can be satisfied; however, if 'T' already has an unrelated
  /// superclass requirement, 'T : C' cannot be satisfied.
  bool canBeSatisfied() const;

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &out) const;
  void print(raw_ostream &os, const PrintOptions &opts) const;
  void print(ASTPrinter &printer, const PrintOptions &opts) const;
};

inline void simple_display(llvm::raw_ostream &out, const Requirement &req) {
  req.print(out, PrintOptions());
}

} // end namespace swift

#endif

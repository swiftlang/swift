//===--- CSFix.h - Constraint Fixes ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides necessary abstractions for constraint fixes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CSFIX_H
#define SWIFT_SEMA_CSFIX_H

#include "swift/AST/Expr.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"

namespace llvm {
class raw_ostream;
}

namespace swift {
namespace constraints {

class ConstraintLocator;
class Solution;

class ConstraintFix {
  ConstraintLocator *Locator;

public:
  ConstraintFix(ConstraintLocator *locator) : Locator(locator) {}

  virtual ~ConstraintFix();

  /// Diagnose a failure associated with this fix given
  /// root expression and information from well-formed solution.
  virtual bool diagnose(Expr *root, const Solution &solution) const = 0;
  virtual void print(llvm::raw_ostream &Out) const = 0;

  LLVM_ATTRIBUTE_DEPRECATED(void dump() const LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  ConstraintLocator *getLocator() const { return Locator; }
};

/// Append 'as! T' to force a downcast to the specified type.
class ForceDowncast final : public ConstraintFix {
  Type DowncastTo;

public:
  ForceDowncast(Type toType, ConstraintLocator *locator)
      : ConstraintFix(locator), DowncastTo(toType) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override;
};

/// Introduce a '!' to force an optional unwrap.
class ForceOptional final : public ConstraintFix {
public:
  ForceOptional(ConstraintLocator *locator) : ConstraintFix(locator) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: force optional]";
  }
};

/// Unwrap an optional base when we have a member access.
class UnwrapOptionalBase final : public ConstraintFix {
  DeclName MemberName;

public:
  UnwrapOptionalBase(ConstraintLocator *locator, DeclName member)
      : ConstraintFix(locator), MemberName(member) {}

  bool diagnose(Expr *root, const Solution &solution) const override;
  void print(llvm::raw_ostream &Out) const override {
    Out << "[fix: unwrap optional base of member lookup]";
  }
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H

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

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSFIX_H

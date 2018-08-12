//===--- CSFix.cpp - Constraint Fixes -------------------------------------===//
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
// This file implements the \c ConstraintFix class and its related types,
// which is used by constraint solver to attempt to fix constraints to be
// able to produce a solution which is easily diagnosable.
//
//===----------------------------------------------------------------------===//

#include "CSFix.h"
#include "CSDiagnostics.h"
#include "ConstraintLocator.h"
#include "ConstraintSystem.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace constraints;

ConstraintFix::~ConstraintFix() {}

void ConstraintFix::dump() const { print(llvm::errs()); }

bool ForceDowncast::diagnose(Expr *expr, const Solution &solution) const {
  MissingExplicitConversionFailure failure(expr, solution, getLocator(),
                                           DowncastTo);
  return failure.diagnose();
}

void ForceDowncast::print(llvm::raw_ostream &Out) const {
  Out << "fix: force downcast (as! " << DowncastTo->getString() << ")\n";
}

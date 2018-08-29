//===--- CSDiag.h - Constraint-based Type Checking ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides shared utility methods for ConstraintSystem diagnosis.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SEMA_CSDIAG_H
#define SWIFT_SEMA_CSDIAG_H

#include "ConstraintSystem.h"
#include "swift/AST/DiagnosticEngine.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace swift {
  class Expr;
  class Type;
  class SourceLoc;

  std::string getTypeListString(Type type);
  
  /// Rewrite any type variables & archetypes in the specified type with
  /// UnresolvedType.
  Type replaceTypeParametersWithUnresolved(Type ty);
  Type replaceTypeVariablesWithUnresolved(Type ty);

  /// Diagnose lvalue expr error.
  void diagnoseSubElementFailure(Expr *destExpr, SourceLoc loc,
                                 constraints::ConstraintSystem &CS,
                                 Diag<StringRef> diagID,
                                 Diag<Type> unknownDiagID);
};

#endif /* SWIFT_SEMA_CSDIAG_H */

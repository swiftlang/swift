//===--- CodeCompletionTypeChecking.h  --------------------------*- C++ -*-===//
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
/// \file
/// Provides TypeCheckCompletionCallback implementations for the various kinds
/// of code completion. These extract and persist information needed to compute
/// completion results from the solutions formed during expression typechecking.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CODECOMPLETIONTYPECHECKING_H
#define SWIFT_SEMA_CODECOMPLETIONTYPECHECKING_H

#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
  class Decl;
  class DeclContext;
  class Type;
  class ValueDecl;
  class CodeCompletionExpr;

  namespace constraints {
    class Solution;
  }

  class TypeCheckCompletionCallback {
    bool GotCallback = false;

  public:
    virtual ~TypeCheckCompletionCallback() {}

    /// Called for each solution produced while  type-checking an expression
    /// that the code completion expression participates in.
    virtual void sawSolution(const constraints::Solution &solution) {
      GotCallback = true;
    };

    /// True if at least one solution was passed via the \c sawSolution
    /// callback.
    bool gotCallback() const { return GotCallback; }

    /// Typecheck the code completion expression in its outermost expression
    /// context, calling \c sawSolution for each solution formed.
    virtual void fallbackTypeCheck(DeclContext *DC);
  };
}

#endif

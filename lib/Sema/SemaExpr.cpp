//===--- SemaExpr.cpp - Swift Expression Semantic Analysis ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements semantic analysis for Swift expressions.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaExpr.h"
#include "swift/Sema/Sema.h"
#include "swift/AST/Expr.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
using namespace swift;

SemaExpr::SemaExpr(Sema &S) : SemaBase(S) {
}

Expr *SemaExpr::ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc) {
  return new (S.Context) IntegerLiteral(Text, Loc);
}

Expr *SemaExpr::ActOnParenExpr(llvm::SMLoc LPLoc, Expr *SubExpr,
                               llvm::SMLoc RPLoc) {
  return new (S.Context) ParenExpr(LPLoc, SubExpr, RPLoc);
}

Expr *SemaExpr::ActOnBinaryExpr(Expr *LHS, llvm::SMLoc OpLoc, Expr *RHS) {
  // TODO: Get Expr opcode.
  return LHS;
}

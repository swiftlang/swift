//===--- SemaExpr.h - Swift Expression Semantic Analysis --------*- C++ -*-===//
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
// This file defines the Sema interface which implement hooks invoked by the 
// parser to build the AST for expressions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_EXPR_H
#define SWIFT_SEMA_EXPR_H

#include "swift/Sema/SemaBase.h"

namespace swift {
  class Sema;
  
/// SemaExpr - Semantic analysis support for Swift expressions.
class SemaExpr : public SemaBase {
public:
  SemaExpr(Sema &s);
  
  Expr *NumericConstant(llvm::StringRef Text, llvm::SMLoc Loc);
  Expr *ParenExpr(llvm::SMLoc LPLoc, Expr *SubExpr, llvm::SMLoc RPLoc);
  Expr *BinaryExpr(Expr *LHS, llvm::SMLoc OpLoc, Expr *RHS);
};
  
} // end namespace swift

#endif

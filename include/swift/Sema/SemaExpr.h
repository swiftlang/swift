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

namespace llvm {
  template <typename PT1, typename PT2>
  class PointerUnion;
  template<class T>
  class NullablePtr;
}
namespace swift {
  class Sema;
  class Expr;
  class NamedDecl;
  
/// SemaExpr - Semantic analysis support for Swift expressions.
class SemaExpr : public SemaBase {
public:
  explicit SemaExpr(Sema &S) : SemaBase(S) {}
  
  llvm::NullablePtr<Expr>
  ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc);
  llvm::NullablePtr<Expr>
  ActOnParenExpr(llvm::SMLoc LPLoc, Expr *SubExpr, llvm::SMLoc RPLoc);
  llvm::NullablePtr<Expr>
  ActOnBraceExpr(llvm::SMLoc LBLoc,
                 const llvm::PointerUnion<Expr*, NamedDecl*> *Elements,
                 unsigned NumElements, bool HasMissingSemi, llvm::SMLoc RBLoc);
  llvm::NullablePtr<Expr> ActOnBinaryExpr(/*ExprKind*/unsigned Kind,
                                          Expr *LHS, llvm::SMLoc OpLoc,
                                          Expr *RHS);
};
  
} // end namespace swift

#endif

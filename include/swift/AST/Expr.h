//===--- Expr.h - Swift Language Expression ASTs ----------------*- C++ -*-===//
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
// This file defines the Expr class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXPR_H
#define SWIFT_EXPR_H

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class ASTContext;
  
enum ExprKind {
  IntegerLiteralKind,
  ParenExprKind,
  
  BinaryAddExprKind,
  BinarySubExprKind,
  BinaryMulExprKind,
  BinaryDivExprKind
};
  
  
/// Expr - Base class for all expressions in swift.
class Expr {
  Expr(const Expr&);                 // DO NOT IMPLEMENT
  void operator=(const Expr&);       // DO NOT IMPLEMENT
  ExprKind Kind;
public:
  // TODO: Type.
  
  Expr(ExprKind kind) : Kind(kind) {}

  ExprKind getKind() const { return Kind; }
  
private:
  // Make placement new and vanilla new/delete illegal for Exprs.
  void *operator new(size_t Bytes) throw();  // DO NOT IMPLEMENT.
  void operator delete(void *Data) throw();  // DO NOT IMPLEMENT.
  void *operator new(size_t Bytes, void *Mem) throw();  // DO NOT IMPLEMENT.
public:
  // Only allow allocation of Stmts using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = 8) throw();  
};


/// IntegerLiteral - Integer literal, like '4'.
class IntegerLiteral : public Expr {
public:
  llvm::StringRef Val;  // TODO: uint64_t.  APInt leaks.
  llvm::SMLoc Loc;
  
  IntegerLiteral(llvm::StringRef V, llvm::SMLoc L)
    : Expr(IntegerLiteralKind), Val(V), Loc(L) {}
};

/// ParenExpr - Parenthesized expressions like '(x+x)'.
class ParenExpr : public Expr {
public:
  llvm::SMLoc LParenLoc;
  Expr *SubExpr;
  llvm::SMLoc RParenLoc;
  
  ParenExpr(llvm::SMLoc lparenloc, Expr *subexpr, llvm::SMLoc rparenloc)
    : Expr(ParenExprKind), LParenLoc(lparenloc), SubExpr(subexpr),
      RParenLoc(rparenloc) {}
};

/// BinaryExpr - Binary expressions like 'x+y'.
class BinaryExpr : public Expr {
public:
  Expr *LHS;
  llvm::SMLoc OpLoc;
  Expr *RHS;
  
  BinaryExpr(ExprKind kind, Expr *lhs, llvm::SMLoc oploc, Expr *rhs)
    : Expr(kind), LHS(lhs), OpLoc(oploc), RHS(rhs) {}
};
  
} // end namespace swift

#endif

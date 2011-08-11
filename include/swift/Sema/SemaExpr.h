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
#include "llvm/ADT/PointerIntPair.h"

namespace llvm {
  template <typename PT1, typename PT2> class PointerUnion;
  template<class T> class NullablePtr;
  template<class T> class ArrayRef;
}
namespace swift {
  class Decl;
  class Expr;
  class FuncExpr;
  class Sema;
  class Stmt;
  class Type;
  class ValueDecl;
  class Identifier;
  
/// SemaExpr - Semantic analysis support for Swift expressions.
class SemaExpr : public SemaBase {
public:
  explicit SemaExpr(Sema &S) : SemaBase(S) {}

  typedef llvm::PointerUnion<Expr*, Decl*> ExprOrDecl;
  
  // Action Implementations.  These return null on semantic errors.
  NullablePtr<Expr>
  ActOnNumericConstant(StringRef Text, SMLoc Loc);
  NullablePtr<Expr>
  ActOnIdentifierExpr(Identifier Text, SMLoc Loc);
  NullablePtr<Expr>
  ActOnScopedIdentifierExpr(Identifier ScopeName, SMLoc ScopeLoc,
                            SMLoc ColonColonLoc,
                            Identifier Name, SMLoc NameLoc);
  
  NullablePtr<Expr>
  ActOnTupleExpr(SMLoc LPLoc, Expr *const *SubExprs,
                 const Identifier *SubExprNames,
                 unsigned NumSubExprs, SMLoc RPLoc,
                 bool IsPrecededByIdentifier);

  /// ActOnCondition - Handle a condition to an if/while statement, inserting
  /// the call that will convert to a 1-bit type.
  Expr *ActOnCondition(Expr *Cond);
 
  FuncExpr *ActOnFuncExprStart(SMLoc FuncLoc, Type FuncTy);
  
};
  
} // end namespace swift

#endif

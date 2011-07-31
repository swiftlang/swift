//===--- ExprVisitor.h - Visitor for Swift Language Expressions -*- C++ -*-===//
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
// This file defines the ExprVisitor class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_EXPRVISITOR_H
#define SWIFT_AST_EXPRVISITOR_H

#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"

namespace swift {
  
/// ExprVisitor - This is a simple visitor class for Swift expressions.
template<typename ImplClass,
         typename ExprRetTy = void, typename StmtRetTy = void> 
class ExprVisitor {
public:

  ExprRetTy visit(Expr *E) {
    switch (E->Kind) {

#define DISPATCH(CLASS) \
  case ExprKind::CLASS: \
  return static_cast<ImplClass*>(this)->visit ## CLASS ## \
    Expr(static_cast<CLASS##Expr*>(E))
        
    DISPATCH(IntegerLiteral);
    DISPATCH(DeclRef);
    DISPATCH(OverloadSetRef);
    DISPATCH(UnresolvedDeclRef);
    DISPATCH(UnresolvedMember);
    DISPATCH(UnresolvedScopedIdentifier);
    DISPATCH(Tuple);
    DISPATCH(UnresolvedDot);
    DISPATCH(TupleElement);
    DISPATCH(TupleShuffle);
    DISPATCH(Apply);
    DISPATCH(Sequence);
    DISPATCH(Closure);
    DISPATCH(AnonClosureArg);
    DISPATCH(Binary);
#undef DISPATCH
    }
    assert(0 && "Not reachable, all cases handled");
    abort();
  }
  
  StmtRetTy visit(Stmt *S) {
    switch (S->Kind) {

#define DISPATCH(CLASS) \
  case StmtKind::CLASS: \
  return static_cast<ImplClass*>(this)->visit ## CLASS ## \
    Stmt(static_cast<CLASS##Stmt*>(S))
        
    DISPATCH(Brace);
    DISPATCH(If);
#undef DISPATCH
    }
    assert(0 && "Not reachable, all cases handled");
    abort();
  }
};
  
  
} // end namespace swift
  
#endif

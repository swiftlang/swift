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

namespace swift {
  
/// ExprVisitor - This is a simple visitor class for Swift expressions.
template<typename ImplClass, typename RetTy = void> 
class ExprVisitor {
public:

  RetTy Visit(Expr *E) {
    switch (E->Kind) {

#define DISPATCH(CLASS) \
  case CLASS##Kind: \
  return static_cast<ImplClass*>(this)->Visit ## CLASS(static_cast<CLASS*>(E))
        
    DISPATCH(IntegerLiteral);
    DISPATCH(DeclRefExpr);
    DISPATCH(UnresolvedMemberExpr);
    DISPATCH(TupleExpr);
    DISPATCH(UnresolvedDotExpr);
    DISPATCH(TupleElementExpr);
    DISPATCH(ApplyExpr);
    DISPATCH(SequenceExpr);
    DISPATCH(BraceExpr);
    DISPATCH(ClosureExpr);
    DISPATCH(BinaryExpr);
#undef DISPATCH
    }
    assert(0 && "Not reachable, all cases handled");
    abort();
  }
};
  
  
} // end namespace swift
  
#endif

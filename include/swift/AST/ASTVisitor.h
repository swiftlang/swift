//===-- ASTVisitor.h - Decl, Expr and Stmt Visitor --------------*- C++ -*-===//
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
// This file defines the ASTVisitor class, and the DeclVisitor, ExprVisitor, and
// StmtVisitor template typedefs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ASTVISITOR_H
#define SWIFT_AST_ASTVISITOR_H

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"

namespace swift {
  
/// ASTVisitor - This is a simple visitor class for Swift expressions.
template<typename ImplClass,
         typename ExprRetTy = void,
         typename StmtRetTy = void,
         typename DeclRetTy = void> 
class ASTVisitor {
public:

  DeclRetTy visit(Decl *D) {
    switch (D->Kind) {
        
#define DISPATCH(CLASS) \
  case DeclKind::CLASS: \
  return static_cast<ImplClass*>(this)->visit ## CLASS ## \
    Decl(static_cast<CLASS##Decl*>(D))

      DISPATCH(Import);
      DISPATCH(TypeAlias);
      DISPATCH(Var);
      DISPATCH(Func);
      DISPATCH(OneOfElement);
      DISPATCH(Arg);
      DISPATCH(ElementRef);
#undef DISPATCH
    }
    assert(0 && "Not reachable, all cases handled");
    abort();
  }
  
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
    DISPATCH(Call);
    DISPATCH(Sequence);
    DISPATCH(Func);
    DISPATCH(Closure);
    DISPATCH(AnonClosureArg);
    DISPATCH(Unary);
    DISPATCH(Binary);
    DISPATCH(ProtocolElement);
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
        
    DISPATCH(Semi);
    DISPATCH(Assign);
    DISPATCH(Brace);
    DISPATCH(Return);
    DISPATCH(If);
    DISPATCH(While);
#undef DISPATCH
    }
    assert(0 && "Not reachable, all cases handled");
    abort();
  }
};
  
  
template<typename ImplClass, typename ExprRetTy = void>
using ExprVisitor = ASTVisitor<ImplClass, ExprRetTy>;

template<typename ImplClass, typename StmtRetTy = void>
using StmtVisitor = ASTVisitor<ImplClass, void, StmtRetTy>;

template<typename ImplClass, typename DeclRetTy = void>
using DeclVisitor = ASTVisitor<ImplClass, void, DeclRetTy>;

} // end namespace swift
  
#endif

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
#include "llvm/Support/ErrorHandling.h"

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
    llvm_unreachable("Not reachable, all cases handled");
  }
  
  ExprRetTy visit(Expr *E) {
    switch (E->getKind()) {

#define EXPR(CLASS, PARENT) \
    case ExprKind::CLASS: \
      return static_cast<ImplClass*>(this) \
        ->visit##CLASS##Expr(static_cast<CLASS##Expr*>(E));
#include "swift/AST/ExprNodes.def"

    }
    llvm_unreachable("Not reachable, all cases handled");
  }
  
  StmtRetTy visit(Stmt *S) {
    switch (S->getKind()) {

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
    llvm_unreachable("Not reachable, all cases handled");
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

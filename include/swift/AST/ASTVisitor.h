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
    switch (D->getKind()) {
#define DECL(CLASS, PARENT) \
    case DeclKind::CLASS: \
      return static_cast<ImplClass*>(this) \
        ->visit##CLASS##Decl(static_cast<CLASS##Decl*>(D));
#include "swift/AST/DeclNodes.def"
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
  
  // Provide default implementations of abstract "visit" implementations that
  // just chain to their base class.  This allows visitors to just implement
  // the base behavior and handle all subclasses if they desire.  Since this is
  // a template, it will only instantiate cases that are used and thus we still
  // require full coverage of the AST nodes by the visitor.
#define ABSTRACT_EXPR(CLASS, PARENT)                                \
  ExprRetTy visit##CLASS##Expr(CLASS##Expr *E) {                    \
     return static_cast<ImplClass*>(this)->visit##PARENT(E);  \
  }
#define EXPR(CLASS, PARENT) ABSTRACT_EXPR(CLASS, PARENT)
#include "swift/AST/ExprNodes.def"

  StmtRetTy visit(Stmt *S) {
    switch (S->getKind()) {

#define STMT(CLASS, PARENT) \
    case StmtKind::CLASS: \
      return static_cast<ImplClass*>(this) \
        ->visit##CLASS##Stmt(static_cast<CLASS##Stmt*>(S));
#include "swift/AST/StmtNodes.def"

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

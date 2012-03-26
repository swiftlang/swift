//===-- ASTVisitor.h - IR-gen ASTVisitor specialization ---------*- C++ -*-===//
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
// This file defines swift::irgen::ASTVisitor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_ASTVISITOR_H
#define SWIFT_IRGEN_ASTVISITOR_H

#include "swift/AST/ASTVisitor.h"

namespace swift {
namespace irgen {
  
/// irgen::ASTVisitor - This is a specialization of swift::ASTVisitor
/// which automatically ignores certain AST node kinds.
template<typename ImplClass,
         typename ExprRetTy = void,
         typename StmtRetTy = void,
         typename DeclRetTy = void> 
class ASTVisitor :
  public swift::ASTVisitor<ImplClass, ExprRetTy, StmtRetTy, DeclRetTy> {
public:

#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) \
  ExprRetTy visit##Id##Expr(Id##Expr *E) { \
    llvm_unreachable(#Id "Expr should not survive to IR-gen"); \
  }
#include "swift/AST/ExprNodes.def"

  ExprRetTy visitErrorExpr(ErrorExpr *E) {
    llvm_unreachable("these expression kinds should not survive to IR-gen");
  }

  ExprRetTy visitParenExpr(ParenExpr *E) {
    return static_cast<ImplClass*>(this)->visit(E->getSubExpr());
  }

  ExprRetTy visitAddressOfExpr(AddressOfExpr *E) {
    return static_cast<ImplClass*>(this)->visit(E->getSubExpr());
  }
};  
  
template<typename ImplClass, typename ExprRetTy = void>
using ExprVisitor = ASTVisitor<ImplClass, ExprRetTy>;

template<typename ImplClass, typename StmtRetTy = void>
using StmtVisitor = ASTVisitor<ImplClass, void, StmtRetTy>;

template<typename ImplClass, typename DeclRetTy = void>
using DeclVisitor = ASTVisitor<ImplClass, void, void, DeclRetTy>;

} // end namespace irgen
} // end namespace swift
  
#endif

//===--- ASTVisitor.h - SILGen ASTVisitor specialization --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines swift::Lowering::ASTVisitor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_ASTVISITOR_H
#define SWIFT_LOWERING_ASTVISITOR_H

#include "swift/AST/ASTVisitor.h"

namespace swift {
namespace Lowering {
  
/// Lowering::ASTVisitor - This is a specialization of
/// swift::ASTVisitor which works only on resolved nodes and
/// which automatically ignores certain AST node kinds.
template<typename ImplClass,
         typename ExprRetTy = void,
         typename StmtRetTy = void,
         typename DeclRetTy = void,
         typename PatternRetTy = void,
         typename... Args>
class ASTVisitor : public swift::ASTVisitor<ImplClass,
                                            ExprRetTy,
                                            StmtRetTy,
                                            DeclRetTy,
                                            PatternRetTy,
                                            void,
                                            void,
                                            Args...>
{
public:
#define EXPR(Id, Parent)
#define UNCHECKED_EXPR(Id, Parent) \
  ExprRetTy visit##Id##Expr(Id##Expr *E, Args...AA) { \
    llvm_unreachable(#Id "Expr should not survive to SILGen"); \
  }
#include "swift/AST/ExprNodes.def"
  
  ExprRetTy visitErrorExpr(ErrorExpr *E, Args...AA) {
    llvm_unreachable("expression kind should not survive to SILGen");
  }

  ExprRetTy visitCodeCompletionExpr(CodeCompletionExpr *E, Args...AA) {
    llvm_unreachable("expression kind should not survive to SILGen");
  }

  ExprRetTy visitVarargExpansionExpr(VarargExpansionExpr *E, Args... AA) {
    llvm_unreachable("vararg expansion should not appear in this position");
  }

  ExprRetTy visitFixedTypeExpr(FixedTypeExpr *E, Args...AA) {
    llvm_unreachable("expression kind should not survive to SILGen");
  }

  ExprRetTy visitIdentityExpr(IdentityExpr *E, Args...AA) {
    return static_cast<ImplClass*>(this)->visit(E->getSubExpr(),
                                                std::forward<Args>(AA)...);
  }

  ExprRetTy visitTryExpr(TryExpr *E, Args...AA) {
    return static_cast<ImplClass*>(this)->visit(E->getSubExpr(),
                                                std::forward<Args>(AA)...);
  }

  ExprRetTy visitLazyInitializerExpr(LazyInitializerExpr *E, Args...AA) {
    return static_cast<ImplClass*>(this)->visit(E->getSubExpr(),
                                                std::forward<Args>(AA)...);
  }
};

template <typename ImplClass,
          typename ExprRetTy = void,
          typename... Args>
using ExprVisitor = ASTVisitor<ImplClass, ExprRetTy, void, void, void, Args...>;

} // end namespace Lowering
} // end namespace swift

#endif

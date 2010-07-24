//===--- SemaExpr.cpp - Swift Expression Semantic Analysis ----------------===//
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
//  This file implements semantic analysis for Swift expressions.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaExpr.h"
#include "swift/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;

Expr *SemaExpr::ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc) {
  return new (S.Context) IntegerLiteral(Text, Loc, S.Context.IntType);
}

Expr *SemaExpr::ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  VarDecl *D = S.decl.LookupName(S.Context.getIdentifier(Text));
  if (D == 0) {
    Error(Loc, "use of undeclared identifier");
    // FIXME: Return error object.
    return new (S.Context) IntegerLiteral(Text, Loc, S.Context.IntType);
  }
  
  return new (S.Context) DeclRefExpr(D, Loc, D->Ty);
}

Expr *SemaExpr::ActOnBraceExpr(llvm::SMLoc LBLoc,
                           const llvm::PointerUnion<Expr*, VarDecl*> *Elements,
                               unsigned NumElements, bool HasMissingSemi,
                               llvm::SMLoc RBLoc) {
  // Diagnose cases where there was a ; missing after a 'var'.
  if (HasMissingSemi && Elements[NumElements-1].is<VarDecl*>()) {
    Error(RBLoc, "expected ';' after var declaration");
    HasMissingSemi = false;
  }
      
  Type *ResultTy;
  if (HasMissingSemi)
    ResultTy = Elements[NumElements-1].get<Expr*>()->Ty;
  else
    ResultTy = S.Context.VoidType;
  
  llvm::PointerUnion<Expr*, VarDecl*> *NewElements = 
    (llvm::PointerUnion<Expr*, VarDecl*> *)
    S.Context.Allocate(sizeof(*Elements)*NumElements, 8);
  memcpy(NewElements, Elements, sizeof(*Elements)*NumElements);
  
  // FIXME: Create the right node.
  return new (S.Context) BraceExpr(LBLoc, NewElements, NumElements,
                                   HasMissingSemi, RBLoc, ResultTy);
}

Expr *SemaExpr::ActOnParenExpr(llvm::SMLoc LPLoc, Expr *SubExpr,
                               llvm::SMLoc RPLoc) {
  // FIXME: This should be a more general tuple expression.
  return new (S.Context) ParenExpr(LPLoc, SubExpr, RPLoc, SubExpr->Ty);
}

Expr *SemaExpr::ActOnBinaryExpr(unsigned Kind, Expr *LHS, llvm::SMLoc OpLoc,
                                Expr *RHS) {
  return new (S.Context) BinaryExpr((ExprKind)Kind, LHS, OpLoc, RHS,
                                    S.Context.IntType);
}

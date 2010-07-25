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
#include "swift/AST/Type.h"
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;
using llvm::NullablePtr;

NullablePtr<Expr> SemaExpr::ActOnNumericConstant(llvm::StringRef Text,
                                                 llvm::SMLoc Loc) {
  return new (S.Context) IntegerLiteral(Text, Loc, S.Context.IntType);
}

NullablePtr<Expr> 
SemaExpr::ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  NamedDecl *D = S.decl.LookupName(S.Context.getIdentifier(Text));
  if (D == 0) {
    Error(Loc, "use of undeclared identifier");
    return 0;
  }
  
  // FIXME: If the decl had an "invalid" type, then return the error object to
  // improve error recovery.
  return new (S.Context) DeclRefExpr(D, Loc, D->Ty);
}

NullablePtr<Expr> 
SemaExpr::ActOnBraceExpr(llvm::SMLoc LBLoc,
                         const llvm::PointerUnion<Expr*, NamedDecl*> *Elements,
                         unsigned NumElements, bool HasMissingSemi,
                         llvm::SMLoc RBLoc) {
  // Diagnose cases where there was a ; missing after a 'var'.
  if (HasMissingSemi && Elements[NumElements-1].is<NamedDecl*>()) {
    Error(RBLoc, "expected ';' after var declaration");
    HasMissingSemi = false;
  }
  
  // TODO: If any of the elements has a function type (which didn't get called),
  // then we want to produce a semantic error.
  
  Type *ResultTy;
  if (HasMissingSemi)
    ResultTy = Elements[NumElements-1].get<Expr*>()->Ty;
  else
    ResultTy = S.Context.VoidType;
  
  llvm::PointerUnion<Expr*, NamedDecl*> *NewElements = 
    (llvm::PointerUnion<Expr*, NamedDecl*> *)
    S.Context.Allocate(sizeof(*Elements)*NumElements, 8);
  memcpy(NewElements, Elements, sizeof(*Elements)*NumElements);
  
  // FIXME: Create the right node.
  return new (S.Context) BraceExpr(LBLoc, NewElements, NumElements,
                                   HasMissingSemi, RBLoc, ResultTy);
}

NullablePtr<Expr> 
SemaExpr::ActOnTupleExpr(llvm::SMLoc LPLoc, Expr **SubExprs,
                         unsigned NumSubExprs, llvm::SMLoc RPLoc) {
  
  // A tuple expr with a single subexpression is just a grouping paren.
  Type *ResultTy;
  if (NumSubExprs == 1) {
    ResultTy = SubExprs[0]->Ty;
  } else {
    // Compute the result type.
    llvm::SmallVector<llvm::PointerUnion<Type*, NamedDecl*>, 8> ResultTyElts;
    
    for (unsigned i = 0, e = NumSubExprs; i != e; ++i)
      ResultTyElts.push_back(SubExprs[i]->Ty);
    
    ResultTy = S.Context.getTupleType(ResultTyElts.data(), NumSubExprs);
  }
  
  Expr **NewSubExprs = (Expr**)S.Context.Allocate(sizeof(Expr*)*NumSubExprs, 8);
  memcpy(NewSubExprs, SubExprs, sizeof(Expr*)*NumSubExprs);
  return new (S.Context) TupleExpr(LPLoc, NewSubExprs, NumSubExprs,
                                   RPLoc, ResultTy);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnSequence(Expr **Exprs, unsigned NumExprs) {
  assert(NumExprs != 0 && "Empty sequence isn't possible");
  
  // Loop over all of the expressions, splitting instances of function
  // application out into ApplyExpr nodes.

  
  
  // FIXME: Generating a BraceExpr node is a hack here, add a new Expr node.
  llvm::PointerUnion<Expr*, NamedDecl*> *NewElements = 
    (llvm::PointerUnion<Expr*, NamedDecl*> *)
      S.Context.Allocate(sizeof(*NewElements)*NumExprs, 8);
  for (unsigned i = 0; i != NumExprs; ++i)
    NewElements[i] = Exprs[i];
  
  Type *ResultTy = Exprs[NumExprs-1]->Ty;
  
  return new (S.Context) BraceExpr(llvm::SMLoc(), NewElements, NumExprs, true,
                                   llvm::SMLoc(), ResultTy);
}


NullablePtr<Expr> 
SemaExpr::ActOnBinaryExpr(unsigned Kind, Expr *LHS, llvm::SMLoc OpLoc,
                          Expr *RHS) {
  
  // For now, the LHS and RHS of all binops have to be ints.
  if (LHS->Ty != S.Context.IntType) {
    // TODO, Improve error message, include source range.
    Error(OpLoc, "LHS subexpression doesn't have int type, it has XXX type");
    return 0;
  }

  if (RHS->Ty != S.Context.IntType) {
    // TODO, Improve error message, include source range.
    Error(OpLoc, "RHS subexpression doesn't have int type, it has XXX type");
    return 0;
  }

  return new (S.Context) BinaryExpr((ExprKind)Kind, LHS, OpLoc, RHS,
                                    S.Context.IntType);
}

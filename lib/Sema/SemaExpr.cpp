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
#include "swift/AST/ExprVisitor.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;
using llvm::NullablePtr;
using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

//===----------------------------------------------------------------------===//
// Action Implementations
//===----------------------------------------------------------------------===//

NullablePtr<Expr> SemaExpr::ActOnNumericConstant(llvm::StringRef Text,
                                                 llvm::SMLoc Loc) {
  // The integer literal must fit in 64-bits.
  unsigned long long Val;
  if (Text.getAsInteger(0, Val)) {
    error(Loc, "invalid immediate for integer literal, value too large");
    Text = "1";
  }

  // The type of an integer literal is always "integer_literal_type", which
  // should be defined by the library.
  Identifier TyName = S.Context.getIdentifier("integer_literal_type");
  Type Ty = S.decl.LookupTypeName(TyName, Loc)->getAliasType(S.Context);
  return new (S.Context) IntegerLiteral(Text, Loc, Ty);
}


NullablePtr<Expr> 
SemaExpr::ActOnIdentifierExpr(Identifier Text, llvm::SMLoc Loc) {
  ValueDecl *D = S.decl.LookupValueName(Text);
  
  if (D == 0)
    return new (S.Context) UnresolvedDeclRefExpr(Text, Loc);
  
  return new (S.Context) DeclRefExpr(D, Loc);
}

llvm::NullablePtr<Expr> SemaExpr::
ActOnScopedIdentifierExpr(Identifier ScopeName, llvm::SMLoc ScopeLoc,
                          llvm::SMLoc ColonColonLoc,
                          Identifier Name, llvm::SMLoc NameLoc) {
  // Note: this is very simplistic support for scoped name lookup, extend when
  // needed.
  TypeAliasDecl *TypeScopeDecl = S.decl.LookupTypeName(ScopeName, ScopeLoc);
  
  return new (S.Context) UnresolvedScopedIdentifierExpr(TypeScopeDecl, ScopeLoc,
                                                        ColonColonLoc, NameLoc,
                                                        Name);
}

NullablePtr<Expr> 
SemaExpr::ActOnTupleExpr(llvm::SMLoc LPLoc, Expr *const *SubExprs,
                         const Identifier *SubExprNames,
                         unsigned NumSubExprs, llvm::SMLoc RPLoc,
                         bool IsPrecededByIdentifier) {
  
  Expr **NewSubExprs =
    S.Context.AllocateCopy<Expr*>(SubExprs, SubExprs+NumSubExprs);

  Identifier *NewSubExprsNames = 0;
  if (SubExprNames)
    NewSubExprsNames =
      S.Context.AllocateCopy<Identifier>(SubExprNames,SubExprNames+NumSubExprs);

  bool IsGrouping = false;
  if (NumSubExprs == 1 &&
      (SubExprNames == 0 || SubExprNames[0].empty()))
    IsGrouping = true;
  
  return new (S.Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames,
                                   NumSubExprs, RPLoc, IsGrouping,
                                   IsPrecededByIdentifier);
}

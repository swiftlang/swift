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
#include "swift/AST/Type.h"
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
  Type *Ty = S.decl.LookupTypeName(TyName, Loc)->getAliasType(S.Context);
  return new (S.Context) IntegerLiteral(Text, Loc, Ty);
}

NullablePtr<Expr> 
SemaExpr::ActOnDollarIdentExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  assert(Text.size() >= 2 && Text[0] == '$' && 
         Text[1] >= '0' && Text[1] <= '9' && "Not a valid anon decl");
  unsigned ArgNo = 0;
  if (Text.substr(1).getAsInteger(10, ArgNo)) {
    error(Loc, "invalid name in $ expression");
    return 0;
  }

  return new (S.Context) AnonClosureArgExpr(ArgNo, Loc);
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
  Type *TypeScope = TypeScopeDecl->UnderlyingTy->getCanonicalType(S.Context);

  // FIXME: Handle UnresolvedType.
  
  // Look through type aliases etc.
  OneOfType *DT = dyn_cast<OneOfType>(TypeScope);
  
  // Reject things like int::x.
  if (DT == 0) {
    error(ScopeLoc, "invalid type '" + ScopeName.str() + "' for scoped access");
    return 0;
  }
  
  if (DT->Elements.empty()) {
    error(ScopeLoc, "oneof '" + ScopeName.str() +
          "' is not complete or has no elements");
    return 0;
  }
  
  OneOfElementDecl *Elt = DT->getElement(Name);
  if (Elt == 0) {
    error(ScopeLoc, "'" + Name.str() + "' is not a member of '" +
          ScopeName.str() + "'");
    return 0;
  }

  return new (S.Context) DeclRefExpr(Elt, ScopeLoc);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnUnresolvedMemberExpr(llvm::SMLoc ColonLoc, llvm::SMLoc NameLoc,
                                    Identifier Name) {
  // Handle :foo by just making an AST node.
  return new (S.Context) UnresolvedMemberExpr(ColonLoc, NameLoc, Name);
}


BraceExpr *SemaExpr::ActOnBraceExpr(llvm::SMLoc LBLoc,
                                    llvm::ArrayRef<ExprOrDecl> Elements,
                                    bool HasMissingSemi, llvm::SMLoc RBLoc) {
  ExprOrDecl *NewElements = 
    S.Context.AllocateCopy<ExprOrDecl>(Elements.begin(), Elements.end());
  
  return new (S.Context) BraceExpr(LBLoc, NewElements, Elements.size(),
                                   HasMissingSemi, RBLoc);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnDotIdentifier(Expr *E, llvm::SMLoc DotLoc,
                             Identifier Name, llvm::SMLoc NameLoc) {
  return new (S.Context) UnresolvedDotExpr(E, DotLoc, Name, NameLoc);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnArraySubscript(Expr *Base, llvm::SMLoc LLoc, Expr *Idx,
                              llvm::SMLoc RLoc) {
  // FIXME: Implement.  This should lookup "subscript(Base, Idx)" as a function
  // call.
  return Base;
}


NullablePtr<Expr> 
SemaExpr::ActOnTupleExpr(llvm::SMLoc LPLoc, Expr *const *SubExprs,
                         const Identifier *SubExprNames,
                         unsigned NumSubExprs, llvm::SMLoc RPLoc) {
  
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
                                   NumSubExprs, RPLoc, IsGrouping);
}


llvm::NullablePtr<Expr>
SemaExpr::ActOnSequence(llvm::ArrayRef<Expr *> Elements) {
  assert(!Elements.empty() && "Empty sequence isn't possible");
  
  Expr **NewElements =
    S.Context.AllocateCopy<Expr*>(Elements.begin(), Elements.end());
  
  return new (S.Context) SequenceExpr(NewElements, Elements.size());
}

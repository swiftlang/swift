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
  
  return new (S.Context) IntegerLiteral(Text, Loc);
}

NullablePtr<Expr> 
SemaExpr::ActOnDollarIdentExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  ValueDecl *D = S.decl.GetAnonDecl(Text, Loc);
  if (D == 0) return 0;
  
  return new (S.Context) DeclRefExpr(D, Loc);
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
  Type *TypeScope = S.Context.getCanonicalType(TypeScopeDecl->UnderlyingTy);

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


NullablePtr<Expr> 
SemaExpr::ActOnBraceExpr(llvm::SMLoc LBLoc,
                     llvm::ArrayRef<llvm::PointerUnion<Expr*, Decl*> > Elements,
                         bool HasMissingSemi, llvm::SMLoc RBLoc) {
  llvm::PointerUnion<Expr*, Decl*> *NewElements = 
    S.Context.AllocateCopy<llvm::PointerUnion<Expr*, Decl*> >(Elements.begin(),
                                                              Elements.end());
  
  return new (S.Context) BraceExpr(LBLoc, NewElements, Elements.size(),
                                   HasMissingSemi, RBLoc);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnDotIdentifier(Expr *E, llvm::SMLoc DotLoc,
                             llvm::StringRef NameStr,
                             llvm::SMLoc NameLoc) {
  Identifier Name = S.Context.getIdentifier(NameStr);
  
#if 0
  Type *ResultTy = 0;
  int FieldNo = -1;
  if (SemaDotIdentifier(E, DotLoc, Name, NameLoc, ResultTy, FieldNo, *this))
    return 0;
  
  // If the field number is -1, the the base expression is dependent.
  if (FieldNo == -1)
    ;
#endif
  return new (S.Context) UnresolvedDotExpr(E, DotLoc, Name, NameLoc);
  
#if 0
  // TODO: Eventually . can be useful for things other than tuples.
  return new (S.Context) TupleElementExpr(E, DotLoc, FieldNo, NameLoc,ResultTy);
#endif
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

  return new (S.Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames,
                                   NumSubExprs, RPLoc);
}


llvm::NullablePtr<Expr>
SemaExpr::ActOnSequence(llvm::ArrayRef<Expr *> Elements) {
  assert(!Elements.empty() && "Empty sequence isn't possible");
  
  Expr **NewElements =
    S.Context.AllocateCopy<Expr*>(Elements.begin(), Elements.end());
  
  return new (S.Context) SequenceExpr(NewElements, Elements.size());
}

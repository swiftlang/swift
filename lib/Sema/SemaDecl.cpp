//===--- SemaDecl.cpp - Swift Semantic Analysis for Declarations ----------===//
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
//  This file implements semantic analysis for Swift declarations.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaDecl.h"
#include "swift/Sema/Sema.h"
#include "swift/Sema/Scope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;

SemaDecl::SemaDecl(Sema &S)
  : SemaBase(S),
    ScopeHT(new ScopeHTType()),
    CurScope(0) {
}

SemaDecl::~SemaDecl() {
  delete ScopeHT;
}

//===----------------------------------------------------------------------===//
// Name lookup.
//===----------------------------------------------------------------------===//

/// AddToScope - Register the specified decl as being in the current lexical
/// scope.
void SemaDecl::AddToScope(NamedDecl *D) {
  // If we have a shadowed variable definition, check to see if we have a
  // redefinition: two definitions in the same scope with the same name.
  std::pair<unsigned, NamedDecl*> Entry = ScopeHT->lookup(D->Name);
  if (Entry.second && Entry.first == CurScope->getDepth()) {
    Error(D->getLocStart(),
          "variable declaration conflicts with previous declaration");
    Note(ScopeHT->lookup(D->Name).second->getLocStart(),
         "previous declaration here");
    return;
  }
  
  ScopeHT->insert(D->Name, std::make_pair(CurScope->getDepth(), D));
}

/// LookupName - Perform a lexical scope lookup for the specified name,
/// returning the active decl if found or null if not.
NamedDecl *SemaDecl::LookupName(Identifier Name) {
  return ScopeHT->lookup(Name).second;
}


//===----------------------------------------------------------------------===//
// Declaration handling.
//===----------------------------------------------------------------------===//

/// ValidateAttributes - Check that the func/var declaration attributes are ok.
static void ValidateAttributes(DeclAttributes &Attrs, Type *Ty, SemaDecl &SD) {
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = llvm::dyn_cast<FunctionType>(Ty))
      if (TupleType *TT = llvm::dyn_cast<TupleType>(FT->Input))
        IsError = TT->NumFields != 2;
    if (IsError) {
      SD.Error(Attrs.LSquareLoc, "function with 'infix' specified must take "
               "a two element tuple as input");
      Attrs.InfixPrecedence = -1;
    }
  }
}

VarDecl *SemaDecl::ActOnVarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name,
                                Type *Ty, Expr *Init, DeclAttributes &Attrs) {
  
  // Diagnose when we don't have a type or an expression.
  if (Ty == 0 && Init == 0) {
    Error(VarLoc, "var declaration must specify a type if no "
          "initializer is specified");
    // TODO: Recover better by still creating var, but making it have 'invalid'
    // type.
    return 0;
  }
  
  // If both a type and an initializer are specified, make sure the
  // initializer's type agrees with the (redundant) type.
  if (Ty && Init && Init->Ty != Ty) {
    // FIXME: Improve this to include the actual types!
    Error(VarLoc, "variable initializer's type ('TODO') does not match "
          "specified type 'TODO'");
    Ty = Init->Ty;
    return 0;
  }
  
  if (Ty == 0)
    Ty = Init->Ty;
  
  // Validate attributes.
  ValidateAttributes(Attrs, Ty, *this);
  
  return new (S.Context) VarDecl(VarLoc, S.Context.getIdentifier(Name),
                                 Ty, Init, Attrs);
}

FuncDecl *SemaDecl::
ActOnFuncDecl(llvm::SMLoc FuncLoc, llvm::StringRef Name,
              Type *Ty, Expr *Body, DeclAttributes &Attrs) {
  assert(Ty && "Type not specified?");

  // FIXME: Validate that the body's type matches the function's type.
  
  // Validate attributes.
  ValidateAttributes(Attrs, Ty, *this);

  return new (S.Context) FuncDecl(FuncLoc, S.Context.getIdentifier(Name),
                                  Ty, Body, Attrs);
}


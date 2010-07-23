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
#include "llvm/Support/SMLoc.h"
using namespace swift;

SemaDecl::SemaDecl(Sema &S)
  : SemaBase(S),
    ScopeHT(new ScopeHTType()) {
}

SemaDecl::~SemaDecl() {
  delete ScopeHT;
}

//===----------------------------------------------------------------------===//
// Name lookup.
//===----------------------------------------------------------------------===//

/// AddToScope - Register the specified decl as being in the current lexical
/// scope.
void SemaDecl::AddToScope(VarDecl *D) {
  // FIXME: For now, don't allow shadowing at all.  This is because we want to
  // reject redeclarations at the same scope (var x : int;  var x : int;) but
  // ScopedHashTable doesn't give us a way to find out if an entry is in the
  // current scope or not!
  if (ScopeHT->count(D->Name)) {
    Error(D->VarLoc,
          "variable declaration conflicts with previous declaration");
    Note(ScopeHT->lookup(D->Name)->VarLoc,
         "previous declaration here");
    return;
  }
  
  ScopeHT->insert(D->Name, D);
}

/// LookupName - Perform a lexical scope lookup for the specified name,
/// returning the active decl if found or null if not.
VarDecl *SemaDecl::LookupName(Identifier Name) {
  return ScopeHT->lookup(Name);
}


//===----------------------------------------------------------------------===//
// Declaration handling.
//===----------------------------------------------------------------------===//


VarDecl *SemaDecl::ActOnVarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name,
                                Type *Ty, Expr *Init) {
  
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
  
  return new (S.Context) VarDecl(VarLoc, S.Context.getIdentifier(Name),
                                 Ty, Init);
}

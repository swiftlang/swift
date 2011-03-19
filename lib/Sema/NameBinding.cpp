//===--- NameBinding.cpp - Name Binding -----------------------------------===//
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
//  This file implements name binding for Swift.
//
//===----------------------------------------------------------------------===//

// FIXME: Entrypoint declared in Parser.h
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;
using llvm::isa;
using llvm::dyn_cast;
using llvm::cast;
using llvm::SMLoc;

namespace {
  class NameBinder {
  public:
    ASTContext &Context;
    NameBinder(ASTContext &C) : Context(C) {}
    
    /// TopLevelValues - This is the list of top-level declarations we have.
    llvm::DenseMap<Identifier, ValueDecl *> TopLevelValues;
    
    void addNamedTopLevelDecl(ValueDecl *VD) {
      TopLevelValues[VD->Name] = VD;
    }
    
    Expr *bindValueName(Identifier I, SMLoc Loc);
    
    
    void note(SMLoc Loc, const llvm::Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "note");
    }
    void warning(SMLoc Loc, const llvm::Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "warning");
    }
    void error(SMLoc Loc, const llvm::Twine &Message) {
      Context.setHadError();
      Context.SourceMgr.PrintMessage(Loc, Message, "error");
    }
  };
}

/// bindValueName - This is invoked for each UnresolvedDeclRefExpr in the AST.
Expr *NameBinder::bindValueName(Identifier Name, SMLoc Loc) {
  // Right now we don't have import or anything else, so we just handle forward
  // references.
  llvm::DenseMap<Identifier, ValueDecl *>::iterator I =
    TopLevelValues.find(Name);
  if (I == TopLevelValues.end()) {
    error(Loc, "use of unresolved identifier '" + Name.str() + "'");
    return 0;
  }
  
  // If we found a resolved decl, replace the unresolved ref with a resolved
  // DeclRefExpr.
  return new (Context) DeclRefExpr(I->second, Loc);
}


static Expr *BindNames(Expr *E, Expr::WalkOrder Order, void *binder) {
  NameBinder &Binder = *static_cast<NameBinder*>(binder);
  
  // Ignore the preorder walk.
  if (Order == Expr::Walk_PreOrder)
    return E;
  
  // Ignore everything except UnresolvedDeclRefExpr.
  UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E);
  if (UDRE == 0) return E;
  
  return Binder.bindValueName(UDRE->Name, UDRE->Loc);  
}

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this parsing has been performed, but we still have UnresolvedDeclRefExpr
/// nodes for unresolved value names, and we may have unresolved type names as
/// well.  This handles import directives and forward references.
void swift::performNameBinding(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  NameBinder Binder(Ctx);
  
  // Do a prepass over the declarations to find the list of top-level value
  // declarations.
  for (llvm::ArrayRef<Decl*>::iterator I = TUD->Decls.begin(),
       E = TUD->Decls.end(); I != E; ++I) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(*I))
      if (!VD->Name.empty())
        Binder.addNamedTopLevelDecl(VD);
  }
  
  // Now that we know the top-level value names, go through and resolve any
  // UnresolvedDeclRefExprs that exist.
  for (llvm::ArrayRef<Decl*>::iterator I = TUD->Decls.begin(),
       E = TUD->Decls.end(); I != E; ++I) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(*I))
      if (VD->Init)
        VD->Init = VD->Init->WalkExpr(BindNames, &Binder);
  }
}

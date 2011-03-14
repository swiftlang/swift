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
//  This file implements name binding for Swift types.
//
//===----------------------------------------------------------------------===//

// FIXME: Entrypoint declared in Parser.h
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
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


/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
void swift::performNameBinding(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  NameBinder Binder(Ctx);
  // At this point name binding has been performed and we have to do full type
  // checking and anonymous name binding resolution.
  for (llvm::ArrayRef<Decl*>::iterator I = TUD->Decls.begin(),
       E = TUD->Decls.end(); I != E; ++I) {
    Decl *D = *I;
    
    // If any top-level value decl has an unresolved type, then it is erroneous.
    // It is not valid to have something like "var x = 4" at the top level, all
    // types must be explicit here.
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
      if (isa<DependentType>(VD->Ty))
        Binder.error(VD->getLocStart(),
                     "top level declarations require a type specifier");
      continue;
    }
        
    // Otherwise, ignore top level typealiases and elementrefs.
    if (isa<TypeAliasDecl>(D) || isa<ElementRefDecl>(D))
      continue;
    
    if (VarDecl *VD = dyn_cast<VarDecl>(D)) {
      (void)VD;
      continue;
    }
    
    (void)cast<FuncDecl>(D);
  }
}

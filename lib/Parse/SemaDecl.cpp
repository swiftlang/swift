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

#include "SemaDecl.h"
#include "Sema.h"
#include "Parser.h"
#include "Scope.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;


/// handleEndOfTranslationUnit - This is invoked at the end of the translation
/// unit.
void SemaDecl::handleEndOfTranslationUnit(TranslationUnitDecl *TUD,
                                          SMLoc FileStart,
                                          ArrayRef<ExprStmtOrDecl> Items,
                                          SMLoc FileEnd, Parser &P) {
  // First thing, we transform the body into a brace expression.
  ExprStmtOrDecl *NewElements = 
    S.Context.AllocateCopy<ExprStmtOrDecl>(Items.begin(), Items.end());
  TUD->Body = new (S.Context) BraceStmt(FileStart, NewElements, Items.size(),
                                        FileEnd);
  
  // Do a prepass over the declarations to make sure they have basic sanity and
  // to find the list of top-level value declarations.
  for (unsigned i = 0, e = TUD->Body->NumElements; i != e; ++i) {
    if (!TUD->Body->Elements[i].is<Decl*>()) continue;
    
    Decl *D = TUD->Body->Elements[i].get<Decl*>();
       
    // If any top-level value decl has an unresolved type, then it is erroneous.
    // It is not valid to have something like "var x = 4" at the top level, all
    // types must be explicit here.
    ValueDecl *VD = dyn_cast<ValueDecl>(D);
    if (VD == 0) continue;

    // FIXME: This can be better handled in the various ActOnDecl methods when
    // they get passed in a parent context decl.

    // Verify that values have a type specified.
    if (false && VD->Ty->is<DependentType>()) {
      error(VD->getLocStart(),
            "top level declarations require a type specifier");
      // FIXME: Should mark the decl as invalid.
      VD->Ty = TupleType::getEmpty(S.Context);
    }
  }
  
  // Verify that any forward declared types were ultimately defined.
  // TODO: Move this to name binding!
  SmallVector<TypeAliasDecl*, 8> UnresolvedTypeList;
  for (TypeAliasDecl *Decl : P.ScopeInfo.getUnresolvedTypeList()) {
    if (Decl->UnderlyingTy.isNull())
      UnresolvedTypeList.push_back(Decl);
  }
    
  TUD->UnresolvedTypesForParser = S.Context.AllocateCopy(UnresolvedTypeList);
}


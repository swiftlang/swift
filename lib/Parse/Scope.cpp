//===--- Scope.cpp - Scope Implementation ---------------------------------===//
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

#include "swift/AST/Attr.h"
#include "Scope.h"
#include "Parser.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Scope Implementation
//===----------------------------------------------------------------------===//

Scope::Scope(Parser *P) : SI(P->ScopeInfo), ValueHTScope(SI.ValueScopeHT),
                          PrevScope(SI.CurScope) {
  if (SI.CurScope)
    Depth = SI.CurScope->Depth+1;
  else
    Depth = 0;
  SI.CurScope = this;
}

//===----------------------------------------------------------------------===//
// ScopeInfo Implementation
//===----------------------------------------------------------------------===//

static void diagnoseRedefinition(ValueDecl *Prev, ValueDecl *New, Parser &P) {
  assert(New != Prev && "Cannot conflict with self");
  P.diagnose(New->getLocStart(), diag::decl_redefinition, New->isDefinition());
  P.diagnose(Prev->getLocStart(), diag::previous_decldef, Prev->isDefinition(),
             Prev->getName());
}

/// checkValidOverload - Check whether it is ok for D1 and D2 to be declared at
/// the same scope.  This check is a transitive relationship, so if "D1 is a
/// valid overload of D2" and "D2 is a valid overload of D3" then we know that
/// D1/D3 are valid overloads and we don't have to check all permutations.
static bool checkValidOverload(const ValueDecl *D1, const ValueDecl *D2,
                               Parser &P) {
  if (D1->getAttrs().isInfix() && D2->getAttrs().isInfix() &&
      D1->getAttrs().getInfixData() != D2->getAttrs().getInfixData()) {
    P.diagnose(D1->getLocStart(), diag::precedence_overload);
    // FIXME: Pass identifier through, when the diagnostics system can handle
    // it.
    P.diagnose(D2->getLocStart(), diag::previous_declaration, D2->getName());
    return true;
  }
  
  // Otherwise, everything is fine.
  return false;
}


/// addToScope - Register the specified decl as being in the current lexical
/// scope.
void ScopeInfo::addToScope(ValueDecl *D) {
  // If we have a shadowed variable definition, check to see if we have a
  // redefinition: two definitions in the same scope with the same name.
  ValueScopeHTTy::iterator EntryI = ValueScopeHT.begin(D->getName());
  
  // A redefinition is a hit in the scoped table at the same depth.
  if (EntryI != ValueScopeHT.end() && EntryI->first == CurScope->getDepth()) {
    ValueDecl *PrevDecl = EntryI->second;
    
    // If this is at top-level scope, we allow overloading.  If not, we don't.
    // FIXME: This should be tied to whether the scope corresponds to a
    // DeclContext like a TranslationUnit or a Namespace.  Add a bit to Scope
    // to track this?
    if (CurScope->getDepth() != 0)
      return diagnoseRedefinition(PrevDecl, D, TheParser);
    
    // If this is at top-level scope, validate that the members of the overload
    // set all agree.
    
    // Check to see if D and PrevDecl are valid in the same overload set.
    if (checkValidOverload(D, PrevDecl, TheParser))
      return;
    
    // Note: we don't check whether all of the elements of the overload set have
    // different argument types.  This is checked later.
  }
  
  ValueScopeHT.insert(D->getName(), std::make_pair(CurScope->getDepth(), D));
}

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

#include "swift/Parse/Scope.h"
#include "swift/AST/Attr.h"
#include "swift/Parse/Parser.h"
#include "llvm/ADT/Twine.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// Scope Implementation
//===----------------------------------------------------------------------===//

static bool isResolvableScope(ScopeKind SK) {
  switch (SK) {
  case ScopeKind::Extension:
  case ScopeKind::EnumBody:
  case ScopeKind::StructBody:
  case ScopeKind::ClassBody:
  case ScopeKind::ProtocolBody:
  case ScopeKind::TopLevel:
    return false;
  case ScopeKind::FunctionBody:
  case ScopeKind::Generics:
  case ScopeKind::ConstructorBody:
  case ScopeKind::DestructorBody:
  case ScopeKind::Brace:
  case ScopeKind::ActiveConfigBlock:
  case ScopeKind::ForVars:
  case ScopeKind::ForeachVars:
  case ScopeKind::ClosureParams:
  case ScopeKind::CaseVars:
  case ScopeKind::IfVars:
  case ScopeKind::WhileVars:
    return true;
  }
}

Scope::Scope(Parser *P, ScopeKind SC, bool InactiveConfigBlock)
  : SI(P->getScopeInfo()),
    HTScope(SI.HT, SI.CurScope ? &SI.CurScope->HTScope : 0),
    PrevScope(SI.CurScope),
    PrevResolvableDepth(SI.ResolvableDepth),
    Kind(SC), IsInactiveConfigBlock(InactiveConfigBlock) {
  assert(PrevScope || Kind == ScopeKind::TopLevel);
  
  if (SI.CurScope) {
    Depth = SI.CurScope->Depth + 1;
    IsInactiveConfigBlock |= SI.CurScope->IsInactiveConfigBlock;
  } else {
    Depth = 0;
  }
  SI.CurScope = this;
  if (!isResolvableScope(Kind))
    SI.ResolvableDepth = Depth + 1;
}

Scope::Scope(Parser *P, SavedScope &&SS):
    SI(P->getScopeInfo()),
    HTScope(std::move(SS.HTDetachedScope)),
    PrevScope(SI.CurScope),
    PrevResolvableDepth(SI.ResolvableDepth),
    Depth(SI.CurScope ? SI.CurScope->Depth + 1 : 0),
    Kind(SS.Kind), IsInactiveConfigBlock(SS.IsInactiveConfigBlock) {
    
    SI.CurScope = this;
    if (!isResolvableScope(Kind))
      SI.ResolvableDepth = Depth + 1;
}

bool Scope::isResolvable() const {
  return isResolvableScope(Kind);
}

//===----------------------------------------------------------------------===//
// ScopeInfo Implementation
//===----------------------------------------------------------------------===//

/// checkValidOverload - Check whether it is ok for D1 and D2 to be declared at
/// the same scope.  This check is a transitive relationship, so if "D1 is a
/// valid overload of D2" and "D2 is a valid overload of D3" then we know that
/// D1/D3 are valid overloads and we don't have to check all permutations.
static bool checkValidOverload(const ValueDecl *D1, const ValueDecl *D2,
                               Parser &P) {
  // Currently, there is no restriction on overloading.
  return false;
}


/// addToScope - Register the specified decl as being in the current lexical
/// scope.
void ScopeInfo::addToScope(ValueDecl *D, Parser &TheParser) {
  if (!CurScope->isResolvable())
    return;

  assert(CurScope->getDepth() >= ResolvableDepth &&
         "inserting names into a non-resolvable scope");

  // If we have a shadowed variable definition, check to see if we have a
  // redefinition: two definitions in the same scope with the same name.
  ScopedHTTy::iterator EntryI = HT.begin(CurScope->HTScope, D->getName());

  // A redefinition is a hit in the scoped table at the same depth.
  if (EntryI != HT.end() && EntryI->first == CurScope->getDepth()) {
    ValueDecl *PrevDecl = EntryI->second;
    
    // If this is in a resolvable scope, diagnose redefinitions.  Later
    // phases will handle scopes like module-scope, etc.
    if (CurScope->getDepth() >= ResolvableDepth)
      return TheParser.diagnoseRedefinition(PrevDecl, D);
    
    // If this is at top-level scope, validate that the members of the overload
    // set all agree.
    
    // Check to see if D and PrevDecl are valid in the same overload set.
    if (checkValidOverload(D, PrevDecl, TheParser))
      return;
    
    // Note: we don't check whether all of the elements of the overload set have
    // different argument types.  This is checked later.
  }

  HT.insertIntoScope(CurScope->HTScope,
                     D->getName(),
                     std::make_pair(CurScope->getDepth(), D));
}

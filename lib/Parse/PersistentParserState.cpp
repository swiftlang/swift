//===--- PersistentParserState.cpp - Parser State Implementation ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements parser state persistent across multiple parses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Parse/PersistentParserState.h"

using namespace swift;

PersistentParserState::PersistentParserState() { }

PersistentParserState::~PersistentParserState() { }

void PersistentParserState::delayDecl(DelayedDeclKind Kind,
                                      unsigned Flags,
                                      DeclContext *ParentContext,
                                      SourceRange BodyRange,
                                      SourceLoc PreviousLoc) {
  assert(!CodeCompletionDelayedDeclState.get() &&
         "only one decl can be delayed for code completion");
  CodeCompletionDelayedDeclState.reset(new DelayedDeclState(
      Kind, Flags, ParentContext, BodyRange, PreviousLoc,
      ScopeInfo.saveCurrentScope()));
}

void PersistentParserState::delayDeclList(IterableDeclContext *D) {
  DelayedDeclLists.push_back(D);
}

void PersistentParserState::parseAllDelayedDeclLists() {
  for (auto IDC : DelayedDeclLists)
    IDC->loadAllMembers();
}

void PersistentParserState::delayTopLevel(TopLevelCodeDecl *TLCD,
                                          SourceRange BodyRange,
                                          SourceLoc PreviousLoc) {
  delayDecl(DelayedDeclKind::TopLevelCodeDecl, 0U, TLCD, BodyRange,
            PreviousLoc);
}

void PersistentParserState::forEachDelayedSourceRange(
    const SourceFile *primaryFile, function_ref<void(SourceRange)> fn) const {
  // FIXME: separate out unparsed ranges by primary file
  for (const auto &afdAndState : DelayedFunctionBodies) {
    const auto sr = afdAndState.getFirst()->getBodySourceRange();
    if (sr.isValid())
      fn(sr);
  }
  for (const auto *idc : DelayedDeclLists) {
    const auto *d = idc->getDecl();
    SourceRange sr;
    if (auto *nt = dyn_cast<NominalTypeDecl>(d))
      sr = nt->getBraces();
    else if (auto *e = dyn_cast<ExtensionDecl>(d))
      sr = e->getBraces();
    if (sr.isValid())
      fn(sr);
  }
  if (auto *dds = CodeCompletionDelayedDeclState.get()) {
    if (dds->BodyPos.Loc.isValid() && dds->BodyEnd.isValid())
      fn(SourceRange(dds->BodyPos.Loc, dds->BodyEnd));
  }
}

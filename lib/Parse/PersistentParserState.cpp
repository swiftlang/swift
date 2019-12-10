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

void PersistentParserState::setCodeCompletionDelayedDeclState(
    CodeCompletionDelayedDeclKind Kind, unsigned Flags,
    DeclContext *ParentContext, SourceRange BodyRange, SourceLoc PreviousLoc) {
  assert(!CodeCompletionDelayedDeclStat.get() &&
         "only one decl can be delayed for code completion");
  CodeCompletionDelayedDeclStat.reset(new CodeCompletionDelayedDeclState(
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

void PersistentParserState::forEachDelayedSourceRange(
    const SourceFile *primaryFile, function_ref<void(SourceRange)> fn) const {
  // FIXME: separate out unparsed ranges by primary file
  for (const auto *idc : DelayedDeclLists) {
    // FIXME: better to check for the exact request (ParseMembersRequest)
    // in the Evaluator cache
    if (!idc->hasUnparsedMembers())
      continue;
    const auto *d = idc->getDecl();
    SourceRange sr;
    if (auto *nt = dyn_cast<NominalTypeDecl>(d))
      sr = nt->getBraces();
    else if (auto *e = dyn_cast<ExtensionDecl>(d))
      sr = e->getBraces();
    if (sr.isValid())
      fn(sr);
  }
}

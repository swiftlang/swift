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

PersistentParserState::PersistentParserState(ASTContext &Ctx):
  Ctx(Ctx) { Ctx.addLazyParser(this); }

PersistentParserState::~PersistentParserState() { Ctx.removeLazyParser(this); }

void PersistentParserState::delayFunctionBodyParsing(AbstractFunctionDecl *AFD,
                                                     SourceRange BodyRange,
                                                     SourceLoc PreviousLoc) {
  std::unique_ptr<FunctionBodyState> State;
  State.reset(new FunctionBodyState(BodyRange, PreviousLoc,
                                    ScopeInfo.saveCurrentScope()));
  assert(DelayedFunctionBodies.find(AFD) == DelayedFunctionBodies.end() &&
         "Already recorded state for this body");
  DelayedFunctionBodies[AFD] = std::move(State);
}

std::unique_ptr<PersistentParserState::FunctionBodyState>
PersistentParserState::takeFunctionBodyState(AbstractFunctionDecl *AFD) {
  assert(AFD->getBodyKind() == AbstractFunctionDecl::BodyKind::Unparsed);
  DelayedFunctionBodiesTy::iterator I = DelayedFunctionBodies.find(AFD);
  assert(I != DelayedFunctionBodies.end() && "State should be saved");
  std::unique_ptr<FunctionBodyState> State = std::move(I->second);
  DelayedFunctionBodies.erase(I);
  return State;
}

bool PersistentParserState::hasFunctionBodyState(AbstractFunctionDecl *AFD) {
  return DelayedFunctionBodies.find(AFD) != DelayedFunctionBodies.end();
}

std::unique_ptr<PersistentParserState::DelayedDeclListState>
PersistentParserState::takeDelayedDeclListState(IterableDeclContext *IDC) {
  auto I = DelayedDeclListStates.find(IDC);
  assert(I != DelayedDeclListStates.end() && "State should be saved");
  auto State = std::move(I->second);
  DelayedDeclListStates.erase(I);
  return State;
}

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

void PersistentParserState::delayDeclList(IterableDeclContext* D,
                                          unsigned Flags,
                                          DeclContext *ParentContext,
                                          SourceRange BodyRange,
                                          SourceLoc PreviousLoc) {
  DelayedDeclListStates[D] = llvm::make_unique<DelayedDeclListState>(Flags,
    ParentContext, BodyRange, PreviousLoc, ScopeInfo.saveCurrentScope());
}

void PersistentParserState::delayTopLevel(TopLevelCodeDecl *TLCD,
                                          SourceRange BodyRange,
                                          SourceLoc PreviousLoc) {
  delayDecl(DelayedDeclKind::TopLevelCodeDecl, 0U, TLCD, BodyRange,
            PreviousLoc);
}

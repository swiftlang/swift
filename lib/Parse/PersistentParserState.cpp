//===--- PersistentParserState.cpp - Parser State Implementation ----------===//
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
//  This file implements parser state persistent across multiple parses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Parse/PersistentParserState.h"

using namespace swift;

void PersistentParserState::delayFunctionBodyParsing(FuncDecl *FD,
                                                     SourceRange BodyRange,
                                                     SourceLoc PreviousLoc) {
  std::unique_ptr<FunctionBodyState> State;
  State.reset(new FunctionBodyState(BodyRange, PreviousLoc,
                                    ScopeInfo.saveCurrentScope()));
  assert(DelayedBodies.find(FD) == DelayedBodies.end() &&
         "Already recorded state for this body");
  DelayedBodies[FD] = std::move(State);
}

std::unique_ptr<PersistentParserState::FunctionBodyState>
PersistentParserState::takeBodyState(FuncDecl *FD) {
  assert(FD->getBodyKind() == FuncDecl::BodyKind::Unparsed);
  DelayedBodiesTy::iterator I = DelayedBodies.find(FD);
  assert(I != DelayedBodies.end() && "State should be saved");
  std::unique_ptr<FunctionBodyState> State = std::move(I->second);
  DelayedBodies.erase(I);
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


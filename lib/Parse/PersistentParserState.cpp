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

#include "swift/Parse/PersistentParserState.h"

using namespace swift;

void PersistentParserState::delayFunctionBodyParsing(FuncExpr *FE,
                                                     SourceRange BodyRange,
                                                     SourceLoc PreviousLoc) {
  std::unique_ptr<FunctionBodyState> State;
  State.reset(new FunctionBodyState(BodyRange, PreviousLoc,
                                    ScopeInfo.saveCurrentScope()));
  assert(DelayedBodies.find(FE) == DelayedBodies.end() &&
         "Already recorded state for this body");
  DelayedBodies[FE] = std::move(State);
}

std::unique_ptr<PersistentParserState::FunctionBodyState>
PersistentParserState::takeBodyState(FuncExpr *FE) {
  std::unique_ptr<FunctionBodyState> State;
  DelayedBodiesTy::iterator I = DelayedBodies.find(FE);
  if (I != DelayedBodies.end()) {
    State = std::move(I->second);
    DelayedBodies.erase(I);
  }

  return State;
}

//===--- PersistentParserState.h - Parser State -----------------*- C++ -*-===//
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
// Parser state persistent across multiple parses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PERSISTENTPARSERSTATE_H
#define SWIFT_PARSE_PERSISTENTPARSERSTATE_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/LocalContext.h"
#include "swift/Parse/ParserPosition.h"
#include "swift/Parse/Scope.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
  class AbstractFunctionDecl;

/// \brief Parser state persistent across multiple parses.
class PersistentParserState {
public:
  struct ParserPos {
    SourceLoc Loc;
    SourceLoc PrevLoc;

    bool isValid() const { return Loc.isValid(); }
  };

  class FunctionBodyState {
    ParserPos BodyPos;
    SavedScope Scope;
    friend class Parser;

    SavedScope takeScope() {
      return std::move(Scope);
    }

  public:
    FunctionBodyState(SourceRange BodyRange, SourceLoc PreviousLoc,
                      SavedScope &&Scope)
      : BodyPos{BodyRange.Start, PreviousLoc}, Scope(std::move(Scope))
    {}
  };

  class AccessorBodyState {
    ParserPos BodyPos;
    SavedScope Scope;
    SourceLoc LBLoc;
    friend class Parser;

    SavedScope takeScope() {
      return std::move(Scope);
    }

  public:
    AccessorBodyState(SourceRange BodyRange, SourceLoc PreviousLoc,
                      SavedScope &&Scope, SourceLoc LBLoc)
        : BodyPos{BodyRange.Start, PreviousLoc}, Scope(std::move(Scope)),
          LBLoc(LBLoc) {}
  };

  enum class DelayedDeclKind {
    TopLevelCodeDecl,
    Decl,
  };

  class DelayedDeclState {
    friend class PersistentParserState;
    friend class Parser;
    DelayedDeclKind Kind;
    unsigned Flags;
    DeclContext *ParentContext;
    ParserPos BodyPos;
    SourceLoc BodyEnd;
    SavedScope Scope;

    SavedScope takeScope() {
      return std::move(Scope);
    }

  public:
    DelayedDeclState(DelayedDeclKind Kind, unsigned Flags,
                     DeclContext *ParentContext, SourceRange BodyRange,
                     SourceLoc PreviousLoc, SavedScope &&Scope)
      : Kind(Kind), Flags(Flags), ParentContext(ParentContext),
        BodyPos{BodyRange.Start, PreviousLoc},
        BodyEnd(BodyRange.End), Scope(std::move(Scope))
    {}
  };

  bool InPoundLineEnvironment = false;
  // FIXME: When condition evaluation moves to a later phase, remove this bit
  // and adjust the client call 'performParseOnly'.
  bool PerformConditionEvaluation = true;
private:
  ScopeInfo ScopeInfo;
  using DelayedFunctionBodiesTy =
      llvm::DenseMap<AbstractFunctionDecl *,
                     std::unique_ptr<FunctionBodyState>>;
  DelayedFunctionBodiesTy DelayedFunctionBodies;

  using DelayedAccessorBodiesTy =
      llvm::DenseMap<AbstractFunctionDecl *,
                     std::unique_ptr<AccessorBodyState>>;
  DelayedAccessorBodiesTy DelayedAccessorBodies;

  /// \brief Parser sets this if it stopped parsing before the buffer ended.
  ParserPosition MarkedPos;

  std::unique_ptr<DelayedDeclState> CodeCompletionDelayedDeclState;

  /// The local context for all top-level code.
  TopLevelContext TopLevelCode;

public:
  swift::ScopeInfo &getScopeInfo() { return ScopeInfo; }

  void delayFunctionBodyParsing(AbstractFunctionDecl *AFD,
                                SourceRange BodyRange,
                                SourceLoc PreviousLoc);
  std::unique_ptr<FunctionBodyState>
  takeFunctionBodyState(AbstractFunctionDecl *AFD);

  bool hasFunctionBodyState(AbstractFunctionDecl *AFD);

  void delayAccessorBodyParsing(AbstractFunctionDecl *AFD,
                                SourceRange BodyRange,
                                SourceLoc PreviousLoc,
                                SourceLoc LBLoc);
  std::unique_ptr<AccessorBodyState>
  takeAccessorBodyState(AbstractFunctionDecl *AFD);

  void delayDecl(DelayedDeclKind Kind, unsigned Flags,
                 DeclContext *ParentContext,
                 SourceRange BodyRange, SourceLoc PreviousLoc);

  void delayTopLevel(TopLevelCodeDecl *TLCD, SourceRange BodyRange,
                     SourceLoc PreviousLoc);

  bool hasDelayedDecl() {
    return CodeCompletionDelayedDeclState.get() != nullptr;
  }
  DelayedDeclKind getDelayedDeclKind() {
    return CodeCompletionDelayedDeclState->Kind;
  }
  SourceLoc getDelayedDeclLoc() {
    return CodeCompletionDelayedDeclState->BodyPos.Loc;
  }
  DeclContext *getDelayedDeclContext() {
    return CodeCompletionDelayedDeclState->ParentContext;
  }
  std::unique_ptr<DelayedDeclState> takeDelayedDeclState() {
    return std::move(CodeCompletionDelayedDeclState);
  }

  TopLevelContext &getTopLevelContext() {
    return TopLevelCode;
  }

  void markParserPosition(ParserPosition Pos,
                          bool InPoundLineEnvironment) {
    MarkedPos = Pos;
    this->InPoundLineEnvironment = InPoundLineEnvironment;
  }

  /// \brief Returns the marked parser position and resets it.
  ParserPosition takeParserPosition() {
    ParserPosition Pos = MarkedPos;
    MarkedPos = ParserPosition();
    return Pos;
  }
};

} // end namespace swift

#endif

//===--- PersistentParserState.h - Parser State -----------------*- C++ -*-===//
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
// Parser state persistent across multiple parses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PERSISTENTPARSERSTATE_H
#define SWIFT_PARSE_PERSISTENTPARSERSTATE_H

#include "swift/Basic/SourceLoc.h"
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

private:
  ScopeInfo ScopeInfo;
  typedef llvm::DenseMap<AbstractFunctionDecl *,
                         std::unique_ptr<FunctionBodyState>>
      DelayedBodiesTy;
  DelayedBodiesTy DelayedBodies;

  /// \brief Parser sets this if it stopped parsing before the buffer ended.
  ParserPos MarkedPos;

  std::unique_ptr<DelayedDeclState> CodeCompletionDelayedDeclState;

public:
  swift::ScopeInfo &getScopeInfo() { return ScopeInfo; }

  void delayFunctionBodyParsing(AbstractFunctionDecl *AFD,
                                SourceRange BodyRange,
                                SourceLoc PreviousLoc);
  std::unique_ptr<FunctionBodyState> takeBodyState(AbstractFunctionDecl *AFD);

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

  void markParserPosition(SourceLoc Loc, SourceLoc PrevLoc) {
    MarkedPos = {Loc, PrevLoc};
  }

  /// \brief Returns the marked parser position and resets it.
  ParserPos takeParserPosition() {
    ParserPos Pos = MarkedPos;
    MarkedPos = ParserPos();
    return Pos;
  }
};

} // end namespace swift

#endif

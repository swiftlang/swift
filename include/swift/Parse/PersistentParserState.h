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

class SourceFile;
class DeclContext;
class IterableDeclContext;

enum class CodeCompletionDelayedDeclKind {
  TopLevelCodeDecl,
  Decl,
  FunctionBody,
};

class CodeCompletionDelayedDeclState {
public:
  CodeCompletionDelayedDeclKind Kind;
  unsigned Flags;
  DeclContext *ParentContext;
  SavedScope Scope;
  unsigned StartOffset;
  unsigned EndOffset;
  unsigned PrevOffset;

  SavedScope takeScope() { return std::move(Scope); }

  CodeCompletionDelayedDeclState(CodeCompletionDelayedDeclKind Kind,
                                 unsigned Flags, DeclContext *ParentContext,
                                 SavedScope &&Scope, unsigned StartOffset,
                                 unsigned EndOffset, unsigned PrevOffset)
      : Kind(Kind), Flags(Flags), ParentContext(ParentContext),
        Scope(std::move(Scope)), StartOffset(StartOffset), EndOffset(EndOffset),
        PrevOffset(PrevOffset) {}
};

/// Parser state persistent across multiple parses.
class PersistentParserState {
public:
  struct ParserPos {
    SourceLoc Loc;
    SourceLoc PrevLoc;

    bool isValid() const { return Loc.isValid(); }
  };

  bool InPoundLineEnvironment = false;
  // FIXME: When condition evaluation moves to a later phase, remove this bit
  // and adjust the client call 'performParseOnly'.
  bool PerformConditionEvaluation = true;
private:
  ScopeInfo ScopeInfo;

  /// Parser sets this if it stopped parsing before the buffer ended.
  ParserPosition MarkedPos;

  std::unique_ptr<CodeCompletionDelayedDeclState> CodeCompletionDelayedDeclStat;

  std::vector<IterableDeclContext *> DelayedDeclLists;

  /// The local context for all top-level code.
  TopLevelContext TopLevelCode;

public:
  swift::ScopeInfo &getScopeInfo() { return ScopeInfo; }
  PersistentParserState();
  PersistentParserState(ASTContext &ctx) : PersistentParserState() { }
  ~PersistentParserState();

  void setCodeCompletionDelayedDeclState(SourceManager &SM, unsigned BufferID,
                                         CodeCompletionDelayedDeclKind Kind,
                                         unsigned Flags,
                                         DeclContext *ParentContext,
                                         SourceRange BodyRange,
                                         SourceLoc PreviousLoc);
  void restoreCodeCompletionDelayedDeclState(
      const CodeCompletionDelayedDeclState &other);

  bool hasCodeCompletionDelayedDeclState() {
    return CodeCompletionDelayedDeclStat.get() != nullptr;
  }

  CodeCompletionDelayedDeclState &getCodeCompletionDelayedDeclState() {
    return *CodeCompletionDelayedDeclStat.get();
  }

  std::unique_ptr<CodeCompletionDelayedDeclState>
  takeCodeCompletionDelayedDeclState() {
    assert(hasCodeCompletionDelayedDeclState());
    return std::move(CodeCompletionDelayedDeclStat);
  }

  void delayDeclList(IterableDeclContext *D);

  void parseAllDelayedDeclLists();

  TopLevelContext &getTopLevelContext() {
    return TopLevelCode;
  }

  void markParserPosition(ParserPosition Pos,
                          bool InPoundLineEnvironment) {
    MarkedPos = Pos;
    this->InPoundLineEnvironment = InPoundLineEnvironment;
  }

  /// Returns the marked parser position and resets it.
  ParserPosition takeParserPosition() {
    ParserPosition Pos = MarkedPos;
    MarkedPos = ParserPosition();
    return Pos;
  }
};

} // end namespace swift

#endif

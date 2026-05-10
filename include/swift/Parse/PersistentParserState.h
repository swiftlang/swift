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

namespace swift {

class SourceFile;
class DeclContext;
class IterableDeclContext;

enum class IDEInspectionDelayedDeclKind {
  TopLevelCodeDecl,
  Decl,
  FunctionBody,
};

class IDEInspectionDelayedDeclState {
public:
  IDEInspectionDelayedDeclKind Kind;
  DeclContext *ParentContext;
  unsigned StartOffset;
  unsigned EndOffset;
  unsigned PrevOffset;

  IDEInspectionDelayedDeclState(IDEInspectionDelayedDeclKind Kind,
                                DeclContext *ParentContext,
                                unsigned StartOffset, unsigned EndOffset,
                                unsigned PrevOffset)
      : Kind(Kind), ParentContext(ParentContext), StartOffset(StartOffset),
        EndOffset(EndOffset), PrevOffset(PrevOffset) {}
};

/// Parser state persistent across multiple parses.
class PersistentParserState {
  std::unique_ptr<IDEInspectionDelayedDeclState> IDEInspectionDelayedDeclStat;

public:
  PersistentParserState();
  PersistentParserState(ASTContext &ctx) : PersistentParserState() { }
  ~PersistentParserState();

  void setIDEInspectionDelayedDeclState(SourceManager &SM, unsigned BufferID,
                                        IDEInspectionDelayedDeclKind Kind,
                                        DeclContext *ParentContext,
                                        SourceRange BodyRange,
                                        SourceLoc PreviousLoc);
  void restoreIDEInspectionDelayedDeclState(
      const IDEInspectionDelayedDeclState &other);

  bool hasIDEInspectionDelayedDeclState() const {
    return IDEInspectionDelayedDeclStat.get() != nullptr;
  }

  IDEInspectionDelayedDeclState &getIDEInspectionDelayedDeclState() {
    return *IDEInspectionDelayedDeclStat.get();
  }
  const IDEInspectionDelayedDeclState &
  getIDEInspectionDelayedDeclState() const {
    return *IDEInspectionDelayedDeclStat.get();
  }

  std::unique_ptr<IDEInspectionDelayedDeclState>
  takeIDEInspectionDelayedDeclState() {
    assert(hasIDEInspectionDelayedDeclState());
    return std::move(IDEInspectionDelayedDeclStat);
  }
};

} // end namespace swift

#endif

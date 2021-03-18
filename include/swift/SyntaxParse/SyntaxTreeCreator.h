//===--- SyntaxTreeCreator.h - Syntax Tree Creation  ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_PARSE_SYNTAXTREECREATOR_H
#define SWIFT_SYNTAX_PARSE_SYNTAXTREECREATOR_H

#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Syntax/References.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
  class SourceManager;
  class SyntaxParsingCache;
  class SourceFile;

namespace syntax {
class SyntaxArena;
class SourceFileSyntax;
}

/// Receives the parsed syntax info from the parser and constructs a persistent
/// syntax tree by converting the data into \c RawSyntax objects, allocated from
/// a \c SyntaxArena.
///
/// It also handles caching re-usable RawSyntax objects and skipping parsed
/// nodes via consulting a \c SyntaxParsingCache.
class SyntaxTreeCreator final : public SyntaxParseActions {
  SourceManager &SM;
  unsigned BufferID;
  RC<syntax::SyntaxArena> Arena;

  /// A string allocated in \c Arena that contains an exact copy of the source
  /// file for which this \c SyntaxTreeCreator creates a syntax tree. \c
  /// RawSyntax nodes can safely reference text inside this buffer since they
  /// retain the \c SyntaxArena which holds the buffer.
  StringRef ArenaSourceBuffer;

  /// A cache of nodes that can be reused when creating the current syntax
  /// tree.
  SyntaxParsingCache *SyntaxCache;

public:
  SyntaxTreeCreator(SourceManager &SM, unsigned bufferID,
                    SyntaxParsingCache *syntaxCache,
                    RC<syntax::SyntaxArena> arena);
  ~SyntaxTreeCreator();

  Optional<syntax::SourceFileSyntax>
  realizeSyntaxRoot(OpaqueSyntaxNode root, const SourceFile &SF) override;

private:
  OpaqueSyntaxNode recordToken(tok tokenKind, StringRef leadingTrivia,
                               StringRef trailingTrivia,
                               CharSourceRange range) override;

  OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) override;

  OpaqueSyntaxNode
  recordRawSyntax(syntax::SyntaxKind kind,
                  ArrayRef<OpaqueSyntaxNode> elements) override;

  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) override;

  OpaqueSyntaxNode makeDeferredToken(tok tokenKind, StringRef leadingTrivia,
                                     StringRef trailingTrivia,
                                     CharSourceRange range,
                                     bool isMissing) override;

  OpaqueSyntaxNode makeDeferredLayout(
      syntax::SyntaxKind k, bool IsMissing,
      const MutableArrayRef<ParsedRawSyntaxNode> &children) override;

  OpaqueSyntaxNode recordDeferredToken(OpaqueSyntaxNode deferred) override;
  OpaqueSyntaxNode recordDeferredLayout(OpaqueSyntaxNode deferred) override;

  DeferredNodeInfo getDeferredChild(OpaqueSyntaxNode node,
                                    size_t ChildIndex) const override;

  CharSourceRange getDeferredChildRange(OpaqueSyntaxNode node,
                                        size_t ChildIndex,
                                        SourceLoc StartLoc) const override;

  size_t getDeferredNumChildren(OpaqueSyntaxNode node) override;
};

} // end namespace swift

#endif

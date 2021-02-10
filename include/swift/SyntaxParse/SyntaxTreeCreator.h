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
  class RawSyntaxTokenCache;
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
class SyntaxTreeCreator: public SyntaxParseActions {
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

  /// Contains all the RawSyntax nodes that were initially created as deferred
  /// nodes and are thus being kept alive by this \c SyntaxTreeCreator.
  /// All of these nodes will receive a \c Release call when the \c
  /// SyntaxTreeCreator is destructed.
  std::vector<OpaqueSyntaxNode> DeferredNodes;

  /// Tokens nodes that have already been created and may be reused in other
  /// parts of the syntax tree.
  std::unique_ptr<RawSyntaxTokenCache> TokenCache;

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
                  const SmallVector<OpaqueSyntaxNode, 4> &elements,
                  CharSourceRange range) override;

  OpaqueSyntaxNode makeDeferredToken(tok tokenKind, StringRef leadingTrivia,
                                     StringRef trailingTrivia,
                                     CharSourceRange range,
                                     bool isMissing) override;

  OpaqueSyntaxNode
  makeDeferredLayout(syntax::SyntaxKind k, CharSourceRange Range,
                     bool IsMissing,
                     const SmallVector<OpaqueSyntaxNode, 4> &children) override;

  OpaqueSyntaxNode recordDeferredToken(OpaqueSyntaxNode deferred) override;
  OpaqueSyntaxNode recordDeferredLayout(OpaqueSyntaxNode deferred) override;

  DeferredNodeInfo getDeferredChild(OpaqueSyntaxNode node, size_t ChildIndex,
                                    SourceLoc ThisNodeLoc) override;

  void discardRecordedNode(OpaqueSyntaxNode node) override;

  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) override;
};

} // end namespace swift

#endif

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

namespace swift {
  class RawSyntaxTokenCache;
  class SourceManager;
  class SyntaxParsingCache;
  class SourceFile;

namespace syntax {
  class SyntaxArena;
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

  /// A cache of nodes that can be reused when creating the current syntax
  /// tree.
  SyntaxParsingCache *SyntaxCache;

  /// Tokens nodes that have already been created and may be reused in other
  /// parts of the syntax tree.
  std::unique_ptr<RawSyntaxTokenCache> TokenCache;

public:
  SyntaxTreeCreator(SourceManager &SM, unsigned bufferID,
                    SyntaxParsingCache *syntaxCache,
                    RC<syntax::SyntaxArena> arena);
  ~SyntaxTreeCreator();

  void acceptSyntaxRoot(OpaqueSyntaxNode root, SourceFile &SF);

private:
  OpaqueSyntaxNode recordToken(tok tokenKind,
                               ArrayRef<ParsedTriviaPiece> leadingTrivia,
                               ArrayRef<ParsedTriviaPiece> trailingTrivia,
                               CharSourceRange range) override;

  OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) override;

  OpaqueSyntaxNode recordRawSyntax(syntax::SyntaxKind kind,
                                   ArrayRef<OpaqueSyntaxNode> elements,
                                   CharSourceRange range) override;

  void discardRecordedNode(OpaqueSyntaxNode node) override;

  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) override;
};

} // end namespace swift

#endif

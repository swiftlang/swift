//===--- SyntaxParseActions.h - Syntax Parsing Actions ----------*- C++ -*-===//
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
//
//  This file defines the interface between the parser and a receiver of
//  raw syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_SYNTAXPARSEACTIONS_H
#define SWIFT_PARSE_SYNTAXPARSEACTIONS_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class CharSourceRange;
class ParsedTriviaPiece;
class SourceFile;
class SourceLoc;
enum class tok;

namespace syntax {
class SourceFileSyntax;
enum class SyntaxKind;
}

typedef void *OpaqueSyntaxNode;

class SyntaxParseActions {
  virtual void _anchor();

public:
  /// Data returned from \c getDeferredChild. This is enough data to construct
  /// a \c ParsedRawSyntaxNode. We don't return \c ParsedRawSyntaxNodes from
  /// \c getDeferredChild to maintain a clean dependency relationship of
  /// \c ParsedRawSyntaxNode being on a higher level than \c SyntaxParseActions.
  struct DeferredNodeInfo {
    OpaqueSyntaxNode Data;
    CharSourceRange Range;
    syntax::SyntaxKind SyntaxKind;
    tok TokenKind;
    bool IsMissing;

    DeferredNodeInfo()
        : Data(nullptr), Range(), SyntaxKind(), TokenKind(), IsMissing(true) {}

    DeferredNodeInfo(OpaqueSyntaxNode Data, CharSourceRange Range,
                     syntax::SyntaxKind SyntaxKind, tok TokenKind,
                     bool IsMissing)
        : Data(Data), Range(Range), SyntaxKind(SyntaxKind),
          TokenKind(TokenKind), IsMissing(IsMissing) {}
  };

  virtual ~SyntaxParseActions() = default;

  virtual OpaqueSyntaxNode recordToken(tok tokenKind, StringRef leadingTrivia,
                                       StringRef trailingTrivia,
                                       CharSourceRange range) = 0;

  /// Record a missing token. \c loc can be invalid or an approximate location
  /// of where the token would be if not missing.
  virtual OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) = 0;

  /// The provided \c elements are an exact layout appropriate for the syntax
  /// \c kind. Missing optional elements are represented with a null
  /// OpaqueSyntaxNode object.
  virtual OpaqueSyntaxNode
  recordRawSyntax(syntax::SyntaxKind kind,
                  const SmallVector<OpaqueSyntaxNode, 4> &elements,
                  CharSourceRange range) = 0;

  /// Create a deferred token node that may or may not be recorded later using
  /// \c recordDeferredToken. The \c SyntaxParseAction is responsible for
  /// keeping the deferred token alive until it is destructed.
  virtual OpaqueSyntaxNode makeDeferredToken(tok tokenKind,
                                             StringRef leadingTrivia,
                                             StringRef trailingTrivia,
                                             CharSourceRange range,
                                             bool isMissing) = 0;

  /// Create a deferred layout node that may or may not be recorded later using
  /// \c recordDeferredLayout. The \c SyntaxParseAction is responsible for
  /// keeping the deferred token alive until it is destructed.
  virtual OpaqueSyntaxNode
  makeDeferredLayout(syntax::SyntaxKind k, CharSourceRange Range,
                     bool IsMissing,
                     const SmallVector<OpaqueSyntaxNode, 4> &children) = 0;

  /// Record a deferred token node that was previously created using \c
  /// makeDeferredToken. The deferred data will never be used again, so it can
  /// be destroyed by this method. Note, however, that not all deferred nodes
  /// will be recorded and that pending deferred nodes need to be freed
  /// when the \c SyntaxParseActions is destructed.
  virtual OpaqueSyntaxNode recordDeferredToken(OpaqueSyntaxNode deferred) = 0;
  virtual OpaqueSyntaxNode recordDeferredLayout(OpaqueSyntaxNode deferred) = 0;

  /// Since most data of \c ParsedRawSyntax is described as opaque data, the
  /// \c ParsedRawSyntax node needs to reach out to the \c SyntaxParseAction
  /// that created it, to retrieve children.
  /// This methods returns all information needed to construct a \c
  /// ParsedRawSyntaxNode of a child node. \p node is the parent node for which
  /// the child at position \p ChildIndex should be retrieved. Furthmore, \p
  /// node starts at \p ThisNodeLoc. This information is needed for the \c
  /// SyntaxTreeCreator in which the \c RawSyntax nodes do not keep track of
  /// their absolute position.
  virtual DeferredNodeInfo getDeferredChild(OpaqueSyntaxNode node,
                                            size_t ChildIndex,
                                            SourceLoc ThisNodeLoc) = 0;

  /// Attempt to realize an opaque raw syntax node for a source file into a
  /// SourceFileSyntax node. This will return \c None if the parsing action
  /// doesn't support the realization of syntax nodes.
  virtual Optional<syntax::SourceFileSyntax>
  realizeSyntaxRoot(OpaqueSyntaxNode root, const SourceFile &SF) = 0;

  /// Discard raw syntax node.
  /// 
  /// FIXME: This breaks invariant that any recorded node will be a part of the
  /// result SourceFile syntax. This method is a temporary workaround, and
  /// should be removed when we fully migrate to libSyntax parsing.
  virtual void discardRecordedNode(OpaqueSyntaxNode node) = 0;

  /// Used for incremental re-parsing.
  virtual std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
    return std::make_pair(0, nullptr);
  }
};

} // end namespace swift

#endif

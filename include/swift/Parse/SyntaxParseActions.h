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
#include "swift/Subsystems.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/Allocator.h"

namespace swift {

class ParsedTriviaPiece;
class SourceFile;
class SourceLoc;
enum class tok : uint8_t;
class ParsedRawSyntaxNode;

namespace syntax {
class SourceFileSyntax;
enum class SyntaxKind : uint16_t;
}

// MARK: - Helper types

/// A syntax node that can either be deferred or recorded. The actual data is
/// opaque and needs to be interpreted by the \c SyntaxParseAction which created
/// it.
class RecordedOrDeferredNode {
public:
  enum class Kind : uint8_t {
    Null,
    Recorded,
    DeferredLayout,
    DeferredToken,
  };

private:
  llvm::PointerIntPair<OpaqueSyntaxNode, 2, Kind> Data;

public:
  RecordedOrDeferredNode(OpaqueSyntaxNode Node, Kind NodeKind)
      : Data(Node, NodeKind) {}

  OpaqueSyntaxNode getOpaque() const { return Data.getPointer(); }
  Kind getKind() const { return Data.getInt(); }
};

/// Data returned from \c getDeferredChild. This is enough data to construct
/// a \c ParsedRawSyntaxNode. We don't return \c ParsedRawSyntaxNodes from
/// \c getDeferredChild to maintain a clean dependency relationship of
/// \c ParsedRawSyntaxNode being on a higher level than \c SyntaxParseActions.
struct DeferredNodeInfo {
  RecordedOrDeferredNode Data;
  syntax::SyntaxKind SyntaxKind;
  tok TokenKind;
  bool IsMissing;

  DeferredNodeInfo(RecordedOrDeferredNode Data, syntax::SyntaxKind SyntaxKind,
                   tok TokenKind, bool IsMissing)
      : Data(Data), SyntaxKind(SyntaxKind), TokenKind(TokenKind),
        IsMissing(IsMissing) {}
};

// MARK: - SyntaxParseActions

class SyntaxParseActions {
  virtual void _anchor();

public:
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
                  ArrayRef<OpaqueSyntaxNode> elements) = 0;

  /// Create a deferred token node that may or may not be recorded later using
  /// \c recordDeferredToken. The \c SyntaxParseAction is responsible for
  /// keeping the deferred token alive until the action is destructed.
  virtual OpaqueSyntaxNode makeDeferredToken(tok tokenKind,
                                             StringRef leadingTrivia,
                                             StringRef trailingTrivia,
                                             CharSourceRange range,
                                             bool isMissing) = 0;

  /// Create a deferred layout node that may or may not be recorded later using
  /// \c recordDeferredLayout. The \c SyntaxParseAction is responsible for
  /// keeping the deferred token alive until it is destructed.
  /// From all nodes in \p children, the underlying opaque data will be *taken*
  /// which resets the nodes.
  virtual OpaqueSyntaxNode
  makeDeferredLayout(syntax::SyntaxKind k, bool isMissing,
                     const MutableArrayRef<ParsedRawSyntaxNode> &children) = 0;

  /// Record a deferred token node that was previously created using \c
  /// makeDeferredToken. The deferred data will never be used again, so it can
  /// be destroyed by this method. Note that not all deferred nodes will be
  /// recorded and that pending deferred nodes need to be freed when the \c
  /// SyntaxParseActions is destructed.
  virtual OpaqueSyntaxNode recordDeferredToken(OpaqueSyntaxNode deferred) = 0;

  /// Record a deferred layout node. See recordDeferredToken.
  virtual OpaqueSyntaxNode recordDeferredLayout(OpaqueSyntaxNode deferred) = 0;

  /// Since most data of \c ParsedRawSyntax is described as opaque data, the
  /// \c ParsedRawSyntax node needs to reach out to the \c SyntaxParseAction,
  /// which created it, to retrieve children.
  /// This method assumes that \p node represents a *deferred* layout node.
  /// This methods returns all information needed to construct a \c
  /// ParsedRawSyntaxNode of a child node, except for the range which can be
  /// retrieved using \c getDeferredChildRange if element ranges should be
  /// verified
  /// \p node is the parent node for which the child at position \p ChildIndex
  /// should be retrieved. Furthmore, \p node starts at \p StartLoc.
  virtual DeferredNodeInfo getDeferredChild(OpaqueSyntaxNode node,
                                            size_t childIndex) const = 0;

  /// To verify \c ParsedRawSyntaxNode element ranges, the range of child nodes
  /// returend by \c getDeferredChild needs to be determined. That's what this
  /// method does.
  /// It assumes that \p node is a deferred layout node starting at \p startLoc
  /// and returns the range of the child node at \p childIndex.
  /// This method is not designed with performance in mind. Do not use in
  /// performance-critical code.
  virtual CharSourceRange getDeferredChildRange(OpaqueSyntaxNode node,
                                                size_t childIndex,
                                                SourceLoc startLoc) const = 0;

  /// Return the number of children, \p node has. These can be retrieved using
  /// \c getDeferredChild.
  virtual size_t getDeferredNumChildren(OpaqueSyntaxNode node) = 0;

  /// Attempt to realize an opaque raw syntax node for a source file into a
  /// SourceFileSyntax node. This will return \c None if the parsing action
  /// doesn't support the realization of syntax nodes.
  virtual Optional<syntax::SourceFileSyntax>
  realizeSyntaxRoot(OpaqueSyntaxNode root, const SourceFile &SF) = 0;

  /// Used for incremental re-parsing.
  virtual std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
    return std::make_pair(0, nullptr);
  }
};

} // end namespace swift

#endif

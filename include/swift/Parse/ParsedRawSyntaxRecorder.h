//===--- ParsedRawSyntaxRecorder.h - Raw Syntax Parsing Recorder ----------===//
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
// This file defines the ParsedRawSyntaxRecorder, which is the interface the
// parser is using to pass parsed syntactic elements to a SyntaxParseActions
// receiver and get a ParsedRawSyntaxNode object back.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H
#define SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H

#include "swift/Basic/LLVM.h"
#include <memory>

namespace swift {

class CharSourceRange;
class ParsedRawSyntaxNode;
struct ParsedTrivia;
class ParsedTriviaPiece;
class SyntaxParseActions;
class SyntaxParsingContext;
class SourceLoc;
class Token;
enum class tok : uint8_t;

namespace syntax {
enum class SyntaxKind : uint16_t;
}

class ParsedRawSyntaxRecorder final {
  std::shared_ptr<SyntaxParseActions> SPActions;

  /// Assuming that \p node is a deferred layout or token node, record it and
  /// return the recorded node.
  /// This consumes the data from \c node, which is unusable after it has been
  /// recorded. The returned node should be used afterwards instead.
  ParsedRawSyntaxNode recordDeferredNode(ParsedRawSyntaxNode &node);

public:
  explicit ParsedRawSyntaxRecorder(std::shared_ptr<SyntaxParseActions> spActions)
    : SPActions(std::move(spActions)) {}

  ParsedRawSyntaxNode recordToken(const Token &tok, StringRef leadingTrivia,
                                  StringRef trailingTrivia);

  ParsedRawSyntaxNode recordToken(tok tokenKind, CharSourceRange tokenRange,
                                  StringRef leadingTrivia,
                                  StringRef trailingTrivia);

  /// Record a missing token. \p loc can be invalid or an approximate location
  /// of where the token would be if not missing.
  ParsedRawSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc);

  /// The provided \p elements are an exact layout appropriate for the syntax
  /// \p kind. Missing optional elements are represented with a null
  /// ParsedRawSyntaxNode object.
  ParsedRawSyntaxNode recordRawSyntax(syntax::SyntaxKind kind,
                                      MutableArrayRef<ParsedRawSyntaxNode> elements);

  /// Record a raw syntax collecton without eny elements. \p loc can be invalid
  /// or an approximate location of where an element of the collection would be
  /// if not missing.
  ParsedRawSyntaxNode recordEmptyRawSyntaxCollection(syntax::SyntaxKind kind,
                                                     SourceLoc loc);

  /// Form a deferred syntax layout node.
  /// All nodes in \p deferred nodes must be deferred. Otherwise, we'd have a
  /// deferred layout node with recorded child nodes. Should we decide to
  /// discard the deferred layout node, we would also need to discard its
  /// recorded children, which cannot be done.
  ParsedRawSyntaxNode
  makeDeferred(syntax::SyntaxKind k,
               MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
               SyntaxParsingContext &ctx);

  /// Form a deferred token node.
  ParsedRawSyntaxNode makeDeferred(Token tok, StringRef leadingTrivia,
                                   StringRef trailingTrivia);

  /// Form a deferred missing token node.
  ParsedRawSyntaxNode makeDeferredMissing(tok tokKind, SourceLoc loc);

  /// Used for incremental re-parsing.
  ParsedRawSyntaxNode lookupNode(size_t lexerOffset, SourceLoc loc,
                                 syntax::SyntaxKind kind);

  /// For a deferred layout node \p parent, retrieve the deferred child node
  /// at \p ChildIndex.
  ParsedRawSyntaxNode getDeferredChild(const ParsedRawSyntaxNode &parent,
                                       size_t ChildIndex) const;

  /// For a deferred layout node \p node, retrieve the number of children.
  size_t getDeferredNumChildren(const ParsedRawSyntaxNode &node) const;

#ifndef NDEBUG
  static void verifyElementRanges(ArrayRef<ParsedRawSyntaxNode> elements);
#endif
};

} // end namespace swift

#endif // SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H

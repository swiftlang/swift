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
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/SyntaxParse/SyntaxTreeCreator.h"
#include <memory>

/// Define a macro that creates a \c ParsedRawSyntaxNode. If \c
/// PARSEDRAWSYNTAXNODE_VERIFY_RANGES is defined, it passes the \c Range
/// parameter, otherwise it ignores it at the pre-processor level, which means
/// that \c Range can be an invalid expression.
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
#define makeParsedRawSyntaxNode(Opaque, SynKind, TokKind, DataKind, IsMissing, \
                                Range)                                         \
  ParsedRawSyntaxNode(Opaque, SynKind, TokKind, DataKind, IsMissing, Range)
#else
#define makeParsedRawSyntaxNode(Opaque, SynKind, TokKind, DataKind, IsMissing, \
                                Range)                                         \
  ParsedRawSyntaxNode(Opaque, SynKind, TokKind, DataKind, IsMissing)
#endif

namespace swift {

class CharSourceRange;
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

/// The information returned from the \c lookupNode method in \c
/// SyntaxParseActions.
struct ParseLookupResult {
  ParsedRawSyntaxNode Node;

  /// The length of \c Node spelled out in source, including trivia.
  size_t Length;

  ParseLookupResult(ParsedRawSyntaxNode &&Node, size_t Length)
      : Node(std::move(Node)), Length(Length) {}
};

class ParsedRawSyntaxRecorder final {
  std::shared_ptr<SyntaxParseActions> SPActions;

  /// Assuming that \p node is a deferred layout or token node, record it and
  /// return the recorded node.
  /// This consumes the data from \c node, which is unusable after it has been
  /// recorded. The returned node should be used afterwards instead.
  ParsedRawSyntaxNode recordDeferredNode(ParsedRawSyntaxNode &node) {
    switch (node.getDataKind()) {
    case RecordedOrDeferredNode::Kind::Null:
    case RecordedOrDeferredNode::Kind::Recorded:
      llvm_unreachable("Not deferred");
    case RecordedOrDeferredNode::Kind::DeferredLayout: {
      OpaqueSyntaxNode Data = SPActions->recordDeferredLayout(node.takeData());
      return makeParsedRawSyntaxNode(Data, node.getKind(), node.getTokenKind(),
                                     ParsedRawSyntaxNode::DataKind::Recorded,
                                     node.isMissing(), node.getRange());
    }
    case RecordedOrDeferredNode::Kind::DeferredToken: {
      OpaqueSyntaxNode Data = SPActions->recordDeferredToken(node.takeData());
      return makeParsedRawSyntaxNode(Data, node.getKind(), node.getTokenKind(),
                                     ParsedRawSyntaxNode::DataKind::Recorded,
                                     node.isMissing(), node.getRange());
    }
    }
  }

public:
  explicit ParsedRawSyntaxRecorder(std::shared_ptr<SyntaxParseActions> spActions)
    : SPActions(std::move(spActions)) {}

  ParsedRawSyntaxNode recordToken(const Token &tok, StringRef leadingTrivia,
                                  StringRef trailingTrivia) {
    return recordToken(tok.getKind(), tok.getRange(), leadingTrivia,
                       trailingTrivia);
  }

  ParsedRawSyntaxNode recordToken(tok tokenKind, CharSourceRange tokenRange,
                                  StringRef leadingTrivia,
                                  StringRef trailingTrivia) {
    SourceLoc offset =
        tokenRange.getStart().getAdvancedLoc(-leadingTrivia.size());
    unsigned length = leadingTrivia.size() + tokenRange.getByteLength() +
                      trailingTrivia.size();
    CharSourceRange range(offset, length);
    OpaqueSyntaxNode n =
        SPActions->recordToken(tokenKind, leadingTrivia, trailingTrivia, range);
    return makeParsedRawSyntaxNode(n, syntax::SyntaxKind::Token, tokenKind,
                                   ParsedRawSyntaxNode::DataKind::Recorded,
                                   /*IsMissing=*/false, range);
  }

  /// Record a missing token. \p loc can be invalid or an approximate location
  /// of where the token would be if not missing.
  ParsedRawSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc);

  /// The provided \p elements are an exact layout appropriate for the syntax
  /// \p kind. Missing optional elements are represented with a null
  /// ParsedRawSyntaxNode object.
  ParsedRawSyntaxNode
  recordRawSyntax(syntax::SyntaxKind kind,
                  MutableArrayRef<ParsedRawSyntaxNode> elements) {
    assert(kind != syntax::SyntaxKind::Token &&
           "Use recordToken to record a token");
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
    auto range = ParsedRawSyntaxRecorder::verifyElementRanges(elements);
#endif

    SmallVector<OpaqueSyntaxNode, 16> subnodes;
    if (!elements.empty()) {
      for (auto &subnode : elements) {
        switch (subnode.getDataKind()) {
        case RecordedOrDeferredNode::Kind::Null:
          subnodes.push_back(nullptr);
          break;
        case RecordedOrDeferredNode::Kind::Recorded:
          subnodes.push_back(subnode.takeData());
          break;
        case RecordedOrDeferredNode::Kind::DeferredLayout:
        case RecordedOrDeferredNode::Kind::DeferredToken: {
          auto recorded = recordDeferredNode(subnode);
          subnodes.push_back(recorded.takeData());
          break;
        }
        }
      }
    }
    OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, subnodes);
    return makeParsedRawSyntaxNode(n, kind, tok::NUM_TOKENS,
                                   ParsedRawSyntaxNode::DataKind::Recorded,
                                   /*IsMissing=*/false, range);
  }

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
               SyntaxParsingContext &ctx) {
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
    auto range = ParsedRawSyntaxRecorder::verifyElementRanges(deferredNodes);
#endif

    assert(llvm::none_of(deferredNodes, [](const ParsedRawSyntaxNode &node) {
      return node.isRecorded();
    }) && "Cannot create a deferred layout node that has recorded children");

    auto data =
        SPActions->makeDeferredLayout(k, /*IsMissing=*/false, deferredNodes);
    return makeParsedRawSyntaxNode(
        data, k, tok::NUM_TOKENS, ParsedRawSyntaxNode::DataKind::DeferredLayout,
        /*IsMissing=*/false, range);
  }

  /// Form a deferred token node.
  ParsedRawSyntaxNode makeDeferred(Token tok, StringRef leadingTrivia,
                                   StringRef trailingTrivia) {
    CharSourceRange tokRange = tok.getRange();
    CharSourceRange RangeWithTrivia = CharSourceRange(
        tokRange.getStart().getAdvancedLoc(-leadingTrivia.size()),
        (unsigned)leadingTrivia.size() + tokRange.getByteLength() +
            (unsigned)trailingTrivia.size());
    auto Data = SPActions->makeDeferredToken(tok.getKind(), leadingTrivia,
                                             trailingTrivia, RangeWithTrivia,
                                             /*IsMissing=*/false);
    return makeParsedRawSyntaxNode(Data, syntax::SyntaxKind::Token,
                                   tok.getKind(),
                                   ParsedRawSyntaxNode::DataKind::DeferredToken,
                                   /*IsMissing=*/false, RangeWithTrivia);
  }

  /// Form a deferred missing token node.
  ParsedRawSyntaxNode makeDeferredMissing(tok tokKind, SourceLoc loc);

  /// Used for incremental re-parsing.
  ParseLookupResult lookupNode(size_t lexerOffset, SourceLoc loc,
                               syntax::SyntaxKind kind);

  /// For a deferred layout node \p parent, retrieve the deferred child node
  /// at \p ChildIndex.
  ParsedRawSyntaxNode getDeferredChild(const ParsedRawSyntaxNode &parent,
                                       size_t ChildIndex) const;

  /// For a deferred layout node \p node, retrieve the number of children.
  size_t getDeferredNumChildren(const ParsedRawSyntaxNode &node) const;

#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
  /// Verify that the ranges of \p elements are all consecutive and return the
  /// range spanned by all \p elements.
  static CharSourceRange
  verifyElementRanges(ArrayRef<ParsedRawSyntaxNode> elements);
#endif
};

} // end namespace swift

#endif // SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H

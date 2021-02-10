//===--- ParsedRawSyntaxRecorder.cpp - Raw Syntax Parsing Recorder --------===//
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

#include "swift/Parse/ParsedRawSyntaxRecorder.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"

using namespace swift;
using namespace swift::syntax;

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordToken(const Token &tok, StringRef leadingTrivia,
                                     StringRef trailingTrivia) {
  return recordToken(tok.getKind(), tok.getRange(), leadingTrivia,
                     trailingTrivia);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordToken(tok tokKind, CharSourceRange tokRange,
                                     StringRef leadingTrivia,
                                     StringRef trailingTrivia) {
  SourceLoc offset = tokRange.getStart().getAdvancedLoc(-leadingTrivia.size());
  unsigned length =
      leadingTrivia.size() + tokRange.getByteLength() + trailingTrivia.size();
  CharSourceRange range(offset, length);
  OpaqueSyntaxNode n =
      SPActions->recordToken(tokKind, leadingTrivia, trailingTrivia, range);
  return ParsedRawSyntaxNode(SyntaxKind::Token, tokKind, range,
                             /*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return ParsedRawSyntaxNode(SyntaxKind::Token, tokenKind, range,
                             /*isMissing=*/true, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordDeferredNode(const ParsedRawSyntaxNode &node) {
  assert(!node.isNull() && !node.isRecorded());
  if (node.isDeferredLayout()) {
    OpaqueSyntaxNode Data = SPActions->recordDeferredLayout(node.getData());
    return ParsedRawSyntaxNode(node.getKind(), node.getTokenKind(),
                               node.getRange(), node.isMissing(), Data,
                               ParsedRawSyntaxNode::DataKind::Recorded);
  } else {
    assert(node.isDeferredToken());
    OpaqueSyntaxNode Data = SPActions->recordDeferredToken(node.getData());
    return ParsedRawSyntaxNode(node.getKind(), node.getTokenKind(),
                               node.getRange(), node.isMissing(), Data,
                               ParsedRawSyntaxNode::DataKind::Recorded);
  }
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordRawSyntax(SyntaxKind kind,
                                         MutableArrayRef<ParsedRawSyntaxNode> elements) {
#ifndef NDEBUG
  ParsedRawSyntaxRecorder::verifyElementRanges(elements);
#endif
  CharSourceRange range;
  SmallVector<OpaqueSyntaxNode, 4> subnodes;
  if (!elements.empty()) {
    SourceLoc offset;
    unsigned length = 0;
    for (auto &subnode : elements) {
      CharSourceRange localRange;
      if (subnode.isNull()) {
        subnodes.push_back(nullptr);
      } else if (subnode.isRecorded()) {
        localRange = subnode.getRange();
        subnodes.push_back(subnode.takeData());
      } else {
        assert(subnode.isDeferredLayout() || subnode.isDeferredToken());
        localRange = subnode.getRange();
        auto recorded = recordDeferredNode(subnode);
        subnodes.push_back(recorded.takeData());
      }

      if (localRange.isValid()) {
        if (offset.isInvalid())
          offset = localRange.getStart();
        length += localRange.getByteLength();
      }
    }
    range = CharSourceRange{offset, length};
  }
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, subnodes, range);
  return ParsedRawSyntaxNode(kind, tok::NUM_TOKENS, range, /*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {}, range);
  return ParsedRawSyntaxNode(kind, tok::NUM_TOKENS, range, /*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

ParsedRawSyntaxNode ParsedRawSyntaxRecorder::makeDeferred(
    SyntaxKind k, MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
    SyntaxParsingContext &ctx) {
  assert(k != syntax::SyntaxKind::Token &&
         "Use makeDeferredToken to create deferred tokens");
  CharSourceRange range;
  if (deferredNodes.empty()) {
    OpaqueSyntaxNode Data =
        SPActions->makeDeferredLayout(k, range, /*IsMissing=*/false, {});
    return ParsedRawSyntaxNode(k, range, Data,
                               ParsedRawSyntaxNode::DataKind::DeferredLayout);
  }
  SmallVector<OpaqueSyntaxNode, 4> children;

#ifndef NDEBUG
  ParsedRawSyntaxRecorder::verifyElementRanges(deferredNodes);
#endif
  for (auto &node : deferredNodes) {
    // Cached range.
    if (!node.isNull() && !node.isMissing()) {
      auto nodeRange = node.getRange();
      if (nodeRange.isValid()) {
        if (range.isInvalid())
          range = nodeRange;
        else
          range.widen(nodeRange);
      }
    }

    children.push_back(node.getData());
  }
  OpaqueSyntaxNode Data =
      SPActions->makeDeferredLayout(k, range, /*IsMissing=*/false, children);
  return ParsedRawSyntaxNode(k, range, Data,
                             ParsedRawSyntaxNode::DataKind::DeferredLayout);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferred(Token tok, StringRef leadingTrivia,
                                      StringRef trailingTrivia) {
  // Compute the range that includes the token and its trivia.
  SourceLoc rangeBegin =
      tok.getRange().getStart().getAdvancedLoc(-leadingTrivia.size());
  unsigned rangeLen = leadingTrivia.size() + tok.getRange().getByteLength() +
                      trailingTrivia.size();
  auto range = CharSourceRange(rangeBegin, rangeLen);

  OpaqueSyntaxNode Data = SPActions->makeDeferredToken(
      tok.getKind(), leadingTrivia, trailingTrivia, range, /*isMissing=*/false);
  return ParsedRawSyntaxNode(tok.getKind(), tok.getLoc(), range,
                             /*IsMissing=*/false, Data,
                             ParsedRawSyntaxNode::DataKind::DeferredToken);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferredMissing(tok tokKind, SourceLoc loc) {
  OpaqueSyntaxNode Data = SPActions->makeDeferredToken(
      tokKind, /*leadingTrivia=*/StringRef(), /*trailingTrivia=*/StringRef(),
      CharSourceRange(loc, /*ByteLength=*/0), /*isMissing=*/true);
  return ParsedRawSyntaxNode(
      tokKind, loc, CharSourceRange(loc, /*ByteLength=*/0), /*IsMissing=*/true,
      Data, ParsedRawSyntaxNode::DataKind::DeferredToken);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::getDeferredChild(const ParsedRawSyntaxNode &parent,
                                          size_t childIndex) const {
  assert(parent.isDeferredLayout());
  auto childInfo = SPActions->getDeferredChild(parent.getData(), childIndex,
                                               parent.Range.getStart());
  return ParsedRawSyntaxNode(
      childInfo.SyntaxKind, childInfo.TokenKind, childInfo.Range,
      /*IsMissing=*/false, childInfo.Data,
      childInfo.SyntaxKind == SyntaxKind::Token
          ? ParsedRawSyntaxNode::DataKind::DeferredToken
          : ParsedRawSyntaxNode::DataKind::DeferredLayout);
}

void ParsedRawSyntaxRecorder::discardRecordedNode(ParsedRawSyntaxNode &node) {
  SPActions->discardRecordedNode(node.takeData());
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::lookupNode(size_t lexerOffset, SourceLoc loc,
                                    SyntaxKind kind) {
  size_t length;
  OpaqueSyntaxNode n;
  std::tie(length, n) = SPActions->lookupNode(lexerOffset, kind);
  if (length == 0) {
    return ParsedRawSyntaxNode::null();
  }
  CharSourceRange range{loc, unsigned(length)};
  return ParsedRawSyntaxNode(kind, tok::NUM_TOKENS, range, /*IsMissing=*/false, n,
                             ParsedRawSyntaxNode::DataKind::Recorded);
}

#ifndef NDEBUG
void ParsedRawSyntaxRecorder::verifyElementRanges(ArrayRef<ParsedRawSyntaxNode> elements) {
  SourceLoc prevEndLoc;
  for (const auto &elem: elements) {
    if (elem.isMissing() || elem.isNull())
      continue;
    CharSourceRange range = elem.getRange();
    if (range.isValid()) {
      assert((prevEndLoc.isInvalid() || range.getStart() == prevEndLoc)
             && "Non-contiguous child ranges?");
      prevEndLoc = range.getEnd();
    }
  }
}
#endif

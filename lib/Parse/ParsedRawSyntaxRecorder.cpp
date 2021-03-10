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
ParsedRawSyntaxRecorder::recordDeferredNode(ParsedRawSyntaxNode &node) {
  switch (node.getDataKind()) {
  case RecordedOrDeferredNode::Kind::Null:
  case RecordedOrDeferredNode::Kind::Recorded:
    llvm_unreachable("Not deferred");
  case RecordedOrDeferredNode::Kind::DeferredLayout: {
    OpaqueSyntaxNode Data = SPActions->recordDeferredLayout(node.takeData());
    return ParsedRawSyntaxNode(
        Data, node.getRange(), node.getKind(), node.getTokenKind(),
        ParsedRawSyntaxNode::DataKind::Recorded, node.isMissing());
  }
  case RecordedOrDeferredNode::Kind::DeferredToken: {
    OpaqueSyntaxNode Data = SPActions->recordDeferredToken(node.takeData());
    return ParsedRawSyntaxNode(
        Data, node.getRange(), node.getKind(), node.getTokenKind(),
        ParsedRawSyntaxNode::DataKind::Recorded, node.isMissing());
  }
  }
}

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
  return ParsedRawSyntaxNode(n, range, SyntaxKind::Token, tokKind,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*IsMissing=*/false);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return ParsedRawSyntaxNode(n, range, SyntaxKind::Token, tokenKind,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*isMissing=*/true);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordRawSyntax(SyntaxKind kind,
                                         MutableArrayRef<ParsedRawSyntaxNode> elements) {
  assert(kind != SyntaxKind::Token && "Use recordToken to record a token");
#ifndef NDEBUG
  ParsedRawSyntaxRecorder::verifyElementRanges(elements);
#endif

  SmallVector<OpaqueSyntaxNode, 16> subnodes;
  SourceLoc startLoc;
  unsigned length = 0;
  if (!elements.empty()) {
    for (auto &subnode : elements) {
      if (!subnode.isNull() && subnode.getRange().isValid()) {
        if (startLoc.isInvalid()) {
          startLoc = subnode.getRange().getStart();
        }
        length += subnode.getRange().getByteLength();
      }
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
  return ParsedRawSyntaxNode(
      n, CharSourceRange(startLoc, length), kind, tok::NUM_TOKENS,
      ParsedRawSyntaxNode::DataKind::Recorded, /*IsMissing=*/false);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {});
  return ParsedRawSyntaxNode(n, range, kind, tok::unknown,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*IsMissing=*/false);
}

/// Create a deferred layout node.
ParsedRawSyntaxNode ParsedRawSyntaxRecorder::makeDeferred(
    syntax::SyntaxKind k, MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
    SyntaxParsingContext &ctx) {
#ifndef NDEBUG
  ParsedRawSyntaxRecorder::verifyElementRanges(deferredNodes);
#endif

  CharSourceRange range;
  RecordedOrDeferredNode *newPtr =
      ctx.getScratchAlloc().Allocate<RecordedOrDeferredNode>(
          deferredNodes.size());
  auto children = llvm::makeMutableArrayRef(newPtr, deferredNodes.size());
  for (size_t i = 0; i < deferredNodes.size(); ++i) {
    auto &node = deferredNodes[i];
    assert(!node.isRecorded() &&
           "Cannot create a deferred layout node that has recorded children");
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

    children[i] = node.takeRecordedOrDeferredNode();
  }
  auto data = SPActions->makeDeferredLayout(k, /*IsMissing=*/false, children);
  return ParsedRawSyntaxNode(data, range, k, tok::NUM_TOKENS,
                             ParsedRawSyntaxNode::DataKind::DeferredLayout,
                             /*IsMissing=*/false);
}

/// Create a deferred token node.
ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferred(Token tok, StringRef leadingTrivia,
                                      StringRef trailingTrivia) {
  CharSourceRange tokRange = tok.getRange();
  CharSourceRange RangeWithTrivia = CharSourceRange(
      tokRange.getStart().getAdvancedLoc(-leadingTrivia.size()),
      (unsigned)leadingTrivia.size() + tokRange.getByteLength() +
          (unsigned)trailingTrivia.size());
  auto Data =
      SPActions->makeDeferredToken(tok.getKind(), leadingTrivia, trailingTrivia,
                                   RangeWithTrivia, /*IsMissing=*/false);
  return ParsedRawSyntaxNode(
      Data, RangeWithTrivia, SyntaxKind::Token, tok.getKind(),
      ParsedRawSyntaxNode::DataKind::DeferredToken, /*IsMissing=*/false);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferredMissing(tok tokKind, SourceLoc loc) {
  auto Data = SPActions->makeDeferredToken(
      tokKind, /*leadingTrivia=*/StringRef(),
      /*trailingTrivia=*/StringRef(), CharSourceRange(loc, /*Length=*/0),
      /*IsMissing=*/true);
  return ParsedRawSyntaxNode(
      Data, CharSourceRange(loc, /*Length=*/0), SyntaxKind::Token, tokKind,
      ParsedRawSyntaxNode::DataKind::DeferredToken, /*IsMissing=*/true);
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
  return ParsedRawSyntaxNode(n, range, kind, tok::unknown,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*IsMissing=*/false);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::getDeferredChild(const ParsedRawSyntaxNode &parent,
                                          size_t childIndex) const {
  assert(parent.isDeferredLayout());
  auto childInfo = SPActions->getDeferredChild(
      parent.getUnsafeDeferredOpaqueData(), childIndex,
      parent.getRange().getStart());
  return ParsedRawSyntaxNode(childInfo.Data.getOpaque(), childInfo.Range,
                             childInfo.SyntaxKind, childInfo.TokenKind,
                             childInfo.Data.getKind(), childInfo.IsMissing);
}

size_t ParsedRawSyntaxRecorder::getDeferredNumChildren(
    const ParsedRawSyntaxNode &node) const {
  assert(node.isDeferredLayout());
  return SPActions->getDeferredNumChildren(node.getUnsafeDeferredOpaqueData());
}

#ifndef NDEBUG
void ParsedRawSyntaxRecorder::verifyElementRanges(ArrayRef<ParsedRawSyntaxNode> elements) {
  SourceLoc prevEndLoc;
  for (const auto &elem: elements) {
    if (elem.isNull() || elem.isMissing())
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

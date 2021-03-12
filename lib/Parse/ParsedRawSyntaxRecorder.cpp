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

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordDeferredNode(ParsedRawSyntaxNode &node) {
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
  return makeParsedRawSyntaxNode(n, SyntaxKind::Token, tokKind,
                                 ParsedRawSyntaxNode::DataKind::Recorded,
                                 /*IsMissing=*/false, range);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return makeParsedRawSyntaxNode(n, SyntaxKind::Token, tokenKind,
                                 ParsedRawSyntaxNode::DataKind::Recorded,
                                 /*isMissing=*/true, CharSourceRange(loc, 0));
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordRawSyntax(SyntaxKind kind,
                                         MutableArrayRef<ParsedRawSyntaxNode> elements) {
  assert(kind != SyntaxKind::Token && "Use recordToken to record a token");
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

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {});
  return makeParsedRawSyntaxNode(n, kind, tok::unknown,
                                 ParsedRawSyntaxNode::DataKind::Recorded,
                                 /*IsMissing=*/false, CharSourceRange(loc, 0));
}

/// Create a deferred layout node.
ParsedRawSyntaxNode ParsedRawSyntaxRecorder::makeDeferred(
    syntax::SyntaxKind k, MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
    SyntaxParsingContext &ctx) {
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
  auto range = ParsedRawSyntaxRecorder::verifyElementRanges(deferredNodes);
#endif

  RecordedOrDeferredNode *newPtr =
      ctx.getScratchAlloc().Allocate<RecordedOrDeferredNode>(
          deferredNodes.size());
  auto children = llvm::makeMutableArrayRef(newPtr, deferredNodes.size());
  for (size_t i = 0; i < deferredNodes.size(); ++i) {
    auto &node = deferredNodes[i];
    assert(!node.isRecorded() &&
           "Cannot create a deferred layout node that has recorded children");

    children[i] = node.takeRecordedOrDeferredNode();
  }
  auto data = SPActions->makeDeferredLayout(k, /*IsMissing=*/false, children);
  return makeParsedRawSyntaxNode(data, k, tok::NUM_TOKENS,
                                 ParsedRawSyntaxNode::DataKind::DeferredLayout,
                                 /*IsMissing=*/false, range);
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
  return makeParsedRawSyntaxNode(Data, SyntaxKind::Token, tok.getKind(),
                                 ParsedRawSyntaxNode::DataKind::DeferredToken,
                                 /*IsMissing=*/false, RangeWithTrivia);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferredMissing(tok tokKind, SourceLoc loc) {
  auto Data = SPActions->makeDeferredToken(
      tokKind, /*leadingTrivia=*/StringRef(),
      /*trailingTrivia=*/StringRef(), CharSourceRange(loc, /*Length=*/0),
      /*IsMissing=*/true);
  return makeParsedRawSyntaxNode(Data, SyntaxKind::Token, tokKind,
                                 ParsedRawSyntaxNode::DataKind::DeferredToken,
                                 /*IsMissing=*/true,
                                 CharSourceRange(loc, /*Length=*/0));
}

ParseLookupResult ParsedRawSyntaxRecorder::lookupNode(size_t lexerOffset,
                                                      SourceLoc loc,
                                                      SyntaxKind kind) {
  size_t length;
  OpaqueSyntaxNode n;
  std::tie(length, n) = SPActions->lookupNode(lexerOffset, kind);
  if (length == 0) {
    return ParseLookupResult(ParsedRawSyntaxNode::null(), length);
  }
  return ParseLookupResult(
      makeParsedRawSyntaxNode(
          n, kind, tok::unknown, ParsedRawSyntaxNode::DataKind::Recorded,
          /*IsMissing=*/false, CharSourceRange(loc, unsigned(length))),
      length);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::getDeferredChild(const ParsedRawSyntaxNode &parent,
                                          size_t childIndex) const {
  assert(parent.isDeferredLayout());
  auto childInfo = SPActions->getDeferredChild(
      parent.getUnsafeDeferredOpaqueData(), childIndex);

#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
  auto range = SPActions->getDeferredChildRange(
      parent.getUnsafeDeferredOpaqueData(), childIndex,
      parent.getRange().getStart());
  return ParsedRawSyntaxNode(childInfo.Data, childInfo.SyntaxKind,
                             childInfo.TokenKind, childInfo.IsMissing, range);
#else
  return ParsedRawSyntaxNode(childInfo.Data, childInfo.SyntaxKind,
                             childInfo.TokenKind, childInfo.IsMissing);
#endif
}

size_t ParsedRawSyntaxRecorder::getDeferredNumChildren(
    const ParsedRawSyntaxNode &node) const {
  assert(node.isDeferredLayout());
  return SPActions->getDeferredNumChildren(node.getUnsafeDeferredOpaqueData());
}

#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
CharSourceRange ParsedRawSyntaxRecorder::verifyElementRanges(
    ArrayRef<ParsedRawSyntaxNode> elements) {
  SourceLoc startLoc;
  unsigned length = 0;

  SourceLoc prevEndLoc;
  for (const auto &elem: elements) {
    if (elem.isNull() || elem.isMissing())
      continue;

    CharSourceRange range = elem.getRange();
    if (range.isValid()) {
      if (startLoc.isInvalid()) {
        startLoc = range.getStart();
      }
      length += range.getByteLength();

      assert((prevEndLoc.isInvalid() || range.getStart() == prevEndLoc)
             && "Non-contiguous child ranges?");
      prevEndLoc = range.getEnd();
    }
  }
  return CharSourceRange(startLoc, length);
}
#endif

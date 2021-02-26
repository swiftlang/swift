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

static ParsedRawSyntaxNode
getRecordedNode(ParsedRawSyntaxNode node, ParsedRawSyntaxRecorder &rec) {
  assert(!node.isNull() && !node.isRecorded());
  if (node.isDeferredLayout())
    return rec.recordRawSyntax(node.getKind(), node.getDeferredChildren());
  assert(node.isDeferredToken());
  CharSourceRange tokRange = node.getDeferredTokenRange();
  tok tokKind = node.getTokenKind();
  if (node.isMissing())
    return rec.recordMissingToken(tokKind, tokRange.getStart());
  return rec.recordToken(tokKind, tokRange, node.getDeferredLeadingTrivia(),
                         node.getDeferredTrailingTrivia());
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordRawSyntax(SyntaxKind kind,
                                         MutableArrayRef<ParsedRawSyntaxNode> elements) {
#ifndef NDEBUG
  ParsedRawSyntaxRecorder::verifyElementRanges(elements);
#endif
  CharSourceRange range;
  SmallVector<OpaqueSyntaxNode, 16> subnodes;
  if (!elements.empty()) {
    SourceLoc offset;
    unsigned length = 0;
    for (auto &subnode : elements) {
      CharSourceRange localRange;
      if (subnode.isNull()) {
        subnodes.push_back(nullptr);
      } else if (subnode.isRecorded()) {
        localRange = subnode.getRange();
        subnodes.push_back(subnode.takeOpaqueNode());
      } else {
        auto recorded = getRecordedNode(subnode.copyDeferred(), *this);
        localRange = recorded.getRange();
        subnodes.push_back(recorded.takeOpaqueNode());
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
  return ParsedRawSyntaxNode(n, range, kind, tok::unknown,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*IsMissing=*/false);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {}, range);
  return ParsedRawSyntaxNode(n, range, kind, tok::unknown,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*IsMissing=*/false);
}

/// Create a deferred layout node.
ParsedRawSyntaxNode ParsedRawSyntaxRecorder::makeDeferred(
    syntax::SyntaxKind k, MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
    SyntaxParsingContext &ctx) {
  CharSourceRange range;
  if (deferredNodes.empty()) {
    auto Data = new DeferredLayoutNode{{}};
    return ParsedRawSyntaxNode(Data, range, k, tok::NUM_TOKENS,
                               ParsedRawSyntaxNode::DataKind::DeferredLayout,
                               /*IsMissing=*/false);
  }
  ParsedRawSyntaxNode *newPtr =
      ctx.getScratchAlloc().Allocate<ParsedRawSyntaxNode>(deferredNodes.size());

#ifndef NDEBUG
  ParsedRawSyntaxRecorder::verifyElementRanges(deferredNodes);
#endif
  auto ptr = newPtr;
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

    // uninitialized move;
    ::new (static_cast<void *>(ptr++)) ParsedRawSyntaxNode(std::move(node));
  }
  auto Data = new DeferredLayoutNode{
      llvm::makeMutableArrayRef(newPtr, deferredNodes.size())};
  return ParsedRawSyntaxNode(Data, range, k, tok::NUM_TOKENS,
                             ParsedRawSyntaxNode::DataKind::DeferredLayout,
                             /*IsMissing=*/false);
}

/// Create a deferred token node.
ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferred(Token tok, StringRef leadingTrivia,
                                      StringRef trailingTrivia) {
  CharSourceRange tokRange = tok.getRange();
  auto Data =
      new DeferredTokenNode{tokRange.getStart(), tokRange.getByteLength(),
                            leadingTrivia, trailingTrivia};
  CharSourceRange RangeWithTrivia = CharSourceRange(
      tokRange.getStart().getAdvancedLoc(-leadingTrivia.size()),
      (unsigned)leadingTrivia.size() + tokRange.getByteLength() +
          (unsigned)trailingTrivia.size());
  return ParsedRawSyntaxNode(
      Data, RangeWithTrivia, SyntaxKind::Token, tok.getKind(),
      ParsedRawSyntaxNode::DataKind::DeferredToken, /*IsMissing=*/false);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::makeDeferredMissing(tok tokKind, SourceLoc loc) {
  auto Data =
      new DeferredTokenNode{loc, /*TokLength=*/0, /*leadingTrivia=*/StringRef(),
                            /*trailingTrivia=*/StringRef()};
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
  return ParsedRawSyntaxNode{n,
                             range,
                             kind,
                             tok::unknown,
                             ParsedRawSyntaxNode::DataKind::Recorded,
                             /*IsMissing=*/false};
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

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
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"

using namespace swift;
using namespace swift::syntax;

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordToken(const Token &tok,
                                     const ParsedTrivia &leadingTrivia,
                                     const ParsedTrivia &trailingTrivia) {
  return recordToken(tok.getKind(), tok.getRangeWithoutBackticks(),
                     leadingTrivia.Pieces, trailingTrivia.Pieces);
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordToken(tok tokKind, CharSourceRange tokRange,
                                ArrayRef<ParsedTriviaPiece> leadingTrivia,
                                ArrayRef<ParsedTriviaPiece> trailingTrivia) {
  size_t leadingTriviaLen = ParsedTriviaPiece::getTotalLength(leadingTrivia);
  size_t trailingTriviaLen = ParsedTriviaPiece::getTotalLength(trailingTrivia);
  SourceLoc offset = tokRange.getStart().getAdvancedLoc(-leadingTriviaLen);
  unsigned length = leadingTriviaLen + tokRange.getByteLength() +
      trailingTriviaLen;
  CharSourceRange range{offset, length};
  OpaqueSyntaxNode n = SPActions->recordToken(tokKind, leadingTrivia,
                                              trailingTrivia, range);
  return ParsedRawSyntaxNode{SyntaxKind::Token, tokKind, range, n};
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return ParsedRawSyntaxNode{SyntaxKind::Token, tokenKind, range, n};
}

static ParsedRawSyntaxNode
getRecordedNode(const ParsedRawSyntaxNode &node, ParsedRawSyntaxRecorder &rec) {
  if (node.isNull() || node.isRecorded())
    return node;
  if (node.isDeferredLayout())
    return rec.recordRawSyntax(node.getKind(), node.getDeferredChildren());
  assert(node.isDeferredToken());
  CharSourceRange tokRange = node.getDeferredTokenRangeWithoutBackticks();
  tok tokKind = node.getTokenKind();
  if (node.isMissing())
    return rec.recordMissingToken(tokKind, tokRange.getStart());
  return rec.recordToken(tokKind,tokRange,
                         node.getDeferredLeadingTriviaPieces(),
                         node.getDeferredTrailingTriviaPieces());
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordRawSyntax(SyntaxKind kind,
                                    ArrayRef<ParsedRawSyntaxNode> elements) {
  CharSourceRange range;
  SmallVector<OpaqueSyntaxNode, 16> subnodes;
  if (!elements.empty()) {
    SourceLoc offset;
    unsigned length = 0;
    for (const auto &elem : elements) {
      auto subnode = getRecordedNode(elem, *this);
      if (subnode.isNull()) {
        subnodes.push_back(nullptr);
      } else {
        subnodes.push_back(subnode.getOpaqueNode());
        auto range = subnode.getRecordedRange();
        if (range.isValid()) {
          if (offset.isInvalid())
            offset = range.getStart();
          length += subnode.getRecordedRange().getByteLength();
        }
      }
    }
    range = CharSourceRange{offset, length};
  }
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, subnodes, range);
  return ParsedRawSyntaxNode{kind, tok::unknown, range, n};
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {}, range);
  return ParsedRawSyntaxNode{kind, tok::unknown, range, n};
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
  return ParsedRawSyntaxNode{kind, tok::unknown, range, n};
}

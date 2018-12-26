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
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace swift::syntax;

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordToken(const Token &tok,
                                     const Trivia &leadingTrivia,
                                     const Trivia &trailingTrivia) {
  SourceLoc tokLoc = tok.getLoc();
  unsigned tokLength = tok.getLength();
  if (tok.isEscapedIdentifier()) {
    // Adjust to account for the backticks that are included in trivia.
    tokLoc = tokLoc.getAdvancedLoc(1);
    tokLength -= 2;
  }
  unsigned leadingTriviaLen = leadingTrivia.getTextLength();
  unsigned trailingTriviaLen = trailingTrivia.getTextLength();
  SourceLoc offset = tokLoc.getAdvancedLoc(-leadingTriviaLen);
  unsigned length = leadingTriviaLen + tokLength + trailingTriviaLen;
  CharSourceRange range{offset, length};
  OpaqueSyntaxNode n = SPActions->recordToken(tok, leadingTrivia,
                                              trailingTrivia, range);
  return ParsedRawSyntaxNode{SyntaxKind::Token, tok.getKind(), range, n};
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  CharSourceRange range{loc, 0};
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return ParsedRawSyntaxNode{SyntaxKind::Token, tokenKind, range, n};
}

static ParsedRawSyntaxNode
getRecordedNode(const ParsedRawSyntaxNode &node, ParsedRawSyntaxRecorder &rec) {
  if (node.isRecorded())
    return node;
  if (node.isDeferredLayout())
    return rec.recordRawSyntax(node.getKind(), node.getDeferredChildren());
  assert(node.isDeferredToken());
  const Token &tok = node.getToken();
  if (node.isMissing())
    return rec.recordMissingToken(tok.getKind(), tok.getLoc());
  return rec.recordToken(tok, node.getLeadingTrivia(),
                         node.getTrailingTrivia());
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
      subnodes.push_back(subnode.getOpaqueNode());
      if (!subnode.isNull()) {
        auto range = subnode.getRange();
        if (range.isValid()) {
          if (offset.isInvalid())
            offset = range.getStart();
          length += subnode.getRange().getByteLength();
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
  CharSourceRange range{loc, unsigned(length)};
  return ParsedRawSyntaxNode{kind, tok::unknown, range, n};
}

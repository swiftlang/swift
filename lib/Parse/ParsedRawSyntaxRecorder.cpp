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
ParsedRawSyntaxRecorder::recordMissingToken(tok tokenKind, SourceLoc loc) {
  OpaqueSyntaxNode n = SPActions->recordMissingToken(tokenKind, loc);
  return makeParsedRawSyntaxNode(n, SyntaxKind::Token, tokenKind,
                                 ParsedRawSyntaxNode::DataKind::Recorded,
                                 /*isMissing=*/true, CharSourceRange(loc, 0));
}

ParsedRawSyntaxNode
ParsedRawSyntaxRecorder::recordEmptyRawSyntaxCollection(SyntaxKind kind,
                                                        SourceLoc loc) {
  OpaqueSyntaxNode n = SPActions->recordRawSyntax(kind, {});
  return makeParsedRawSyntaxNode(n, kind, tok::unknown,
                                 ParsedRawSyntaxNode::DataKind::Recorded,
                                 /*IsMissing=*/false, CharSourceRange(loc, 0));
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

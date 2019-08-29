//===--- ParsedRawSyntaxNode.cpp - Parsed Raw Syntax Node -----------------===//
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

#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/SyntaxParsingContext.h"

using namespace swift;
using namespace swift::syntax;
using namespace llvm;

ParsedRawSyntaxNode
ParsedRawSyntaxNode::makeDeferred(SyntaxKind k,
                                  ArrayRef<ParsedRawSyntaxNode> deferredNodes,
                                  SyntaxParsingContext &ctx) {
  if (deferredNodes.empty()) {
    return ParsedRawSyntaxNode(k, {});
  }
  ParsedRawSyntaxNode *newPtr =
    ctx.getScratchAlloc().Allocate<ParsedRawSyntaxNode>(deferredNodes.size());
  std::uninitialized_copy(deferredNodes.begin(), deferredNodes.end(), newPtr);
  return ParsedRawSyntaxNode(k, makeArrayRef(newPtr, deferredNodes.size()));
}

ParsedRawSyntaxNode
ParsedRawSyntaxNode::makeDeferred(Token tok,
                                  const ParsedTrivia &leadingTrivia,
                                  const ParsedTrivia &trailingTrivia,
                                  SyntaxParsingContext &ctx) {
  CharSourceRange tokRange = tok.getRangeWithoutBackticks();
  size_t piecesCount = leadingTrivia.size() + trailingTrivia.size();
  ParsedTriviaPiece *piecesPtr = nullptr;
  if (piecesCount > 0) {
    piecesPtr = ctx.getScratchAlloc().Allocate<ParsedTriviaPiece>(piecesCount);
    std::uninitialized_copy(leadingTrivia.begin(), leadingTrivia.end(),
                            piecesPtr);
    std::uninitialized_copy(trailingTrivia.begin(), trailingTrivia.end(),
                            piecesPtr + leadingTrivia.size());
  }
  return ParsedRawSyntaxNode(tok.getKind(), tokRange.getStart(),
                             tokRange.getByteLength(), piecesPtr,
                             leadingTrivia.size(), trailingTrivia.size());
}

void ParsedRawSyntaxNode::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
  llvm::errs() << '\n';
}

void ParsedRawSyntaxNode::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  for (decltype(Indent) i = 0; i < Indent; ++i)
    OS << ' ';
  OS << '(';
  dumpSyntaxKind(OS, getKind());

  switch (DK) {
    case DataKind::Null:
      OS << "<NULL>";
      break;
    case DataKind::Recorded:
      OS << " [recorded] ";
      if (isToken()) {
        dumpTokenKind(OS, getTokenKind());
      } else {
        OS << "<layout>";
      }
      break;
    case DataKind::DeferredLayout:
      OS << " [deferred]";
      for (const auto &child : getDeferredChildren()) {
        OS << "\n";
        child.dump(OS, Indent + 2);
      }
      break;
    case DataKind::DeferredToken:
      OS << " [deferred] ";
      dumpTokenKind(OS, getTokenKind());
      break;
  }
  OS << ')';
}

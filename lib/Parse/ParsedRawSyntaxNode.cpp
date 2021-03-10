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

tok ParsedRawSyntaxNode::getTokenKind(const SyntaxParseActions *Actions) const {
  return Actions->getTokenKind(getUnsafeRecordedOrDeferredNode());
}

syntax::SyntaxKind
ParsedRawSyntaxNode::getKind(const SyntaxParseActions *Actions) const {
  return Actions->getSyntaxKind(getUnsafeRecordedOrDeferredNode());
}

bool ParsedRawSyntaxNode::isMissing(const SyntaxParseActions *Actions) const {
  return Actions->isMissing(getUnsafeRecordedOrDeferredNode());
}

ParsedRawSyntaxNode ParsedRawSyntaxNode::getDeferredChild(
    size_t ChildIndex, const SyntaxParsingContext *SyntaxContext) const {
  assert(isDeferredLayout());
  return SyntaxContext->getRecorder().getDeferredChild(*this, ChildIndex);
}

size_t ParsedRawSyntaxNode::getDeferredNumChildren(
    const SyntaxParsingContext *SyntaxContext) const {
  assert(isDeferredLayout());
  return SyntaxContext->getRecorder().getDeferredNumChildren(*this);
}

void ParsedRawSyntaxNode::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
  llvm::errs() << '\n';
}

void ParsedRawSyntaxNode::dump(llvm::raw_ostream &OS,
                               const SyntaxParsingContext *Context,
                               unsigned Indent) const {
  for (decltype(Indent) i = 0; i < Indent; ++i)
    OS << ' ';
  OS << '(';

  if (Context) {
    const SyntaxParseActions *Actions = Context->getActions();
    switch (getDataKind()) {
    case DataKind::Null:
      OS << "<NULL>";
      break;
    case DataKind::Recorded:
      dumpSyntaxKind(OS, getKind(Actions));
      OS << " [recorded] ";
      if (isToken(Actions)) {
        dumpTokenKind(OS, getTokenKind(Actions));
      } else {
        OS << "<layout>";
      }
      break;
    case DataKind::DeferredLayout: {
      dumpSyntaxKind(OS, getKind(Actions));
      OS << " [deferred]";
      size_t numChildren = getDeferredNumChildren(Context);
      for (size_t i = 0; i < numChildren; ++i) {
        auto child = getDeferredChild(i, Context);
        OS << "\n";
        child.dump(OS, Context, Indent + 2);
      }
      break;
    }
    case DataKind::DeferredToken:
      dumpSyntaxKind(OS, getKind(Actions));
      OS << " [deferred] ";
      dumpTokenKind(OS, getTokenKind(Actions));
      break;
    }
  } else {
    switch (getDataKind()) {
    case DataKind::Null:
      OS << "<NULL>";
      break;
    case DataKind::Recorded:
      OS << " [recorded] <unknown type>";
      break;
    case DataKind::DeferredLayout:
      OS << " [deferred layout] <unknown children>";
      break;
    case DataKind::DeferredToken:
      OS << " [deferred token] <unknown kind>";
      break;
    }
  }
  OS << ')';
}

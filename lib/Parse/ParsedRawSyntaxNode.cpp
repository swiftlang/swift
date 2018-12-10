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

using namespace swift;
using namespace swift::syntax;

ParsedRawSyntaxNode
ParsedRawSyntaxNode::makeDeferredMissing(tok kind, SourceLoc loc) {
  // Pass appropriate text for the missing token to have the provided location
  // but zero length.
  StringRef tokText = CharSourceRange{loc, 0}.str();
  auto raw = ParsedRawSyntaxNode::makeDeferred(Token{kind, tokText}, {}, {});
  raw.IsMissing = true;
  return raw;
}

void ParsedRawSyntaxNode::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
  llvm::errs() << '\n';
}

void ParsedRawSyntaxNode::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  auto indent = [&](unsigned Amount) {
    for (decltype(Amount) i = 0; i < Amount; ++i) {
      OS << ' ';
    }
  };

  indent(Indent);

  if (isNull()) {
    OS << "(<NULL>)";
    return;
  }

  OS << '(';
  dumpSyntaxKind(OS, getKind());

  if (isToken()) {
    OS << ' ' << getTokenText(getTokenKind());

  } else {
    if (isRecorded()) {
      OS << " [recorded]";
    } else if (isDeferredLayout()) {
      for (const auto &child : getDeferredChildren()) {
        OS << "\n";
        child.dump(OS, Indent + 1);
      }
    } else {
      assert(isDeferredToken());
      OS << " [deferred token]";
    }
  }
  OS << ')';
}

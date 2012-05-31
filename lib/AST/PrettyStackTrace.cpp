//===--- PrettyStackTrace.cpp - Swift-specific PrettyStackTraceEntries ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements several Swift-specific implementations of
//  PrettyStackTraceEntry.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/Basic/Optional.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

struct DecomposedLoc {
  llvm::MemoryBuffer *Buffer;
  unsigned Line;
  unsigned Column;
};

static Optional<DecomposedLoc> decompose(llvm::SourceMgr &SM, SourceLoc loc) {
  if (!loc.isValid()) return Nothing;

  int bufferIndex = SM.FindBufferContainingLoc(loc.Value);
  if (bufferIndex == -1) return Nothing;

  DecomposedLoc result;
  result.Buffer = SM.getBufferInfo(bufferIndex).Buffer;
  result.Line = SM.FindLineNumber(loc.Value, bufferIndex);

  const char *lineStart = loc.Value.getPointer();
  while (lineStart != result.Buffer->getBufferStart() &&
         lineStart[-1] != '\n' && lineStart[-1] != '\r')
    --lineStart;
  result.Column = loc.Value.getPointer() - lineStart;

  return result;
}

static void printDecomposedLoc(llvm::raw_ostream &out,
                               const DecomposedLoc &loc) {
  out << loc.Buffer->getBufferIdentifier()
      << ":" << loc.Line << ':' << loc.Column;
}

static void printDecomposedLoc(llvm::raw_ostream &out,
                               const Optional<DecomposedLoc> &loc) {
  if (!loc) {
    out << "<<invalid location>>";
    return;
  }

  return printDecomposedLoc(out, loc.getValue());
}

void swift::printSourceLoc(llvm::raw_ostream &out, SourceLoc loc,
                           ASTContext &Context) {
  printDecomposedLoc(out, decompose(Context.SourceMgr, loc));
}

static void printSourceRange(llvm::raw_ostream &out, SourceRange range,
                             ASTContext &Context) {
  llvm::SourceMgr &SM = Context.SourceMgr;
  Optional<DecomposedLoc> start = decompose(SM, range.Start);
  Optional<DecomposedLoc> end = decompose(SM, range.End);

  // Use a unified message if both locations are invalid.
  if (!start && !end) {
    out << "<<invalid source range>>";
    return;
  }

  // Print the start location as normal.
  printDecomposedLoc(out, start);
  out << '-';

  // Only print the non-matching information from the second location.
  if (!start || !end || start.getValue().Buffer != end.getValue().Buffer) {
    printDecomposedLoc(out, end);
    return;
  }

  if (start.getValue().Line != end.getValue().Line) {
    out << end.getValue().Line << ':';
  }

  out << end.getValue().Column;
}

void PrettyStackTraceDecl::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheDecl) {
    out << "NULL declaration!\n";
    return;
  }

  if (ValueDecl *named = dyn_cast<ValueDecl>(TheDecl)) {
    if (named->getName().get())
      out << '\'' << named->getName() << '\'';
    else
      out << "'anonname=" << (const void*)named << '\'';
  } else {
    out << "declaration";
  }
  out << " at ";
  printSourceLoc(out, TheDecl->getStartLoc(), TheDecl->getASTContext());
  out << '\n';
}

void PrettyStackTraceExpr::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheExpr) {
    out << "NULL expression!\n";
    return;
  }

  if (isa<FuncExpr>(TheExpr)) {
    out << "function";
  } else {
    out << "expression";
  }
  out << " at ";
  printSourceRange(out, TheExpr->getSourceRange(), Context);
  out << '\n';
}

void PrettyStackTraceStmt::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheStmt) {
    out << "NULL statement!\n";
    return;
  }

  out << "statement at ";
  printSourceRange(out, TheStmt->getSourceRange(), Context);
  out << '\n';
}

void PrettyStackTraceLocation::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " starting at ";
  printSourceLoc(out, Loc, Context);
}

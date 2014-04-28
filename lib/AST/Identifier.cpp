//===--- Identifier.cpp - Uniqued Identifier --------------------*- C++ -*-===//
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
// This file implements the Identifier interface.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Identifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ConvertUTF.h"
using namespace swift;


raw_ostream &llvm::operator<<(raw_ostream &OS, Identifier I) {
  if (I.get() == 0) return OS << "_";
  return OS << I.get();
}

raw_ostream &llvm::operator<<(raw_ostream &OS, DeclName I) {
  if (I.isSimpleName())
    return OS << I.getBaseName();

  OS << I.getBaseName() << "(";
  for (auto c : I.getArgumentNames()) {
    if (!c.empty())
      OS << c;
    OS << ':';
  }
  OS << ")";
  return OS;
}

raw_ostream &llvm::operator<<(raw_ostream &OS, swift::ObjCSelector S) {
  unsigned n = S.getNumArgs();
  if (n == 0) {
    OS << S.getSelectorPieces()[0];
    return OS;
  }

  for (auto piece : S.getSelectorPieces()) {
    if (!piece.empty())
      OS << piece;
    OS << ":";
  }
  return OS;
}

bool Identifier::isOperatorSlow() const {
  StringRef data = str();
  auto *s = reinterpret_cast<UTF8 const *>(data.begin()),
  *end = reinterpret_cast<UTF8 const *>(data.end());
  UTF32 codePoint;
  ConversionResult res = llvm::convertUTF8Sequence(&s, end, &codePoint,
                                                   strictConversion);
  assert(res == conversionOK && "invalid UTF-8 in identifier?!");
  (void)res;
  return !empty() && isOperatorStartCodePoint(codePoint);
}

void DeclName::dump() const {
  llvm::errs() << *this << "\n";
}

ObjCSelector::ObjCSelector(ASTContext &ctx, unsigned numArgs,
                           ArrayRef<Identifier> pieces) {
  if (numArgs == 0) {
    assert(pieces.size() == 1 && "No-argument selector requires one piece");
    Storage = DeclName(pieces[0]);
    return;
  }

  assert(numArgs == pieces.size() && "Wrong number of selector pieces");
  Storage = DeclName(ctx, Identifier(), pieces);
}

StringRef ObjCSelector::getString(llvm::SmallVectorImpl<char> &scratch) const {
  // Fast path for zero-argument selectors.
  if (getNumArgs() == 0) {
    auto name = getSelectorPieces()[0];
    if (name.empty())
      return "";
    return name.str();
  }

  scratch.clear();
  llvm::raw_svector_ostream os(scratch);
  os << *this;
  return os.str();
}

void ObjCSelector::dump() const {
  llvm::errs() << *this << "\n";
}



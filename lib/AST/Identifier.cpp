//===--- Identifier.cpp - Uniqued Identifier ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the Identifier interface.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Identifier.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ConvertUTF.h"
using namespace swift;

void *DeclBaseName::SubscriptIdentifierData =
    &DeclBaseName::SubscriptIdentifierData;

raw_ostream &llvm::operator<<(raw_ostream &OS, Identifier I) {
  if (I.get() == nullptr)
    return OS << "_";
  return OS << I.get();
}

raw_ostream &llvm::operator<<(raw_ostream &OS, DeclBaseName I) {
  if (I.empty())
    return OS << "_";
  return OS << I.userFacingStr();
}

raw_ostream &llvm::operator<<(raw_ostream &OS, DeclName I) {
  if (I.isSimpleName())
    return OS << I.getBaseName();

  OS << I.getBaseName() << "(";
  for (auto c : I.getArgumentNames()) {
    OS << c << ':';
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
  auto *s = reinterpret_cast<llvm::UTF8 const *>(data.begin()),
  *end = reinterpret_cast<llvm::UTF8 const *>(data.end());
  llvm::UTF32 codePoint;
  llvm::ConversionResult res =
    llvm::convertUTF8Sequence(&s, end, &codePoint, llvm::strictConversion);
  assert(res == llvm::conversionOK && "invalid UTF-8 in identifier?!");
  (void)res;
  return !empty() && isOperatorStartCodePoint(codePoint);
}

int Identifier::compare(Identifier other) const {
  // Handle empty identifiers.
  if (empty() || other.empty()) {
    if (empty() != other.empty()) {
      return other.empty() ? -1 : 1;
    }

    return 0;
  }

  return str().compare(other.str());
}

int DeclName::compare(DeclName other) const {
  // Compare base names.
  if (int result = getBaseName().compare(other.getBaseName()))
    return result;

  // Compare argument names.
  auto argNames = getArgumentNames();
  auto otherArgNames = other.getArgumentNames();
  for (unsigned i = 0, n = std::min(argNames.size(), otherArgNames.size());
       i != n; ++i) {
    if (int result = argNames[i].compare(otherArgNames[i]))
      return result;
  }

  if (argNames.size() == otherArgNames.size())
    return 0;

  return argNames.size() < otherArgNames.size() ? -1 : 1;
}

void DeclName::dump() const {
  llvm::errs() << *this << "\n";
}

StringRef DeclName::getString(llvm::SmallVectorImpl<char> &scratch,
                              bool skipEmptyArgumentNames) const {
  {
    llvm::raw_svector_ostream out(scratch);
    print(out, skipEmptyArgumentNames);
  }

  return StringRef(scratch.data(), scratch.size());
}

llvm::raw_ostream &DeclName::print(llvm::raw_ostream &os,
                                   bool skipEmptyArgumentNames) const {
  // Print the base name.
  os << getBaseName();

  // If this is a simple name, we're done.
  if (isSimpleName())
    return os;

  if (skipEmptyArgumentNames) {
    // If there is more than one argument yet none of them have names,
    // we're done.
    if (getArgumentNames().size() > 0) {
      bool anyNonEmptyNames = false;
      for (auto c : getArgumentNames()) {
        if (!c.empty()) {
          anyNonEmptyNames = true;
          break;
        }
      }

      if (!anyNonEmptyNames)
        return os;
    }
  }

  // Print the argument names.
  os << "(";
  for (auto c : getArgumentNames()) {
    os << c << ':';
  }
  os << ")";
  return os;

}

llvm::raw_ostream &DeclName::printPretty(llvm::raw_ostream &os) const {
  return print(os, /*skipEmptyArgumentNames=*/true);
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



//===--- RawSyntax.cpp - Swift Raw Syntax Implementation ------------------===//
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

#include "swift/Basic/ColorUtils.h"
#include "swift/Syntax/RawSyntax.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>

using llvm::dyn_cast;
using namespace swift;
using namespace swift::syntax;

namespace {
static bool isTrivialSyntaxKind(SyntaxKind Kind) {
  if (isUnknownKind(Kind))
    return true;
  if (isCollectionKind(Kind))
    return true;
  switch(Kind) {
  case SyntaxKind::SourceFile:
  case SyntaxKind::TopLevelCodeDecl:
  case SyntaxKind::ExpressionStmt:
  case SyntaxKind::DeclarationStmt:
    return true;
  default:
    return false;
  }
}

static void printSyntaxKind(SyntaxKind Kind, llvm::raw_ostream &OS,
                            SyntaxPrintOptions Opts, bool Open) {
  std::unique_ptr<swift::OSColor> Color;
  if (Opts.Visual) {
    Color.reset(new swift::OSColor(OS, llvm::raw_ostream::GREEN));
  }
  OS << "<";
  if (!Open)
    OS << "/";
  dumpSyntaxKind(OS, Kind);
  OS << ">";
}

static void dumpTokenKind(llvm::raw_ostream &OS, tok Kind) {
  switch (Kind) {
#define TOKEN(X)                                                               \
  case tok::X:                                                                 \
    OS << #X;                                                                  \
    break;
#include "swift/Syntax/TokenKinds.def"
  case tok::NUM_TOKENS:
    OS << "NUM_TOKENS (unset)";
    break;
  }
}

} // end of anonymous namespace

RawSyntax::RawSyntax(SyntaxKind Kind, ArrayRef<RC<RawSyntax>> Layout,
                     SourcePresence Presence) {
  assert(Kind != SyntaxKind::Token &&
         "'token' syntax node must be constructed with dedicated constructor");
  Bits.Kind = unsigned(Kind);
  Bits.Presence = unsigned(Presence);
  Bits.NumChildren = Layout.size();

  // Initialize layout data.
  std::uninitialized_copy(Layout.begin(), Layout.end(),
                          getTrailingObjects<RC<RawSyntax>>());
}

RawSyntax::RawSyntax(tok TokKind, OwnedString Text, SourcePresence Presence,
                     ArrayRef<TriviaPiece> LeadingTrivia,
                     ArrayRef<TriviaPiece> TrailingTrivia) {
  Bits.Kind = unsigned(SyntaxKind::Token);
  Bits.Presence = unsigned(Presence);
  Bits.TokenKind = unsigned(TokKind);
  Bits.NumLeadingTrivia = LeadingTrivia.size();
  Bits.NumTrailingTrivia = TrailingTrivia.size();

  // Initialize token text.
  ::new (static_cast<void *>(getTrailingObjects<OwnedString>()))
      OwnedString(Text);
  // Initialize leading trivia.
  std::uninitialized_copy(LeadingTrivia.begin(), LeadingTrivia.end(),
                          getTrailingObjects<TriviaPiece>());
  // Initialize trailing trivia.
  std::uninitialized_copy(TrailingTrivia.begin(), TrailingTrivia.end(),
                          getTrailingObjects<TriviaPiece>() +
                              Bits.NumLeadingTrivia);
}

RawSyntax::~RawSyntax() {
  if (isToken()) {
    getTrailingObjects<OwnedString>()->~OwnedString();
    for (auto trivia : getLeadingTrivia())
      trivia.~TriviaPiece();
    for (auto trivia : getTrailingTrivia())
      trivia.~TriviaPiece();
  } else {
    for (auto child : getLayout())
      child.~RC<RawSyntax>();
  }
}

RC<RawSyntax> RawSyntax::make(SyntaxKind Kind,
                                ArrayRef<RC<RawSyntax>> Layout,
                                SourcePresence Presence) {
  auto size = totalSizeToAlloc<RC<RawSyntax>, OwnedString, TriviaPiece>(
      Layout.size(), 0, 0);
  void *data = ::operator new(size);
  return RC<RawSyntax>(new (data) RawSyntax(Kind, Layout, Presence));
}

RC<RawSyntax> RawSyntax::make(tok TokKind, OwnedString Text,
                              SourcePresence Presence,
                              ArrayRef<TriviaPiece> LeadingTrivia,
                              ArrayRef<TriviaPiece> TrailingTrivia) {
  auto size = totalSizeToAlloc<RC<RawSyntax>, OwnedString, TriviaPiece>(
      0, 1, LeadingTrivia.size() + TrailingTrivia.size());
  void *data = ::operator new(size);
  return RC<RawSyntax>(new (data) RawSyntax(TokKind, Text, Presence,
                                            LeadingTrivia, TrailingTrivia));
}

RC<RawSyntax> RawSyntax::append(RC<RawSyntax> NewLayoutElement) const {
  std::vector<RC<RawSyntax>> NewLayout;
  std::copy(getLayout().begin(), getLayout().end(),
            std::back_inserter(NewLayout));
  NewLayout.push_back(NewLayoutElement);
  return RawSyntax::make(getKind(), NewLayout, SourcePresence::Present);
}

RC<RawSyntax> RawSyntax::replaceChild(CursorIndex Index,
                                      RC<RawSyntax> NewLayoutElement) const {
  auto Layout = getLayout();
  std::vector<RC<RawSyntax>> NewLayout;

  std::copy(Layout.begin(), Layout.begin() + Index,
            std::back_inserter(NewLayout));

  NewLayout.push_back(NewLayoutElement);

  std::copy(Layout.begin() + Index + 1, Layout.end(),
            std::back_inserter(NewLayout));

  return RawSyntax::make(getKind(), NewLayout, getPresence());
}

llvm::Optional<AbsolutePosition>
RawSyntax::accumulateAbsolutePosition(AbsolutePosition &Pos) const {
  llvm::Optional<AbsolutePosition> Ret;
  if (isToken()) {
    if (isMissing())
      return None;
    for (auto &Leader : getLeadingTrivia())
      Leader.accumulateAbsolutePosition(Pos);
    Ret = Pos;
    Pos.addText(getTokenText());
    for (auto &Trailer : getTrailingTrivia())
      Trailer.accumulateAbsolutePosition(Pos);
  } else {
    for (auto &Child : getLayout()) {
      auto Result = Child->accumulateAbsolutePosition(Pos);
      if (!Ret && Result)
        Ret = Result;
    }
  }
  return Ret;
}

void RawSyntax::print(llvm::raw_ostream &OS, SyntaxPrintOptions Opts) const {
  if (isMissing())
    return;

  if (isToken()) {
    for (const auto &Leader : getLeadingTrivia())
      Leader.print(OS);

    OS << getTokenText();

    for (const auto &Trailer : getTrailingTrivia())
      Trailer.print(OS);
  } else {
    auto Kind = getKind();
    const bool PrintKind = Opts.PrintSyntaxKind && (Opts.PrintTrivialNodeKind ||
                                                    !isTrivialSyntaxKind(Kind));
    if (PrintKind)
      printSyntaxKind(Kind, OS, Opts, true);

    for (const auto &LE : getLayout())
      LE->print(OS, Opts);

    if (PrintKind)
      printSyntaxKind(Kind, OS, Opts, false);
  }
}

void RawSyntax::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
}

void RawSyntax::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  auto indent = [&](unsigned Amount) {
    for (decltype(Amount) i = 0; i < Amount; ++i) {
      OS << ' ';
    }
  };

  indent(Indent);
  OS << '(';
  dumpSyntaxKind(OS, getKind());

  if (isMissing())
    OS << " [missing] ";

  if (isToken()) {
    OS << " ";
    dumpTokenKind(OS, getTokenKind());

    for (auto &Leader : getLeadingTrivia()) {
      OS << "\n";
      Leader.dump(OS, Indent + 1);
    }

    OS << "\n";
    indent(Indent + 1);
    OS << "(text=\"";
    OS.write_escaped(getTokenText(), /*UseHexEscapes=*/true);
    OS << "\")";

    for (auto &Trailer : getTrailingTrivia()) {
      OS << "\n";
      Trailer.dump(OS, Indent + 1);
    }
  } else {
    for (auto &Child : getLayout()) {
      OS << "\n";
      Child->dump(OS, Indent + 1);
    }
  }
  OS << ')';
}

void AbsolutePosition::printLineAndColumn(llvm::raw_ostream &OS) const {
  OS << getLine() << ':' << getColumn();
}

void AbsolutePosition::dump(llvm::raw_ostream &OS) const {
  OS << "(absolute_position ";
  OS << "offset=" << getOffset() << " ";
  OS << "line=" << getLine() << " ";
  OS << "column=" << getColumn();
  OS << ')';
}

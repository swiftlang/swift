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
#include "swift/Syntax/SyntaxArena.h"
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
  case SyntaxKind::CodeBlockItem:
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

unsigned RawSyntax::NextFreeNodeId = 1;

RawSyntax::RawSyntax(SyntaxKind Kind, ArrayRef<RC<RawSyntax>> Layout,
                     SourcePresence Presence, const RC<SyntaxArena> &Arena,
                     llvm::Optional<unsigned> NodeId) {
  assert(Kind != SyntaxKind::Token &&
         "'token' syntax node must be constructed with dedicated constructor");

  RefCount = 0;

  if (NodeId.hasValue()) {
    this->NodeId = NodeId.getValue();
    NextFreeNodeId = std::max(this->NodeId + 1, NextFreeNodeId);
  } else {
    this->NodeId = NextFreeNodeId++;
  }
  Bits.Common.Kind = unsigned(Kind);
  Bits.Common.Presence = unsigned(Presence);
  Bits.Layout.NumChildren = Layout.size();
  Bits.Layout.TextLength = UINT32_MAX;

  this->Arena = Arena;

  // Initialize layout data.
  std::uninitialized_copy(Layout.begin(), Layout.end(),
                          getTrailingObjects<RC<RawSyntax>>());
}

RawSyntax::RawSyntax(tok TokKind, OwnedString Text,
                     ArrayRef<TriviaPiece> LeadingTrivia,
                     ArrayRef<TriviaPiece> TrailingTrivia,
                     SourcePresence Presence, const RC<SyntaxArena> &Arena,
                     llvm::Optional<unsigned> NodeId) {
  RefCount = 0;

  if (NodeId.hasValue()) {
    this->NodeId = NodeId.getValue();
    NextFreeNodeId = std::max(this->NodeId + 1, NextFreeNodeId);
  } else {
    this->NodeId = NextFreeNodeId++;
  }
  Bits.Common.Kind = unsigned(SyntaxKind::Token);
  Bits.Common.Presence = unsigned(Presence);
  Bits.Token.TokenKind = unsigned(TokKind);
  Bits.Token.NumLeadingTrivia = LeadingTrivia.size();
  Bits.Token.NumTrailingTrivia = TrailingTrivia.size();

  this->Arena = Arena;

  // Initialize token text.
  ::new (static_cast<void *>(getTrailingObjects<OwnedString>()))
      OwnedString(Text);
  // Initialize leading trivia.
  std::uninitialized_copy(LeadingTrivia.begin(), LeadingTrivia.end(),
                          getTrailingObjects<TriviaPiece>());
  // Initialize trailing trivia.
  std::uninitialized_copy(TrailingTrivia.begin(), TrailingTrivia.end(),
                          getTrailingObjects<TriviaPiece>() +
                              Bits.Token.NumLeadingTrivia);
}

RawSyntax::~RawSyntax() {
  if (isToken()) {
    getTrailingObjects<OwnedString>()->~OwnedString();
    for (auto &trivia : getLeadingTrivia())
      trivia.~TriviaPiece();
    for (auto &trivia : getTrailingTrivia())
      trivia.~TriviaPiece();
  } else {
    for (auto &child : getLayout())
      child.~RC<RawSyntax>();
  }
}

RC<RawSyntax> RawSyntax::make(SyntaxKind Kind, ArrayRef<RC<RawSyntax>> Layout,
                              SourcePresence Presence,
                              const RC<SyntaxArena> &Arena,
                              llvm::Optional<unsigned> NodeId) {
  auto size = totalSizeToAlloc<RC<RawSyntax>, OwnedString, TriviaPiece>(
      Layout.size(), 0, 0);
  void *data = Arena ? Arena->Allocate(size, alignof(RawSyntax))
                     : ::operator new(size);
  return RC<RawSyntax>(
      new (data) RawSyntax(Kind, Layout, Presence, Arena, NodeId));
}

RC<RawSyntax> RawSyntax::make(tok TokKind, OwnedString Text,
                              ArrayRef<TriviaPiece> LeadingTrivia,
                              ArrayRef<TriviaPiece> TrailingTrivia,
                              SourcePresence Presence,
                              const RC<SyntaxArena> &Arena,
                              llvm::Optional<unsigned> NodeId) {
  auto size = totalSizeToAlloc<RC<RawSyntax>, OwnedString, TriviaPiece>(
      0, 1, LeadingTrivia.size() + TrailingTrivia.size());
  void *data = Arena ? Arena->Allocate(size, alignof(RawSyntax))
                     : ::operator new(size);
  return RC<RawSyntax>(new (data) RawSyntax(TokKind, Text, LeadingTrivia,
                                            TrailingTrivia, Presence,
                                            Arena, NodeId));
}

RC<RawSyntax> RawSyntax::append(RC<RawSyntax> NewLayoutElement) const {
  auto Layout = getLayout();
  std::vector<RC<RawSyntax>> NewLayout;
  NewLayout.reserve(Layout.size() + 1);
  std::copy(Layout.begin(), Layout.end(), std::back_inserter(NewLayout));
  NewLayout.push_back(NewLayoutElement);
  return RawSyntax::make(getKind(), NewLayout, SourcePresence::Present);
}

RC<RawSyntax> RawSyntax::replaceChild(CursorIndex Index,
                                      RC<RawSyntax> NewLayoutElement) const {
  auto Layout = getLayout();
  std::vector<RC<RawSyntax>> NewLayout;
  NewLayout.reserve(Layout.size());

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
      if (!Child)
        continue;
      auto Result = Child->accumulateAbsolutePosition(Pos);
      if (!Ret && Result)
        Ret = Result;
    }
  }
  return Ret;
}

bool RawSyntax::accumulateLeadingTrivia(AbsolutePosition &Pos) const {
 if (isToken()) {
    if (!isMissing()) {
      for (auto &Leader: getLeadingTrivia())
        Leader.accumulateAbsolutePosition(Pos);
      return true;
    }
  } else {
    for (auto &Child: getLayout()) {
      if (!Child || Child->isMissing())
        continue;
      if (Child->accumulateLeadingTrivia(Pos))
        return true;
    }
  }
  return false;
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
      if (LE)
        LE->print(OS, Opts);

    if (PrintKind)
      printSyntaxKind(Kind, OS, Opts, false);
  }
}

void RawSyntax::dump() const {
  dump(llvm::errs(), /*Indent*/ 0);
  llvm::errs() << '\n';
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
      if (!Child)
        continue;
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

void RawSyntax::Profile(llvm::FoldingSetNodeID &ID, tok TokKind,
                        OwnedString Text, ArrayRef<TriviaPiece> LeadingTrivia,
                        ArrayRef<TriviaPiece> TrailingTrivia) {
  ID.AddInteger(unsigned(TokKind));
  switch (TokKind) {
#define TOKEN_DEFAULT(NAME) case tok::NAME:
#define PUNCTUATOR(NAME, X) TOKEN_DEFAULT(NAME)
#define KEYWORD(KW) TOKEN_DEFAULT(kw_##KW)
#define POUND_KEYWORD(KW) TOKEN_DEFAULT(pound_##KW)
#include "swift/Syntax/TokenKinds.def"
    break;
  default:
    ID.AddString(Text.str());
    break;
  }
  for (auto &Piece : LeadingTrivia)
    Piece.Profile(ID);
  for (auto &Piece : TrailingTrivia)
    Piece.Profile(ID);
}

llvm::raw_ostream &llvm::operator<<(raw_ostream &OS, AbsolutePosition Pos) {
  Pos.printLineAndColumn(OS);
  return OS;
}

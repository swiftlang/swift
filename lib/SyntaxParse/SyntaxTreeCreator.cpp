//===--- SyntaxTreeCreator.cpp - Syntax Tree Creation  ----------*- C++ -*-===//
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

#include "swift/SyntaxParse/SyntaxTreeCreator.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxVisitor.h"
#include "swift/Syntax/Trivia.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Parse/Token.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/OwnedString.h"
#include "RawSyntaxTokenCache.h"

using namespace swift;
using namespace swift::syntax;

static RC<RawSyntax> transferOpaqueNode(OpaqueSyntaxNode opaqueN) {
  if (!opaqueN)
    return nullptr;
  RC<RawSyntax> raw{(RawSyntax *)opaqueN};
  raw->Release(); // -1 since it's transfer of ownership.
  return raw;
}

SyntaxTreeCreator::SyntaxTreeCreator(SourceManager &SM, unsigned bufferID,
                                     SyntaxParsingCache *syntaxCache,
                                     RC<syntax::SyntaxArena> arena)
    : SM(SM), BufferID(bufferID),
      Arena(std::move(arena)),
      SyntaxCache(syntaxCache),
      TokenCache(new RawSyntaxTokenCache()) {
}

SyntaxTreeCreator::~SyntaxTreeCreator() = default;

namespace {
/// This verifier traverses a syntax node to emit proper diagnostics.
class SyntaxVerifier: public SyntaxVisitor {
  SourceManager &SourceMgr;
  unsigned BufferID;
  DiagnosticEngine &Diags;

  template<class T>
  SourceLoc getSourceLoc(T Node) {
    return SourceMgr.getLocForOffset(BufferID,
      Node.getAbsolutePosition().getOffset());
  }
public:
  SyntaxVerifier( SourceManager &SM, unsigned bufID, DiagnosticEngine &diags)
    : SourceMgr(SM), BufferID(bufID), Diags(diags) {}

  void visit(UnknownDeclSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "declaration");
    visitChildren(Node);
  }
  void visit(UnknownExprSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "expression");
    visitChildren(Node);
  }
  void visit(UnknownStmtSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "statement");
    visitChildren(Node);
  }
  void visit(UnknownTypeSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "type");
    visitChildren(Node);
  }
  void visit(UnknownPatternSyntax Node) override {
    Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                   "pattern");
    visitChildren(Node);
  }
  void verify(Syntax Node) {
    Node.accept(*this);
  }
};
} // anonymous namespace

void SyntaxTreeCreator::acceptSyntaxRoot(OpaqueSyntaxNode rootN,
                                         SourceFile &SF) {
  auto raw = transferOpaqueNode(rootN);
  SF.setSyntaxRoot(make<SourceFileSyntax>(raw));

  // Verify the tree if specified.
  if (SF.getASTContext().LangOpts.VerifySyntaxTree) {
    ASTContext &ctx = SF.getASTContext();
    SyntaxVerifier Verifier(ctx.SourceMgr, SF.getBufferID().getValue(),
                            ctx.Diags);
    Verifier.verify(SF.getSyntaxRoot());
  }
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordToken(tok tokenKind,
                               ArrayRef<ParsedTriviaPiece> leadingTriviaPieces,
                               ArrayRef<ParsedTriviaPiece> trailingTriviaPieces,
                               CharSourceRange range) {
  size_t leadingTriviaLen =
    ParsedTriviaPiece::getTotalLength(leadingTriviaPieces);
  size_t trailingTriviaLen =
    ParsedTriviaPiece::getTotalLength(trailingTriviaPieces);
  SourceLoc tokLoc = range.getStart().getAdvancedLoc(leadingTriviaLen);
  unsigned tokLength = range.getByteLength() -
      leadingTriviaLen - trailingTriviaLen;
  CharSourceRange tokRange = CharSourceRange{tokLoc, tokLength};
  SourceLoc leadingTriviaLoc = range.getStart();
  SourceLoc trailingTriviaLoc = tokLoc.getAdvancedLoc(tokLength);
  Trivia syntaxLeadingTrivia =
    ParsedTriviaPiece::convertToSyntaxTrivia(leadingTriviaPieces,
                                             leadingTriviaLoc, SM, BufferID);
  Trivia syntaxTrailingTrivia =
    ParsedTriviaPiece::convertToSyntaxTrivia(trailingTriviaPieces,
                                             trailingTriviaLoc, SM, BufferID);
  StringRef tokenText = SM.extractText(tokRange, BufferID);
  auto ownedText = OwnedString::makeRefCounted(tokenText);
  auto raw = TokenCache->getToken(Arena, tokenKind, ownedText,
                    syntaxLeadingTrivia.Pieces, syntaxTrailingTrivia.Pieces);
  OpaqueSyntaxNode opaqueN = raw.get();
  raw.resetWithoutRelease();
  return opaqueN;
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordMissingToken(tok kind, SourceLoc loc) {
  auto ownedText = OwnedString::makeRefCounted(getTokenText(kind));
  auto raw = RawSyntax::missing(kind, ownedText, Arena);
  OpaqueSyntaxNode opaqueN = raw.get();
  raw.resetWithoutRelease();
  return opaqueN;
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordRawSyntax(syntax::SyntaxKind kind,
                                   ArrayRef<OpaqueSyntaxNode> elements,
                                   CharSourceRange range) {
  SmallVector<RC<RawSyntax>, 16> parts;
  parts.reserve(elements.size());
  for (OpaqueSyntaxNode opaqueN : elements) {
    parts.push_back(transferOpaqueNode(opaqueN));
  }
  auto raw = RawSyntax::make(kind, parts, SourcePresence::Present, Arena);
  OpaqueSyntaxNode opaqueN = raw.get();
  raw.resetWithoutRelease();
  return opaqueN;
}

std::pair<size_t, OpaqueSyntaxNode>
SyntaxTreeCreator::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  if (!SyntaxCache)
    return {0, nullptr};
  auto cacheLookup = SyntaxCache->lookUp(lexerOffset, kind);
  if (!cacheLookup)
    return {0, nullptr};
  RC<RawSyntax> raw = cacheLookup->getRaw();
  OpaqueSyntaxNode opaqueN = raw.get();
  size_t length = raw->getTextLength();
  raw.resetWithoutRelease();
  return {length, opaqueN};
}

void SyntaxTreeCreator::discardRecordedNode(OpaqueSyntaxNode opaqueN) {
  if (!opaqueN)
    return;
  static_cast<RawSyntax *>(opaqueN)->Release();
}

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
  StringRef BufferContent = SM.getEntireTextForBuffer(BufferID);
  char *Data = (char *)Arena->Allocate(BufferContent.size(), alignof(char *));
  std::uninitialized_copy(BufferContent.begin(), BufferContent.end(), Data);
  ArenaSourceBuffer = StringRef(Data, BufferContent.size());
  assert(ArenaSourceBuffer == BufferContent);
  Arena->setHotUseMemoryRegion(ArenaSourceBuffer.begin(),
                               ArenaSourceBuffer.end());
}

SyntaxTreeCreator::~SyntaxTreeCreator() {
  // Release all deferred nodes that are being kept alive by this
  for (auto node : DeferredNodes) {
    static_cast<RawSyntax *>(node)->Release();
  }
  Arena.reset();
}

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

Optional<SourceFileSyntax>
SyntaxTreeCreator::realizeSyntaxRoot(OpaqueSyntaxNode rootN,
                                     const SourceFile &SF) {
  auto raw = transferOpaqueNode(rootN);
  auto rootNode = makeRoot<SourceFileSyntax>(raw);

  // Verify the tree if specified.
  if (SF.getASTContext().LangOpts.VerifySyntaxTree) {
    ASTContext &ctx = SF.getASTContext();
    SyntaxVerifier Verifier(ctx.SourceMgr, SF.getBufferID().getValue(),
                            ctx.Diags);
    Verifier.verify(rootNode);
  }
  return rootNode;
}

OpaqueSyntaxNode SyntaxTreeCreator::recordToken(tok tokenKind,
                                                StringRef leadingTrivia,
                                                StringRef trailingTrivia,
                                                CharSourceRange range) {
  unsigned tokLength =
      range.getByteLength() - leadingTrivia.size() - trailingTrivia.size();
  auto leadingTriviaStartOffset =
      SM.getLocOffsetInBuffer(range.getStart(), BufferID);
  auto tokStartOffset = leadingTriviaStartOffset + leadingTrivia.size();
  auto trailingTriviaStartOffset = tokStartOffset + tokLength;

  // Get StringRefs of the token's texts that point into the syntax arena's
  // buffer.
  StringRef leadingTriviaText =
      ArenaSourceBuffer.substr(leadingTriviaStartOffset, leadingTrivia.size());
  StringRef tokenText = ArenaSourceBuffer.substr(tokStartOffset, tokLength);
  StringRef trailingTriviaText = ArenaSourceBuffer.substr(
      trailingTriviaStartOffset, trailingTrivia.size());

  auto ownedText = OwnedString::makeRefCounted(tokenText);
  auto raw =
      TokenCache->getToken(Arena, tokenKind, range.getByteLength(), ownedText,
                           leadingTriviaText, trailingTriviaText);
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

OpaqueSyntaxNode SyntaxTreeCreator::recordRawSyntax(
    syntax::SyntaxKind kind, const SmallVector<OpaqueSyntaxNode, 4> &elements,
    CharSourceRange range) {
  SmallVector<RC<RawSyntax>, 16> parts;
  parts.reserve(elements.size());
  for (OpaqueSyntaxNode opaqueN : elements) {
    parts.push_back(transferOpaqueNode(opaqueN));
  }
  size_t TextLength = range.isValid() ? range.getByteLength() : 0;
  auto raw =
      RawSyntax::make(kind, parts, TextLength, SourcePresence::Present, Arena);
  OpaqueSyntaxNode opaqueN = raw.get();
  raw.resetWithoutRelease();
  return opaqueN;
}

OpaqueSyntaxNode SyntaxTreeCreator::makeDeferredToken(tok tokenKind,
                                                      StringRef leadingTrivia,
                                                      StringRef trailingTrivia,
                                                      CharSourceRange range,
                                                      bool isMissing) {
  // Instead of creating dedicated deferred nodes that will be recorded only if
  // needed, the SyntaxTreeCreator always records all nodes and forms RawSyntax
  // nodes for them. This eliminates a bunch of copies that would otherwise
  // be required to record the deferred nodes.
  // Should a deferred node not be recorded, its data stays alive in the
  // SyntaxArena. This causes a small memory leak but since most nodes are
  // being recorded, it is acceptable.
  if (isMissing) {
    auto Node = recordMissingToken(tokenKind, range.getStart());
    // The SyntaxTreeCreator still owns the deferred node. Record it so we can
    // release it when the creator is being destructed.
    DeferredNodes.push_back(Node);
    return Node;
  } else {
    auto Node = recordToken(tokenKind, leadingTrivia, trailingTrivia, range);
    // See comment above.
    DeferredNodes.push_back(Node);
    return Node;
  }
}

OpaqueSyntaxNode SyntaxTreeCreator::makeDeferredLayout(
    syntax::SyntaxKind k, CharSourceRange Range, bool IsMissing,
    const SmallVector<OpaqueSyntaxNode, 4> &children) {
  // Also see comment in makeDeferredToken

  for (auto child : children) {
    if (child != nullptr) {
      // With the deferred layout being created all of the child nodes are now
      // being owned through the newly created deferred layout node.
      // Technically, we should remove the child nodes from DeferredNodes.
      // However, finding it in the vector is fairly expensive. Instead, we
      // issue a Retain call that cancels with the Release call that will be
      // issued once the creator is being destructed.
      static_cast<RawSyntax *>(child)->Retain();
    }
  }
  auto Node = recordRawSyntax(k, children, Range);
  DeferredNodes.push_back(Node);
  return Node;
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordDeferredToken(OpaqueSyntaxNode deferred) {
  // The deferred node is currently being owned by the SyntaxTreeCreator and
  // will be released when the creator is being destructed. We now pass
  // ownership to whoever owns the recorded node. Technically, we should thus
  // remove the node from DeferredNodes. However, finding it in the vector is
  // fairly expensive. Instead, we issue a Retain call that cancels with the
  // Release call that will be issued once the creator is being destructed.
  // Also see comment in makeDeferredToken.
  static_cast<RawSyntax *>(deferred)->Retain();
  return deferred;
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordDeferredLayout(OpaqueSyntaxNode deferred) {
  // Also see comment in recordDeferredToken
  static_cast<RawSyntax *>(deferred)->Retain();
  return deferred;
}

SyntaxParseActions::DeferredNodeInfo
SyntaxTreeCreator::getDeferredChild(OpaqueSyntaxNode node, size_t ChildIndex,
                                    SourceLoc ThisNodeLoc) {
  RawSyntax *raw = static_cast<RawSyntax *>(node);
  SourceLoc StartLoc = ThisNodeLoc;
  for (size_t i = 0; i < ChildIndex; ++i) {
    StartLoc = StartLoc.getAdvancedLoc(raw->getChildRef(i)->getTextLength());
  }
  RawSyntax *Child = raw->getChildRef(ChildIndex);
  CharSourceRange Range(StartLoc, Child->getTextLength());
  if (Child->isToken()) {
    return DeferredNodeInfo(Child, Range, SyntaxKind::Token,
                            Child->getTokenKind(), Child->isMissing());
  } else {
    return DeferredNodeInfo(Child, Range, Child->getKind(), tok::NUM_TOKENS,
                            Child->isMissing());
  }
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

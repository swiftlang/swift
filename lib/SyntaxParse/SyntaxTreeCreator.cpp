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
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/OwnedString.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxVisitor.h"
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace swift::syntax;

SyntaxTreeCreator::SyntaxTreeCreator(SourceManager &SM, unsigned bufferID,
                                     SyntaxParsingCache *syntaxCache,
                                     RC<syntax::SyntaxArena> arena)
    : SM(SM), BufferID(bufferID), Arena(std::move(arena)),
      SyntaxCache(syntaxCache) {
  StringRef BufferContent = SM.getEntireTextForBuffer(BufferID);
  const char *Data = BufferContent.data();
  Arena->copyStringToArenaIfNecessary(Data, BufferContent.size());
  ArenaSourceBuffer = StringRef(Data, BufferContent.size());
  if (!ArenaSourceBuffer.empty()) {
    Arena->setHotUseMemoryRegion(ArenaSourceBuffer.begin(),
                                 ArenaSourceBuffer.end());
  }
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

Optional<SourceFileSyntax>
SyntaxTreeCreator::realizeSyntaxRoot(OpaqueSyntaxNode rootN,
                                     const SourceFile &SF) {
  auto raw = static_cast<const RawSyntax *>(rootN);
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

  auto raw = RawSyntax::make(tokenKind, tokenText, range.getByteLength(),
                             leadingTriviaText, trailingTriviaText,
                             SourcePresence::Present, Arena);
  return static_cast<OpaqueSyntaxNode>(raw);
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordMissingToken(tok kind, SourceLoc loc) {
  auto raw = RawSyntax::missing(kind, getTokenText(kind), Arena);
  return static_cast<OpaqueSyntaxNode>(raw);
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordRawSyntax(syntax::SyntaxKind kind,
                                   ArrayRef<OpaqueSyntaxNode> elements) {
  const RawSyntax *const *rawChildren =
      reinterpret_cast<const RawSyntax *const *>(elements.begin());
  auto raw = RawSyntax::make(kind, rawChildren, elements.size(),
                             SourcePresence::Present, Arena);
  return static_cast<OpaqueSyntaxNode>(raw);
}

std::pair<size_t, OpaqueSyntaxNode>
SyntaxTreeCreator::lookupNode(size_t lexerOffset, syntax::SyntaxKind kind) {
  if (!SyntaxCache)
    return {0, nullptr};
  auto cacheLookup = SyntaxCache->lookUp(lexerOffset, kind);
  if (!cacheLookup)
    return {0, nullptr};
  const RawSyntax *raw = cacheLookup->getRaw();
  size_t length = raw->getTextLength();
  return {length, static_cast<OpaqueSyntaxNode>(raw)};
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
    return Node;
  } else {
    auto Node = recordToken(tokenKind, leadingTrivia, trailingTrivia, range);
    return Node;
  }
}

OpaqueSyntaxNode SyntaxTreeCreator::makeDeferredLayout(
    syntax::SyntaxKind kind, bool IsMissing,
    const MutableArrayRef<ParsedRawSyntaxNode> &parsedChildren) {
  assert(!IsMissing && "Missing layout nodes not implemented yet");

  auto rawChildren = llvm::map_iterator(
      parsedChildren.begin(),
      [](ParsedRawSyntaxNode &parsedChild) -> const RawSyntax * {
        return static_cast<const RawSyntax *>(parsedChild.takeData());
      });
  auto raw = RawSyntax::make(kind, rawChildren, parsedChildren.size(),
                             SourcePresence::Present, Arena);
  return static_cast<OpaqueSyntaxNode>(raw);
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordDeferredToken(OpaqueSyntaxNode deferred) {
  // We don't diffirentiate between deferred and recorded nodes. See comment in
  // makeDeferredToken.
  return deferred;
}

OpaqueSyntaxNode
SyntaxTreeCreator::recordDeferredLayout(OpaqueSyntaxNode deferred) {
  // We don't diffirentiate between deferred and recorded nodes. See comment in
  // makeDeferredToken.
  return deferred;
}

DeferredNodeInfo SyntaxTreeCreator::getDeferredChild(OpaqueSyntaxNode node,
                                                     size_t ChildIndex) const {
  const RawSyntax *raw = static_cast<const RawSyntax *>(node);

  const RawSyntax *Child = raw->getChild(ChildIndex);
  if (Child == nullptr) {
    return DeferredNodeInfo(
        RecordedOrDeferredNode(nullptr, RecordedOrDeferredNode::Kind::Null),
        syntax::SyntaxKind::Unknown, tok::NUM_TOKENS, /*IsMissing=*/false);
  } else if (Child->isToken()) {
    return DeferredNodeInfo(
        RecordedOrDeferredNode(Child,
                               RecordedOrDeferredNode::Kind::DeferredToken),
        syntax::SyntaxKind::Token, Child->getTokenKind(), Child->isMissing());
  } else {
    return DeferredNodeInfo(
        RecordedOrDeferredNode(Child,
                               RecordedOrDeferredNode::Kind::DeferredLayout),
        Child->getKind(), tok::NUM_TOKENS,
        /*IsMissing=*/false);
  }
}

CharSourceRange SyntaxTreeCreator::getDeferredChildRange(
    OpaqueSyntaxNode node, size_t ChildIndex, SourceLoc StartLoc) const {
  const RawSyntax *raw = static_cast<const RawSyntax *>(node);

  // Compute the start offset of the child node by advancing StartLoc by the
  // length of all previous child nodes.
  for (unsigned i = 0; i < ChildIndex; ++i) {
    const RawSyntax *child = raw->getChild(i);
    if (child) {
      StartLoc = StartLoc.getAdvancedLoc(child->getTextLength());
    }
  }

  const RawSyntax *Child = raw->getChild(ChildIndex);
  if (Child == nullptr) {
    return CharSourceRange(StartLoc, /*Length=*/0);
  } else {
    return CharSourceRange(StartLoc, Child->getTextLength());
  }
}

size_t SyntaxTreeCreator::getDeferredNumChildren(OpaqueSyntaxNode node) {
  const syntax::RawSyntax *raw = static_cast<const syntax::RawSyntax *>(node);
  return raw->getNumChildren();
}

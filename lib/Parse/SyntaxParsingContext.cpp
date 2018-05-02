//===--- SyntaxParsingContext.cpp - Syntax Tree Parsing Support------------===//
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

#include "swift/Parse/SyntaxParsingContext.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Defer.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxVisitor.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace swift::syntax;

using RootContextData = SyntaxParsingContext::RootContextData;

SyntaxParsingContext::SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder,
                                           SourceFile &SF, unsigned BufferID)
    : RootDataOrParent(new RootContextData(SF, SF.getASTContext().Diags,
                                           SF.getASTContext().SourceMgr,
                                           BufferID)),
      CtxtHolder(CtxtHolder), Arena(SF.getASTContext().getSyntaxArena()),
      Storage(getRootData().Storage), Offset(0), Mode(AccumulationMode::Root),
      Enabled(SF.shouldBuildSyntaxTree()) {
  CtxtHolder = this;
  Storage.reserve(128);
}

RC<RawSyntax>
SyntaxParsingContext::makeUnknownSyntax(SyntaxKind Kind,
                                        ArrayRef<RC<RawSyntax>> Parts) {
  assert(isUnknownKind(Kind));
  return RawSyntax::make(Kind, Parts, SourcePresence::Present, &Arena);
}

RC<RawSyntax>
SyntaxParsingContext::createSyntaxAs(SyntaxKind Kind,
                                     ArrayRef<RC<RawSyntax>> Parts) {
  // Try to create the node of the given syntax.
  if (auto Node = SyntaxFactory::createRaw(Kind, Parts, &Arena))
    return Node;

  // Fallback to unknown syntax for the category.
  return makeUnknownSyntax(
      getUnknownKind(Kind), Parts);
}

RC<RawSyntax> SyntaxParsingContext::bridgeAs(SyntaxContextKind Kind,
                                             ArrayRef<RC<RawSyntax>> Parts) {
  if (Parts.size() == 1) {
    auto RawNode = Parts.front();
    switch (Kind) {
    case SyntaxContextKind::Stmt:
      if (!RawNode->isStmt())
        return makeUnknownSyntax(SyntaxKind::UnknownStmt, Parts);
      break;
    case SyntaxContextKind::Decl:
      if (!RawNode->isDecl())
        return makeUnknownSyntax(SyntaxKind::UnknownDecl, Parts);
      break;
    case SyntaxContextKind::Expr:
      if (!RawNode->isExpr())
        return makeUnknownSyntax(SyntaxKind::UnknownExpr, Parts);
      break;
    case SyntaxContextKind::Type:
      if (!RawNode->isType())
        return makeUnknownSyntax(SyntaxKind::UnknownType, Parts);
      break;
    case SyntaxContextKind::Pattern:
      if (!RawNode->isPattern())
        return makeUnknownSyntax(SyntaxKind::UnknownPattern, Parts);
      break;
    case SyntaxContextKind::Syntax:
      // We don't need to coerce in this case.
      break;
    }
    return RawNode;
  } else {
    SyntaxKind UnknownKind;
    switch (Kind) {
    case SyntaxContextKind::Stmt:
      UnknownKind = SyntaxKind::UnknownStmt;
      break;
    case SyntaxContextKind::Decl:
      UnknownKind = SyntaxKind::UnknownDecl;
      break;
    case SyntaxContextKind::Expr:
      UnknownKind = SyntaxKind::UnknownExpr;
      break;
    case SyntaxContextKind::Type:
      UnknownKind = SyntaxKind::UnknownType;
      break;
    case SyntaxContextKind::Pattern:
      UnknownKind = SyntaxKind::UnknownPattern;
      break;
    case SyntaxContextKind::Syntax:
      UnknownKind = SyntaxKind::Unknown;
      break;
    }
    return makeUnknownSyntax(UnknownKind, Parts);
  }
}

/// Add RawSyntax to the parts.
void SyntaxParsingContext::addRawSyntax(RC<RawSyntax> Raw) {
  Storage.emplace_back(Raw);
}

SyntaxParsingContext *SyntaxParsingContext::getRoot() {
  auto Curr = this;
  while (!Curr->isRoot())
    Curr = Curr->getParent();
  return Curr;
}

/// Add Token with Trivia to the parts.
void SyntaxParsingContext::addToken(Token &Tok, Trivia &LeadingTrivia,
                                    Trivia &TrailingTrivia) {
  if (!Enabled)
    return;

  addRawSyntax(RawSyntax::getToken(Arena, Tok.getKind(), Tok.getText(),
                                   LeadingTrivia.Pieces,
                                   TrailingTrivia.Pieces));
}

/// Add Syntax to the parts.
void SyntaxParsingContext::addSyntax(Syntax Node) {
  if (!Enabled)
    return;
  addRawSyntax(Node.getRaw());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind, size_t N) {
  if (N == 0) {
    Storage.push_back(createSyntaxAs(Kind, {}));
    return;
  }

  auto I = Storage.end() - N;
  *I = createSyntaxAs(Kind, getParts().take_back(N));

  // Remove consumed parts.
  if (N != 1)
    Storage.erase(I + 1, Storage.end());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind) {
  assert(isTopOfContextStack());
  if (!Enabled)
    return;

  switch (Kind) {
  case SyntaxKind::SuperRefExpr:
  case SyntaxKind::OptionalChainingExpr:
  case SyntaxKind::ForcedValueExpr:
  case SyntaxKind::PostfixUnaryExpr:
  case SyntaxKind::TernaryExpr:
  case SyntaxKind::AvailabilityLabeledArgument: {
    auto Pair = SyntaxFactory::countChildren(Kind);
    assert(Pair.first == Pair.second);
    createNodeInPlace(Kind, Pair.first);
    break;
  }
  case SyntaxKind::CodeBlockItem:
  case SyntaxKind::IdentifierExpr:
  case SyntaxKind::SpecializeExpr:
  case SyntaxKind::MemberAccessExpr:
  case SyntaxKind::DotSelfExpr:
  case SyntaxKind::ImplicitMemberExpr:
  case SyntaxKind::SimpleTypeIdentifier:
  case SyntaxKind::MemberTypeIdentifier:
  case SyntaxKind::FunctionCallExpr:
  case SyntaxKind::SubscriptExpr:
  case SyntaxKind::ExprList: {
    createNodeInPlace(Kind, getParts().size());
    break;
  }
  default:
    llvm_unreachable("Unrecognized node kind.");
  }
}

void SyntaxParsingContext::collectNodesInPlace(SyntaxKind ColletionKind) {
  assert(isCollectionKind(ColletionKind));
  assert(isTopOfContextStack());
  if (!Enabled)
    return;
  auto Parts = getParts();
  auto Count = 0;
  for (auto I = Parts.rbegin(), End = Parts.rend(); I != End; ++I) {
    if (!SyntaxFactory::canServeAsCollectionMemberRaw(ColletionKind, *I))
      break;
    ++Count;
  }
  if (Count)
    createNodeInPlace(ColletionKind, Count);
}

/// This verifier traverses a syntax node to emit proper diagnostics.
class SyntaxVerifier: public SyntaxVisitor {
  RootContextData &RootData;
  template<class T>
  SourceLoc getSourceLoc(T Node) {
    return RootData.SourceMgr.getLocForOffset(RootData.BufferID,
      Node.getAbsolutePosition().getOffset());
  }
public:
  SyntaxVerifier(RootContextData &RootData) : RootData(RootData) {}
  void visit(UnknownDeclSyntax Node) override {
    RootData.Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                            "declaration");
    visitChildren(Node);
  }
  void visit(UnknownExprSyntax Node) override {
    RootData.Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                            "expression");
    visitChildren(Node);
  }
  void visit(UnknownStmtSyntax Node) override {
    RootData.Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                            "statement");
    visitChildren(Node);
  }
  void visit(UnknownTypeSyntax Node) override {
    RootData.Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                            "type");
    visitChildren(Node);
  }
  void visit(UnknownPatternSyntax Node) override {
    RootData.Diags.diagnose(getSourceLoc(Node), diag::unknown_syntax_entity,
                            "pattern");
    visitChildren(Node);
  }
  void verify(Syntax Node) {
    Node.accept(*this);
  }
};

namespace {
void finalizeSourceFile(RootContextData &RootData,
                        ArrayRef<RC<RawSyntax>> Parts) {
  SourceFile &SF = RootData.SF;
  std::vector<RC<RawSyntax>> AllTopLevel;
  RC<RawSyntax> EOFToken;

  if (SF.hasSyntaxRoot()) {
    auto SourceRaw = SF.getSyntaxRoot().getRaw();
    auto Decls =
        SourceRaw->getChild(SourceFileSyntax::Cursor::Statements)->getLayout();
    std::copy(Decls.begin(), Decls.end(), std::back_inserter(AllTopLevel));
    EOFToken = SourceRaw->getChild(SourceFileSyntax::Cursor::EOFToken);
  }

  if (!Parts.empty() && Parts.back()->isToken(tok::eof)) {
    EOFToken = Parts.back();
    Parts = Parts.drop_back();
  }

  for (auto RawNode : Parts) {
    if (RawNode->getKind() != SyntaxKind::CodeBlockItemList)
      // FIXME: Skip toplevel garbage nodes for now. we shouldn't emit them in
      // the first place.
      continue;

    auto Items = RawNode->getLayout();
    std::copy(Items.begin(), Items.end(), std::back_inserter(AllTopLevel));
  }

  if (!EOFToken)
    EOFToken = RawSyntax::missing(tok::eof, "");

  auto newRaw = SyntaxFactory::createRaw(
      SyntaxKind::SourceFile,
      {
          SyntaxFactory::createRaw(SyntaxKind::CodeBlockItemList, AllTopLevel),
          EOFToken,
      });
  assert(newRaw);
  SF.setSyntaxRoot(make<SourceFileSyntax>(newRaw));

  if (SF.getASTContext().LangOpts.VerifySyntaxTree) {
    // Verify the added nodes if specified.
    SyntaxVerifier Verifier(RootData);
    Verifier.verify(SF.getSyntaxRoot());
  }
}
} // End of anonymous namespace

void SyntaxParsingContext::finalizeRoot() {
  if (!Enabled)
    return;
  assert(isTopOfContextStack() && "some sub-contexts are not destructed");
  assert(isRoot() && "only root context can finalize the tree");
  assert(Mode == AccumulationMode::Root);
  finalizeSourceFile(getRootData(), getParts());

  // Clear the parts because we will call this function again when destroying
  // the root context.
  getRootData().Storage.clear();
}

void SyntaxParsingContext::synthesize(tok Kind, StringRef Text) {
  if (!Enabled)
    return;
  if (Text.empty())
    Text = getTokenText(Kind);
  Storage.push_back(RawSyntax::missing(Kind, Text));
}

void SyntaxParsingContext::synthesize(SyntaxKind Kind) {
  if (!Enabled)
    return;
  Storage.push_back(RawSyntax::missing(Kind));
}

SyntaxParsingContext::~SyntaxParsingContext() {
  assert(isTopOfContextStack() && "destructed in wrong order");

  SWIFT_DEFER {
    // Pop this context from the stack.
    if (!isRoot())
      CtxtHolder = getParent();
    else
      delete RootDataOrParent.get<RootContextData*>();
  };

  if (!Enabled)
    return;

  switch (Mode) {
  // Create specified Syntax node from the parts and add it to the parent.
  case AccumulationMode::CreateSyntax:
    assert(!isRoot());
    createNodeInPlace(SynKind, Storage.size() - Offset);
    break;

  // Ensure the result is specified Syntax category and add it to the parent.
  case AccumulationMode::CoerceKind: {
    assert(!isRoot());
    if (Storage.size() == Offset) {
      Storage.push_back(bridgeAs(CtxtKind, {}));
    } else {
      auto I = Storage.begin() + Offset;
      *I = bridgeAs(CtxtKind, getParts());
      // Remove used parts.
      if (Storage.size() > Offset + 1)
        Storage.erase(Storage.begin() + (Offset + 1), Storage.end());
    }
    break;
  }

  // Do nothing.
  case AccumulationMode::Transparent:
    assert(!isRoot());
    break;

  // Remove all parts in this context.
  case AccumulationMode::Discard:
    Storage.resize(Offset);
    break;

  // Accumulate parsed toplevel syntax onto the SourceFile.
  case AccumulationMode::Root:
    finalizeRoot();
    break;

  // Never.
  case AccumulationMode::NotSet:
    assert(!Enabled && "Cleanup mode must be specified before destruction");
    break;
  }
}

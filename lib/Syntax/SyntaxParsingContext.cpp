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

#include "swift/Syntax/SyntaxParsingContext.h"
#include "swift/AST/Module.h"
#include "swift/Basic/Defer.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/RawTokenSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace swift::syntax;

namespace {
static RC<RawSyntax> makeUnknownSyntax(SyntaxKind Kind,
                                       ArrayRef<RC<RawSyntax>> Parts) {
  assert(isUnknownKind(Kind));
  RawSyntax::LayoutList Layout(Parts);
  return RawSyntax::make(Kind, Layout, SourcePresence::Present);
}

static RC<RawSyntax> createSyntaxAs(SyntaxKind Kind,
                                    ArrayRef<RC<RawSyntax>> Parts) {
  // Convert RawSyntax to Syntax for SyntaxFactory.
  llvm::SmallVector<Syntax, 8> Scratch;
  std::transform(Parts.begin(), Parts.end(), std::back_inserter(Scratch),
                 [](const RC<RawSyntax> &Raw) { return make<Syntax>(Raw); });

  // Try to create the node of the given syntax.
  if (auto Node = SyntaxFactory::createSyntax(Kind, Scratch))
    return Node->getRaw();

  // Fallback to unknown syntax for the category.
  return makeUnknownSyntax(SyntaxFactory::getUnknownKind(Kind), Parts);
}

} // End of anonymous namespace

SyntaxParsingContext::SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder,
                                           SourceFile &SF)
    : Parent(nullptr), CtxtHolder(CtxtHolder), Mode(AccumulationMode::Root),
      SF(&SF), Enabled(SF.shouldBuildSyntaxTree()) {
  CtxtHolder = this;
}

/// Add RawSyntax to the parts.
void SyntaxParsingContext::addRawSyntax(RC<RawSyntax> Raw) {
  Parts.emplace_back(Raw);
}

SyntaxParsingContext *SyntaxParsingContext::getRoot() {
  auto Curr = this;
  while (!Curr->isRoot())
    Curr = Curr->Parent;
  return Curr;
}

/// Add Token with Trivia to the parts.
void SyntaxParsingContext::addToken(Token &Tok, Trivia &LeadingTrivia,
                                    Trivia &TrailingTrivia) {
  if (!Enabled)
    return;

  addRawSyntax(RawTokenSyntax::make(Tok.getKind(), Tok.getText(),
                                    SourcePresence::Present, LeadingTrivia,
                                    TrailingTrivia));
}

/// Add Syntax to the parts.
void SyntaxParsingContext::addSyntax(Syntax Node) {
  if (!Enabled)
    return;
  addRawSyntax(Node.getRaw());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind, size_t N) {
  assert(N >= 1);

  auto I = Parts.end() - N;
  *I = createSyntaxAs(Kind, llvm::makeArrayRef(Parts).take_back(N));

  // Remove used parts.
  if (N != 1)
    Parts.erase(I + 1, Parts.end());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind) {
  assert(isTopOfContextStack());
  if (!Enabled)
    return;

  switch (Kind) {
  case SyntaxKind::MemberAccessExpr:
  case SyntaxKind::TernaryExpr: {
    auto Pair = SyntaxFactory::countChildren(Kind);
    assert(Pair.first == Pair.second);
    createNodeInPlace(Kind, Pair.first);
    break;
  }
  case SyntaxKind::ExprList: {
    createNodeInPlace(Kind, Parts.size());
    break;
  }
  default:
    llvm_unreachable("Unrecognized node kind.");
  }
}

namespace {
RC<RawSyntax> bridgeAs(SyntaxContextKind Kind, ArrayRef<RC<RawSyntax>> Parts) {
  if (Parts.size() == 1) {
    auto RawNode = Parts.front();
    switch (Kind) {
    case SyntaxContextKind::Stmt: {
      if (RawNode->isStmt())
        return RawNode;
      else if (RawNode->isDecl())
        return createSyntaxAs(SyntaxKind::DeclarationStmt, Parts);
      else if (RawNode->isExpr())
        return createSyntaxAs(SyntaxKind::ExpressionStmt, Parts);
      else
        return makeUnknownSyntax(SyntaxKind::UnknownStmt, Parts);
      break;
    }
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

void finalizeSourceFile(SourceFile *SF, ArrayRef<RC<RawSyntax>> Parts) {
  std::vector<DeclSyntax> AllTopLevel;
  llvm::Optional<TokenSyntax> EOFToken;

  if (SF->hasSyntaxRoot()) {
    EOFToken.emplace(SF->getSyntaxRoot().getEOFToken());
    for (auto It : SF->getSyntaxRoot().getTopLevelDecls()) {
      AllTopLevel.push_back(It);
    }
  }

  if (Parts.back()->isToken() &&
      cast<RawTokenSyntax>(Parts.back())->is(tok::eof)) {
    EOFToken.emplace(make<TokenSyntax>(Parts.back()));
    Parts = Parts.drop_back();
  }

  for (auto RawNode : Parts) {
    if (RawNode->Kind != SyntaxKind::StmtList)
      // FIXME: Skip for now.
      continue;
    AllTopLevel.push_back(
        SyntaxFactory::makeTopLevelCodeDecl(make<StmtListSyntax>(RawNode)));
  }
  SF->setSyntaxRoot(SyntaxFactory::makeSourceFile(
      SyntaxFactory::makeDeclList(AllTopLevel),
      EOFToken.hasValue() ? *EOFToken
                          : TokenSyntax::missingToken(tok::eof, "")));
}

} // End of anonymous namespace

SyntaxParsingContext::~SyntaxParsingContext() {
  assert(isTopOfContextStack() && "destructed in wrong order");

  SWIFT_DEFER {
    // Pop this context from the stack.
    if (!isRoot())
      CtxtHolder = Parent;
  };

  if (!Enabled)
    return;

  switch (Mode) {
  // Create specified Syntax node from the parts and add it to the parent.
  case AccumulationMode::CreateSyntax:
    assert(!isRoot());
    Parent->addRawSyntax(createSyntaxAs(SynKind, Parts));
    break;

  // Ensure the result is specified Syntax category and add it to the parent.
  case AccumulationMode::CoerceKind:
    assert(!isRoot());
    Parent->addRawSyntax(bridgeAs(CtxtKind, Parts));
    break;

  // Just move the parts to the tail of the parent.
  case AccumulationMode::Transparent:
    assert(!isRoot());
    std::move(Parts.begin(), Parts.end(), std::back_inserter(Parent->Parts));
    break;

  // Do nothing. Just let it discarded.
  case AccumulationMode::Discard:
    assert(!isRoot());
    break;

  // Accumulate parsed toplevel syntax onto the SourceFile.
  case AccumulationMode::Root:
    assert(isRoot() && "AccumulationMode::Root is only for root context");
    finalizeSourceFile(SF, Parts);
    break;

  // Never.
  case AccumulationMode::NotSet:
    assert(!Enabled && "Cleanup mode must be spefcified before destruction");
    break;
  }
}

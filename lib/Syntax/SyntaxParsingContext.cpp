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

RC<RawSyntax> createSyntaxAs(SyntaxKind Kind, ArrayRef<RC<RawSyntax>> Parts) {
  // Convert RawSyntax to Syntax for SyntaxFactory.
  llvm::SmallVector<Syntax, 8> Scratch;
  std::transform(Parts.begin(), Parts.end(), std::back_inserter(Scratch),
                 [](const RC<RawSyntax> &Raw) { return make<Syntax>(Raw); });

  // Try to create the node of the given syntax.
  if (auto Node = SyntaxFactory::createSyntax(Kind, Scratch))
    return Node->getRaw();

  // Fallback to unknown syntax for the category.
  return makeUnknownSyntax(getUnknownKind(Kind), Parts);
}
}// End of anonymous namespace

SyntaxParsingContext::SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder,
                                           SourceFile &SF,
                                           DiagnosticEngine &Diags)
    : RootDataOrParent(new RootContextData(SF, Diags)), CtxtHolder(CtxtHolder),
      Storage(getRootData().Storage), Offset(0), Mode(AccumulationMode::Root),
      Enabled(SF.shouldKeepSyntaxInfo()) {
  CtxtHolder = this;
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
  case SyntaxKind::TernaryExpr: {
    auto Pair = SyntaxFactory::countChildren(Kind);
    assert(Pair.first == Pair.second);
    createNodeInPlace(Kind, Pair.first);
    break;
  }
  case SyntaxKind::MemberAccessExpr:
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
    if (!SyntaxFactory::canServeAsCollectionMember(ColletionKind,
                                                   make<Syntax>(*I)))
      break;
    ++Count;
  }
  if (Count)
    createNodeInPlace(ColletionKind, Count);
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

void finalizeSourceFile(SourceFile &SF, ArrayRef<RC<RawSyntax>> Parts) {
  std::vector<DeclSyntax> AllTopLevel;
  llvm::Optional<TokenSyntax> EOFToken;

  if (SF.hasSyntaxRoot()) {
    EOFToken.emplace(SF.getSyntaxRoot().getEOFToken());
    for (auto It : SF.getSyntaxRoot().getTopLevelDecls()) {
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
  SF.setSyntaxRoot(SyntaxFactory::makeSourceFile(
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
    assert(isRoot() && "AccumulationMode::Root is only for root context");
    finalizeSourceFile(getRootData().SF, getParts());
    break;

  // Never.
  case AccumulationMode::NotSet:
    assert(!Enabled && "Cleanup mode must be specified before destruction");
    break;
  }
}

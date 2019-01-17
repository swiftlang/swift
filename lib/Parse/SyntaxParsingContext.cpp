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
#include "swift/Parse/ParsedSyntax.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/Trivia.h"

using namespace swift;
using namespace swift::syntax;

void SyntaxParseActions::_anchor() {}

using RootContextData = SyntaxParsingContext::RootContextData;

SyntaxParsingContext::SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder,
                                           SourceFile &SF, unsigned BufferID,
                                 std::shared_ptr<SyntaxParseActions> SPActions)
    : RootDataOrParent(new RootContextData(
          SF, SF.getASTContext().Diags, SF.getASTContext().SourceMgr, BufferID,
          std::move(SPActions))),
      CtxtHolder(CtxtHolder),
      RootData(RootDataOrParent.get<RootContextData *>()), Offset(0),
      Mode(AccumulationMode::Root), Enabled(SF.shouldBuildSyntaxTree()) {
  CtxtHolder = this;
  getStorage().reserve(128);
}

size_t SyntaxParsingContext::lookupNode(size_t LexerOffset, SourceLoc Loc) {
  if (!Enabled)
    return 0;

  assert(getStorage().size() == Offset &&
         "Cannot do lookup if nodes have already been gathered");
  assert(Mode == AccumulationMode::CreateSyntax &&
         "Loading from cache is only supported for mode CreateSyntax");
  auto foundNode = getRecorder().lookupNode(LexerOffset, Loc, SynKind);
  if (foundNode.isNull()) {
    return 0;
  }
  Mode = AccumulationMode::SkippedForIncrementalUpdate;
  getStorage().push_back(foundNode);
  return foundNode.getRange().getByteLength();
}

ParsedRawSyntaxNode
SyntaxParsingContext::makeUnknownSyntax(SyntaxKind Kind,
                                        ArrayRef<ParsedRawSyntaxNode> Parts) {
  assert(isUnknownKind(Kind));
  if (IsBacktracking)
    return ParsedRawSyntaxNode::makeDeferred(Kind, Parts);
  else
    return getRecorder().recordRawSyntax(Kind, Parts);
}

ParsedRawSyntaxNode
SyntaxParsingContext::createSyntaxAs(SyntaxKind Kind,
                                     ArrayRef<ParsedRawSyntaxNode> Parts,
                                     SyntaxNodeCreationKind nodeCreateK) {
  // Try to create the node of the given syntax.
  ParsedRawSyntaxNode rawNode;
  auto &rec = getRecorder();
  auto formNode = [&](SyntaxKind kind, ArrayRef<ParsedRawSyntaxNode> layout) {
    if (nodeCreateK == SyntaxNodeCreationKind::Deferred || IsBacktracking) {
      rawNode = ParsedRawSyntaxNode::makeDeferred(kind, layout);
    } else {
      rawNode = rec.recordRawSyntax(kind, layout);
    }
  };
  if (ParsedSyntaxRecorder::formExactLayoutFor(Kind, Parts, formNode))
    return rawNode;

  // Fallback to unknown syntax for the category.
  return makeUnknownSyntax(getUnknownKind(Kind), Parts);
}

Optional<ParsedRawSyntaxNode>
SyntaxParsingContext::bridgeAs(SyntaxContextKind Kind,
                               ArrayRef<ParsedRawSyntaxNode> Parts) {
  if (Parts.size() == 1) {
    auto RawNode = Parts.front();
    SyntaxKind RawNodeKind = RawNode.getKind();
    switch (Kind) {
    case SyntaxContextKind::Stmt:
      if (!isStmtKind(RawNodeKind))
        return makeUnknownSyntax(SyntaxKind::UnknownStmt, Parts);
      break;
    case SyntaxContextKind::Decl:
      if (!isDeclKind(RawNodeKind))
        return makeUnknownSyntax(SyntaxKind::UnknownDecl, Parts);
      break;
    case SyntaxContextKind::Expr:
      if (!isExprKind(RawNodeKind))
        return makeUnknownSyntax(SyntaxKind::UnknownExpr, Parts);
      break;
    case SyntaxContextKind::Type:
      if (!isTypeKind(RawNodeKind))
        return makeUnknownSyntax(SyntaxKind::UnknownType, Parts);
      break;
    case SyntaxContextKind::Pattern:
      if (!isPatternKind(RawNodeKind))
        return makeUnknownSyntax(SyntaxKind::UnknownPattern, Parts);
      break;
    case SyntaxContextKind::Syntax:
      // We don't need to coerce in this case.
      break;
    }
    return RawNode;
  } else if (Parts.empty()) {
    // Just omit the unknown node if it does not have any children
    return None;
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
void SyntaxParsingContext::addRawSyntax(ParsedRawSyntaxNode Raw) {
  getStorage().emplace_back(std::move(Raw));
}

const SyntaxParsingContext *SyntaxParsingContext::getRoot() const {
  auto Curr = this;
  while (!Curr->isRoot())
    Curr = Curr->getParent();
  return Curr;
}

ParsedTokenSyntax SyntaxParsingContext::popToken() {
  return popIf<ParsedTokenSyntax>().getValue();
}

/// Add Token with Trivia to the parts.
void SyntaxParsingContext::addToken(Token &Tok, Trivia &LeadingTrivia,
                                    Trivia &TrailingTrivia) {
  if (!Enabled)
    return;

  ParsedRawSyntaxNode raw;
  if (IsBacktracking)
    raw = ParsedRawSyntaxNode::makeDeferred(Tok, LeadingTrivia, TrailingTrivia);
  else
    raw = getRecorder().recordToken(Tok, LeadingTrivia, TrailingTrivia);
  addRawSyntax(std::move(raw));
}

/// Add Syntax to the parts.
void SyntaxParsingContext::addSyntax(ParsedSyntax Node) {
  if (!Enabled)
    return;
  addRawSyntax(Node.getRaw());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind, size_t N,
                                          SyntaxNodeCreationKind nodeCreateK) {
  if (N == 0) {
    if (!parserShallOmitWhenNoChildren(Kind))
      getStorage().push_back(createSyntaxAs(Kind, {}, nodeCreateK));
    return;
  }

  auto I = getStorage().end() - N;
  *I = createSyntaxAs(Kind, getParts().take_back(N), nodeCreateK);

  // Remove consumed parts.
  if (N != 1)
    getStorage().erase(I + 1, getStorage().end());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind,
                                          SyntaxNodeCreationKind nodeCreateK) {
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
    createNodeInPlace(Kind, Pair.first, nodeCreateK);
    break;
  }
  case SyntaxKind::CodeBlockItem:
  case SyntaxKind::IdentifierExpr:
  case SyntaxKind::SpecializeExpr:
  case SyntaxKind::MemberAccessExpr:
  case SyntaxKind::SimpleTypeIdentifier:
  case SyntaxKind::MemberTypeIdentifier:
  case SyntaxKind::FunctionCallExpr:
  case SyntaxKind::SubscriptExpr:
  case SyntaxKind::ExprList: {
    createNodeInPlace(Kind, getParts().size(), nodeCreateK);
    break;
  }
  default:
    llvm_unreachable("Unrecognized node kind.");
  }
}

void SyntaxParsingContext::collectNodesInPlace(SyntaxKind ColletionKind,
                                         SyntaxNodeCreationKind nodeCreateK) {
  assert(isCollectionKind(ColletionKind));
  assert(isTopOfContextStack());
  if (!Enabled)
    return;
  auto Parts = getParts();
  auto Count = 0;
  for (auto I = Parts.rbegin(), End = Parts.rend(); I != End; ++I) {
    if (!SyntaxFactory::canServeAsCollectionMemberRaw(ColletionKind, I->getKind()))
      break;
    ++Count;
  }
  if (Count)
    createNodeInPlace(ColletionKind, Count, nodeCreateK);
}

static ParsedRawSyntaxNode finalizeSourceFile(RootContextData &RootData,
                                           ArrayRef<ParsedRawSyntaxNode> Parts) {
  ParsedRawSyntaxRecorder &Recorder = RootData.Recorder;
  std::vector<ParsedRawSyntaxNode> AllTopLevel;

  assert(!Parts.empty() && Parts.back().isToken(tok::eof));
  ParsedRawSyntaxNode EOFToken = Parts.back();
  Parts = Parts.drop_back();

  for (auto RawNode : Parts) {
    if (RawNode.getKind() != SyntaxKind::CodeBlockItem)
      // FIXME: Skip toplevel garbage nodes for now. we shouldn't emit them in
      // the first place.
      continue;

    AllTopLevel.push_back(RawNode);
  }

  auto itemList = Recorder.recordRawSyntax(SyntaxKind::CodeBlockItemList,
                                           AllTopLevel);
  auto root = Recorder.recordRawSyntax(SyntaxKind::SourceFile,
                                       { itemList, EOFToken });
  return root;
}

ParsedRawSyntaxNode SyntaxParsingContext::finalizeRoot() {
  if (!Enabled)
    return ParsedRawSyntaxNode::null();
  assert(isTopOfContextStack() && "some sub-contexts are not destructed");
  assert(isRoot() && "only root context can finalize the tree");
  assert(Mode == AccumulationMode::Root);
  auto parts = getParts();
  if (parts.empty()) {
    return ParsedRawSyntaxNode::null(); // already finalized.
  }
  ParsedRawSyntaxNode root = finalizeSourceFile(*getRootData(), parts);

  // Clear the parts because we will call this function again when destroying
  // the root context.
  getStorage().clear();

  return root;
}

void SyntaxParsingContext::synthesize(tok Kind, SourceLoc Loc) {
  if (!Enabled)
    return;

  ParsedRawSyntaxNode raw;
  if (IsBacktracking)
    raw = ParsedRawSyntaxNode::makeDeferredMissing(Kind, Loc);
  else
    raw = getRecorder().recordMissingToken(Kind, Loc);
  getStorage().push_back(std::move(raw));
}

void SyntaxParsingContext::dumpStorage() const  {
  llvm::errs() << "======================\n";
  for (auto Node : getStorage()) {
    Node.dump();
    llvm::errs() << "\n--------------\n";
  }
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

  auto &Storage = getStorage();

  switch (Mode) {
  // Create specified Syntax node from the parts and add it to the parent.
  case AccumulationMode::CreateSyntax:
  case AccumulationMode::DeferSyntax:
    assert(!isRoot());
    createNodeInPlace(SynKind, Storage.size() - Offset,
        Mode == AccumulationMode::DeferSyntax ?
          SyntaxNodeCreationKind::Deferred : SyntaxNodeCreationKind::Recorded);
    break;

  // Ensure the result is specified Syntax category and add it to the parent.
  case AccumulationMode::CoerceKind: {
    assert(!isRoot());
    if (Storage.size() == Offset) {
      if (auto BridgedNode = bridgeAs(CtxtKind, {})) {
        Storage.push_back(BridgedNode.getValue());
      }
    } else {
      auto I = Storage.begin() + Offset;
      *I = bridgeAs(CtxtKind, getParts()).getValue();
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
  case AccumulationMode::Discard: {
    auto &nodes = getStorage();
    nodes.erase(nodes.begin()+Offset, nodes.end());
    break;
  }

  case AccumulationMode::SkippedForIncrementalUpdate:
    break;

  // Accumulate parsed toplevel syntax.
  case AccumulationMode::Root:
    finalizeRoot();
    break;

  // Never.
  case AccumulationMode::NotSet:
    assert(!Enabled && "Cleanup mode must be specified before destruction");
    break;
  }
}

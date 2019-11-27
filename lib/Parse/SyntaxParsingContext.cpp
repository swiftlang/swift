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
#include "swift/AST/SourceFile.h"
#include "swift/Basic/Defer.h"
#include "swift/Parse/ParsedSyntax.h"
#include "swift/Parse/ParsedRawSyntaxRecorder.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxFactory.h"

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
  auto length = foundNode.getRecordedRange().getByteLength();
  getStorage().push_back(std::move(foundNode));
  return length;
}

ParsedRawSyntaxNode
SyntaxParsingContext::makeUnknownSyntax(SyntaxKind Kind,
                                        MutableArrayRef<ParsedRawSyntaxNode> Parts) {
  assert(isUnknownKind(Kind));
  if (shouldDefer())
    return ParsedRawSyntaxNode::makeDeferred(Kind, Parts, *this);
  else
    return getRecorder().recordRawSyntax(Kind, Parts);
}

ParsedRawSyntaxNode
SyntaxParsingContext::createSyntaxAs(SyntaxKind Kind,
                                     MutableArrayRef<ParsedRawSyntaxNode> Parts,
                                     SyntaxNodeCreationKind nodeCreateK) {
  // Try to create the node of the given syntax.
  ParsedRawSyntaxNode rawNode;
  auto &rec = getRecorder();
  auto formNode = [&](SyntaxKind kind, MutableArrayRef<ParsedRawSyntaxNode> layout) {
    if (nodeCreateK == SyntaxNodeCreationKind::Deferred || shouldDefer()) {
      rawNode = ParsedRawSyntaxNode::makeDeferred(kind, layout, *this);
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
                               MutableArrayRef<ParsedRawSyntaxNode> Parts) {
  if (Parts.size() == 1) {
    auto &RawNode = Parts.front();
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
    return std::move(RawNode);
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
  auto tok = popIf<ParsedTokenSyntax>();
  return std::move(tok.getValue());
}

/// Add Token with Trivia to the parts.
void SyntaxParsingContext::addToken(Token &Tok,
                                    const ParsedTrivia &LeadingTrivia,
                                    const ParsedTrivia &TrailingTrivia) {
  if (!Enabled)
    return;

  ParsedRawSyntaxNode raw;
  if (shouldDefer())
    raw = ParsedRawSyntaxNode::makeDeferred(Tok, LeadingTrivia, TrailingTrivia,
                                            *this);
  else
    raw = getRecorder().recordToken(Tok, LeadingTrivia, TrailingTrivia);
  addRawSyntax(std::move(raw));
}

/// Add Syntax to the parts.
void SyntaxParsingContext::addSyntax(ParsedSyntax Node) {
  if (!Enabled)
    return;
  addRawSyntax(Node.takeRaw());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind, size_t N,
                                          SyntaxNodeCreationKind nodeCreateK) {
  if (N == 0) {
    if (!parserShallOmitWhenNoChildren(Kind))
      getStorage().push_back(createSyntaxAs(Kind, {}, nodeCreateK));
    return;
  }

  auto node = createSyntaxAs(Kind, getParts().take_back(N), nodeCreateK);
  auto &storage = getStorage();
  getStorage().erase(storage.end() - N, getStorage().end());
  getStorage().emplace_back(std::move(node));
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
                                           MutableArrayRef<ParsedRawSyntaxNode> Parts) {
  ParsedRawSyntaxRecorder &Recorder = RootData.Recorder;
  ParsedRawSyntaxNode Layout[2];

  assert(!Parts.empty() && Parts.back().isToken(tok::eof));
  Layout[1] = std::move(Parts.back());
  Parts = Parts.drop_back();


  assert(llvm::all_of(Parts, [](const ParsedRawSyntaxNode& node) {
    return node.getKind() == SyntaxKind::CodeBlockItem;
  }) && "all top level element must be 'CodeBlockItem'");

  Layout[0] = Recorder.recordRawSyntax(SyntaxKind::CodeBlockItemList, Parts);

  return Recorder.recordRawSyntax(SyntaxKind::SourceFile,
                                  llvm::makeMutableArrayRef(Layout, 2));
}

OpaqueSyntaxNode SyntaxParsingContext::finalizeRoot() {
  if (!Enabled)
    return nullptr;
  assert(isTopOfContextStack() && "some sub-contexts are not destructed");
  assert(isRoot() && "only root context can finalize the tree");
  assert(Mode == AccumulationMode::Root);
  auto parts = getParts();
  if (parts.empty()) {
    return nullptr; // already finalized.
  }
  ParsedRawSyntaxNode root = finalizeSourceFile(*getRootData(), parts);

  // Clear the parts because we will call this function again when destroying
  // the root context.
  getStorage().clear();

  return root.takeOpaqueNode();
}

void SyntaxParsingContext::synthesize(tok Kind, SourceLoc Loc) {
  if (!Enabled)
    return;

  ParsedRawSyntaxNode raw;
  if (shouldDefer())
    raw = ParsedRawSyntaxNode::makeDeferredMissing(Kind, Loc);
  else
    raw = getRecorder().recordMissingToken(Kind, Loc);
  getStorage().push_back(std::move(raw));
}

void SyntaxParsingContext::dumpStorage() const  {
  llvm::errs() << "======================\n";
  auto &storage = getStorage();
  for (unsigned i = 0; i != storage.size(); ++i) {
    storage[i].dump(llvm::errs());
    llvm::errs() << "\n";
    if (i + 1 == Offset)
      llvm::errs() << "--------------\n";
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
        Storage.push_back(std::move(BridgedNode.getValue()));
      }
    } else {
      auto node(std::move(bridgeAs(CtxtKind, getParts()).getValue()));
      Storage.erase(Storage.begin() + Offset, Storage.end());
      Storage.emplace_back(std::move(node));
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
    for (auto i = nodes.begin()+Offset, e = nodes.end(); i != e; ++i) {
      // FIXME: This should not be needed. This breaks invariant that any
      // recorded node must be a part of result souce syntax tree.
      if (i->isRecorded())
        getRecorder().discardRecordedNode(*i);
    }
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

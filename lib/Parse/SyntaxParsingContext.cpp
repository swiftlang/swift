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
#include "swift/Parse/SyntaxParsingCache.h"
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
    : RootDataOrParent(new RootContextData(
          SF, SF.getASTContext().Diags, SF.getASTContext().SourceMgr, BufferID,
          SF.getASTContext().getSyntaxArena(), SF.SyntaxParsingCache)),
      CtxtHolder(CtxtHolder),
      RootData(RootDataOrParent.get<RootContextData *>()), Offset(0),
      Mode(AccumulationMode::Root), Enabled(SF.shouldBuildSyntaxTree()) {
  CtxtHolder = this;
  getStorage().reserve(128);
}

size_t SyntaxParsingContext::loadFromCache(size_t LexerOffset) {
  assert(getStorage().size() == Offset &&
         "Cannot load from cache if nodes have "
         "already been gathered");
  assert(Mode == AccumulationMode::CreateSyntax &&
         "Loading from cache is only supported for mode CreateSyntax");
  if (!getSyntaxParsingCache()) {
    // We don't have a cache, so there's nothing to look up
    return 0;
  }
  auto CacheLookup = getSyntaxParsingCache()->lookUp(LexerOffset, SynKind);
  if (!CacheLookup) {
    return 0;
  }
  Mode = AccumulationMode::LoadedFromCache;
  RC<RawSyntax> RawLookup = CacheLookup->getRaw().get();
  getStorage().push_back(RawLookup);
  return RawLookup->getTextLength();
}

RC<RawSyntax>
SyntaxParsingContext::makeUnknownSyntax(SyntaxKind Kind,
                                        ArrayRef<RC<RawSyntax>> Parts) {
  assert(isUnknownKind(Kind));
  const RC<SyntaxArena> &Arena = getArena();
  return RawSyntax::make(Kind, Parts, SourcePresence::Present, Arena);
}

RC<RawSyntax>
SyntaxParsingContext::createSyntaxAs(SyntaxKind Kind,
                                     ArrayRef<RC<RawSyntax>> Parts) {
  // Try to create the node of the given syntax.
  const RC<SyntaxArena> &Arena = getArena();
  if (auto Node = SyntaxFactory::createRaw(Kind, Parts, Arena))
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
  } else if (Parts.empty()) {
    // Just omit the unknown node if it does not have any children
    return nullptr;
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
  getStorage().emplace_back(Raw);
}

const SyntaxParsingContext *SyntaxParsingContext::getRoot() const {
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

  auto Arena = getArena();
  auto Text = OwnedString::makeRefCounted(Tok.getText());

  addRawSyntax(getTokenCache().getToken(
      Arena, Tok.getKind(), Text, LeadingTrivia.Pieces, TrailingTrivia.Pieces));
}

/// Add Syntax to the parts.
void SyntaxParsingContext::addSyntax(Syntax Node) {
  if (!Enabled)
    return;
  addRawSyntax(Node.getRaw());
}

void SyntaxParsingContext::createNodeInPlace(SyntaxKind Kind, size_t N) {
  if (N == 0) {
    if (!parserShallOmitWhenNoChildren(Kind))
      getStorage().push_back(createSyntaxAs(Kind, {}));
    return;
  }

  auto I = getStorage().end() - N;
  *I = createSyntaxAs(Kind, getParts().take_back(N));

  // Remove consumed parts.
  if (N != 1)
    getStorage().erase(I + 1, getStorage().end());
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
  RC<SyntaxArena> &Arena = RootData.Arena;
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
    EOFToken =
        RawSyntax::missing(tok::eof, OwnedString::makeUnowned(""), Arena);

  auto newRaw = SyntaxFactory::createRaw(
      SyntaxKind::SourceFile,
      {
          SyntaxFactory::createRaw(SyntaxKind::CodeBlockItemList, AllTopLevel,
                                   Arena),
          EOFToken,
      },
      Arena);
  assert(newRaw);
  SF.setSyntaxRoot(make<SourceFileSyntax>(newRaw));

  // Verify the tree if specified.
  // Do this only when we see the real EOF token because parseIntoSourceFile()
  // can get called multiple times for single source file.
  if (EOFToken->isPresent() && SF.getASTContext().LangOpts.VerifySyntaxTree) {
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
  finalizeSourceFile(*getRootData(), getParts());

  // Clear the parts because we will call this function again when destroying
  // the root context.
  getStorage().clear();
}

void SyntaxParsingContext::synthesize(tok Kind, StringRef Text) {
  if (!Enabled)
    return;
  if (Text.empty())
    Text = getTokenText(Kind);
  auto OwnedText = OwnedString::makeRefCounted(Text);
  getStorage().push_back(RawSyntax::missing(Kind, OwnedText));
}

void SyntaxParsingContext::synthesize(SyntaxKind Kind) {
  if (!Enabled)
    return;
  getStorage().push_back(RawSyntax::missing(Kind));
}

void SyntaxParsingContext::dumpStorage() const  {
  llvm::errs() << "======================\n";
  for (auto Node : getStorage()) {
    Node->dump();
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
    assert(!isRoot());
    createNodeInPlace(SynKind, Storage.size() - Offset);
    break;

  // Ensure the result is specified Syntax category and add it to the parent.
  case AccumulationMode::CoerceKind: {
    assert(!isRoot());
    if (Storage.size() == Offset) {
      if (auto BridgedNode = bridgeAs(CtxtKind, {})) {
        Storage.push_back(BridgedNode);
      }
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

  case AccumulationMode::LoadedFromCache:
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

bool shouldCacheNode(tok TokKind, OwnedString &Text,
                     llvm::ArrayRef<TriviaPiece> LeadingTrivia,
                     llvm::ArrayRef<TriviaPiece> TrailingTrivia) {
  // Is string_literal with >16 length.
  if (TokKind == tok::string_literal && Text.size() > 16) {
    return false;
  }

  // Has leading comment trivia et al.
  if (any_of(LeadingTrivia,
             [](const syntax::TriviaPiece &T) { return T.getText().size(); })) {
    return false;
  }

  // Has trailing comment trivia et al.
  if (any_of(TrailingTrivia,
             [](const syntax::TriviaPiece &T) { return T.getText().size(); })) {
    return false;
  }

  // We can cache the node
  return true;
}

RC<RawSyntax>
RawSyntaxTokenCache::getToken(RC<SyntaxArena> &Arena, tok TokKind,
                              OwnedString Text,
                              llvm::ArrayRef<TriviaPiece> LeadingTrivia,
                              llvm::ArrayRef<TriviaPiece> TrailingTrivia) {
  // Determine whether this token is worth to cache.
  if (!shouldCacheNode(TokKind, Text, LeadingTrivia, TrailingTrivia)) {
    // Do not use cache.
    return RawSyntax::make(TokKind, Text, LeadingTrivia, TrailingTrivia,
                           SourcePresence::Present, Arena);
  }

  // This node is cacheable. Get or create.
  llvm::FoldingSetNodeID ID;
  RawSyntax::Profile(ID, TokKind, Text, LeadingTrivia, TrailingTrivia);

  void *insertPos = nullptr;
  if (auto existing = CachedTokens.FindNodeOrInsertPos(ID, insertPos)) {
    // Found in the cache. Just return it.
    return existing->get();
  }

  // Could not found in the cache. Create it.
  auto Raw = RawSyntax::make(TokKind, Text, LeadingTrivia, TrailingTrivia,
                             SourcePresence::Present, Arena);
  auto IDRef = ID.Intern(Arena->getAllocator());
  auto CacheNode = new (Arena) RawSyntaxCacheNode(Raw, IDRef);
  // Keep track of the created RawSyntaxCacheNode so that we can destruct it
  // later.
  CacheNodes.push_back(CacheNode);
  CachedTokens.InsertNode(CacheNode, insertPos);
  return Raw;
}

RawSyntaxTokenCache::~RawSyntaxTokenCache() {
  // The CachedTokens folding set is no longer used. It does not, however, clean
  // up the RawSyntaxCacheNodes we created for it and would keep a strong
  // reference to their RawSyntax nodes.
  // We thus need to manually destruct the RawSyntaxCacheNodes here.
  // After all RawSyntax nodes in the current arena are disposed of, the
  // RawSyntaxCacheNode will also be destroyed, as they are allocated in that
  // arena.
  for (RawSyntaxCacheNode *Node : CacheNodes) {
    Node->~RawSyntaxCacheNode();
  }
}

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

#include "swift/AST/Module.h"
#include "swift/Basic/Defer.h"
#include "swift/Parse/Token.h"
#include "swift/Parse/Parser.h"
#include "swift/Syntax/RawTokenSyntax.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/Trivia.h"
#include "swift/Syntax/SyntaxParsingContext.h"
#include "swift/Syntax/SyntaxFactory.h"

using namespace swift;
using namespace swift::syntax;

namespace {
static Syntax makeUnknownSyntax(SyntaxKind Kind, ArrayRef<Syntax> SubExpr) {
  assert(isUnknownKind(Kind));
  RawSyntax::LayoutList Layout;
  std::transform(SubExpr.begin(), SubExpr.end(), std::back_inserter(Layout),
                 [](const Syntax &S) { return S.getRaw(); });
  return make<Syntax>(RawSyntax::make(Kind, Layout, SourcePresence::Present));
}

static ArrayRef<Syntax> getSyntaxNodes(ArrayRef<RawSyntaxInfo> RawNodes,
                                       llvm::SmallVectorImpl<Syntax> &Scratch) {
  std::transform(RawNodes.begin(), RawNodes.end(), std::back_inserter(Scratch),
    [](const RawSyntaxInfo &Info) { return Info.makeSyntax<Syntax>(); });
  return Scratch;
}

static SourceRange getNodesRange(ArrayRef<RawSyntaxInfo> RawNodes) {
  SourceLoc StartLoc, EndLoc;
  for (auto Info: RawNodes) {
    if (Info.isImplicit())
      continue;
    if (StartLoc.isInvalid()) {
      StartLoc = Info.getStartLoc();
    }
    EndLoc = Info.getEndLoc();
  }
  assert(StartLoc.isValid() == EndLoc.isValid());
  return SourceRange(StartLoc, EndLoc);
}

static RawSyntaxInfo createSyntaxAs(ArrayRef<RawSyntaxInfo> Parts,
                                    SyntaxKind Kind) {
  llvm::SmallVector<Syntax, 8> Scratch;
  auto SyntaxParts = getSyntaxNodes(Parts, Scratch);

  // Try to create the node of the given syntax.
  Optional<Syntax> Result = SyntaxFactory::createSyntax(Kind, SyntaxParts);
  if (!Result) {

    // If unable to create, we should create an unknown node.
    Result.emplace(makeUnknownSyntax(SyntaxFactory::getUnknownKind(Kind),
                                     SyntaxParts));
  }
  return { getNodesRange(Parts), Result->getRaw() };
}
} // End of anonymous namespace

struct SyntaxParsingContext::ContextInfo {
  bool Enabled;
private:
  SourceLoc ContextStartLoc;
  std::vector<RawSyntaxInfo> PendingSyntax;

  // All tokens after the start of this context.
  ArrayRef<RawSyntaxInfo> Tokens;

  ArrayRef<RawSyntaxInfo>::const_iterator findTokenAt(SourceLoc Loc) const {
    for (auto It = Tokens.begin(); It != Tokens.end(); It ++) {
      assert(It->getStartLoc() == It->getEndLoc());
      if (It->getStartLoc() == Loc)
        return It;
    }
    llvm_unreachable("cannot find the token on the given location");
  }

public:
  ContextInfo(SourceFile &File, unsigned BufferID):
      Enabled(File.shouldKeepSyntaxInfo()) {
    if (Enabled) {
      populateTokenSyntaxMap(File.getASTContext().LangOpts,
                             File.getASTContext().SourceMgr,
                             BufferID, File.AllRawTokenSyntax);
      Tokens = File.AllRawTokenSyntax;
      assert(Tokens.back().makeSyntax<TokenSyntax>().getTokenKind() == tok::eof);
    }
  }

  ContextInfo(ArrayRef<RawSyntaxInfo> Tokens, bool Enabled): Enabled(Enabled) {
    if (Enabled) {
      this->Tokens = Tokens;
    }
  }

  // Squash N syntax nodex from the back of the pending list into one.
  void createFromBackTokens(SyntaxKind Kind, ArrayRef<RawSyntaxInfo> UsedTokens,
                            unsigned N);
  void createWhole(SyntaxKind Kind, ArrayRef<RawSyntaxInfo> NodesToUse);
  std::vector<RawSyntaxInfo> collectAllSyntax(SourceLoc EndLoc);
  ArrayRef<RawSyntaxInfo> allTokens() const { return Tokens; }
  ArrayRef<RawSyntaxInfo> getPendingSyntax() const { return PendingSyntax; }
  SourceLoc getContextStartLoc() const { return ContextStartLoc; }

  void addPendingSyntax(RawSyntaxInfo Info) {
    assert(Info.isImplicit() || PendingSyntax.empty() ||
           PendingSyntax.back().getStartLoc().getOpaquePointerValue() <
             Info.getStartLoc().getOpaquePointerValue());
    PendingSyntax.push_back(Info);
  }

  void setContextStart(SourceLoc Loc) {
    assert(ContextStartLoc.isInvalid());
    ContextStartLoc = Loc;
    Tokens = Tokens.slice(findTokenAt(Loc) - Tokens.begin());
  }

  ArrayRef<RawSyntaxInfo> dropTokenAt(SourceLoc Loc) const {
    return Tokens.take_front(findTokenAt(Loc) - Tokens.begin());
  }

  // Check if the pending syntax is a token syntax in the given kind.
  bool checkTokenFromBack(tok Kind, ArrayRef<RawSyntaxInfo> UsedTokens,
                          unsigned OffsetFromBack = 0) {
    if (UsedTokens.size() - 1 < OffsetFromBack)
      return false;
    auto Back = UsedTokens[UsedTokens.size() - 1 - OffsetFromBack].
      makeSyntax<Syntax>().getAs<TokenSyntax>();
    return Back.hasValue() && Back->getTokenKind() == Kind;
  }
};

static void addNodeToResults(std::vector<RawSyntaxInfo> &Results,
                             std::vector<RawSyntaxInfo> &ImplicitNodes,
                             RawSyntaxInfo Info) {
  // Add implicit nodes before adding the explicit nodes they attach to.
  assert(!Info.isImplicit());
  auto StartSize = Results.size();

  // Find all implicit nodes where the attach-to location is the start position
  // of this non-implicit nodes.
  Results.insert(Results.end(),
                 std::find_if(ImplicitNodes.begin(), ImplicitNodes.end(),
                   [&](const RawSyntaxInfo &Imp) {
                     return Imp.BeforeLoc == Info.getStartLoc();
                   }),
                 ImplicitNodes.end());

  // If any implicit nodes are inserted to results, we should clear the buffer
  // to avoid re-inserting them.
  if (StartSize != Results.size()) {
    ImplicitNodes.clear();
  }

  // Add the non-implicit node.
  Results.emplace_back(Info);
}

std::vector<RawSyntaxInfo>
SyntaxParsingContext::ContextInfo::collectAllSyntax(SourceLoc EndLoc) {
  std::vector<RawSyntaxInfo> Results;
  std::vector<RawSyntaxInfo> ImplicitNodes;
  auto CurSyntax = PendingSyntax.begin();
  for (auto It = Tokens.begin(); It->getStartLoc() != EndLoc;) {
    auto Tok = *It;
    if (CurSyntax == PendingSyntax.end()) {
      // If no remaining syntax nodes, add the token.
      addNodeToResults(Results, ImplicitNodes, Tok);
      It ++;
    } else if (CurSyntax->isImplicit()) {
      ImplicitNodes.emplace_back(*CurSyntax);
      // Skip implicit syntax node.
      CurSyntax ++;
    } else if (CurSyntax->getStartLoc() == Tok.getStartLoc()) {
      // Prefer syntax nodes to tokens.
      addNodeToResults(Results, ImplicitNodes, *CurSyntax);
      while(It->getEndLoc() != CurSyntax->getEndLoc()) It++;
      assert(It < Tokens.end() && It->getEndLoc() == CurSyntax->getEndLoc());
      It ++;
      CurSyntax ++;
    } else {
      // We have to add token in this case since the next syntax node has not
      // started.
      assert(Tok.getStartLoc().getOpaquePointerValue() <
             CurSyntax->getStartLoc().getOpaquePointerValue());
      addNodeToResults(Results, ImplicitNodes, Tok);
      It ++;
    }
  }
  // Add the remaining syntax nodes.
  for (;CurSyntax != PendingSyntax.end(); CurSyntax ++) {
    Results.emplace_back(*CurSyntax);
  }
  return Results;
}

void SyntaxParsingContext::ContextInfo::
createFromBackTokens(SyntaxKind Kind, ArrayRef<RawSyntaxInfo> UsedTokens,
                     unsigned N) {
  auto Size = UsedTokens.size();
  assert(N && Size >= N);
  auto Parts = llvm::makeArrayRef(UsedTokens).slice(Size - N);
  addPendingSyntax(createSyntaxAs(Parts, Kind));
}

void SyntaxParsingContext::ContextInfo::
createWhole(SyntaxKind Kind, ArrayRef<RawSyntaxInfo> NodesToUse) {
  auto Result = createSyntaxAs(NodesToUse, Kind);
  PendingSyntax.clear();
  addPendingSyntax(Result);
}

SyntaxParsingContext::
SyntaxParsingContext(SourceFile &SF, unsigned BufferID, Token &Tok):
  ContextData(*new ContextInfo(SF, BufferID)),
    Tok(Tok) {}

SyntaxParsingContext::SyntaxParsingContext(SyntaxParsingContext &Another):
  ContextData(*new ContextInfo(Another.ContextData.allTokens(),
    Another.ContextData.Enabled)), Tok(Another.Tok) {}

SyntaxParsingContext::~SyntaxParsingContext() { delete &ContextData; }

void SyntaxParsingContext::disable() { ContextData.Enabled = false; }

SyntaxParsingContextRoot::~SyntaxParsingContextRoot() {
  if (!ContextData.Enabled)
    return;
  std::vector<DeclSyntax> AllTopLevel;
  if (File.hasSyntaxRoot()) {
    for (auto It: File.getSyntaxRoot().getTopLevelDecls()) {
      AllTopLevel.push_back(It);
    }
  }
  for (auto Info: ContextData.getPendingSyntax()) {
    assert(Info.RawNode->Kind == SyntaxKind::StmtList);
    AllTopLevel.push_back(SyntaxFactory::makeTopLevelCodeDecl(
      Info.makeSyntax<StmtListSyntax>()));
  }

  File.setSyntaxRoot(
    SyntaxFactory::makeSourceFile(SyntaxFactory::makeDeclList(AllTopLevel),
    // The last node must be eof.
    ContextData.allTokens().back().makeSyntax<TokenSyntax>()));
}

SyntaxParsingContextRoot &SyntaxParsingContextChild::getRoot() {
  for (SyntaxParsingContext *Root = getParent(); ;
       Root = static_cast<SyntaxParsingContextChild*>(Root)->getParent()){
    if (Root->getKind() == SyntaxParsingContextKind::Root)
      return *static_cast<SyntaxParsingContextRoot*>(Root);
  }
  llvm_unreachable("can not find root");
}

SyntaxParsingContextChild::
SyntaxParsingContextChild(SyntaxParsingContext *&ContextHolder,
      Optional<SyntaxContextKind> Kind, Optional<SyntaxKind> KnownSyntax):
    SyntaxParsingContext(*ContextHolder), Parent(ContextHolder),
    ContextHolder(ContextHolder), Kind(Kind), KnownSyntax(KnownSyntax) {
  assert(Kind.hasValue() != KnownSyntax.hasValue());
  ContextHolder = this;
  if (ContextData.Enabled)
    ContextData.setContextStart(Tok.getLoc());
}

void SyntaxParsingContextChild::makeNode(SyntaxKind Kind, SourceLoc LastTokLoc) {
  if (!ContextData.Enabled)
    return;
  auto UsedTokens = ContextData.dropTokenAt(LastTokLoc);
  UsedTokens = llvm::makeArrayRef(UsedTokens.data(), UsedTokens.size() + 1);

  // Create syntax nodes according to the given kind.
  switch (Kind) {
  case SyntaxKind::FloatLiteralExpr:
  case SyntaxKind::IntegerLiteralExpr: {
    // Integer may include the signs before the digits, so check if the sign
    // exists and create.
    ContextData.createFromBackTokens(Kind, UsedTokens, ContextData.
      checkTokenFromBack(tok::oper_prefix, UsedTokens, 1) ? 2 : 1);
    break;
  }
  case SyntaxKind::StringLiteralExpr: {
    ContextData.createFromBackTokens(Kind, UsedTokens, 1);
    break;
  }

  default:
    llvm_unreachable("Unrecognized node kind.");
  }
}

void SyntaxParsingContextChild::makeNodeWhole(SyntaxKind Kind) {
  assert(ContextData.Enabled);
  auto EndLoc = Tok.getLoc();
  auto AllNodes = ContextData.collectAllSyntax(EndLoc);
  switch (Kind) {
  case SyntaxKind::FunctionCallArgument:
  case SyntaxKind::CodeBlock: {
    ContextData.createWhole(Kind, AllNodes);
    break;
  }
  case SyntaxKind::StmtList:
  case SyntaxKind::FunctionCallArgumentList: {
    if (AllNodes.empty()) {
      // Create an empty argument list if no arguments are in the context.
      RawSyntaxInfo Empty(SyntaxFactory::
                          makeBlankCollectionSyntax(Kind).getRaw());
      Empty.setBeforeLoc(EndLoc);
      ContextData.addPendingSyntax(Empty);
    } else {
      ContextData.createWhole(Kind, AllNodes);
    }
    break;
  }
  default:
    llvm_unreachable("Unrecognized node kind.");
  }
}

void RawSyntaxInfo::brigeWithContext(SyntaxContextKind Kind) {
  switch (Kind) {
  case SyntaxContextKind::Stmt: {
    if (RawNode->isDecl()) {
      // Wrap a declaration with a declaration statement
      RawNode = SyntaxFactory::createSyntax(SyntaxKind::DeclarationStmt,
        { makeSyntax<Syntax>() })->getRaw();
    } else if (RawNode->isExpr()) {
      // Wrap an expression with an expression statement
      RawNode = SyntaxFactory::createSyntax(SyntaxKind::ExpressionStmt,
        { makeSyntax<Syntax>() })->getRaw();
    } else if (RawNode->isToken()) {
      // Wrap a standalone token withn an expression statement
      RawNode = SyntaxFactory::createSyntax(SyntaxKind::ExpressionStmt,
        makeUnknownSyntax(SyntaxKind::UnknownExpr,
                          {make<Syntax>(RawNode)}))->getRaw();
    }
    assert(RawNode->isStmt());
    break;
  }
  case SyntaxContextKind::Decl:
  case SyntaxContextKind::Expr:
    break;
  }
}

SyntaxParsingContextChild::~SyntaxParsingContextChild() {
  SWIFT_DEFER {
    // Reset the context holder to be Parent.
    ContextHolder = Parent;
  };
  if (!ContextData.Enabled)
    return;

  SourceLoc EndLoc = Tok.getLoc();
  if (KnownSyntax) {
    // If the entire context should be created to a known syntax kind, create
    // all pending syntax nodes into that node.
    makeNodeWhole(*KnownSyntax);
    assert(ContextData.getPendingSyntax().size() == 1);
    Parent->ContextData.addPendingSyntax(ContextData.getPendingSyntax().front());
    return;
  }
  auto AllNodes = ContextData.collectAllSyntax(EndLoc);
  RC<RawSyntax> FinalResult;
  if (AllNodes.empty())
    return;

  // Make sure we used all tokens.
  assert(AllNodes.front().getStartLoc() == ContextData.getContextStartLoc());

  if (AllNodes.size() == 1) {
    // If we have only one syntax node remaining, we are done.
    auto Result = AllNodes.front();
    // Bridge the syntax node to the expected context kind.
    Result.brigeWithContext(*Kind);
    Parent->ContextData.addPendingSyntax(Result);
    return;
  }

  llvm::SmallVector<Syntax, 8> Scratch;
  auto SyntaxNodes = getSyntaxNodes(AllNodes, Scratch);
  SourceLoc Start = AllNodes.front().getStartLoc();
  SourceLoc End = AllNodes.back().getEndLoc();
  SyntaxKind UnknownKind;
  switch (*Kind) {
    case SyntaxContextKind::Expr:
      UnknownKind = SyntaxKind::UnknownExpr;
      break;
    case SyntaxContextKind::Decl:
      UnknownKind = SyntaxKind::UnknownDecl;
      break;
    case SyntaxContextKind::Stmt:
      UnknownKind = SyntaxKind::UnknownStmt;
      break;
  }
  // Create an unknown node and give it to the parent context.
  Parent->ContextData.addPendingSyntax({SourceRange(Start, End),
    makeUnknownSyntax(UnknownKind, SyntaxNodes).getRaw()});
}

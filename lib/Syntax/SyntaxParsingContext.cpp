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

static unsigned countTokens(ArrayRef<RawSyntaxInfo> AllNodes) {
  return std::accumulate(AllNodes.begin(), AllNodes.end(), 0,
    [](unsigned Sum, const RawSyntaxInfo &Info) { return Sum + Info.TokCount; });
}
} // End of anonymous namespace

RawSyntaxInfo::RawSyntaxInfo(SourceLoc StartLoc, unsigned TokCount,
  RC<RawSyntax> RawNode): StartLoc(StartLoc), TokCount(TokCount), RawNode(RawNode) {
    assert(StartLoc.isValid());
}

struct SyntaxParsingContext::ContextInfo {
  bool Enabled;
private:
  SourceLoc ContextStartLoc;
  SourceLoc ContextEndLoc;
  std::vector<RawSyntaxInfo> PendingSyntax;

  // All tokens after the start of this context.
  ArrayRef<RawSyntaxInfo> Tokens;

  ArrayRef<RawSyntaxInfo>::const_iterator findTokenAt(SourceLoc Loc) {
    for (auto It = Tokens.begin(); It != Tokens.end(); It ++) {
      assert(It->TokCount == 1);
      if (It->StartLoc == Loc)
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
  void createFromBack(SyntaxKind Kind, unsigned N = 0);
  std::vector<RawSyntaxInfo> collectAllSyntax();
  ArrayRef<RawSyntaxInfo> allTokens() const { return Tokens; }
  ArrayRef<RawSyntaxInfo> getPendingSyntax() const { return PendingSyntax; };

  void addPendingSyntax(RawSyntaxInfo Info) {
    assert(PendingSyntax.empty() || PendingSyntax.back().StartLoc.
           getOpaquePointerValue() < Info.StartLoc.getOpaquePointerValue());
    PendingSyntax.push_back(Info);
  }

  void setContextStart(SourceLoc Loc) {
    assert(ContextStartLoc.isInvalid());
    ContextStartLoc = Loc;
    Tokens = Tokens.slice(findTokenAt(Loc) - Tokens.begin());
  }

  void setContextEnd(SourceLoc Loc) {
    assert(ContextEndLoc.isInvalid());
    ContextEndLoc = Loc;
    Tokens = Tokens.take_front(findTokenAt(Loc) - Tokens.begin());
  }

  void promoteTokenAt(SourceLoc Loc) {
    PendingSyntax.push_back(*findTokenAt(Loc));
  }

  // Check if the pending syntax is a token syntax in the given kind.
  bool checkTokenFromBack(tok Kind, unsigned OffsetFromBack = 0) {
    if (PendingSyntax.size() - 1 < OffsetFromBack)
      return false;
    auto Back = PendingSyntax[PendingSyntax.size() - 1 - OffsetFromBack].
      makeSyntax<Syntax>().getAs<TokenSyntax>();
    return Back.hasValue() && Back->getTokenKind() == Kind;
  }
};

std::vector<RawSyntaxInfo>
SyntaxParsingContext::ContextInfo::collectAllSyntax() {
  std::vector<RawSyntaxInfo> Results;
  auto CurSyntax = PendingSyntax.begin();
  for (auto It = Tokens.begin(); It != Tokens.end();) {
    auto Tok = *It;
    if (CurSyntax == PendingSyntax.end()) {
      // If no remaining syntax nodes, add the token.
      Results.emplace_back(Tok);
      It ++;
    } else if (CurSyntax->StartLoc == Tok.StartLoc) {
      // Prefer syntax nodes to tokens.
      Results.emplace_back(*CurSyntax);
      It += CurSyntax->TokCount;
      CurSyntax ++;
    } else {
      // We have to add token in this case since the next syntax node has not
      // started.
      assert(Tok.StartLoc.getOpaquePointerValue() <
             CurSyntax->StartLoc.getOpaquePointerValue());
      Results.push_back(Tok);
      It ++;
    }
  }
  // Add the remaining syntax nodes.
  for (;CurSyntax != PendingSyntax.end(); CurSyntax ++) {
    Results.emplace_back(*CurSyntax);
  }
  return Results;
}

void
SyntaxParsingContext::ContextInfo::createFromBack(SyntaxKind Kind, unsigned N) {
  auto Size = PendingSyntax.size();
  if (!N)
    N = Size;
  assert(Size >= N);
  auto Parts = llvm::makeArrayRef(PendingSyntax).slice(Size - N);
  llvm::SmallVector<Syntax, 8> Scratch;
  auto SyntaxParts = getSyntaxNodes(Parts, Scratch);

  // Try to create the node of the given syntax.
  Optional<Syntax> Result = SyntaxFactory::createSyntax(Kind, SyntaxParts);
  if (!Result) {

    // If unable to create, we should create an unknown node.
    Result.emplace(makeUnknownSyntax(SyntaxFactory::getUnknownKind(Kind),
                                     SyntaxParts));
  }
  RawSyntaxInfo NewSyntaxNode(Parts.front().StartLoc, countTokens(Parts),
                              Result->getRaw());
  // Remove the building bricks and re-append the result.
  for (unsigned I = 0; I < N; I ++)
    PendingSyntax.pop_back();
  addPendingSyntax(NewSyntaxNode);
  assert(Size - N + 1 == PendingSyntax.size());
}

SyntaxParsingContext::
SyntaxParsingContext(SourceFile &SF, unsigned BufferID, Token &Tok):
  ContextData(*new ContextInfo(SF, BufferID)), Tok(Tok) {}

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
    std::vector<StmtSyntax> AllStmts;
    auto S = Info.makeSyntax<Syntax>();
    if (S.isDecl()) {
      AllStmts.push_back(SyntaxFactory::makeDeclarationStmt(
        S.getAs<DeclSyntax>().getValue(), None));
    } else if (S.isExpr()) {
      AllStmts.push_back(SyntaxFactory::makeExpressionStmt(
        S.getAs<ExprSyntax>().getValue(), None));
    } else if (S.isStmt()) {
      AllStmts.push_back(S.getAs<StmtSyntax>().getValue());
    } else {
      // If this is a standalone token, we create an unknown expression wrapper
      // for it.
      AllStmts.push_back(SyntaxFactory::makeExpressionStmt(
        *makeUnknownSyntax(SyntaxKind::UnknownExpr,
                           { *S.getAs<TokenSyntax>() }).getAs<ExprSyntax>(),
                                                           None));
    }
    AllTopLevel.push_back(SyntaxFactory::makeTopLevelCodeDecl(
      SyntaxFactory::makeStmtList(AllStmts)));
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
                          SyntaxContextKind Kind):
    SyntaxParsingContext(*ContextHolder), Parent(ContextHolder),
    ContextHolder(ContextHolder), Kind(Kind) {
  ContextHolder = this;
  if (ContextData.Enabled)
    ContextData.setContextStart(Tok.getLoc());
}

void SyntaxParsingContextChild::addTokenSyntax(SourceLoc Loc) {
  if (ContextData.Enabled)
    ContextData.promoteTokenAt(Loc);
}

void SyntaxParsingContextChild::makeNode(SyntaxKind Kind) {
  if (!ContextData.Enabled)
    return;

  // Create syntax nodes according to the given kind.
  switch (Kind) {
  case SyntaxKind::FloatLiteralExpr:
  case SyntaxKind::IntegerLiteralExpr: {
    // Integer may include the signs before the digits, so check if the sign
    // exists and create.
    ContextData.createFromBack(Kind, ContextData.
      checkTokenFromBack(tok::oper_prefix, 1) ? 2 : 1);
    break;
  }
  case SyntaxKind::StringLiteralExpr: {
    ContextData.createFromBack(Kind, 1);
    break;
  }

  default:
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

  // Set the end of the context.
  ContextData.setContextEnd(Tok.getLoc());
  auto AllNodes = ContextData.collectAllSyntax();
  assert(countTokens(AllNodes) == ContextData.allTokens().size());
  RC<RawSyntax> FinalResult;
  if (AllNodes.empty())
    return;

  if (AllNodes.size() == 1) {
    // FIXME: Check kind
    Parent->ContextData.addPendingSyntax(AllNodes.front());
    return;
  }

  llvm::SmallVector<Syntax, 8> Scratch;
  auto SyntaxNodes = getSyntaxNodes(AllNodes, Scratch);
  SourceLoc Start = AllNodes.front().StartLoc;
  unsigned TokCount = countTokens(AllNodes);
  SyntaxKind UnknownKind;
  switch (Kind) {
    case SyntaxContextKind::Expr:
      UnknownKind = SyntaxKind::UnknownExpr;
      break;
    case SyntaxContextKind::Decl:
      UnknownKind = SyntaxKind::UnknownDecl;
      break;
  }
  // Create an unknown node and give it to the parent context.
  Parent->ContextData.addPendingSyntax({Start, TokCount,
    makeUnknownSyntax(UnknownKind, SyntaxNodes).getRaw()});
}

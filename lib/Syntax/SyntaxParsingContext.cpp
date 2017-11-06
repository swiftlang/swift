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
} // End of anonymous namespace

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
  void createFromBack(SyntaxKind Kind, unsigned N = 0);
  std::vector<RawSyntaxInfo> collectAllSyntax();
  ArrayRef<RawSyntaxInfo> allTokens() const { return Tokens; }
  ArrayRef<RawSyntaxInfo> getPendingSyntax() const { return PendingSyntax; };

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
    } else if (CurSyntax->isImplicit()) {
      // Skip implicit syntax node.
      CurSyntax ++;
    } else if (CurSyntax->getStartLoc() == Tok.getStartLoc()) {
      // Prefer syntax nodes to tokens.
      Results.emplace_back(*CurSyntax);
      while(It->getEndLoc() != CurSyntax->getEndLoc()) It++;
      assert(It < Tokens.end() && It->getEndLoc() == CurSyntax->getEndLoc());
      It ++;
      CurSyntax ++;
    } else {
      // We have to add token in this case since the next syntax node has not
      // started.
      assert(Tok.getStartLoc().getOpaquePointerValue() <
             CurSyntax->getStartLoc().getOpaquePointerValue());
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
  RawSyntaxInfo NewSyntaxNode(getNodesRange(Parts), Result->getRaw());

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
    llvm_unreachable("Unrecognized node kind.");
  }
}

void SyntaxParsingContextChild::makeNodeWhole(SyntaxKind Kind) {
  assert(ContextData.Enabled);
  switch (Kind) {
  case SyntaxKind::CodeBlock: {
    ContextData.createFromBack(Kind);
    break;
  }
  case SyntaxKind::StmtList: {
    if (ContextData.getPendingSyntax().empty()) {
      // Create an empty statement list if no statement is in the context.
      ContextData.addPendingSyntax({SyntaxFactory::makeBlankStmtList().getRaw()});
    } else {
      ContextData.createFromBack(Kind);
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

  // Set the end of the context.
  ContextData.setContextEnd(Tok.getLoc());
  if (KnownSyntax) {
    // If the entire context should be created to a known syntax kind, create
    // all pending syntax nodes into that node.
    makeNodeWhole(*KnownSyntax);
    assert(ContextData.getPendingSyntax().size() == 1);
    auto AllNodes = ContextData.collectAllSyntax();
    assert(AllNodes.size() == 1);
    Parent->ContextData.addPendingSyntax(AllNodes.front());
    return;
  }
  auto AllNodes = ContextData.collectAllSyntax();
  RC<RawSyntax> FinalResult;
  if (AllNodes.empty())
    return;

  // Make sure we used all tokens.
  assert(AllNodes.front().getStartLoc() == ContextData.allTokens().front().getStartLoc());
  assert(AllNodes.back().getEndLoc() == ContextData.allTokens().back().getStartLoc());

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

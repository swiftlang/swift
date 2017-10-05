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
#include "swift/Parse/Parser.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/SyntaxParsingContext.h"
#include "swift/Syntax/SyntaxFactory.h"

using namespace swift;
using namespace swift::syntax;

namespace {
static ExprSyntax getUnknownExpr(ArrayRef<Syntax> SubExpr) {
  RawSyntax::LayoutList Layout;
  std::transform(SubExpr.begin(), SubExpr.end(), std::back_inserter(Layout),
                 [](const Syntax &S) { return S.getRaw(); });
  return make<ExprSyntax>(RawSyntax::make(SyntaxKind::UnknownExpr,
                                          Layout,
                                          SourcePresence::Present));
}
} // End of anonymous namespace

struct SyntaxParsingContext::ContextInfo {
  bool Enabled;
  std::vector<Syntax> PendingSyntax;

  ContextInfo(bool Enabled): Enabled(Enabled) {}

  // Pop back from PendingSyntax if the back is a token of the given Kind.
  Optional<TokenSyntax> checkBackToken(tok Kind) {
    if (PendingSyntax.empty())
      return None;
    auto Back = PendingSyntax.back().getAs<TokenSyntax>();
    if (Back.hasValue() && (*Back).getTokenKind() == Kind) {
      PendingSyntax.pop_back();
      return Back;
    }
    return None;
  }

  void addPendingSyntax(ArrayRef<Syntax> More) {
    std::transform(More.begin(), More.end(), std::back_inserter(PendingSyntax),
                 [](const Syntax &S) { return make<Syntax>(S.getRaw()); });
  }

  // Pop back from PendingSyntax.
  Syntax popPendingSyntax() {
    assert(!PendingSyntax.empty());
    auto Result = PendingSyntax.back();
    PendingSyntax.pop_back();
    return Result;
  }
};

SyntaxParsingContext::SyntaxParsingContext(bool Enabled):
  ContextData(*new ContextInfo(Enabled)) {}

SyntaxParsingContext::SyntaxParsingContext(SyntaxParsingContext &Another):
    SyntaxParsingContext(Another.ContextData.Enabled) {}

SyntaxParsingContext::~SyntaxParsingContext() { delete &ContextData; }

void SyntaxParsingContext::disable() { ContextData.Enabled = false; }

struct SyntaxParsingContextRoot::GlobalInfo {
  // The source file under parsing.
  SourceFile &File;

  // All tokens in the source file. This list will shrink from the start when
  // we start to build syntax nodes.
  ArrayRef<RawTokenInfo> Tokens;

  GlobalInfo(SourceFile &File) : File(File) {}
  TokenSyntax retrieveTokenSyntax(SourceLoc Loc) {
    auto TargetLoc = Loc.getOpaquePointerValue();
    for (unsigned I = 0, N = Tokens.size(); I < N; I ++) {
      auto Info = Tokens[I];
      auto InfoLoc = Info.Loc.getOpaquePointerValue();
      if (InfoLoc < TargetLoc)
        continue;
      assert(InfoLoc == TargetLoc);
      Tokens = Tokens.slice(I + 1);
      return make<TokenSyntax>(Info.Token);
    }
    llvm_unreachable("can not find token at Loc");
  }
};

SyntaxParsingContextRoot::
SyntaxParsingContextRoot(SourceFile &File, unsigned BufferID):
    SyntaxParsingContext(File.shouldKeepTokens()),
    GlobalData(*new GlobalInfo(File)) {
  populateTokenSyntaxMap(File.getASTContext().LangOpts,
                         File.getASTContext().SourceMgr,
                         BufferID, File.AllRawTokenSyntax);
  // Keep track of the raw tokens.
  GlobalData.Tokens = llvm::makeArrayRef(File.AllRawTokenSyntax);
}

SyntaxParsingContextRoot::~SyntaxParsingContextRoot() {
  std::vector<DeclSyntax> AllTopLevel;
  if (GlobalData.File.SyntaxRoot.hasValue()) {
    for (auto It: GlobalData.File.getSyntaxRoot().getTopLevelDecls()) {
      AllTopLevel.push_back(It);
    }
  }
  for (auto S: ContextData.PendingSyntax) {
    std::vector<StmtSyntax> AllStmts;
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
        getUnknownExpr({ *S.getAs<TokenSyntax>() }), None));
    }
    AllTopLevel.push_back(SyntaxFactory::makeTopLevelCodeDecl(
      SyntaxFactory::makeStmtList(AllStmts)));
  }

  Trivia Leading = Trivia::newlines(1), Trailing;
  GlobalData.File.SyntaxRoot.emplace(
    SyntaxFactory::makeSourceFile(SyntaxFactory::makeDeclList(AllTopLevel),
      SyntaxFactory::makeToken(tok::eof, "", SourcePresence::Present,
                               Leading, Trailing)));
  delete &GlobalData;
}

SyntaxParsingContextRoot &SyntaxParsingContextChild::getRoot() {
  for (SyntaxParsingContext *Root = getParent(); ;
       Root = static_cast<SyntaxParsingContextChild*>(Root)->getParent()){
    if (Root->getKind() == SyntaxParsingContextKind::Root)
      return *static_cast<SyntaxParsingContextRoot*>(Root);
  }
  llvm_unreachable("can not find root");
}

void SyntaxParsingContextChild::addTokenSyntax(SourceLoc Loc) {
  if (ContextData.Enabled)
    ContextData.PendingSyntax.push_back(getRoot().GlobalData.
      retrieveTokenSyntax(Loc));
}

SyntaxParsingContextChild::~SyntaxParsingContextChild() {
  // Parent should take care of the created syntax.
  Parent->ContextData.addPendingSyntax(ContextData.PendingSyntax);

  // Reset the context holder to be Parent.
  ContextHolder = Parent;
}

void SyntaxParsingContextExpr::makeNode(SyntaxKind Kind) {
  if (!ContextData.Enabled)
    return;

  // Create syntax nodes according to the given kind.
  switch (Kind) {
  case SyntaxKind::IntegerLiteralExpr: {
    auto Digit = *ContextData.popPendingSyntax().getAs<TokenSyntax>();
    ContextData.PendingSyntax.push_back(SyntaxFactory::makeIntegerLiteralExpr(
      ContextData.checkBackToken(tok::oper_prefix), Digit));
    break;
  }
  case SyntaxKind::StringLiteralExpr: {
    auto StringToken = *ContextData.popPendingSyntax().getAs<TokenSyntax>();
    ContextData.PendingSyntax.push_back(SyntaxFactory::
      makeStringLiteralExpr(StringToken));
    break;
  }

  default:
    break;
  }
}

SyntaxParsingContextExpr::~SyntaxParsingContextExpr() {
  // If we've created more than one expression syntax, we should enclose them
  // under a unknown expression.
  if (ContextData.PendingSyntax.size() > 1) {
    auto Result = getUnknownExpr(ContextData.PendingSyntax);
    ContextData.PendingSyntax.clear();
    ContextData.PendingSyntax.push_back(Result);
  }
}

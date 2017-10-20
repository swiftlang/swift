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
static Syntax makeUnknownSyntax(SyntaxKind Kind, ArrayRef<Syntax> SubExpr) {
  RawSyntax::LayoutList Layout;
  std::transform(SubExpr.begin(), SubExpr.end(), std::back_inserter(Layout),
                 [](const Syntax &S) { return S.getRaw(); });
  return make<Syntax>(RawSyntax::make(Kind, Layout, SourcePresence::Present));
}
} // End of anonymous namespace

struct SyntaxParsingContext::ContextInfo {
  bool Enabled;
  std::vector<Syntax> PendingSyntax;

  ContextInfo(bool Enabled): Enabled(Enabled) {}

  // Check if the pending syntax is a token syntax in the given kind.
  bool checkTokenFromBack(tok Kind, unsigned OffsetFromBack = 0) {
    if (PendingSyntax.size() - 1 < OffsetFromBack)
      return false;
    auto Back = PendingSyntax[PendingSyntax.size() - 1 - OffsetFromBack].
      getAs<TokenSyntax>();
    return Back.hasValue() && Back->getTokenKind() == Kind;
  }

  void addPendingSyntax(ArrayRef<Syntax> More) {
    std::transform(More.begin(), More.end(), std::back_inserter(PendingSyntax),
                 [](const Syntax &S) { return make<Syntax>(S.getRaw()); });
  }

  // Squash N syntax nodex from the back of the pending list into one.
  void createFromBack(SyntaxKind Kind, unsigned N = 0) {
    auto Size = PendingSyntax.size();
    assert(Size >= N);
    if (!N)
      N = Size;
    auto Parts = llvm::makeArrayRef(PendingSyntax).slice(Size - N);

    // Try to create the node of the given syntax.
    Optional<Syntax> Result = SyntaxFactory::createSyntax(Kind, Parts);
    if (!Result) {

      // If unable to create, we should create an unknown node.
      Result.emplace(makeUnknownSyntax(SyntaxFactory::getUnknownKind(Kind),
                                       Parts));
    }

    // Remove the building bricks and re-append the result.
    for (unsigned I = 0; I < N; I ++)
      PendingSyntax.pop_back();
    addPendingSyntax({ *Result });
    assert(Size - N + 1 == PendingSyntax.size());
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
        *makeUnknownSyntax(SyntaxKind::UnknownExpr,
                           { *S.getAs<TokenSyntax>() }).getAs<ExprSyntax>(),
                                                           None));
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

void SyntaxParsingContextChild::makeNode(SyntaxKind Kind) {
  if (!ContextData.Enabled)
    return;

  // Create syntax nodes according to the given kind.
  switch (Kind) {
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
    // Parent should take care of the created syntax.
    Parent->ContextData.addPendingSyntax(ContextData.PendingSyntax);

    // Reset the context holder to be Parent.
    ContextHolder = Parent;
  };

  // If we've created more than one syntax node, we should try building by using
  // the final kind.
  if (ContextData.PendingSyntax.size() > 1) {
    ContextData.createFromBack(FinalKind);
  }
}

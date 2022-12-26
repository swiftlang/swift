//===--- CursorInfo.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CursorInfo.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"
#include "swift/Parse/IDEInspectionCallbacks.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"

using namespace swift;
using namespace swift::constraints;
using namespace ide;

namespace {

// MARK: - Utilities

void typeCheckDeclAndParentClosures(ValueDecl *VD) {
  // We need to type check any parent closures because their types are
  // encoded in the USR of ParentContexts in the cursor info response.
  auto DC = VD->getDeclContext();
  while (DC->getParent()) {
    if (auto Closure = dyn_cast<AbstractClosureExpr>(DC)) {
      if (Closure->getType().isNull()) {
        typeCheckASTNodeAtLoc(
            TypeCheckASTNodeAtLocContext::declContext(DC->getParent()),
            Closure->getLoc());
      }
    }
    DC = DC->getParent();
  }

  typeCheckASTNodeAtLoc(
      TypeCheckASTNodeAtLocContext::declContext(VD->getDeclContext()),
      VD->getLoc());
}

// MARK: - NodeFinderResults

enum class NodeFinderResultKind { Decl };

class NodeFinderResult {
  NodeFinderResultKind Kind;

protected:
  NodeFinderResult(NodeFinderResultKind Kind) : Kind(Kind) {}

public:
  NodeFinderResultKind getKind() const { return Kind; }
};

class NodeFinderDeclResult : public NodeFinderResult {
  ValueDecl *ValueD;

public:
  NodeFinderDeclResult(ValueDecl *ValueD)
      : NodeFinderResult(NodeFinderResultKind::Decl), ValueD(ValueD) {}

  ValueDecl *getDecl() const { return ValueD; }

  static bool classof(const NodeFinderResult *Res) {
    return Res->getKind() == NodeFinderResultKind::Decl;
  }
};

// MARK: - NodeFinder

/// Walks the AST, looking for a node at \c LocToResolve. While walking the
/// AST, also gathers information about shorthand shadows.
class NodeFinder : ASTWalker {
  SourceFile &SrcFile;
  SourceLoc LocToResolve;

  /// As we are walking the tree, this variable is updated to the last seen
  /// DeclContext.
  SmallVector<DeclContext *> DeclContextStack;

  /// The found node.
  std::unique_ptr<NodeFinderResult> Result;

  /// If a decl shadows another decl using shorthand syntax (`[foo]` or
  /// `if let foo {`), this maps the re-declared variable to the one that is
  /// being shadowed.
  /// The transitive closure of shorthand shadowed decls should be reported as
  /// additional results in cursor info.
  llvm::DenseMap<ValueDecl *, ValueDecl *> ShorthandShadowedDecls;

public:
  NodeFinder(SourceFile &SrcFile, SourceLoc LocToResolve)
      : SrcFile(SrcFile), LocToResolve(LocToResolve),
        DeclContextStack({&SrcFile}) {}

  void resolve() { SrcFile.walk(*this); }

  std::unique_ptr<NodeFinderResult> takeResult() { return std::move(Result); }

  /// Get the declarations that \p ShadowingDecl shadows using shorthand shadow
  /// syntax.
  SmallVector<ValueDecl *, 2>
  getShorthandShadowedDecls(ValueDecl *ShadowingDecl) {
    SmallVector<ValueDecl *, 2> Result;
    auto ShorthandShadowedDecl = ShorthandShadowedDecls[ShadowingDecl];
    while (ShorthandShadowedDecl) {
      Result.push_back(ShorthandShadowedDecl);
      ShorthandShadowedDecl = ShorthandShadowedDecls[ShorthandShadowedDecl];
    }
    return Result;
  }

private:
  SourceManager &getSourceMgr() const {
    return SrcFile.getASTContext().SourceMgr;
  }

  /// The decl context that is currently being walked.
  DeclContext *getCurrentDeclContext() { return DeclContextStack.back(); }

  bool rangeContainsLocToResolve(SourceRange Range) const {
    return Range.contains(LocToResolve);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (!rangeContainsLocToResolve(D->getSourceRangeIncludingAttrs())) {
      return PreWalkAction::SkipChildren;
    }

    if (auto *newDC = dyn_cast<DeclContext>(D)) {
      DeclContextStack.push_back(newDC);
    }

    if (D->getLoc() != LocToResolve) {
      return Action::Continue();
    }

    if (auto VD = dyn_cast<ValueDecl>(D)) {
      if (VD->hasName()) {
        assert(Result == nullptr);
        Result = std::make_unique<NodeFinderDeclResult>(VD);
        return Action::Stop();
      }
    }

    return Action::Continue();
  }

  PostWalkAction walkToDeclPost(Decl *D) override {
    if (auto *newDC = dyn_cast<DeclContext>(D)) {
      assert(DeclContextStack.back() == newDC);
      DeclContextStack.pop_back();
    }
    return Action::Continue();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (auto closure = dyn_cast<ClosureExpr>(E)) {
      DeclContextStack.push_back(closure);
    }

    if (auto CaptureList = dyn_cast<CaptureListExpr>(E)) {
      for (auto ShorthandShadows :
           getShorthandShadows(CaptureList, getCurrentDeclContext())) {
        assert(ShorthandShadowedDecls.count(ShorthandShadows.first) == 0);
        ShorthandShadowedDecls[ShorthandShadows.first] =
            ShorthandShadows.second;
      }
    }

    return Action::Continue(E);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
    if (auto *closure = dyn_cast<ClosureExpr>(E)) {
      assert(DeclContextStack.back() == closure);
      DeclContextStack.pop_back();
    }
    return Action::Continue(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    if (auto CondStmt = dyn_cast<LabeledConditionalStmt>(S)) {
      for (auto ShorthandShadow :
           getShorthandShadows(CondStmt, getCurrentDeclContext())) {
        assert(ShorthandShadowedDecls.count(ShorthandShadow.first) == 0);
        ShorthandShadowedDecls[ShorthandShadow.first] = ShorthandShadow.second;
      }
    }
    return Action::Continue(S);
  }
};

// MARK: - CursorInfoDoneParsingCallback

class CursorInfoDoneParsingCallback : public IDEInspectionCallbacks {
  CursorInfoConsumer &Consumer;
  SourceLoc RequestedLoc;

public:
  CursorInfoDoneParsingCallback(Parser &P, CursorInfoConsumer &Consumer,
                                SourceLoc RequestedLoc)
      : IDEInspectionCallbacks(P), Consumer(Consumer),
        RequestedLoc(RequestedLoc) {}

  std::unique_ptr<ResolvedCursorInfo>
  getDeclResult(NodeFinderDeclResult *DeclResult, SourceFile *SrcFile,
                NodeFinder &Finder) const {
    typeCheckDeclAndParentClosures(DeclResult->getDecl());
    auto CursorInfo = std::make_unique<ResolvedValueRefCursorInfo>(
        ResolvedCursorInfo(SrcFile), DeclResult->getDecl(),
        /*CtorTyRef=*/nullptr,
        /*ExtTyRef=*/nullptr, /*IsRef=*/false, /*Ty=*/Type(),
        /*ContainerType=*/Type());
    CursorInfo->setLoc(RequestedLoc);
    CursorInfo->setShorthandShadowedDecls(
        Finder.getShorthandShadowedDecls(DeclResult->getDecl()));
    return CursorInfo;
  }

  void doneParsing(SourceFile *SrcFile) override {
    if (!SrcFile) {
      return;
    }
    NodeFinder Finder(*SrcFile, RequestedLoc);
    Finder.resolve();
    auto Result = Finder.takeResult();
    if (!Result) {
      return;
    }
    std::unique_ptr<ResolvedCursorInfo> CursorInfo;
    switch (Result->getKind()) {
    case NodeFinderResultKind::Decl:
      CursorInfo = getDeclResult(cast<NodeFinderDeclResult>(Result.get()),
                                 SrcFile, Finder);
      break;
    }
    if (Result) {
      Consumer.handleResults(*CursorInfo);
    }
  }
};

} // anonymous namespace.

IDEInspectionCallbacksFactory *
swift::ide::makeCursorInfoCallbacksFactory(CursorInfoConsumer &Consumer,
                                           SourceLoc RequestedLoc) {
  class CursorInfoCallbacksFactoryImpl : public IDEInspectionCallbacksFactory {
    CursorInfoConsumer &Consumer;
    SourceLoc RequestedLoc;

  public:
    CursorInfoCallbacksFactoryImpl(CursorInfoConsumer &Consumer,
                                   SourceLoc RequestedLoc)
        : Consumer(Consumer), RequestedLoc(RequestedLoc) {}

    IDEInspectionCallbacks *createIDEInspectionCallbacks(Parser &P) override {
      return new CursorInfoDoneParsingCallback(P, Consumer, RequestedLoc);
    }
  };

  return new CursorInfoCallbacksFactoryImpl(Consumer, RequestedLoc);
}

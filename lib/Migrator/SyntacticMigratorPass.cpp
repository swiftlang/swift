//===--- SyntacticMigratorPass.cpp ----------------------------------------===//
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

#include "swift/AST/USRGeneration.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/Utils.h"
#include "swift/Migrator/EditorAdapter.h"
#include "swift/Migrator/FixitApplyDiagnosticConsumer.h"
#include "swift/Migrator/Migrator.h"
#include "swift/Migrator/RewriteBufferEditsReceiver.h"
#include "swift/Migrator/SyntacticMigratorPass.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Edit/EditedSource.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/Support/FileSystem.h"
#include "swift/IDE/APIDigesterData.h"

using namespace swift;
using namespace swift::migrator;
using namespace swift::ide;
using namespace swift::ide::api;

struct SyntacticMigratorPass::Implementation : public SourceEntityWalker {
  SourceFile *SF;
  const StringRef FileName;
  unsigned BufferId;
  SourceManager &SM;
  EditorAdapter &Editor;
  const MigratorOptions &Opts;

  APIDiffItemStore DiffStore;

  ArrayRef<APIDiffItem*> getRelatedDiffItems(ValueDecl *VD) {
    llvm::SmallString<64> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    if (swift::ide::printDeclUSR(VD, OS))
      return {};
    // FIXME: overrides and conformances.
    return DiffStore.getDiffItems(Buffer.str());
  }

  DeclNameViewer getFuncRename(ValueDecl *VD, bool &IgnoreBase) {
    for (auto *Item : getRelatedDiffItems(VD)) {
      if (auto *CI = dyn_cast<CommonDiffItem>(Item)) {
        if (CI->isRename()) {
          IgnoreBase = true;
          switch(CI->NodeKind) {
          case SDKNodeKind::Function:
            IgnoreBase = false;
            LLVM_FALLTHROUGH;
          case SDKNodeKind::Constructor:
            return DeclNameViewer(CI->getNewName());
          default:
            return DeclNameViewer();
          }
        }
      }
    }
    return DeclNameViewer();
  }

  bool isSimpleReplacement(APIDiffItem *Item, std::string &Text) {
    if (auto *MD = dyn_cast<TypeMemberDiffItem>(Item)) {
      // We need to pull the self if self index is set.
      if (MD->selfIndex.hasValue())
        return false;
      Text = (llvm::Twine(MD->newTypeName) + "." + MD->newPrintedName).str();
      return true;
    }

    // Simple rename.
    if (auto CI = dyn_cast<CommonDiffItem>(Item)) {
      if (CI->NodeKind == SDKNodeKind::Var && CI->isRename()) {
        Text = CI->getNewName();
        return true;
      }
    }
    return false;
  }

  Implementation(SourceFile *SF, EditorAdapter &Editor,
                 const MigratorOptions &Opts) :
    SF(SF), FileName(SF->getFilename()), BufferId(SF->getBufferID().getValue()),
      SM(SF->getASTContext().SourceMgr), Editor(Editor), Opts(Opts) {}

  void run() {
    if (Opts.APIDigesterDataStorePath.empty())
      return;
    DiffStore.addStorePath(Opts.APIDigesterDataStorePath);
    DiffStore.printIncomingUsr(Opts.DumpUsr);
    walk(SF);
  }

  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                          Type T, ReferenceMetaData Data) override {
    for (auto* Item : getRelatedDiffItems(D)) {
      std::string RepText;
      if (isSimpleReplacement(Item, RepText)) {
        Editor.replace(Range, RepText);
        return true;
      }
    }
    return true;
  }

  struct ReferenceCollector : public SourceEntityWalker {
    ValueDecl *Target;
    CharSourceRange Result;
    ReferenceCollector(ValueDecl* Target) : Target(Target) {}
    bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                            TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef,
                            Type T, ReferenceMetaData Data) override {
      if (D == Target) {
        Result = Range;
        return false;
      }
      return true;
    }
  };

  void handleFuncRename(ValueDecl *FD, Expr* FuncRefContainer, Expr *Arg) {
    bool IgnoreBase = false;
    if (auto View = getFuncRename(FD, IgnoreBase)) {
      if(!IgnoreBase) {
        ReferenceCollector Walker(FD);
        Walker.walk(FuncRefContainer);
        Editor.replace(Walker.Result, View.base());
      }
      unsigned Idx = 0;
      for (auto LR :getCallArgLabelRanges(SM, Arg,
                                          LabelRangeEndAt::LabelNameOnly)) {
        if (Idx < View.argSize()) {
          auto Label = View.args()[Idx++];

          // FIXME: We update only when args are consistently valid.
          if (Label != "_" && LR.getByteLength())
            Editor.replace(LR, Label);
        }
      }
    }
  }

  bool walkToExprPre(Expr *E) override {
    if (auto *CE = dyn_cast<CallExpr>(E)) {
      auto Fn = CE->getFn();
      auto Args = CE->getArg();
      switch (Fn->getKind()) {
      case ExprKind::DeclRef: {
        if (auto FD = Fn->getReferencedDecl().getDecl())
          handleFuncRename(FD, Fn, Args);
        break;
      }
      case ExprKind::DotSyntaxCall: {
        auto DSC = cast<DotSyntaxCallExpr>(Fn);
        if (auto FD = DSC->getFn()->getReferencedDecl().getDecl())
          handleFuncRename(FD, DSC->getFn(), Args);
        break;
      }
      case ExprKind::ConstructorRefCall: {
        auto CCE = cast<ConstructorRefCallExpr>(Fn);
        if (auto FD = CCE->getFn()->getReferencedDecl().getDecl())
          handleFuncRename(FD, CCE->getFn(), Args);
        break;
      }
      default:
        break;
      }
    }
    return true;
  }
};

SyntacticMigratorPass::
SyntacticMigratorPass(EditorAdapter &Editor, SourceFile *SF,
  const MigratorOptions &Opts) : Impl(*new Implementation(SF, Editor, Opts)) {}

SyntacticMigratorPass::~SyntacticMigratorPass() { delete &Impl; }

void SyntacticMigratorPass::run() { Impl.run(); }

const clang::edit::Commit &SyntacticMigratorPass::getEdits() const {
  return Impl.Editor.getEdits();
}

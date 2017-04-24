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
};

SyntacticMigratorPass::
SyntacticMigratorPass(EditorAdapter &Editor, SourceFile *SF,
  const MigratorOptions &Opts) : Impl(*new Implementation(SF, Editor, Opts)) {}

SyntacticMigratorPass::~SyntacticMigratorPass() { delete &Impl; }

void SyntacticMigratorPass::run() { Impl.run(); }

const clang::edit::Commit &SyntacticMigratorPass::getEdits() const {
  return Impl.Editor.getEdits();
}

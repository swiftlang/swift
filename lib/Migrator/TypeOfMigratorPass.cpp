//===--- TypeOfMigratorPass.cpp -------------------------------------------===//
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

#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/IDE/SourceEntityWalker.h"
#include "swift/Migrator/ASTMigratorPass.h"
#include "swift/Parse/Lexer.h"

using namespace swift;
using namespace swift::migrator;

namespace {

class TypeOfMigratorPass: public ASTMigratorPass,
  public SourceEntityWalker {

  std::vector<DeclContext *> ContextStack;

  void handleTypeOf(const DynamicTypeExpr *DTE) {
    if (!SF->getASTContext().LangOpts.isSwiftVersion3()) {
      return;
    }
    const auto CSR = Lexer::getCharSourceRangeFromSourceRange(SM,
      DTE->getSourceRange());
    if (SM.extractText(CSR).startswith("Swift.")) {
      return;
    }

    UnqualifiedLookup Lookup {
      { SF->getASTContext().getIdentifier("type") },
      ContextStack.empty() ? SF->getModuleScopeContext() : ContextStack.back(),
      /*TypeResolver=*/SF->getASTContext().getLazyResolver(),
      DTE->getLoc()
    };
    auto isShadowing = [&]() -> bool {
      if (Lookup.Results.empty())
        return false;
      if (Lookup.Results.size() != 1)
        return true;
      if (auto VD = Lookup.Results.front().getValueDecl()) {
        return !VD->getModuleContext()->isStdlibModule();
      }
      return false;
    };
    if (!isShadowing()) {
      // There won't be a name shadowing here in Swift 4, so we don't need to
      // do anything.
      return;
    }
    Editor.insertBefore(DTE->getLoc(), "Swift.");
  }

  bool walkToExprPre(Expr *E) override {
    if (const auto *DTE = dyn_cast<DynamicTypeExpr>(E)) {
      handleTypeOf(DTE);
    }
    return true;
  }

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    if (auto DC = dyn_cast<DeclContext>(D))
      ContextStack.push_back(DC);
    return true;
  }

  bool walkToDeclPost(Decl *D) override {
    if (isa<DeclContext>(D))
      ContextStack.pop_back();
    return true;
  }

public:
  TypeOfMigratorPass(EditorAdapter &Editor,
                         SourceFile *SF,
                         const MigratorOptions &Opts)
    : ASTMigratorPass(Editor, SF, Opts) {}
};

} // end anonymous namespace

void migrator::runTypeOfMigratorPass(EditorAdapter &Editor,
                                     SourceFile *SF,
                                     const MigratorOptions &Opts) {
  TypeOfMigratorPass { Editor, SF, Opts }.walk(SF);
}

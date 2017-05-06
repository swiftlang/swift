//===--- TypeOfMigratorPass.cpp ------------------------------------------===//
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
#include "swift/Migrator/ASTMigratorPass.h"
#include "swift/Parse/Lexer.h"

using namespace swift;
using namespace swift::migrator;

namespace {

struct TypeOfMigratorPass: public ASTMigratorPass,
  public SourceEntityWalker {

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
      SF->getModuleScopeContext(),
      /*TypeResolver=*/nullptr,
      /*IsKnownPrivate=*/false,
      DTE->getLoc()
    };
    if (Lookup.Results.empty()) {
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

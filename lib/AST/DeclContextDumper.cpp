//===--- DeclContextDumper.cpp --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/SetVector.h"

using namespace swift;

namespace {

/// Collect 'DeclContext' hierarchy from AST.
class DeclContextHierarchyCollector : public ASTWalker {
  llvm::DenseMap<DeclContext *, llvm::SetVector<DeclContext *>> &Map;

public:
  DeclContextHierarchyCollector(
      llvm::DenseMap<DeclContext *, llvm::SetVector<DeclContext *>> &Map)
      : Map(Map) {}

  // Insert DC and its ascendants into the map.
  void handle(DeclContext *DC) {
    if (!DC)
      return;

    auto *parentDC = DC->getParent();
    if (Map[parentDC].insert(DC))
      handle(parentDC);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    handle(D->getDeclContext());

    if (auto *dc = dyn_cast<DeclContext>(D)) {
      handle(dc);
    }

    if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
      for (unsigned i = 0, e = PBD->getNumPatternEntries(); i != e; ++i)
        handle(PBD->getInitContext(i));
    }

    for (auto *attr : D->getAttrs().getAttributes<CustomAttr>()) {
      handle(attr->getInitContext());
    }

    return Action::Continue();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (auto *dc = dyn_cast<AbstractClosureExpr>(E)) {
      handle(dc);
    }
    switch (E->getKind()) {
    case ExprKind::SingleValueStmt:
      handle(cast<SingleValueStmtExpr>(E)->getDeclContext());
      break;
    case ExprKind::MacroExpansion:
      handle(cast<MacroExpansionExpr>(E)->getDeclContext());
      break;
    default:
      break;
    }
    return Action::Continue(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    switch (S->getKind()) {
    case StmtKind::DoCatch:
      handle(cast<DoCatchStmt>(S)->getDeclContext());
      break;
    case StmtKind::Fallthrough:
      handle(cast<FallthroughStmt>(S)->getDeclContext());
      break;
    case StmtKind::Break:
      handle(cast<BreakStmt>(S)->getDeclContext());
      break;
    case StmtKind::Continue:
      handle(cast<ContinueStmt>(S)->getDeclContext());
      break;
    default:
      break;
    }
    return Action::Continue(S);
  }

  PreWalkResult<Pattern *> walkToPatternPre(Pattern *P) override {
    switch (P->getKind()) {
    case PatternKind::EnumElement:
      handle(cast<EnumElementPattern>(P)->getDeclContext());
      break;
    case PatternKind::Expr:
      handle(cast<ExprPattern>(P)->getDeclContext());
      break;
    default:
      break;
    }
    return Action::Continue(P);
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    switch (T->getKind()) {
    case TypeReprKind::QualifiedIdent:
    case TypeReprKind::UnqualifiedIdent: {
      auto *tyR = cast<DeclRefTypeRepr>(T);
      if (tyR->isBound())
        handle(tyR->getDeclContext());
      break;
    }
    default:
      break;
    }
    return Action::Continue();
  }
};
} // namespace

void swift::dumpDeclContextHierarchy(llvm::raw_ostream &OS,
                                     SourceFile &SF) {
  llvm::DenseMap<DeclContext *, llvm::SetVector<DeclContext *>> map;

  DeclContextHierarchyCollector collector(map);
  SF.walk(collector);

  std::function<void(DeclContext *, size_t)> printChildrenDC =
      [&](DeclContext *parentDC, size_t indent) -> void {
    for (auto *DC : map[parentDC]) {
      DC->printContext(OS, indent, /*onlyAPartialLine=*/true);
      OS << "\n";
      printChildrenDC(DC, indent + 2);
    }
  };

  printChildrenDC(nullptr, 0);
}

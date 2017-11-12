//===--- GenClangDecl.cpp - Swift IRGen for imported Clang declarations ---===//
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

#include "IRGenModule.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;
using namespace irgen;

namespace {
class ClangDeclRefFinder
    : public clang::RecursiveASTVisitor<ClangDeclRefFinder> {
  std::function<void(const clang::DeclRefExpr *)> callback;
public:
  template <typename Fn>
  explicit ClangDeclRefFinder(Fn fn) : callback(fn) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr *DRE) {
    callback(DRE);
    return true;
  }
};
} // end anonymous namespace

void IRGenModule::emitClangDecl(const clang::Decl *decl) {
  auto valueDecl = dyn_cast<clang::ValueDecl>(decl);
  if (!valueDecl || valueDecl->isExternallyVisible()) {
    ClangCodeGen->HandleTopLevelDecl(
                          clang::DeclGroupRef(const_cast<clang::Decl*>(decl)));
    return;
  }

  if (!GlobalClangDecls.insert(decl->getCanonicalDecl()).second)
    return;
  SmallVector<const clang::Decl *, 8> stack;
  stack.push_back(decl);

  ClangDeclRefFinder refFinder([&](const clang::DeclRefExpr *DRE) {
    const clang::Decl *D = DRE->getDecl();
    // Check that this is a file-level declaration and not inside a function.
    // If it's a member of a file-level decl, like a C++ static member variable,
    // we want to add the entire file-level declaration because Clang doesn't
    // expect to see members directly here.
    for (auto *DC = D->getDeclContext();; DC = DC->getParent()) {
      if (DC->isFunctionOrMethod())
        return;
      if (DC->isFileContext())
        break;
      D = cast<const clang::Decl>(DC);
    }
    if (!GlobalClangDecls.insert(D->getCanonicalDecl()).second)
      return;
    stack.push_back(D);
  });

  while (!stack.empty()) {
    auto *next = const_cast<clang::Decl *>(stack.pop_back_val());
    if (auto fn = dyn_cast<clang::FunctionDecl>(next)) {
      const clang::FunctionDecl *definition;
      if (fn->hasBody(definition)) {
        refFinder.TraverseDecl(const_cast<clang::FunctionDecl *>(definition));
        next = const_cast<clang::FunctionDecl *>(definition);
      }
    }
    ClangCodeGen->HandleTopLevelDecl(clang::DeclGroupRef(next));
  }
}

llvm::Constant *
IRGenModule::getAddrOfClangGlobalDecl(clang::GlobalDecl global,
                                      ForDefinition_t forDefinition) {
  // Register the decl with the clang code generator.
  if (auto decl = global.getDecl())
    emitClangDecl(decl);

  return ClangCodeGen->GetAddrOfGlobal(global, (bool) forDefinition);
}

void IRGenModule::finalizeClangCodeGen() {
  ClangCodeGen->HandleTranslationUnit(
      *const_cast<clang::ASTContext *>(ClangASTContext));
}

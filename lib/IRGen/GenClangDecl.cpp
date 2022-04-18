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
#include "swift/AST/ASTContext.h"
#include "swift/AST/IRGenOptions.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclGroup.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/CodeGen/ModuleBuilder.h"
#include "clang/Sema/Sema.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;
using namespace irgen;

namespace {
class ClangDeclFinder
    : public clang::RecursiveASTVisitor<ClangDeclFinder> {
  std::function<void(const clang::Decl *)> callback;
public:
  template <typename Fn>
  explicit ClangDeclFinder(Fn fn) : callback(fn) {}

  bool VisitDeclRefExpr(clang::DeclRefExpr *DRE) {
    if (isa<clang::FunctionDecl>(DRE->getDecl()) ||
        isa<clang::VarDecl>(DRE->getDecl())) {
      callback(DRE->getDecl());
    }

    return true;
  }

  bool VisitMemberExpr(clang::MemberExpr *ME) {
    if (isa<clang::FunctionDecl>(ME->getMemberDecl()) ||
        isa<clang::VarDecl>(ME->getMemberDecl()) || 
        isa<clang::FieldDecl>(ME->getMemberDecl())) {
      callback(ME->getMemberDecl());
    }
    return true;
  }

  bool VisitCXXConstructorDecl(clang::CXXConstructorDecl *CXXCD) {
    callback(CXXCD);
    for (clang::CXXCtorInitializer *CXXCI : CXXCD->inits()) {
      if (clang::FieldDecl *FD = CXXCI->getMember())
        callback(FD);
    }
    return true;
  }

  bool VisitCXXConstructExpr(clang::CXXConstructExpr *CXXCE) {
    callback(CXXCE->getConstructor());
    return true;
  }

  bool VisitVarDecl(clang::VarDecl *VD) {
    if (auto cxxRecord = VD->getType()->getAsCXXRecordDecl())
      if (auto dtor = cxxRecord->getDestructor())
        callback(dtor);

    return true;
  }

  bool shouldVisitTemplateInstantiations() const { return true; }
  bool shouldVisitImplicitCode() const { return true; }
};

// If any (re)declaration of `decl` contains executable code, returns that
// redeclaration; otherwise, returns nullptr.
// In the case of a function, executable code is contained in the function
// definition. In the case of a variable, executable code can be contained in
// the initializer of the variable.
clang::Decl *getDeclWithExecutableCode(clang::Decl *decl) {
  if (auto fd = dyn_cast<clang::FunctionDecl>(decl)) {
    const clang::FunctionDecl *definition;
    if (fd->hasBody(definition)) {
      return const_cast<clang::FunctionDecl *>(definition);
    }

    // If this is a potentially not-yet-instanciated template, we might
    // still have a body.
    if (fd->getTemplateInstantiationPattern())
      return fd;
  } else if (auto vd = dyn_cast<clang::VarDecl>(decl)) {
    clang::VarDecl *initializingDecl = vd->getInitializingDeclaration();
    if (initializingDecl) {
      return initializingDecl;
    }
  } else if (auto fd = dyn_cast<clang::FieldDecl>(decl)) {
    if(fd->hasInClassInitializer()) {
      return fd;
    }
  }
  return nullptr;
}

} // end anonymous namespace

void IRGenModule::emitClangDecl(const clang::Decl *decl) {
  // Ignore this decl if we've seen it before.
  if (!GlobalClangDecls.insert(decl->getCanonicalDecl()).second)
    return;

  // Fast path for the case where `decl` doesn't contain executable code, so it
  // can't reference any other declarations that we would need to emit.
  if (getDeclWithExecutableCode(const_cast<clang::Decl *>(decl)) == nullptr) {
    ClangCodeGen->HandleTopLevelDecl(
                          clang::DeclGroupRef(const_cast<clang::Decl*>(decl)));
    return;
  }

  SmallVector<const clang::Decl *, 8> stack;
  stack.push_back(decl);

  auto callback = [&](const clang::Decl *D) {
    for (auto *DC = D->getDeclContext();; DC = DC->getParent()) {
      // Check that this is not a local declaration inside a function.
      if (DC->isFunctionOrMethod()) {
        return;
      }
      if (DC->isFileContext()) {
        break;
      }
      if (isa<clang::TagDecl>(DC)) {
        break;
      }
      D = cast<const clang::Decl>(DC);
    }
    if (!GlobalClangDecls.insert(D->getCanonicalDecl()).second) {
      return;
    }

    stack.push_back(D);
  };

  ClangDeclFinder refFinder(callback);

  while (!stack.empty()) {
    auto *next = const_cast<clang::Decl *>(stack.pop_back_val());

    // If a function calls another method in a class template specialization, we
    // need to instantiate that other function. Do that here.
    if (auto *fn = dyn_cast<clang::FunctionDecl>(next)) {
      // Make sure that this method is part of a class template specialization.
      if (fn->getTemplateInstantiationPattern())
        Context.getClangModuleLoader()
            ->getClangSema()
            .InstantiateFunctionDefinition(fn->getLocation(), fn);
    }

    if (clang::Decl *executableDecl = getDeclWithExecutableCode(next)) {
        refFinder.TraverseDecl(executableDecl);
        next = executableDecl;
    }

    // Unfortunately, implicitly defined CXXDestructorDecls don't have a real
    // body, so we need to traverse these manually.
    if (auto *dtor = dyn_cast<clang::CXXDestructorDecl>(next)) {
      auto cxxRecord = dtor->getParent();

      for (auto field : cxxRecord->fields()) {
        if (auto fieldCxxRecord = field->getType()->getAsCXXRecordDecl())
          if (auto *fieldDtor = fieldCxxRecord->getDestructor())
            callback(fieldDtor);
      }

      for (auto base : cxxRecord->bases()) {
        if (auto baseCxxRecord = base.getType()->getAsCXXRecordDecl())
          if (auto *baseDtor = baseCxxRecord->getDestructor())
            callback(baseDtor);
      }
    }

    if (auto var = dyn_cast<clang::VarDecl>(next))
      if (!var->isFileVarDecl())
        continue;
    if (isa<clang::FieldDecl>(next)) {
      continue;
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
  // FIXME: We try to avoid looking for PragmaCommentDecls unless we need to,
  // since clang::DeclContext::decls_begin() can trigger expensive
  // de-serialization.
  if (Triple.isWindowsMSVCEnvironment() || Triple.isWindowsItaniumEnvironment() ||
      IRGen.Opts.LLVMLTOKind != IRGenLLVMLTOKind::None) {
    // Ensure that code is emitted for any `PragmaCommentDecl`s. (These are
    // always guaranteed to be directly below the TranslationUnitDecl.)
    // In Clang, this happens automatically during the Sema phase, but here we
    // need to take care of it manually because our Clang CodeGenerator is not
    // attached to Clang Sema as an ASTConsumer.
    for (const auto *D : ClangASTContext->getTranslationUnitDecl()->decls()) {
      if (const auto *PCD = dyn_cast<clang::PragmaCommentDecl>(D)) {
        emitClangDecl(PCD);
      }
    }
  }

  ClangCodeGen->HandleTranslationUnit(
      *const_cast<clang::ASTContext *>(ClangASTContext));
}

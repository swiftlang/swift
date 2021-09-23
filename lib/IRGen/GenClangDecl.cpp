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

#define DEBUG_TYPE "clang-decl-finder"

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

  bool VisitCXXConstructExpr(clang::CXXConstructExpr *CXXCE) {
    callback(CXXCE->getConstructor());
    return true;
  }
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

auto walkCallGraphFromCtor(
    const clang::CXXConstructorDecl *toplevelCtor,
    llvm::SmallPtrSet<const clang::Decl *, 8> visited,
    llvm::SmallPtrSet<const clang::Decl *, 8> &functionDecls) {

  llvm::SmallVector<clang::Stmt *, 8> stack;
  auto recurseStmt = [&stack](clang::Stmt *s) {
    if (s) {
      stack.push_back(s);
      return true;
    }
    return false;
  };

  // Keep track of decls we have already walked over so that we don't re-walk
  // over them redundantly. Returns true if the insertion took place.
  auto visitDecl = [&visited](const clang::Decl *decl) {
    return decl ? visited.insert(decl).second : false;
  };

  auto handleCtor = [&recurseStmt,
                     &functionDecls](const clang::CXXConstructorDecl *ctor) {
    functionDecls.insert(ctor);
    if (ctor->getParent()->getDestructor())
      functionDecls.insert(ctor->getParent()->getDestructor());
    recurseStmt(ctor->getBody());
    for (auto *init : ctor->inits()) {
      recurseStmt(init->getInit());
      if (clang::FieldDecl *field = init->getMember())
        recurseStmt(field->getInClassInitializer());
    }
  };

  auto handleFunctionDecl = [&recurseStmt, &functionDecls](
                                clang::FunctionDecl *functionDecl) {
    LLVM_DEBUG({
      llvm::dbgs() << "HANDLE FUNCTION DECL:\n";
      llvm::dbgs() << "IS INLINE "
                   << (functionDecl->isInlineSpecified() ? "YES" : "NO")
                   << "\n";
      llvm::dbgs() << "IS TEMPLATE INST "
                   << (functionDecl->isTemplateInstantiation() ? "YES" : "NO")
                   << "\n";
    });

    if (functionDecl->isInlineSpecified() ||     // is 'inline' specified
        functionDecl->isInlined() ||             // is inlined or constexpr
        functionDecl->isTemplateInstantiation()) // is template instance
      functionDecls.insert(functionDecl);

    // Even if this function is not inlined or a template instance we want to
    // recurse on it in case it calls something or calls something that calls
    // something that is.
    recurseStmt(functionDecl->getBody());
  };

  handleCtor(toplevelCtor);

  while (!stack.empty()) {
    auto *back = stack.pop_back_val();

    LLVM_DEBUG({
      llvm::dbgs() << "\nClang Decl Walker Candidate:\n";
      back->dump(llvm::dbgs(), toplevelCtor->getASTContext());
    });

    // Handle if the expression is a callsite or a ExprWithCleanups.
    if (auto *fn = dyn_cast<clang::MemberExpr>(back)) {
      if (visitDecl(fn->getMemberDecl()))
        recurseStmt(fn->getMemberDecl()->getBody());
    } else if (auto *fn = dyn_cast<clang::CallExpr>(back)) {
      auto *memberCall = dyn_cast<clang::CXXMemberCallExpr>(back);
      if (memberCall && visitDecl(memberCall->getMethodDecl())) {
        functionDecls.insert(memberCall->getMethodDecl());
        recurseStmt(memberCall->getMethodDecl()->getBody());
      }

      if (visitDecl(fn->getCalleeDecl())) {
        functionDecls.insert(fn->getCalleeDecl());
        recurseStmt(fn->getCalleeDecl()->getBody());
      }
    } else if (auto *fn = dyn_cast<clang::CXXConstructExpr>(back)) {

      // Use the constructor expression to traverse corresponding destructors.
      // This is done because default destructor decls are empty and have very
      // little to traverse.
      if (auto *dtor = fn->getConstructor() && fn->getConstructor()->getParent()
                           ? fn->getConstructor()->getParent()->getDestructor()
                           : nullptr) {
        handleFunctionDecl(dtor);
        if (dtor->getBody())
          recurseStmt(dtor->getBody());
      }

      for (auto *child : fn->children())
        recurseStmt(child);

      if (visitDecl(fn->getConstructor()))
        handleCtor(fn->getConstructor());
    } else if (auto *declRef = dyn_cast<clang::DeclRefExpr>(back)) {
      if (auto *fn = dyn_cast_or_null<clang::FunctionDecl>(declRef->getDecl()))
        handleFunctionDecl(fn);
    }

    // Walk the expression's children.
    for (clang::Stmt *child : back->children())
      recurseStmt(child);
  }
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

  ClangDeclFinder refFinder([&](const clang::Decl *D) {
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
  });

  std::vector<const clang::Decl *> ctors;
  llvm::copy_if(stack, std::back_inserter(ctors), [](const clang::Decl *decl) {
    return isa<clang::CXXConstructorDecl>(decl);
  });

  llvm::SmallPtrSet<const clang::Decl *, 8> visited;
  llvm::SmallPtrSet<const clang::Decl *, 8> functionDecls;
  for (auto *ctor : ctors)
    walkCallGraphFromCtor(cast<clang::CXXConstructorDecl>(ctor), visited,
                          functionDecls);
  llvm::copy(functionDecls, std::back_inserter(stack));

  LLVM_DEBUG({
    llvm::dbgs()
        << "\nAdditional Function/Method decls found on constructor path:\n";
    for (auto *functionDecl : functionDecls)
      functionDecl->dump(llvm::dbgs());
    llvm::dbgs() << "\n";
  });

  while (!stack.empty()) {
    auto *next = const_cast<clang::Decl *>(stack.pop_back_val());
    if (clang::Decl *executableDecl = getDeclWithExecutableCode(next)) {
        refFinder.TraverseDecl(executableDecl);
        next = executableDecl;
    }

    if (auto var = dyn_cast<clang::VarDecl>(next))
      if (!var->isFileVarDecl())
        continue;
    if (isa<clang::FieldDecl>(next)) {
      continue;
    }
    // If a method calls another method in a class template specialization, we
    // need to instantiate that other method. Do that here.
    if (auto *method = dyn_cast<clang::CXXMethodDecl>(next)) {
      // Make sure that this method is part of a class template specialization.
      if (method->getTemplateInstantiationPattern())
        Context.getClangModuleLoader()
            ->getClangSema()
            .InstantiateFunctionDefinition(method->getLocation(), method);
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

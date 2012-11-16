//===--- ClangImporter.cpp - Import Clang Modules -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements support for loading Clang modules into Swift.
//
//===----------------------------------------------------------------------===//
#include "swift/ClangImporter/ClangImporter.h"
#include "ImporterImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/STLExtras.h"
#include <memory>

using namespace swift;

// Commonly-used Clang classes.
using clang::CompilerInstance;
using clang::CompilerInvocation;

#pragma mark Internal data structures
namespace {
  class SwiftModuleLoaderAction : public clang::SyntaxOnlyAction {
  protected:
    /// BeginSourceFileAction - Callback at the start of processing a single
    /// input.
    ///
    /// \return True on success; on failure \see ExecutionAction() and
    /// EndSourceFileAction() will not be called.
    virtual bool BeginSourceFileAction(CompilerInstance &ci,
                                       StringRef filename) {
      // Enable incremental processing, so we can load modules after we've
      // finished parsing our fake translation unit.
      ci.getPreprocessor().enableIncrementalProcessing();

      return clang::SyntaxOnlyAction::BeginSourceFileAction(ci, filename);
    }
  };
}


ClangImporter::ClangImporter(ASTContext &ctx)
  : Impl(*new Implementation(ctx))
{
}

ClangImporter::~ClangImporter() {
  delete &Impl;
}

#pragma mark Module loading

ClangImporter *ClangImporter::create(ASTContext &ctx, StringRef sdkroot,
                                     StringRef targetTriple,
                                     StringRef moduleCachePath) {
  std::unique_ptr<ClangImporter> importer(new ClangImporter(ctx));

  // Create a Clang diagnostics engine.
  // FIXME: Route these diagnostics back to Swift's diagnostics engine,
  // somehow. We'll lose macro expansions, but so what.
  auto clangDiags(CompilerInstance::createDiagnostics(
                    new clang::DiagnosticOptions, 0, nullptr));


  // Construct the invocation arguments for Objective-C ARC with the current
  // target.
  //
  // FIXME: Figure out an appropriate OS deployment version to pass along.
  std::vector<std::string> invocationArgStrs = {
    "-x", "objective-c", "-fobjc-arc", "-fmodules", "-fsyntax-only", "-w",
    "-isysroot", sdkroot.str(), "-triple", targetTriple.str(), "swift.m"
  };

  // If there is a module cache path, pass it along.
  if (!moduleCachePath.empty()) {
    invocationArgStrs.push_back("-fmodule-cache-path");
    invocationArgStrs.push_back(moduleCachePath.str());
  }

  std::vector<const char *> invocationArgs;
  for (auto &argStr : invocationArgStrs)
    invocationArgs.push_back(argStr.c_str());

  // Create a new Clang compiler invocation.
  llvm::IntrusiveRefCntPtr<CompilerInvocation> invocation
    = new CompilerInvocation;
  if (!CompilerInvocation::CreateFromArgs(*invocation,
                                         &invocationArgs.front(),
                                         (&invocationArgs.front() +
                                          invocationArgs.size()),
                                         *clangDiags))
    return nullptr;

  // Create an almost-empty memory buffer corresponding to the file "swift.m"
  auto sourceBuffer = llvm::MemoryBuffer::getMemBuffer("extern int __swift;");
  invocation->getPreprocessorOpts().addRemappedFile("swift.m", sourceBuffer);

  // Create a compiler instance.
  importer->Impl.Instance.reset(new CompilerInstance);
  auto &instance = *importer->Impl.Instance;
  instance.setDiagnostics(&*clangDiags);
  instance.setInvocation(&*invocation);

  // Create the associated action.
  importer->Impl.Action.reset(new SwiftModuleLoaderAction);

  // Execute the action. We effectively inline mosst of
  // CompilerInstance::ExecuteAction here, because we need to leave the AST
  // open for future module loading.
  // FIXME: This has to be cleaned up on the Clang side before we can improve
  // things here.

  // Create the target instance.
  instance.setTarget(
    clang::TargetInfo::CreateTargetInfo(*clangDiags,&instance.getTargetOpts()));
  if (!instance.hasTarget())
    return nullptr;

  // Inform the target of the language options.
  //
  // FIXME: We shouldn't need to do this, the target should be immutable once
  // created. This complexity should be lifted elsewhere.
  instance.getTarget().setForcedLangOptions(instance.getLangOpts());

  // Run the action.
  auto &action = *importer->Impl.Action;
  if (action.BeginSourceFile(instance, instance.getFrontendOpts().Inputs[0])) {
    action.Execute();
    // Note: don't call EndSourceFile here!
  }

  return importer.release();
}

Module *ClangImporter::loadModule(
          SourceLoc importLoc,
          ArrayRef<std::pair<Identifier, SourceLoc>> path) {
  // Convert the Swift import path over to a Clang import path.
  // FIXME: Map source locations over. Fun, fun!
  SmallVector<std::pair<clang::IdentifierInfo *, clang::SourceLocation>, 4>
    clangPath;

  auto &clangContext = Impl.Instance->getASTContext();
  for (auto component : path) {
    clangPath.push_back({&clangContext.Idents.get(component.first.str()),
                         clang::SourceLocation()} );
  }

  // Load the Clang module.
  auto clangModule = Impl.Instance->loadModule(clang::SourceLocation(),
                                               clangPath,
                                               clang::Module::AllVisible,
                                               /*IsInclusionDirective=*/false);
  if (!clangModule)
    return nullptr;

  // FIXME: Revisit this once components are fleshed out. Clang components
  // are likely born-fragile.
  auto component = new (Impl.SwiftContext.Allocate<Component>(1)) Component();

  // Build the representation of the Clang module in Swift.
  auto result = new (Impl.SwiftContext) ClangModule(Impl.SwiftContext,
                                                    component, clangModule);

  // FIXME: Total hack.
  if (!Impl.firstClangModule)
    Impl.firstClangModule = result;

  return result;
}

#pragma mark Importing names
clang::DeclarationName
ClangImporter::Implementation::importName(Identifier name) {
  // FIXME: When we start dealing with C++, we can map over some operator
  // names.
  if (name.isOperator())
    return clang::DeclarationName();

  // Map the identifier. If it's some kind of keyword, it can't be mapped.
  auto ident = &Instance->getASTContext().Idents.get(name.str());
  if (ident->getTokenID() != clang::tok::identifier)
    return clang::DeclarationName();

  return ident;
}

Identifier
ClangImporter::Implementation::importName(clang::DeclarationName name) {
  // FIXME: At some point, we'll be able to import operators as well.
  if (!name || name.getNameKind() != clang::DeclarationName::Identifier)
    return Identifier();

  // Make the identifier over.
  // FIXME: Check for Swift keywords, and filter those out.
  return SwiftContext.getIdentifier(name.getAsIdentifierInfo()->getName());
}


#pragma mark Name lookup
void ClangImporter::lookupValue(ClangModule *module,
                                Module::AccessPathTy accessPath,
                                Identifier name,
                                NLKind lookupKind,
                                SmallVectorImpl<ValueDecl*> &results) {
  auto &sema = Impl.Instance->getSema();

  // Map the name. If we can't represent the Swift name in Clang, bail out now.
  auto clangName = Impl.importName(name);
  if (!clangName)
    return;

  // Perform name lookup into the global scope.
  // FIXME: Map source locations over.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   clang::Sema::LookupOrdinaryName);
  if (!sema.LookupName(lookupResult, /*Scope=*/0))
    return;

  // FIXME: Filter based on access path? C++ access control?
  for (auto decl : lookupResult) {
    if (auto swiftDecl = Impl.importDecl(decl))
      results.push_back(swiftDecl);
  }
}

#pragma mark Declaration imports
ValueDecl *ClangImporter::Implementation::importDecl(clang::NamedDecl *decl) {
  auto known = ImportedDecls.find(decl);
  if (known != ImportedDecls.end())
    return known->second;

  if (decl->getKind() == clang::Decl::Function) {
    auto func = cast<clang::FunctionDecl>(decl);
    auto type = importType(func->getType());
    if (!type)
      return nullptr;

    auto name = importName(decl->getDeclName());

    // FIXME: Map source locations!
    auto result = new (SwiftContext) FuncDecl(SourceLoc(),
                                              SourceLoc(),
                                              name,
                                              SourceLoc(),
                                              /*GenericParams=*/0,
                                              type,
                                              /*Body=*/nullptr,
                                              firstClangModule);
    return ImportedDecls[decl] = result;
  }

  return nullptr;
}


#pragma mark Type imports
Type ClangImporter::Implementation::importType(clang::QualType type) {
  auto &clangContext = getClangASTContext();
  if (auto funcTy = type->getAs<clang::FunctionProtoType>()) {
    // Only allow void(void) types for now.
    if (!clangContext.hasSameUnqualifiedType(clangContext.VoidTy,
                                             funcTy->getResultType()) ||
        funcTy->getNumArgs() != 0 ||
        funcTy->isVariadic())
      return Type();

    return FunctionType::get(SwiftContext.TheEmptyTupleType,
                             SwiftContext.TheEmptyTupleType,
                             SwiftContext);
  }

  return Type();
}


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
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/Version.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/PathV2.h"
#include "llvm/Support/SourceMgr.h"
#include <memory>
#include <cstdlib>
#include <dlfcn.h>
#include <sys/param.h>

using namespace swift;

// Commonly-used Clang classes.
using clang::CompilerInstance;
using clang::CompilerInvocation;

// Dummy function used with dladdr.
void swift_clang_importer() { }

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
    "-x", "objective-c", "-fobjc-arc", "-fmodules", "-fblocks",
    "-fsyntax-only", "-w",
    "-isysroot", sdkroot.str(), "-triple", targetTriple.str(),
    "swift.m"
  };

  // Set the module cache path.
  if (moduleCachePath.empty()) {
    llvm::SmallString<128> DefaultModuleCache;
    llvm::sys::path::system_temp_directory(/*erasedOnReboot=*/false,
                                           DefaultModuleCache);
    invocationArgStrs.push_back("-fmodule-cache-path");
    invocationArgStrs.push_back(DefaultModuleCache.str());
  } else {
    invocationArgStrs.push_back("-fmodule-cache-path");
    invocationArgStrs.push_back(moduleCachePath.str());
  }

  // Figure out where Swift lives; since Clang is linked into Swift,
  // we assume that the headers are in the same place Clang would look.
  // This silly cast below avoids a C++ warning.
  Dl_info info;
  if (dladdr((void *)(uintptr_t)swift_clang_importer, &info) == 0)
    llvm_unreachable("Call to dladdr() failed");

  // Resolve symlinks.
  char swiftPath[MAXPATHLEN];
  if (!realpath(info.dli_fname, swiftPath)) {
    // FIXME: Diagnose this.
    return nullptr;
  }

  llvm::SmallString<128> resourceDir(swiftPath);

  // We now have the swift executable/library path. Adjust it to refer to Clang.
  llvm::sys::path::remove_filename(resourceDir);
  llvm::sys::path::remove_filename(resourceDir);
#ifdef SWIFT_BUILT_WITH_CMAKE
  // One extra level of adjustment to do.
  // FIXME: This is unspeakably horrible.
  llvm::sys::path::remove_filename(resourceDir);
#endif
  llvm::sys::path::append(resourceDir, "lib", "clang", CLANG_VERSION_STRING);

  // Set the Clang resource directory to the path we computed.
  invocationArgStrs.push_back("-resource-dir");
  invocationArgStrs.push_back(resourceDir.str());

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
  // FIXME: This is necessary because Clang doesn't really support what we're
  // doing, and TUScope has gone stale.
  instance.getSema().TUScope = nullptr;

  // Create the selectors we'll be looking for.
  auto &clangContext = importer->Impl.Instance->getASTContext();
  importer->Impl.objectAtIndexedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectAtIndexedSubscript"));
  clang::IdentifierInfo *setObjectAtIndexedSubscriptIdents[2] = {
    &clangContext.Idents.get("setObject"),
    &clangContext.Idents.get("atIndexedSubscript")
  };
  importer->Impl.setObjectAtIndexedSubscript
    = clangContext.Selectors.getSelector(2, setObjectAtIndexedSubscriptIdents);


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

  // Bump the generation count.
  ++Impl.Generation;

  return result;
}

#pragma mark Source locations
clang::SourceLocation
ClangImporter::Implementation::importSourceLoc(SourceLoc loc) {
  // FIXME: Implement!
  return clang::SourceLocation();
}

SourceLoc
ClangImporter::Implementation::importSourceLoc(clang::SourceLocation loc) {
  // FIXME: Implement!
  return SourceLoc(
           llvm::SMLoc::getFromPointer(
             SwiftContext.SourceMgr.getBufferInfo(0).Buffer->getBufferStart()));
}

SourceRange
ClangImporter::Implementation::importSourceRange(clang::SourceRange loc) {
  // FIXME: Implement!
  return SourceRange();
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
void ClangImporter::lookupValue(Module *module,
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
  if (!sema.LookupName(lookupResult, /*Scope=*/0)) {
    // If we didn't find an ordinary name, fall back to a tag name.
    lookupResult.clear(clang::Sema::LookupTagName);
    if (!sema.LookupName(lookupResult, /*Scope=*/0))
      return;
  }

  // FIXME: Filter based on access path? C++ access control?
  for (auto decl : lookupResult) {
    if (auto swiftDecl = Impl.importDecl(decl->getUnderlyingDecl()))
      if (auto valueDecl = dyn_cast<ValueDecl>(swiftDecl))
      results.push_back(valueDecl);
  }
}

ArrayRef<ExtensionDecl*>
ClangImporter::lookupExtensions(Module *module, Type type) {
  // Figure out if this type is actually an Objective-C class imported into
  // Swift.
  auto classDecl = type->getClassOrBoundGenericClass();
  if (!classDecl)
    return { };

  auto objcClass
    = dyn_cast_or_null<clang::ObjCInterfaceDecl>(classDecl->getClangDecl());
  if (!objcClass)
    return { };

  // Check the cache. If it is up-to-date, use it.
  auto &cache = Impl.ClassExtensions[classDecl];
  if (cache.Generation == Impl.Generation) {
    if (cache.Extensions)
      return *cache.Extensions;

    return { };
  }

  // Rebuild the cache.
  cache.Generation = Impl.Generation;
  auto extensions = cache.Extensions;
  if (extensions)
    extensions->clear();
  for (auto category = objcClass->getCategoryList(); category;
       category = category->getNextClassCategory()) {
    if (auto imported = cast_or_null<ExtensionDecl>(Impl.importDecl(category))){
      if (!extensions) {
        extensions = new SmallVector<ExtensionDecl *, 4>;

        // The dense map may have been re-allocated, so we can't use 'cache'
        // here.
        Impl.ClassExtensions[classDecl].Extensions = extensions;
      }
      extensions->push_back(imported);
    }
  }

  if (extensions)
    return *extensions;

  return { };
}

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
#include "swift/ClangImporter/ClangModule.h"
#include "ImporterImpl.h"
#include "swift/Subsystems.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Range.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"
#include "clang/AST/ASTContext.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/Version.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Path.h"
#include <memory>

using namespace swift;

// Commonly-used Clang classes.
using clang::CompilerInstance;
using clang::CompilerInvocation;

ClangImporterCtorTy swift::getClangImporterCtor() {
  return &ClangImporter::create;
}

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
                                     StringRef swiftRuntimeIncludePath,
                                     StringRef moduleCachePath,
                                     ArrayRef<std::string> importSearchPaths,
                                     ArrayRef<std::string> frameworkSearchPaths,
                                     StringRef overrideResourceDir,
                                     ArrayRef<std::string> extraArgs) {
  std::unique_ptr<ClangImporter> importer(new ClangImporter(ctx));

  // Create a Clang diagnostics engine.
  // FIXME: Route these diagnostics back to Swift's diagnostics engine,
  // somehow. We'll lose macro expansions, but so what.
  auto clangDiags(CompilerInstance::createDiagnostics(
                    new clang::DiagnosticOptions, 0, nullptr));

  // Don't stop emitting messages if we ever can't find a file.
  // FIXME: This is actually a general problem: any "fatal" error could mess up
  // the CompilerInvocation.
  clangDiags->setDiagnosticErrorAsFatal(clang::diag::err_module_not_found,
                                        false);

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

  for (auto path : importSearchPaths) {
    invocationArgStrs.push_back("-I");
    invocationArgStrs.push_back(path);
  }

  for (auto path : frameworkSearchPaths) {
    invocationArgStrs.push_back("-F");
    invocationArgStrs.push_back(path);
  }

  // Set the module cache path.
  if (moduleCachePath.empty()) {
    llvm::SmallString<128> DefaultModuleCache;
    llvm::sys::path::system_temp_directory(/*erasedOnReboot=*/false,
                                           DefaultModuleCache);
    llvm::sys::path::append(DefaultModuleCache, "org.llvm.clang");
    llvm::sys::path::append(DefaultModuleCache, "ModuleCache");
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(DefaultModuleCache.str());
  } else {
    invocationArgStrs.push_back("-fmodules-cache-path=");
    invocationArgStrs.back().append(moduleCachePath.str());
  }

  if (overrideResourceDir.empty()) {
    llvm::SmallString<128> resourceDir(swiftRuntimeIncludePath);
  
    // Adjust the path torefer to our copy of the Clang headers under
    // lib/swift/clang.
  
    llvm::sys::path::append(resourceDir, "clang", CLANG_VERSION_STRING);
  
    // Set the Clang resource directory to the path we computed.
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(resourceDir.str());
  } else {
    invocationArgStrs.push_back("-resource-dir");
    invocationArgStrs.push_back(overrideResourceDir);
  }

  for (auto extraArg : extraArgs) {
    invocationArgStrs.push_back(extraArg);
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

  // Execute the action. We effectively inline most of
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
  importer->Impl.objectForKeyedSubscript
    = clangContext.Selectors.getUnarySelector(
        &clangContext.Idents.get("objectForKeyedSubscript"));
  clang::IdentifierInfo *setObjectForKeyedSubscriptIdents[2] = {
    &clangContext.Idents.get("setObject"),
    &clangContext.Idents.get("forKeyedSubscript")
  };
  importer->Impl.setObjectForKeyedSubscript
    = clangContext.Selectors.getSelector(2, setObjectForKeyedSubscriptIdents);

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
  // FIXME: The source location here is completely bogus. It can't be
  // invalid, and it can't be the same thing twice in a row, so we just use
  // a counter. Having real source locations would be far, far better.
  // FIXME: This should not print a message if we just can't find a Clang
  // module -- that's Swift's responsibility, since there could in theory be a
  // later module loader.
  auto &srcMgr = clangContext.getSourceManager();
  clang::SourceLocation clangImportLoc
    = srcMgr.getLocForStartOfFile(srcMgr.getMainFileID())
            .getLocWithOffset(Impl.ImportCounter++);
  auto clangModule = Impl.Instance->loadModule(clangImportLoc,
                                               clangPath,
                                               clang::Module::AllVisible,
                                               /*IsInclusionDirective=*/false);
  if (!clangModule)
    return nullptr;
  auto &cachedResult = Impl.ModuleWrappers[clangModule];
  if (Module *result = cachedResult.getPointer()) {
    if (!cachedResult.getInt()) {
      // Force load adapter modules for all imported modules.
      // FIXME: This forces the creation of wrapper modules for all imports as
      // well, and may do unnecessary work.
      cachedResult.setInt(true);
      result->forAllVisibleModules(path, [&](Module::ImportedModule import) {});
    }
    return result;
  }

  // Build the representation of the Clang module in Swift.
  // FIXME: The name of this module could end up as a key in the ASTContext,
  // but that's not correct for submodules.
  auto result = new (Impl.SwiftContext)
    ClangModule(Impl.SwiftContext, (*clangModule).getFullModuleName(),
                *this, clangModule);
  cachedResult.setPointer(result);

  // FIXME: Total hack.
  if (!Impl.firstClangModule)
    Impl.firstClangModule = result;

  // Force load adapter modules for all imported modules.
  // FIXME: This forces the creation of wrapper modules for all imports as
  // well, and may do unnecessary work.
  cachedResult.setInt(true);
  result->forAllVisibleModules(path, [](Module::ImportedModule import) {});

  // Bump the generation count.
  ++Impl.Generation;
  Impl.SwiftContext.bumpGeneration();

  return result;
}

ClangModule *
ClangImporter::Implementation::getWrapperModule(ClangImporter &importer,
                                                clang::Module *underlying) {
  auto &cacheEntry = ModuleWrappers[underlying];
  if (ClangModule *cachedModule = cacheEntry.getPointer())
    return cachedModule;

  auto result = new (SwiftContext) ClangModule(SwiftContext,
                                               underlying->getFullModuleName(),
                                               importer, underlying);
  cacheEntry.setPointer(result);
  return result;
}

ClangModule *ClangImporter::Implementation::getClangModuleForDecl(
    const clang::Decl *D) {
  if (auto OID = dyn_cast<clang::ObjCInterfaceDecl>(D))
    // Put the Objective-C class into the module that contains the @interface
    // definition, not just @class forward declaration.
    D = OID->getDefinition();
  else
    D = D->getCanonicalDecl();
  if (!D)
    return nullptr;

  clang::Module *M = D->getOwningModule();
  if (!M)
    return nullptr;
  // Get the parent module because currently we don't represent submodules with
  // ClangModule.
  // FIXME: this is just a workaround until we can import submodules.
  M = M->getTopLevelModule();

  auto &importer =
    static_cast<ClangImporter &>(*SwiftContext.getClangModuleLoader());
  return getWrapperModule(importer, M);
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
  return SourceLoc();
}

SourceRange
ClangImporter::Implementation::importSourceRange(clang::SourceRange loc) {
  // FIXME: Implement!
  return SourceRange();
}

#pragma mark Importing names

/// \brief Determine whether the given name is reserved for Swift.
static bool isSwiftReservedName(StringRef name) {
  /// FIXME: Check Swift keywords.
  return llvm::StringSwitch<bool>(name)
           .Cases("true", "false", true)
           .Default(false);
}

clang::DeclarationName
ClangImporter::Implementation::importName(Identifier name) {
  // FIXME: When we start dealing with C++, we can map over some operator
  // names.
  if (name.isOperator())
    return clang::DeclarationName();

  if (isSwiftReservedName(name.str()))
    return clang::DeclarationName();

  // Map the identifier. If it's some kind of keyword, it can't be mapped.
  auto ident = &Instance->getASTContext().Idents.get(name.str());
  if (ident->getTokenID() != clang::tok::identifier)
    return clang::DeclarationName();

  return ident;
}

Identifier
ClangImporter::Implementation::importName(clang::DeclarationName name,
                                          StringRef suffix) {
  // FIXME: At some point, we'll be able to import operators as well.
  if (!name || name.getNameKind() != clang::DeclarationName::Identifier)
    return Identifier();

  // Get the Swift identifier.
  if (suffix.empty()) {
    StringRef nameStr = name.getAsIdentifierInfo()->getName();
    if (isSwiftReservedName(nameStr))
      return Identifier();

    return SwiftContext.getIdentifier(nameStr);
  }

  // Append the suffix, and try again.
  llvm::SmallString<64> nameBuf;
  nameBuf += name.getAsIdentifierInfo()->getName();
  nameBuf += suffix;

  if (isSwiftReservedName(nameBuf))
    return Identifier();

  return SwiftContext.getIdentifier(nameBuf);
}


#pragma mark Name lookup
void ClangImporter::lookupValue(Module *module,
                                Module::AccessPathTy accessPath,
                                Identifier name,
                                NLKind lookupKind,
                                SmallVectorImpl<ValueDecl*> &results) {
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");
  if (accessPath.size() == 1 && accessPath.front().first != name)
    return;

  auto &pp = Impl.Instance->getPreprocessor();
  auto &sema = Impl.Instance->getSema();

  // If the name ends with 'Proto', strip off the 'Proto' and look for an
  // Objective-C protocol.
  // FIXME: Revisit this notion. We could append 'Proto' only when there is both
  // a class and a protocol with the same name, as with NSObject. However,
  // doing so requires our input modules to be "sane", in the sense that
  // one cannot introduce a class X in one module and a protocol X in a another
  // module that does *not* depend on 
  auto lookupNameKind = clang::Sema::LookupOrdinaryName;
  if (name.str().endswith("Proto")) {
    name = Impl.SwiftContext.getIdentifier(
             name.str().substr(0, name.str().size() - 5));
    lookupNameKind = clang::Sema::LookupObjCProtocolName;
  }

  // Map the name. If we can't represent the Swift name in Clang, bail out now.
  auto clangName = Impl.importName(name);
  if (!clangName)
    return;
  
  // See if there's a preprocessor macro we can import by this name.
  clang::IdentifierInfo *clangID = clangName.getAsIdentifierInfo();
  if (clangID && clangID->hasMacroDefinition()) {
    if (auto clangMacro = pp.getMacroInfo(clangID)) {
      if (auto valueDecl = Impl.importMacro(name, clangMacro)) {
        results.push_back(valueDecl);
      }
    }
  }

  // Perform name lookup into the global scope.
  // FIXME: Map source locations over.
  clang::LookupResult lookupResult(sema, clangName, clang::SourceLocation(),
                                   lookupNameKind);
  bool FoundType = false;
  if (sema.LookupName(lookupResult, /*Scope=*/0)) {
    // FIXME: Filter based on access path? C++ access control?
    for (auto decl : lookupResult) {
      if (auto swiftDecl = Impl.importDecl(decl->getUnderlyingDecl()))
        if (auto valueDecl = dyn_cast<ValueDecl>(swiftDecl)) {
          // If the importer gave us a declaration from the stdlib, make sure
          // it does not show up in the lookup results for the imported module.
          if (valueDecl->getDeclContext() != Impl.getSwiftModule()) {
            results.push_back(valueDecl);
            FoundType = FoundType || isa<TypeDecl>(valueDecl);
          }
        }
    }
  }

  if (lookupNameKind == clang::Sema::LookupOrdinaryName && !FoundType) {
    // Look up a tag name if we did not find a type with this name already.
    // We don't want to introduce multiple types with same name.
    lookupResult.clear(clang::Sema::LookupTagName);
    if (!sema.LookupName(lookupResult, /*Scope=*/0))
      return;

    // FIXME: Filter based on access path? C++ access control?
    for (auto decl : lookupResult) {
      if (auto swiftDecl = Impl.importDecl(decl->getUnderlyingDecl()))
        if (auto valueDecl = dyn_cast<ValueDecl>(swiftDecl))
          results.push_back(valueDecl);
    }
  }
}

void
ClangImporter::lookupVisibleDecls(clang::VisibleDeclConsumer &consumer) const {
  auto &sema = Impl.Instance->getSema();
  sema.LookupVisibleDecls(Impl.getClangASTContext().getTranslationUnitDecl(),
                          clang::Sema::LookupNameKind::LookupAnyName,
                          consumer);
}

namespace {
class ImportingVisibleDeclConsumer : public clang::VisibleDeclConsumer {
  ClangImporter &TheClangImporter;
  ClangImporter::Implementation &Impl;
  swift::VisibleDeclConsumer &NextConsumer;
  const Module *ModuleFilter = nullptr;

public:
  ImportingVisibleDeclConsumer(ClangImporter &TheClangImporter,
                               ClangImporter::Implementation &Impl,
                               swift::VisibleDeclConsumer &NextConsumer)
      : TheClangImporter(TheClangImporter), Impl(Impl),
        NextConsumer(NextConsumer) {}

  void filterByModule(const Module *M) {
    ModuleFilter = M;
  }

  void FoundDecl(clang::NamedDecl *ND, clang::NamedDecl *Hiding,
                 clang::DeclContext *Ctx,
                 bool InBaseClass) override {
    if (ND->getName().empty())
      return;

    if (ND->isModulePrivate())
      return;

    SmallVector<ValueDecl *, 4> Results;
    TheClangImporter.lookupValue(/*module*/ nullptr,
                                 Module::AccessPathTy(),
                                 Impl.SwiftContext.getIdentifier(ND->getName()),
                                 NLKind::UnqualifiedLookup,
                                 Results);
    for (auto *VD : Results)
      if (!ModuleFilter || VD->getModuleContext() == ModuleFilter)
        NextConsumer.foundDecl(VD, DeclVisibilityKind::VisibleAtTopLevel);
  }
};
} // unnamed namespace

void ClangImporter::lookupVisibleDecls(VisibleDeclConsumer &Consumer) const {
  ImportingVisibleDeclConsumer ImportingConsumer(
      const_cast<ClangImporter &>(*this), Impl, Consumer);
  lookupVisibleDecls(ImportingConsumer);
}

void ClangImporter::lookupVisibleDecls(const Module *M,
                                       Module::AccessPathTy AccessPath,
                                       VisibleDeclConsumer &Consumer,
                                       NLKind LookupKind) {
  ImportingVisibleDeclConsumer ImportingConsumer(
      const_cast<ClangImporter &>(*this), Impl, Consumer);
  ImportingConsumer.filterByModule(M);
  lookupVisibleDecls(ImportingConsumer);
}

void ClangImporter::loadExtensions(NominalTypeDecl *nominal,
                                   unsigned previousGeneration) {
  auto objcClass = dyn_cast_or_null<clang::ObjCInterfaceDecl>(
                     nominal->getClangDecl());
  if (!objcClass)
    return;

  // Import all of the visible categories. Simply loading them adds them to
  // the list of extensions.
  for (auto I = objcClass->visible_categories_begin(),
            E = objcClass->visible_categories_end();
       I != E; ++I) {
    Impl.importDecl(*I);
  }
}

void ClangImporter::getImportedModules(
    const Module *module,
    SmallVectorImpl<Module::ImportedModule> &imports,
    bool includePrivate) {

  auto underlying = cast<ClangModule>(module)->clangModule;
  auto topLevelAdapter = cast<ClangModule>(module)->getAdapterModule();

  SmallVector<clang::Module *, 8> imported;
  if (includePrivate) {
    imported.append(underlying->Imports.begin(), underlying->Imports.end());
    // FIXME: The parent module isn't exactly a private import, but it is
    // needed for link dependencies.
    if (underlying->Parent)
      imported.push_back(underlying->Parent);
  } else
    underlying->getExportedModules(imported);

  for (auto importMod : imported) {
    auto wrapper = Impl.getWrapperModule(*this, importMod);

    auto actualMod = wrapper->getAdapterModule();
    if (!actualMod || actualMod == topLevelAdapter)
      actualMod = wrapper;

    imports.push_back({Module::AccessPathTy(), actualMod});
  }
}

/// Returns true if the first selector piece matches the given identifier.
static bool selectorStartsWithName(clang::Selector sel, Identifier name) {
  return sel.getNameForSlot(0) == name.str();
}

namespace {
  /// Inserts found decls into an externally-owned SmallVector.
  class VectorDeclConsumer : public VisibleDeclConsumer {
  public:
    SmallVectorImpl<ValueDecl *> &results;
    explicit VectorDeclConsumer(SmallVectorImpl<ValueDecl *> &decls)
      : results(decls) {}

    void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
      results.push_back(VD);
    }
  };
}

static void lookupClassMembersImpl(ClangImporter::Implementation &Impl,
                                   VisibleDeclConsumer &consumer,
                                   Identifier name = Identifier()) {
  clang::Sema &S = Impl.Instance->getSema();
  clang::ExternalASTSource *source = S.getExternalSource();

  // When looking for a subscript, we actually look for the getters
  // and setters.
  clang::IdentifierInfo *objectAtIndexedSubscriptId = nullptr;
  clang::IdentifierInfo *objectForKeyedSubscriptId = nullptr;
  clang::IdentifierInfo *setObjectId = nullptr;
  clang::IdentifierInfo *atIndexedSubscriptId = nullptr;
  clang::IdentifierInfo *forKeyedSubscriptId = nullptr;
  bool isSubscript = !name.empty() && name.str().equals("subscript");
  if (isSubscript) {
    auto &identTable = S.Context.Idents;
    objectAtIndexedSubscriptId = &identTable.get("objectAtIndexedSubscript");
    objectForKeyedSubscriptId = &identTable.get("objectForKeyedSubscript");
    setObjectId = &identTable.get("setObject");
    atIndexedSubscriptId = &identTable.get("atIndexedSubscript");
    forKeyedSubscriptId = &identTable.get("forKeyedSubscript");
  }

  // Function that determines whether the given selector is acceptable.
  auto acceptableSelector = [&](clang::Selector sel) -> bool {
    if (name.empty())
      return true;

    switch (sel.getNumArgs()) {
    case 0:
      if (isSubscript)
        return false;

      break;

    case 1:
      if (isSubscript)
        return sel.getIdentifierInfoForSlot(0) == objectAtIndexedSubscriptId ||
               sel.getIdentifierInfoForSlot(0) == objectForKeyedSubscriptId;

      break;

    case 2:
      if (isSubscript)
        return (sel.getIdentifierInfoForSlot(0) == setObjectId &&
                (sel.getIdentifierInfoForSlot(1) == atIndexedSubscriptId ||
                 sel.getIdentifierInfoForSlot(1) == forKeyedSubscriptId));

      break;

    default:
      if (isSubscript)
        return false;

      break;
    }

    return selectorStartsWithName(sel, name);
  };

  // Force load all external methods.
  // FIXME: Copied from Clang's SemaCodeComplete.
  for (uint32_t i = 0, n = source->GetNumExternalSelectors(); i != n; ++i) {
    clang::Selector sel = source->GetExternalSelector(i);
    if (sel.isNull() || S.MethodPool.count(sel))
      continue;
    if (!acceptableSelector(sel))
      continue;

    S.ReadMethodPool(sel);
  }

  // FIXME: Does not include methods from protocols.
  // FIXME: Do we really have to import every single method?
  // FIXME: Need a more efficient table in Clang to find "all selectors whose
  // first piece is this name".
  auto importMethods = [&](const clang::ObjCMethodList *list) {
    for (; list != nullptr; list = list->getNext()) {
      if (list->Method->isUnavailable())
        continue;

      // If the method is a property accessor, we want the property.
      const clang::NamedDecl *searchForDecl = list->Method;
      if (list->Method->isPropertyAccessor()) {
        if (auto property = list->Method->findPropertyDecl()) {
          // ... unless we are enumerating all decls.  In this case, if we see
          // a getter, return a property.  If we see a setter, we know that
          // there is a getter, and we will visit it and return a property at
          // that time.
          if (name.empty() && list->Method->param_size() != 0)
            continue;
          searchForDecl = property;
        }
      }

      if (auto VD = cast_or_null<ValueDecl>(Impl.importDecl(searchForDecl))) {
        if (isSubscript || name.empty()) {
          // When searching for a subscript, we may have found a getter.  If so,
          // use the subscript instead.
          if (auto func = dyn_cast<FuncDecl>(VD)) {
            auto known = Impl.Subscripts.find({func, nullptr});
            if (known != Impl.Subscripts.end()) {
              consumer.foundDecl(known->second, DeclVisibilityKind::DynamicLookup);
            }
          }

          // If we were looking only for subscripts, don't report the getter.
          if (isSubscript)
            continue;
        }

        consumer.foundDecl(VD, DeclVisibilityKind::DynamicLookup);
      }
    }
  };

  for (auto entry : S.MethodPool) {
    if (!acceptableSelector(entry.first))
      continue;
  
    auto &methodListPair = entry.second;
    if (methodListPair.first.Method)
      importMethods(&methodListPair.first);
    if (methodListPair.second.Method)
      importMethods(&methodListPair.second);
  }
}

void ClangImporter::lookupClassMember(const Module *module,
                                      Module::AccessPathTy accessPath,
                                      Identifier name,
                                      SmallVectorImpl<ValueDecl*> &results) {
  // FIXME: Not limited by module.
  VectorDeclConsumer consumer(results);
  lookupClassMembersImpl(Impl, consumer, name);
}

void ClangImporter::lookupClassMembers(const Module *module,
                                       Module::AccessPathTy accessPath,
                                       VisibleDeclConsumer &consumer) {
  // FIXME: Not limited by module.
  lookupClassMembersImpl(Impl, consumer);
}

void ClangImporter::getLinkLibraries(const Module *module,
                                     Module::LinkLibraryCallback callback) {
  auto underlying = cast<ClangModule>(module)->clangModule;
  for (auto clangLinkLib : underlying->LinkLibraries) {
    LibraryKind kind;
    if (clangLinkLib.IsFramework)
      kind = LibraryKind::Framework;
    else
      kind = LibraryKind::Library;
    
    callback(LinkLibrary(clangLinkLib.Library, kind));
  }
}

void ClangImporter::getTopLevelDecls(const Module *module,
                                     SmallVectorImpl<Decl*> &results) {
  clang::ASTContext &clangCtx = Impl.Instance->getASTContext();
  const clang::TranslationUnitDecl *clangTU = clangCtx.getTranslationUnitDecl();
  
  // FIXME: Do we want a noload variant here?
  for (auto D : make_range(clangTU->decls_begin(),
                           clangTU->decls_end())) {
    auto named = dyn_cast<clang::NamedDecl>(D);
    if (!named)
       continue;
    if (Impl.getClangModuleForDecl(D) != module)
      continue;
    if (auto imported = Impl.importDecl(named))
      results.push_back(imported);
  }
}

void ClangImporter::getDisplayDecls(const Module *module,
                                    SmallVectorImpl<Decl*> &results) {
  return getTopLevelDecls(module, results);
}

StringRef ClangImporter::getModuleFilename(const Module *Module) {
  auto Underlying = cast<ClangModule>(Module)->clangModule;
  return Underlying->getASTFile()->getName();
}

clang::TargetInfo &ClangImporter::getTargetInfo() const {
  return Impl.Instance->getTarget();
}

void ClangImporter::verifyAllModules() {
  for (auto &I : Impl.ImportedDecls) {
    if (Decl *D = I.second)
      verify(D);
  }
}

//===----------------------------------------------------------------------===//
// ClangModule Implementation
//===----------------------------------------------------------------------===//

ClangModule::ClangModule(ASTContext &ctx, StringRef DebugModuleName,
                         ModuleLoader &owner, clang::Module *clangModule)
  : LoadedModule(ModuleKind::Clang, ctx.getIdentifier(clangModule->Name),
                 DebugModuleName, ctx, owner), clangModule(clangModule) {
}

bool ClangModule::isTopLevel() const {
  return !clangModule->isSubModule();
}

StringRef ClangModule::getTopLevelModuleName() const {
  return clangModule->getTopLevelModuleName();
}

Module *ClangModule::getAdapterModule() const {
  if (!isTopLevel()) {
    // FIXME: Is this correct for submodules?
    auto &importer = static_cast<ClangImporter&>(getOwner());
    auto topLevel = clangModule->getTopLevelModule();
    auto wrapper = importer.Impl.getWrapperModule(importer, topLevel);
    return wrapper->getAdapterModule();
  }

  if (!adapterModule.getInt()) {
    // FIXME: Include proper source location.
    auto adapter = Ctx.getModule(Module::AccessPathTy({Name, SourceLoc()}));
    if (isa<ClangModule>(adapter)) {
      adapter = nullptr;
    } else {
      auto &sharedModuleRef = Ctx.LoadedModules[Name.str()];
      assert(!sharedModuleRef || sharedModuleRef == adapter ||
             sharedModuleRef == this);
      sharedModuleRef = adapter;
    }

    auto mutableThis = const_cast<ClangModule *>(this);
    mutableThis->adapterModule.setPointerAndInt(adapter, true);
  }

  return adapterModule.getPointer();
}

//===--- ModuleNameLookup.cpp - Name lookup within a module ---------------===//
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

#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ClangModuleLoader.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/Basic/Assertions.h"
#include "clang/AST/Attr.h"
#include "clang/AST/ExprCXX.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace namelookup;

namespace {

/// Encapsulates the work done for a recursive qualified lookup into a module.
///
/// The \p LookupStrategy handles the non-recursive part of the lookup. It
/// must be a subclass of ModuleNameLookup. It should provide a
/// \c doLocalLookup() method; this method will be passed appropriate
/// \c NLOptions but does not necessarily need to honor them.
template <typename LookupStrategy>
class ModuleNameLookup {
  ASTContext &ctx;
  const ResolutionKind resolutionKind;
  const bool respectAccessControl;

  LookupStrategy *getDerived() {
    static_assert(std::is_base_of<ModuleNameLookup<LookupStrategy>,
                                  LookupStrategy>::value,
                  "ModuleNameLookup is a CRTP class");
    return static_cast<LookupStrategy *>(this);
  }

public:
  ModuleNameLookup(ASTContext &ctx, ResolutionKind resolutionKind)
      : ctx(ctx),
        resolutionKind(resolutionKind),
        respectAccessControl(!ctx.isAccessControlDisabled()) {}

  /// Performs the qualified lookup requested by \p LookupStrategy into the
  /// given module and, if necessary, its reexports.
  ///
  /// If 'moduleOrFile' is a ModuleDecl, we search the module and its
  /// public imports. If 'moduleOrFile' is a SourceFile, we search the
  /// file's parent module, the module's public imports, and the source
  /// file's private imports.
  ///
  /// \param[out] decls Results are appended to this vector.
  /// \param moduleOrFile The module or file unit to search, including imports.
  /// \param accessPath The access path that was imported; if not empty, only
  ///                   the named declaration will be imported.
  /// \param moduleScopeContext The top-level context from which the lookup is
  ///        being performed, for checking access. This must be either a
  ///        FileUnit or a Module.
  /// \param hasModuleSelector Whether \p name was originally qualified by a
  ///        module selector. This information is threaded through to underlying
  ///        lookup calls; the callee is responsible for actually applying the
  ///        module selector.
  /// \param options Lookup options to apply.
  void lookupInModule(SmallVectorImpl<ValueDecl *> &decls,
                      const DeclContext *moduleOrFile,
                      ImportPath::Access accessPath,
                      const DeclContext *moduleScopeContext,
                      bool hasModuleSelector,
                      NLOptions options);
};

// Exclude names introduced by macro expansions.
enum class LocalLookupFlags {
  ExcludeMacroExpansions,
};

/// Encapsulates the work done for a recursive qualified lookup into a module
/// by full name.
class LookupByName : public ModuleNameLookup<LookupByName> {
  using Super = ModuleNameLookup<LookupByName>;
  friend Super;

  const DeclName name;
  const NLKind lookupKind;

public:
  /// \param ctx The AST context that the lookup will be performed in.
  /// \param name The name that will be looked up.
  /// \param lookupKind Whether this lookup is qualified or unqualified.
  /// \param resolutionKind What sort of decl is expected.
  LookupByName(ASTContext &ctx, ResolutionKind resolutionKind,
               DeclName name, NLKind lookupKind)
    : Super(ctx, resolutionKind), name(name),
      lookupKind(lookupKind) {}

private:
  /// Returns whether it's okay to stop recursively searching imports, given
  /// that we found something non-overloadable.
  static bool canReturnEarly() {
    return true;
  }

  /// \param module The module to search for declarations in.
  /// \param path The access path that was imported; if not empty, only the
  ///             named declaration will be imported.
  /// \param[out] localDecls Results are appended to this vector.
  void doLocalLookup(ModuleDecl *module, ImportPath::Access path,
                     OptionSet<ModuleLookupFlags> flags,
                     SmallVectorImpl<ValueDecl *> &localDecls) {
    // If this import is specific to some named decl ("import Swift.Int")
    // then filter out any lookups that don't match.
    if (!path.matches(name))
      return;
    module->lookupValue(name, lookupKind, flags, localDecls);
  }
};

/// Encapsulates the work done for a recursive qualified lookup into a module
/// to find all visible decls.
class LookupVisibleDecls : public ModuleNameLookup<LookupVisibleDecls> {
  using Super = ModuleNameLookup<LookupVisibleDecls>;
  friend Super;

  const NLKind lookupKind;

public:
  LookupVisibleDecls(ASTContext &ctx, ResolutionKind resolutionKind,
                     NLKind lookupKind)
    : ModuleNameLookup(ctx, resolutionKind),
      lookupKind(lookupKind) {}

private:
  /// Returns whether it's okay to stop recursively searching imports, given
  /// that we found something non-overloadable.
  static bool canReturnEarly() {
    return false;
  }

  void doLocalLookup(ModuleDecl *module, ImportPath::Access path,
                     OptionSet<ModuleLookupFlags> flags,
                     SmallVectorImpl<ValueDecl *> &localDecls) {
    VectorDeclConsumer consumer(localDecls);
    module->lookupVisibleDecls(path, consumer, lookupKind);
  }
};

} // end anonymous namespace

bool swift::declIsVisibleToNameLookup(
    const ValueDecl *decl, const DeclContext *moduleScopeContext,
    NLOptions options) {
  // NL_IgnoreAccessControl only applies to the current module. If
  // it applies here, the declaration is visible.
  if ((options & NL_IgnoreAccessControl) &&
      moduleScopeContext &&
      moduleScopeContext->getParentModule() ==
          decl->getDeclContext()->getParentModule())
    return true;

  bool includeUsableFromInline = options & NL_IncludeUsableFromInline;
  return decl->isAccessibleFrom(moduleScopeContext, false,
                                includeUsableFromInline);
}

template <typename LookupStrategy>
void ModuleNameLookup<LookupStrategy>::lookupInModule(
    SmallVectorImpl<ValueDecl *> &decls,
    const DeclContext *moduleOrFile,
    ImportPath::Access accessPath,
    const DeclContext *moduleScopeContext,
    bool hasModuleSelector,
    NLOptions options) {
  assert(moduleOrFile->isModuleScopeContext());

  // Does the module scope have any separately-imported overlays shadowing
  // the module we're looking into?
  SmallVector<ModuleDecl *, 4> overlays;
  moduleScopeContext->getSeparatelyImportedOverlays(
      moduleOrFile->getParentModule(), overlays);
  if (!overlays.empty()) {
    // If so, look in each of those overlays.
    bool selfOverlay = false;
    for (auto overlay : overlays) {
      if (overlay == moduleOrFile->getParentModule())
        selfOverlay = true;
      else
        lookupInModule(decls, overlay, accessPath, moduleScopeContext,
                       hasModuleSelector, options);
    }
    // FIXME: This may not work gracefully if more than one of these lookups
    // finds something.
    if (!selfOverlay)
      return;
  }

  const size_t initialCount = decls.size();
  size_t currentCount = decls.size();

  auto updateNewDecls = [&](const DeclContext *moduleScopeContext) {
    if (decls.size() == currentCount)
      return;

    auto newEnd = std::remove_if(
      decls.begin() + currentCount, decls.end(),
      [&](ValueDecl *VD) {
        if (resolutionKind == ResolutionKind::TypesOnly && !isa<TypeDecl>(VD))
          return true;
        if (resolutionKind == ResolutionKind::MacrosOnly && !isa<MacroDecl>(VD))
          return true;
        if (respectAccessControl &&
            !declIsVisibleToNameLookup(VD, moduleScopeContext, options))
          return true;
        if (!ABIRoleInfo(VD).matchesOptions(options))
          return true;
        return false;
      });
    decls.erase(newEnd, decls.end());

    currentCount = decls.size();
  };

  OptionSet<ModuleLookupFlags> currentModuleLookupFlags = {};
  if (options & NL_ExcludeMacroExpansions)
    currentModuleLookupFlags |= ModuleLookupFlags::ExcludeMacroExpansions;
  if (options & NL_ABIProviding)
    currentModuleLookupFlags |= ModuleLookupFlags::ABIProviding;
  if (hasModuleSelector)
    currentModuleLookupFlags |= ModuleLookupFlags::HasModuleSelector;

  // Do the lookup into the current module.
  auto *module = moduleOrFile->getParentModule();
  getDerived()->doLocalLookup(
      module, accessPath, currentModuleLookupFlags, decls);
  updateNewDecls(moduleScopeContext);

  bool canReturnEarly = (initialCount != decls.size() &&
                         getDerived()->canReturnEarly());
  if (canReturnEarly &&
      resolutionKind == ResolutionKind::Overloadable) {
    // If we only found top-level functions or macros, keep looking, since
    // we may find additional overloads.
    if (std::all_of(decls.begin() + initialCount, decls.end(),
                    [](ValueDecl *VD) {
      return isa<FuncDecl>(VD) || isa<MacroDecl>(VD);
    }))
      canReturnEarly = false;
  }

  // If needed, search for decls in re-exported modules as well.
  if (!canReturnEarly) {
    auto &imports = ctx.getImportCache().getImportSet(moduleOrFile);

    OptionSet<ModuleLookupFlags> importedModuleLookupFlags = {};
    if (options & NL_ABIProviding)
      currentModuleLookupFlags |= ModuleLookupFlags::ABIProviding;
    // Do not propagate HasModuleSelector here; the selector wasn't specific.

    auto visitImport = [&](ImportedModule import,
                           const DeclContext *moduleScopeContext) {
      if (import.accessPath.empty())
        import.accessPath = accessPath;
      else if (!accessPath.empty() &&
               !import.accessPath.isSameAs(accessPath))
        return;

      getDerived()->doLocalLookup(import.importedModule, import.accessPath,
                                  importedModuleLookupFlags, decls);
      updateNewDecls(moduleScopeContext);
    };

    // If the ClangImporter's special header import module appears in the
    // import set, we must visit it first.
    ModuleDecl *headerImportModule = nullptr;
    if (imports.hasHeaderImportModule()) {
      if (auto *loader = ctx.getClangModuleLoader()) {
        headerImportModule = loader->getImportedHeaderModule();
        if (headerImportModule) {
          visitImport(ImportedModule(headerImportModule), nullptr);
        }
      }
    }

    for (auto import : imports.getTopLevelImports()) {
      // A module appears in its own top-level import list; since we checked
      // it already, skip it.
      if (import.importedModule == module)
        continue;

      // Skip the special import set module; we've already visited it.
      if (import.importedModule == headerImportModule)
        continue;

      visitImport(import, moduleScopeContext);
    }

    for (auto import : imports.getTransitiveImports()) {
      // Skip the special import set module; we've already visited it.
      if (import.importedModule == headerImportModule)
        continue;

      visitImport(import, moduleScopeContext);
    }
  }

  // Nothing more to do if we don't have ambiguous results.
  if (decls.size() - initialCount <= 1)
    return;

  // Some functions like `memchr` are defined both in libc and in libc++.
  // Importing both would result in ambiguities, but there are some attributes
  // that mark the preferred overloads. See _LIBCPP_PREFERRED_OVERLOAD.
  llvm::SmallPtrSet<ValueDecl *, 4> declsToRemove;
  bool hasPreferredOverload = false;
  for (auto decl : decls)
    if (const auto *clangDecl = decl->getClangDecl()) {
      if (clangDecl->hasAttr<clang::EnableIfAttr>()) {
        // FIXME: at some point we might want to call into Clang to implement
        // the full enable_if semantics including the constant evaluation of the
        // conditions. For now, just look for the first enable_if(true, "...")
        // and assume all the rest of the enable_ifs evaluate to true.
        bool thisDeclHasPreferredOverload = false;
        for (auto clangAttr :
             clangDecl->specific_attrs<clang::EnableIfAttr>()) {
          if (auto litExpr =
                  dyn_cast<clang::CXXBoolLiteralExpr>(clangAttr->getCond())) {
            if (litExpr->getValue()) {
              thisDeclHasPreferredOverload = hasPreferredOverload = true;
              break;
            }
          }
        }
        if (!thisDeclHasPreferredOverload)
          declsToRemove.insert(decl);
      } else
        declsToRemove.insert(decl);
    }

  if (hasPreferredOverload) {
    decls.erase(std::remove_if(decls.begin() + initialCount, decls.end(),
                               [&](ValueDecl *d) -> bool {
                                 return declsToRemove.contains(d);
                               }),
                decls.end());
  }

  // Remove duplicated declarations, which can happen when the same module is
  // imported with multiple access paths.
  llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
  decls.erase(std::remove_if(decls.begin() + initialCount, decls.end(),
                             [&](ValueDecl *d) -> bool {
                               return !knownDecls.insert(d).second;
                             }),
              decls.end());
}

QualifiedLookupResult
LookupInModuleRequest::evaluate(
    Evaluator &evaluator, const DeclContext *moduleOrFile, DeclName name,
    bool hasModuleSelector, NLKind lookupKind, ResolutionKind resolutionKind,
    const DeclContext *moduleScopeContext, NLOptions options) const {
  assert(moduleScopeContext->isModuleScopeContext());

  QualifiedLookupResult decls;
  LookupByName lookup(moduleOrFile->getASTContext(), resolutionKind,
                      name, lookupKind);
  lookup.lookupInModule(decls, moduleOrFile, {}, moduleScopeContext,
                        hasModuleSelector, options);
  return decls;
}

void namelookup::lookupInModule(const DeclContext *moduleOrFile,
                                DeclName name,
                                bool hasModuleSelector,
                                SmallVectorImpl<ValueDecl *> &decls,
                                NLKind lookupKind,
                                ResolutionKind resolutionKind,
                                const DeclContext *moduleScopeContext,
                                SourceLoc loc, NLOptions options) {
  auto &ctx = moduleOrFile->getASTContext();
  LookupInModuleRequest req(moduleOrFile, name, hasModuleSelector, lookupKind,
                            resolutionKind, moduleScopeContext, loc, options);
  auto results = evaluateOrDefault(ctx.evaluator, req, {});
  decls.append(results.begin(), results.end());
}

void namelookup::lookupVisibleDeclsInModule(
    const DeclContext *moduleOrFile,
    ImportPath::Access accessPath,
    SmallVectorImpl<ValueDecl *> &decls,
    NLKind lookupKind,
    ResolutionKind resolutionKind,
    const DeclContext *moduleScopeContext) {
  assert(moduleScopeContext->isModuleScopeContext());
  auto &ctx = moduleOrFile->getASTContext();
  LookupVisibleDecls lookup(ctx, resolutionKind, lookupKind);
  lookup.lookupInModule(decls, moduleOrFile, accessPath, moduleScopeContext,
                        /*hasModuleSelector=*/false,
                        NL_QualifiedDefault);
}

void namelookup::simple_display(llvm::raw_ostream &out, ResolutionKind kind) {
  switch (kind) {
  case ResolutionKind::Overloadable:
    out << "Overloadable";
    return;
  case ResolutionKind::TypesOnly:
    out << "TypesOnly";
    return;
  case ResolutionKind::MacrosOnly:
    out << "MacrosOnly";
    return;
  }
  llvm_unreachable("Unhandled case in switch");
}

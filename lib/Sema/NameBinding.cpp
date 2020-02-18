//===--- NameBinding.cpp - Name Binding -----------------------------------===//
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
//
//  This file binds the names in non-ValueDecls Decls like imports, operators,
//  and precedence groups.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-name-binding"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Statistic.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>
#include <system_error>
using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: NameBinder and supporting types
//===----------------------------------------------------------------------===//

using ImportedModule = ModuleDecl::ImportedModule;
using ImportedModuleDesc = SourceFile::ImportedModuleDesc;
using ImportOptions = SourceFile::ImportOptions;
using ImportFlags = SourceFile::ImportFlags;

namespace {
  /// Represents an import which the NameBinder knows exists, but which has not
  /// yet had its options checked, module loaded, or cross-imports found.
  ///
  /// An UnboundImport may represent a physical ImportDecl written in the
  /// source, or it may represent a cross-import overlay that has been found and
  /// needs to be loaded.
  struct UnboundImport {
    /// If this UnboundImport directly represents an ImportDecl, the ImportDecl
    /// it represents.
    ///
    /// This property should only be used to control where information is added
    /// to the AST. Don't retrieve information directly from the ImportDecl; use
    /// the other member variables.
    NullablePtr<ImportDecl> ID;

    SourceLoc importLoc;
    ImportOptions options;
    StringRef privateImportFileName;
    Located<ImportKind> importKind;
    ModuleDecl::AccessPathTy modulePath;
    ModuleDecl::AccessPathTy declPath;

    NullablePtr<DeclAttributes> attrs;
    NullablePtr<ModuleDecl> underlyingModule;

    /// Create an UnboundImport for a user-written import declaration.
    explicit UnboundImport(ImportDecl *ID);

    /// Create an UnboundImport for a cross-import overlay.
    explicit UnboundImport(ASTContext &ctx,
                           const UnboundImport &base, Identifier overlayName,
                           const ImportedModuleDesc &declaringImport,
                           const ImportedModuleDesc &bystandingImport);

    /// Make sure the import is not a self-import.
    bool checkNotTautological(const SourceFile &SF);

    /// Make sure the module actually loaded, and diagnose if it didn't.
    bool checkModuleLoaded(ModuleDecl *M, SourceFile &SF);

    /// Find the top-level module for this module. If \p M is not a submodule,
    /// returns \p M. If it is a submodule, returns either the parent module of
    /// \p M or \c nullptr if the current module is the parent (which can happen
    /// in a mixed-language framework).
    NullablePtr<ModuleDecl> getTopLevelModule(ModuleDecl *M, SourceFile &SF);

    /// Diagnose any errors concerning the \c @_exported, \c @_implementationOnly,
    /// \c @testable, or \c @_private attributes, including a
    /// non-implementation-only import of a fragile library from a resilient one.
    void validateOptions(NullablePtr<ModuleDecl> topLevelModule, SourceFile &SF);

    /// Create an \c ImportedModuleDesc from the information in this
    /// UnboundImport.
    ImportedModuleDesc makeDesc(ModuleDecl *module) const {
      return ImportedModuleDesc({ declPath, module }, options,
                                privateImportFileName);
    }

  private:
    void validatePrivate(ModuleDecl *topLevelModule);
    void validateImplementationOnly(ASTContext &ctx);
    void validateTestable(ModuleDecl *topLevelModule);
    void validateResilience(NullablePtr<ModuleDecl> topLevelModule,
                            SourceFile &SF);

    /// Diagnoses an inability to import \p modulePath in this situation and, if
    /// \p attrs is provided and has an \p attrKind, invalidates the attribute and
    /// offers a fix-it to remove it.
    void diagnoseInvalidAttr(DeclAttrKind attrKind, DiagnosticEngine &diags,
                             Diag<Identifier> diagID);
  };

  /// Represents an import whose options have been checked and module has been
  /// loaded, but its scope (if any) has not been validated.
  struct BoundImport {
    UnboundImport unbound;
    ImportedModuleDesc desc;
    ModuleDecl *module;
    bool needsScopeValidation = false;

    BoundImport(UnboundImport unbound, ImportedModuleDesc desc,
                ModuleDecl *module, bool needsScopeValidation);

    /// Validate the scope of the import, if needed.
    void validateScope(SourceFile &SF);
  };

  class NameBinder : public DeclVisitor<NameBinder> {
    SourceFile &SF;
    ASTContext &ctx;

    /// Imports which still need their options checked, modules loaded, and
    /// cross-imports found.
    SmallVector<UnboundImport, 4> unboundImports;

    /// Imports which still need their scoped imports validated.
    SmallVector<BoundImport, 16> unvalidatedImports;

    /// All imported modules, including by re-exports.
    SmallSetVector<ImportedModuleDesc, 16> visibleModules;

    /// The index of the next module in \c visibleModules that should be
    /// cross-imported.
    size_t nextModuleToCrossImport = 0;

  public:
    NameBinder(SourceFile &SF)
      : SF(SF), ctx(SF.getASTContext())
    { }

    // Special behavior for these decls:
    void visitImportDecl(ImportDecl *ID);
    void visitPrecedenceGroupDecl(PrecedenceGroupDecl *group);
    void visitPrefixOperatorDecl(PrefixOperatorDecl *OpDecl);
    void visitInfixOperatorDecl(InfixOperatorDecl *OpDecl);
    void visitPostfixOperatorDecl(PostfixOperatorDecl *OpDecl);

    // Ignore other decls.
    void visitDecl(Decl *D) {}

    /// Postprocess the imports this NameBinder has bound and collect them into
    /// a vector.
    void finishImports(SmallVectorImpl<ImportedModuleDesc> &imports);

  protected:
    template<typename ...ArgTypes>
    InFlightDiagnostic diagnose(ArgTypes &&...Args) {
      return ctx.Diags.diagnose(std::forward<ArgTypes>(Args)...);
    }

    /// Check a single unbound import, bind it, add it to \c unvalidatedImports,
    /// and add its cross-import overlays to \c unboundImports.
    void bindImport(UnboundImport &&I);

    /// Adds \p I and \p M to \c unvalidatedImports and \c visibleModules.
    void addImport(const UnboundImport &I, ModuleDecl *M,
                   bool needsScopeValidation);

    /// Adds \p desc and everything it re-exports to \c visibleModules using
    /// the settings from \c desc.
    void addVisibleModules(ImportedModuleDesc desc);

    /// * If \p I is a cross-import overlay, registers \p M as overlaying
    ///   \p I.underlyingModule in \c SF.
    /// * Discovers any cross-imports between \p I and previously bound imports,
    ///   then adds them to \c unboundImports using source locations from \p I.
    void crossImport(ModuleDecl *M, UnboundImport &I);

    /// Discovers any cross-imports between \p declaringImport and
    /// \p bystandingImport and adds them to \c unboundImports, using source
    /// locations from \p I.
    void findCrossImports(UnboundImport &I,
                          const ImportedModuleDesc &declaringImport,
                          const ImportedModuleDesc &bystandingImport,
                          SmallVectorImpl<Identifier> &overlayNames);

    /// Load a module referenced by an import statement.
    ///
    /// Returns null if no module can be loaded.
    ModuleDecl *getModule(ArrayRef<Located<Identifier>> ModuleID);

    template<typename OP_DECL>
    void insertOperatorDecl(SourceFile::OperatorMap<OP_DECL*> &Operators,
                            OP_DECL *OpDecl);
  };
} // end anonymous namespace

//===----------------------------------------------------------------------===//
// MARK: performNameBinding
//===----------------------------------------------------------------------===//

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this point parsing has been performed, but we still have
/// UnresolvedDeclRefExpr nodes for unresolved value names, and we may have
/// unresolved type names as well. This handles import directives and forward
/// references.
void swift::performNameBinding(SourceFile &SF) {
  FrontendStatsTracer tracer(SF.getASTContext().Stats, "Name binding");

  // Make sure we skip adding the standard library imports if the
  // source file is empty.
  if (SF.ASTStage == SourceFile::NameBound || SF.getTopLevelDecls().empty()) {
    SF.ASTStage = SourceFile::NameBound;
    return;
  }

  NameBinder Binder(SF);

  // Bind each import and operator declaration.
  for (auto D : SF.getTopLevelDecls())
    Binder.visit(D);

  // Validate all scoped imports. We defer this until now because a scoped
  // import of a cross-import overlay's declaring module can select declarations
  // in the overlay, and we don't know all of the overlays we're loading until
  // we've bound all imports in the file.
  SmallVector<ImportedModuleDesc, 8> ImportedModules;
  Binder.finishImports(ImportedModules);
  SF.addImports(ImportedModules);

  SF.ASTStage = SourceFile::NameBound;
  verify(SF);
}

//===----------------------------------------------------------------------===//
// MARK: Operator declarations
//===----------------------------------------------------------------------===//

template<typename OP_DECL> void
NameBinder::insertOperatorDecl(SourceFile::OperatorMap<OP_DECL*> &Operators,
                               OP_DECL *OpDecl) {
  auto previousDecl = Operators.find(OpDecl->getName());
  if (previousDecl != Operators.end()) {
    diagnose(OpDecl->getLoc(), diag::operator_redeclared);
    diagnose(previousDecl->second.getPointer(),
             diag::previous_operator_decl);
    return;
  }

  // FIXME: The second argument indicates whether the given operator is visible
  // outside the current file.
  Operators[OpDecl->getName()] = { OpDecl, true };
}

void NameBinder::visitPrefixOperatorDecl(PrefixOperatorDecl *OpDecl) {
  insertOperatorDecl(SF.PrefixOperators, OpDecl);
}

void NameBinder::visitInfixOperatorDecl(InfixOperatorDecl *OpDecl) {
  insertOperatorDecl(SF.InfixOperators, OpDecl);
}

void NameBinder::visitPostfixOperatorDecl(PostfixOperatorDecl *OpDecl) {
  insertOperatorDecl(SF.PostfixOperators, OpDecl);
}

void NameBinder::visitPrecedenceGroupDecl(PrecedenceGroupDecl *group) {
  auto previousDecl = SF.PrecedenceGroups.find(group->getName());
  if (previousDecl != SF.PrecedenceGroups.end()) {
    diagnose(group->getLoc(), diag::precedence_group_redeclared);
    diagnose(previousDecl->second.getPointer(),
             diag::previous_precedence_group_decl);
    return;
  }

  // FIXME: The second argument indicates whether the given precedence
  // group is visible outside the current file.
  SF.PrecedenceGroups[group->getName()] = { group, true };
}

//===----------------------------------------------------------------------===//
// MARK: Import handling generally
//===----------------------------------------------------------------------===//

void NameBinder::visitImportDecl(ImportDecl *ID) {
  assert(unboundImports.empty());

  unboundImports.emplace_back(ID);
  while(!unboundImports.empty())
    bindImport(unboundImports.pop_back_val());
}

void NameBinder::bindImport(UnboundImport &&I) {
  if (!I.checkNotTautological(SF)) {
    // No need to process this import further.
    if (I.ID)
      I.ID.get()->setModule(SF.getParentModule());
    return;
  }

  ModuleDecl *M = getModule(I.modulePath);
  if (!I.checkModuleLoaded(M, SF)) {
    // Can't process further. checkModuleLoaded() will have diagnosed this.
    if (I.ID)
      I.ID.get()->setModule(nullptr);
    return;
  }

  auto topLevelModule = I.getTopLevelModule(M, SF);

  I.validateOptions(topLevelModule, SF);

  if (topLevelModule && topLevelModule != M) {
    // If we have distinct submodule and top-level module, add both, disabling
    // the top-level module's scoped import validation.
    addImport(I, M, false);
    addImport(I, topLevelModule.get(), true);
  }
  else {
    // Add only the import itself.
    addImport(I, M, true);
  }

  crossImport(M, I);

  if (I.ID)
    I.ID.get()->setModule(M);
}

void NameBinder::addImport(const UnboundImport &I, ModuleDecl *M,
                           bool needsScopeValidation) {
  auto importDesc = I.makeDesc(M);

  addVisibleModules(importDesc);
  unvalidatedImports.emplace_back(I, importDesc, M, needsScopeValidation);
}

//===----------------------------------------------------------------------===//
// MARK: Import module loading
//===----------------------------------------------------------------------===//

ModuleDecl *NameBinder::getModule(ArrayRef<Located<Identifier>> modulePath) {
  assert(!modulePath.empty());
  auto moduleID = modulePath[0];

  // The Builtin module cannot be explicitly imported unless we're a .sil file
  // or in the REPL.
  if ((SF.Kind == SourceFileKind::SIL || SF.Kind == SourceFileKind::REPL) &&
      moduleID.Item == ctx.TheBuiltinModule->getName())
    return ctx.TheBuiltinModule;

  // If the imported module name is the same as the current module,
  // skip the Swift module loader and use the Clang module loader instead.
  // This allows a Swift module to extend a Clang module of the same name.
  //
  // FIXME: We'd like to only use this in SIL mode, but unfortunately we use it
  // for clang overlays as well.
  if (moduleID.Item == SF.getParentModule()->getName() &&
      modulePath.size() == 1) {
    if (auto importer = ctx.getClangModuleLoader())
      return importer->loadModule(moduleID.Loc, modulePath);
    return nullptr;
  }

  return ctx.getModule(modulePath);
}

NullablePtr<ModuleDecl>
UnboundImport::getTopLevelModule(ModuleDecl *M, SourceFile &SF) {
  if (modulePath.size() == 1)
    return M;

  // If we imported a submodule, import the top-level module as well.
  Identifier topLevelName = modulePath.front().Item;
  ModuleDecl *topLevelModule = SF.getASTContext().getLoadedModule(topLevelName);

  if (!topLevelModule) {
    // Clang can sometimes import top-level modules as if they were
    // submodules.
    assert(!M->getFiles().empty() &&
           isa<ClangModuleUnit>(M->getFiles().front()));
    return M;
  }

  if (topLevelModule == SF.getParentModule())
    // This can happen when compiling a mixed-source framework (or overlay)
    // that imports a submodule of its C part.
    return nullptr;

  return topLevelModule;
}

//===----------------------------------------------------------------------===//
// MARK: Import validation (except for scoped imports)
//===----------------------------------------------------------------------===//

/// Create an UnboundImport for a user-written import declaration.
UnboundImport::UnboundImport(ImportDecl *ID)
  : ID(ID), importLoc(ID->getLoc()), options(), privateImportFileName(),
    importKind(ID->getImportKind(), ID->getKindLoc()),
    modulePath(ID->getModulePath()), declPath(ID->getDeclPath()),
    attrs(&ID->getAttrs()), underlyingModule()
{
  if (ID->isExported())
    options |= ImportFlags::Exported;

  if (ID->getAttrs().hasAttribute<TestableAttr>())
    options |= ImportFlags::Testable;

  if (ID->getAttrs().hasAttribute<ImplementationOnlyAttr>())
    options |= ImportFlags::ImplementationOnly;

  if (auto *privateImportAttr =
          ID->getAttrs().getAttribute<PrivateImportAttr>()) {
    options |= ImportFlags::PrivateImport;
    privateImportFileName = privateImportAttr->getSourceFile();
  }
}

bool UnboundImport::checkNotTautological(const SourceFile &SF) {
  // Exit early if this is not a self-import.
  if (modulePath.front().Item != SF.getParentModule()->getName() ||
      // Overlays use an @_exported self-import to load their clang module.
      options.contains(ImportFlags::Exported) ||
      // Imports of your own submodules are allowed in cross-language libraries.
      modulePath.size() != 1 ||
      // SIL files self-import to get decls from the rest of the module.
      SF.Kind == SourceFileKind::SIL)
    return true;

  ASTContext &ctx = SF.getASTContext();

  StringRef filename = llvm::sys::path::filename(SF.getFilename());
  if (filename.empty())
    ctx.Diags.diagnose(importLoc, diag::sema_import_current_module,
                       modulePath.front().Item);
  else
    ctx.Diags.diagnose(importLoc, diag::sema_import_current_module_with_file,
                       filename, modulePath.front().Item);

  return false;
}

bool UnboundImport::checkModuleLoaded(ModuleDecl *M, SourceFile &SF) {
  if (M)
    return true;

  ASTContext &ctx = SF.getASTContext();

  SmallString<64> modulePathStr;
  interleave(modulePath, [&](ImportDecl::AccessPathElement elem) {
               modulePathStr += elem.Item.str();
             },
             [&] { modulePathStr += "."; });

  auto diagKind = diag::sema_no_import;
  if (SF.Kind == SourceFileKind::REPL || ctx.LangOpts.DebuggerSupport)
    diagKind = diag::sema_no_import_repl;
  ctx.Diags.diagnose(importLoc, diagKind, modulePathStr);

  if (ctx.SearchPathOpts.SDKPath.empty() &&
      llvm::Triple(llvm::sys::getProcessTriple()).isMacOSX()) {
    ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk);
    ctx.Diags.diagnose(SourceLoc(), diag::sema_no_import_no_sdk_xcrun);
  }
  return false;
}

void UnboundImport::validateOptions(NullablePtr<ModuleDecl> topLevelModule,
                                    SourceFile &SF) {
  validateImplementationOnly(SF.getASTContext());

  if (auto *top = topLevelModule.getPtrOrNull()) {
    // FIXME: Having these two calls in this if condition seems dubious.
    //
    // Here's the deal: Per getTopLevelModule(), we will only skip this block
    // if you are in a mixed-source module and trying to import a submodule from
    // your clang half. But that means you're trying to @testable import or
    // @_private import part of yourself--and, moreover, a clang part of
    // yourself--which doesn't make any sense to do. Shouldn't we diagnose that?
    //
    // I'm leaving this alone for now because I'm trying to refactor without
    // changing behavior, but it smells funny.
    validateTestable(top);
    validatePrivate(top);
  }

  validateResilience(topLevelModule, SF);
}

void UnboundImport::validatePrivate(ModuleDecl *topLevelModule) {
  assert(topLevelModule);
  ASTContext &ctx = topLevelModule->getASTContext();

  if (!options.contains(ImportFlags::PrivateImport))
    return;

  if (topLevelModule->arePrivateImportsEnabled())
    return;

  diagnoseInvalidAttr(DAK_PrivateImport, ctx.Diags,
                      diag::module_not_compiled_for_private_import);
  privateImportFileName = StringRef();
}

void UnboundImport::validateImplementationOnly(ASTContext &ctx) {
  if (!options.contains(ImportFlags::ImplementationOnly) ||
      !options.contains(ImportFlags::Exported))
    return;

  // Remove one flag to maintain the invariant.
  options -= ImportFlags::ImplementationOnly;

  diagnoseInvalidAttr(DAK_ImplementationOnly, ctx.Diags,
                      diag::import_implementation_cannot_be_exported);
}

void UnboundImport::validateTestable(ModuleDecl *topLevelModule) {
  assert(topLevelModule);
  ASTContext &ctx = topLevelModule->getASTContext();

  if (!options.contains(ImportFlags::Testable) ||
      topLevelModule->isTestingEnabled() ||
      topLevelModule->isNonSwiftModule() ||
      !ctx.LangOpts.EnableTestableAttrRequiresTestableModule)
    return;

  diagnoseInvalidAttr(DAK_Testable, ctx.Diags, diag::module_not_testable);
}

void UnboundImport::validateResilience(NullablePtr<ModuleDecl> topLevelModule,
                                       SourceFile &SF) {
  if (options.contains(ImportFlags::ImplementationOnly))
    return;

  // Per getTopLevelModule(), we'll only get nullptr here for non-Swift modules,
  // so these two really mean the same thing.
  if (!topLevelModule || topLevelModule.get()->isNonSwiftModule())
    return;

  if (!SF.getParentModule()->isResilient() ||
      topLevelModule.get()->isResilient())
    return;

  ASTContext &ctx = SF.getASTContext();
  ctx.Diags.diagnose(modulePath.front().Loc,
                     diag::module_not_compiled_with_library_evolution,
                     topLevelModule.get()->getName(),
                     SF.getParentModule()->getName());
  // FIXME: Once @_implementationOnly is a real feature, we should have a fix-it
  // to add it.
}

void UnboundImport::diagnoseInvalidAttr(DeclAttrKind attrKind,
                                        DiagnosticEngine &diags,
                                        Diag<Identifier> diagID) {
  auto diag = diags.diagnose(modulePath.front().Loc, diagID,
                             modulePath.front().Item);

  if (!attrs) return;
  auto *attr = attrs.get()->getAttribute(attrKind);
  if (!attr) return;

  diag.fixItRemove(attr->getRangeWithAt());
  attr->setInvalid();
}

//===----------------------------------------------------------------------===//
// MARK: Scoped imports
//===----------------------------------------------------------------------===//

BoundImport::BoundImport(UnboundImport unbound, ImportedModuleDesc desc,
                         ModuleDecl *module, bool needsScopeValidation)
  : unbound(unbound), desc(desc), module(module),
    needsScopeValidation(needsScopeValidation) {
  assert(module && "Can't have an import bound to nothing");
}

void NameBinder::finishImports(SmallVectorImpl<ImportedModuleDesc> &imports) {
  for (auto &unvalidated : unvalidatedImports) {
    unvalidated.validateScope(SF);
    imports.push_back(unvalidated.desc);
  }
}

/// Returns true if a decl with the given \p actual kind can legally be
/// imported via the given \p expected kind.
static bool isCompatibleImportKind(ImportKind expected, ImportKind actual) {
  if (expected == actual)
    return true;
  if (expected != ImportKind::Type)
    return false;

  switch (actual) {
  case ImportKind::Module:
    llvm_unreachable("module imports do not bring in decls");
  case ImportKind::Type:
    llvm_unreachable("individual decls cannot have abstract import kind");
  case ImportKind::Struct:
  case ImportKind::Class:
  case ImportKind::Enum:
    return true;
  case ImportKind::Protocol:
  case ImportKind::Var:
  case ImportKind::Func:
    return false;
  }

  llvm_unreachable("Unhandled ImportKind in switch.");
}

static bool isNominalImportKind(ImportKind kind) {
  switch (kind) {
  case ImportKind::Module:
    llvm_unreachable("module imports do not bring in decls");
  case ImportKind::Struct:
  case ImportKind::Class:
  case ImportKind::Enum:
  case ImportKind::Protocol:
    return true;
  case ImportKind::Type:
  case ImportKind::Var:
  case ImportKind::Func:
    return false;
  }
  llvm_unreachable("unhandled kind");
}

static const char *getImportKindString(ImportKind kind) {
  switch (kind) {
  case ImportKind::Module:
    llvm_unreachable("module imports do not bring in decls");
  case ImportKind::Type:
    return "typealias";
  case ImportKind::Struct:
    return "struct";
  case ImportKind::Class:
    return "class";
  case ImportKind::Enum:
    return "enum";
  case ImportKind::Protocol:
    return "protocol";
  case ImportKind::Var:
    return "var";
  case ImportKind::Func:
    return "func";
  }

  llvm_unreachable("Unhandled ImportKind in switch.");
}

void BoundImport::validateScope(SourceFile &SF) {
  if (unbound.importKind.Item == ImportKind::Module || !needsScopeValidation)
    return;

  ASTContext &ctx = SF.getASTContext();

  // If we're importing a specific decl, validate the import kind.
  using namespace namelookup;

  // FIXME: Doesn't handle scoped testable imports correctly.
  assert(unbound.declPath.size() == 1 && "can't handle sub-decl imports");
  SmallVector<ValueDecl *, 8> decls;
  lookupInModule(module, unbound.declPath.front().Item, decls,
                 NLKind::QualifiedLookup, ResolutionKind::Overloadable,
                 &SF);

  if (decls.empty()) {
    ctx.Diags.diagnose(unbound.importLoc, diag::decl_does_not_exist_in_module,
                       static_cast<unsigned>(unbound.importKind.Item),
                       unbound.declPath.front().Item,
                       unbound.modulePath.front().Item)
      .highlight(SourceRange(unbound.declPath.front().Loc,
                             unbound.declPath.back().Loc));
    return;
  }

  Optional<ImportKind> actualKind = ImportDecl::findBestImportKind(decls);
  if (!actualKind.hasValue()) {
    // FIXME: print entire module name?
    ctx.Diags.diagnose(unbound.importLoc, diag::ambiguous_decl_in_module,
                       unbound.declPath.front().Item, module->getName());
    for (auto next : decls)
      ctx.Diags.diagnose(next, diag::found_candidate);

  } else if (!isCompatibleImportKind(unbound.importKind.Item, *actualKind)) {
    Optional<InFlightDiagnostic> emittedDiag;
    if (*actualKind == ImportKind::Type &&
        isNominalImportKind(unbound.importKind.Item)) {
      assert(decls.size() == 1 &&
             "if we start suggesting ImportKind::Type for, e.g., a mix of "
             "structs and classes, we'll need a different message here");
      assert(isa<TypeAliasDecl>(decls.front()) &&
             "ImportKind::Type is only the best choice for a typealias");
      auto *typealias = cast<TypeAliasDecl>(decls.front());
      emittedDiag.emplace(ctx.Diags.diagnose(unbound.importLoc,
          diag::imported_decl_is_wrong_kind_typealias,
          typealias->getDescriptiveKind(),
          TypeAliasType::get(typealias, Type(), SubstitutionMap(),
                             typealias->getUnderlyingType()),
                             getImportKindString(unbound.importKind.Item)));
    } else {
      emittedDiag.emplace(ctx.Diags.diagnose(unbound.importLoc,
          diag::imported_decl_is_wrong_kind,
          unbound.declPath.front().Item,
          getImportKindString(unbound.importKind.Item),
          static_cast<unsigned>(*actualKind)));
    }

    emittedDiag->fixItReplace(SourceRange(unbound.importKind.Loc),
                              getImportKindString(*actualKind));
    emittedDiag->flush();

    if (decls.size() == 1)
      ctx.Diags.diagnose(decls.front(), diag::decl_declared_here,
                         decls.front()->getFullName());
  }

  unbound.ID.get()->setDecls(ctx.AllocateCopy(decls));
}

//===----------------------------------------------------------------------===//
// MARK: Cross-import overlays
//===----------------------------------------------------------------------===//

static bool canCrossImport(const ImportedModuleDesc &import) {
  if (import.importOptions.contains(ImportFlags::Testable))
    return false;
  if (import.importOptions.contains(ImportFlags::PrivateImport))
    return false;

  return true;
}

/// Create an UnboundImport for a cross-import overlay.
UnboundImport::UnboundImport(ASTContext &ctx,
                             const UnboundImport &base, Identifier overlayName,
                             const ImportedModuleDesc &declaringImport,
                             const ImportedModuleDesc &bystandingImport)
  : ID(nullptr), importLoc(base.importLoc), options(),
    privateImportFileName(), importKind({ ImportKind::Module, SourceLoc() }),
    modulePath(),
    // If the declaring import was scoped, inherit that scope in the
    // overlay's import. Note that we do *not* set importKind; this keeps
    // BoundImport::validateScope() from unnecessarily revalidating the
    // scope.
    declPath(declaringImport.module.first),
    attrs(nullptr), underlyingModule(declaringImport.module.second)
{
  modulePath = ctx.AllocateCopy(
      ModuleDecl::AccessPathTy( { overlayName, base.modulePath[0].Loc }));

  // A cross-import is never private or testable, and never comes from a private
  // or testable import.
  assert(canCrossImport(declaringImport));
  assert(canCrossImport(bystandingImport));

  auto &declaringOptions = declaringImport.importOptions;
  auto &bystandingOptions = bystandingImport.importOptions;

  // If both are exported, the cross-import is exported.
  if (declaringOptions.contains(ImportFlags::Exported) &&
      bystandingOptions.contains(ImportFlags::Exported))
    options |= ImportFlags::Exported;

  // If either are implementation-only, the cross-import is
  // implementation-only.
  if (declaringOptions.contains(ImportFlags::ImplementationOnly) ||
      bystandingOptions.contains(ImportFlags::ImplementationOnly))
    options |= ImportFlags::ImplementationOnly;
}

void NameBinder::crossImport(ModuleDecl *M, UnboundImport &I) {
  // FIXME: There is a fundamental problem with this find-as-we-go approach:
  // The '@_exported import'-ed modules in this module's other files should be
  // taken into account, but they haven't been bound yet, and binding them would
  // require cross-importing. Chicken, meet egg.
  //
  // The way to fix this is probably to restructure name binding so we first
  // bind all exported imports in all files, then bind all other imports in each
  // file. This may become simpler if we bind all ImportDecls before we start
  // computing cross-imports, but I haven't figured that part out yet.
  //
  // Fixing this is tracked within Apple by rdar://problem/59527118. I haven't
  // filed an SR because I plan to address it myself, but if this comment is
  // still here in April 2020 without an SR number, please file a Swift bug and
  // harass @brentdax to fill in the details.

  if (!SF.shouldCrossImport())
    return;

  if (I.underlyingModule)
    // FIXME: Should we warn if M doesn't reexport underlyingModule?
    SF.addSeparatelyImportedOverlay(M, I.underlyingModule.get());

  auto newImports = visibleModules.getArrayRef().slice(nextModuleToCrossImport);
  for (auto &newImport : newImports) {
    if (!canCrossImport(newImport))
      continue;

    // Search imports up to, but not including or after, `newImport`.
    auto oldImports =
        make_range(visibleModules.getArrayRef().data(), &newImport);
    for (auto &oldImport : oldImports) {
      if (!canCrossImport(oldImport))
        continue;

      SmallVector<Identifier, 2> newImportOverlays;
      findCrossImports(I, newImport, oldImport, newImportOverlays);

      SmallVector<Identifier, 2> oldImportOverlays;
      findCrossImports(I, oldImport, newImport, oldImportOverlays);

      // If both sides of the cross-import declare some of the same overlays,
      // this will cause some strange name lookup behavior; let's warn about it.
      for (auto name : newImportOverlays) {
        if (llvm::is_contained(oldImportOverlays, name)) {
          ctx.Diags.diagnose(I.importLoc, diag::cross_imported_by_both_modules,
                             newImport.module.second->getName(),
                             oldImport.module.second->getName(), name);
        }
      }

      // If findCrossImports() ever changed the visibleModules list, we'd see
      // memory smashers here.
      assert(newImports.data() == &visibleModules[nextModuleToCrossImport] &&
             "findCrossImports() should never mutate visibleModules");
    }
  }

  nextModuleToCrossImport = visibleModules.size();
}

void NameBinder::findCrossImports(UnboundImport &I,
                                  const ImportedModuleDesc &declaringImport,
                                  const ImportedModuleDesc &bystandingImport,
                                  SmallVectorImpl<Identifier> &names) {
  assert(&declaringImport != &bystandingImport);

  LLVM_DEBUG(
      llvm::dbgs() << "Discovering cross-imports for '"
                   << declaringImport.module.second->getName() << "' -> '"
                   << bystandingImport.module.second->getName() << "'\n");

  // Find modules we need to import.
  declaringImport.module.second->findDeclaredCrossImportOverlays(
      bystandingImport.module.second->getName(), names, I.importLoc);

  // Add import statements.
  for (auto &name : names) {
    // If we are actually compiling part of this overlay, don't try to load the
    // overlay.
    if (name == SF.getParentModule()->getName())
      continue;

    unboundImports.emplace_back(declaringImport.module.second->getASTContext(),
                                I, name, declaringImport, bystandingImport);

    LLVM_DEBUG({
      auto &crossImportOptions = unboundImports.back().options;
      llvm::dbgs() << "  ";
      if (crossImportOptions.contains(ImportFlags::Exported))
        llvm::dbgs() << "@_exported ";
      if (crossImportOptions.contains(ImportFlags::ImplementationOnly))
        llvm::dbgs() << "@_implementationOnly ";
      llvm::dbgs() << "import " << name << "\n";
    });
  }
}

void NameBinder::addVisibleModules(ImportedModuleDesc importDesc) {
  // FIXME: namelookup::getAllImports() doesn't quite do what we need (mainly
  // w.r.t. scoped imports), but it seems like we could extend it to do so, and
  // then eliminate most of this.

  SmallVector<ImportedModule, 16> importsWorklist = { importDesc.module };

  while (!importsWorklist.empty()) {
    auto nextImport = importsWorklist.pop_back_val();

    // If they are both scoped, and they are *differently* scoped, this import
    // cannot possibly expose anything new. Skip it.
    if (!importDesc.module.first.empty() && !nextImport.first.empty() &&
        importDesc.module.first != nextImport.first)
      continue;

    // Drop this module into the ImportDesc so we treat it as imported with the
    // same options and scope as `I`.
    importDesc.module.second = nextImport.second;

    // If we've already imported it, we've also already imported its
    // imports.
    if (!visibleModules.insert(importDesc))
      continue;

    // Add the module's re-exports to worklist.
    nextImport.second->getImportedModules(importsWorklist,
                                          ModuleDecl::ImportFilterKind::Public);
  }
}

LLVM_ATTRIBUTE_USED static void dumpCrossImportOverlays(ModuleDecl* M) {
  llvm::dbgs() << "'" << M->getName() << "' declares cross-imports with bystanders:\n";

  SmallVector<Identifier, 4> secondaries;
  M->getDeclaredCrossImportBystanders(secondaries);

  for (auto secondary : secondaries)
    llvm::dbgs() << "  " << secondary << "\n";
}

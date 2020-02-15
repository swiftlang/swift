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

    /// Create an UnboundImport for a user-written import declaration.
    explicit UnboundImport(ImportDecl *ID);

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

  if (I.ID)
    I.ID.get()->setModule(M);
}

void NameBinder::addImport(const UnboundImport &I, ModuleDecl *M,
                           bool needsScopeValidation) {
  auto importDesc = I.makeDesc(M);

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
    attrs(&ID->getAttrs())
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
    needsScopeValidation(needsScopeValidation)
{ }

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

  // Per getTopLevelModule(), topLevelModule should only be nullptr if we are
  // importing a submodule, and we should never do a scoped import of a
  // submodule.
//  assert(topLevelModule && "scoped import of a submodule import?");

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

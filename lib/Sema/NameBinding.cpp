//===--- NameBinding.cpp - Name Binding -----------------------------------===//
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
//  This file implements name binding for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTMutationListener.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/ClangImporter/ClangModule.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/Path.h"
#include <algorithm>
using namespace swift;

//===----------------------------------------------------------------------===//
// NameBinder
//===----------------------------------------------------------------------===//

typedef TranslationUnit::ImportedModule ImportedModule;
typedef llvm::PointerUnion<const ImportedModule*, EnumType*> BoundScope;

namespace {  
  class NameBinder {    
  public:
    TranslationUnit *TU;
    ASTContext &Context;

    NameBinder(TranslationUnit *TU) : TU(TU), Context(TU->Ctx) {
      for (auto importPair : TU->getImports()) {
        Module *M = importPair.first.second;
        // Don't add the builtin module to the LoadedModules list.
        if (isa<BuiltinModule>(M))
          continue;
        
        Module *&ref = Context.LoadedModules[M->Name.str()];
        if (ref)
          assert(ref == M || isa<ClangModule>(M));
        else
          ref = M;
      }
    }
    ~NameBinder() {
    }
    
    template<typename ...ArgTypes>
    InFlightDiagnostic diagnose(ArgTypes... Args) {
      return Context.Diags.diagnose(Args...);
    }
    
    Optional<std::pair<ImportedModule, bool>> addImport(ImportDecl *ID);

    /// Load a module referenced by an import statement.
    ///
    /// Returns null if no module can be loaded.
    Module *getModule(ArrayRef<std::pair<Identifier,SourceLoc>> ModuleID);
  };
}

Module *
NameBinder::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> modulePath) {
  assert(!modulePath.empty());
  auto moduleID = modulePath[0];
  
  // TODO: We currently just recursively parse referenced modules.  This works
  // fine for now since they are each a single file.  Ultimately we'll want a
  // compiled form of AST's like clang's that support lazy deserialization.

  // The Builtin module cannot be explicitly imported unless we're a .sil file
  // or in the REPL.
  if ((TU->Kind == TranslationUnit::SIL || TU->Kind == TranslationUnit::REPL) &&
      moduleID.first.str() == "Builtin")
    return TU->Ctx.TheBuiltinModule;

  // If the imported module name is the same as the current translation unit,
  // skip the Swift module loader and use the Clang module loader instead.
  // This allows a Swift module to extend a Clang module of the same name.
  if (moduleID.first == TU->Name && modulePath.size() == 1) {
    if (auto importer = Context.getClangModuleLoader())
      return importer->loadModule(moduleID.second, modulePath);
    return nullptr;
  }
  
  return Context.getModule(modulePath);
}

/// Gets the import kind that is most appropriate for \p VD.
///
/// Note that this will never return \c Type; an imported typealias will use
/// the more specific kind from its underlying type.
static ImportKind getBestImportKind(const ValueDecl *VD) {
  switch (VD->getKind()) {
  case DeclKind::Import:
  case DeclKind::Extension:
  case DeclKind::PatternBinding:
  case DeclKind::TopLevelCode:
  case DeclKind::InfixOperator:
  case DeclKind::PrefixOperator:
  case DeclKind::PostfixOperator:
    llvm_unreachable("not a ValueDecl");

  case DeclKind::AssociatedType:
  case DeclKind::Constructor:
  case DeclKind::Destructor:
  case DeclKind::GenericTypeParam:
  case DeclKind::Subscript:
  case DeclKind::EnumElement:
    llvm_unreachable("not a top-level ValueDecl");

  case DeclKind::Protocol:
    return ImportKind::Protocol;

  case DeclKind::Class:
    return ImportKind::Class;
  case DeclKind::Enum:
    return ImportKind::Enum;
  case DeclKind::Struct:
    return ImportKind::Struct;

  case DeclKind::TypeAlias: {
    Type underlyingTy = cast<TypeAliasDecl>(VD)->getUnderlyingType();
    return getBestImportKind(underlyingTy->getAnyNominal());
  }

  case DeclKind::Func:
    return ImportKind::Func;

  case DeclKind::Var:
    return ImportKind::Var;
  }
}

/// Returns the most appropriate import kind for the given list of decls.
///
/// If the list is non-homogenous, or if there is more than one decl that cannot
/// be overloaded, returns Nothing.
Optional<ImportKind> findBestImportKind(ArrayRef<ValueDecl *> decls) {
  assert(!decls.empty());
  ImportKind firstKind = getBestImportKind(decls.front());

  // Only functions can be overloaded.
  if (decls.size() == 1)
    return firstKind;
  if (firstKind != ImportKind::Func)
    return Nothing;

  for (auto next : decls.slice(1)) {
    if (getBestImportKind(next) != firstKind)
      return Nothing;
  }

  return firstKind;
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
}

Optional<std::pair<ImportedModule, bool>> NameBinder::addImport(ImportDecl *ID) {
  Module *M = getModule(ID->getModulePath());
  if (M == 0) {
    // FIXME: print entire path regardless.
    if (ID->getModulePath().size() == 1) {
      diagnose(ID->getLoc(), diag::sema_no_import,
               ID->getModulePath().front().first.str());
    } else {
      diagnose(ID->getLoc(), diag::sema_no_import_submodule);
    }
    return Nothing;
  }

  auto result = std::make_pair(ImportedModule(ID->getDeclPath(), M),
                               ID->isExported());

  // If we're importing a specific decl, validate the import kind.
  if (ID->getImportKind() != ImportKind::Module) {
    auto declPath = ID->getDeclPath();

    assert(declPath.size() == 1 && "can't handle sub-decl imports");
    SmallVector<ValueDecl *, 8> decls;
    M->lookupQualified(ModuleType::get(M), declPath.front().first,
                       NL_QualifiedDefault, decls);

    if (decls.empty()) {
      diagnose(ID, diag::no_decl_in_module)
        .highlight(SourceRange(declPath.front().second,
                               declPath.back().second));
      return result;
    }

    Optional<ImportKind> actualKind = findBestImportKind(decls);
    if (!actualKind.hasValue()) {
      // FIXME: print entire module name?
      diagnose(ID, diag::ambiguous_decl_in_module,
               declPath.front().first, M->Name);
      for (auto next : decls)
        diagnose(next, diag::found_candidate);

    } else if (!isCompatibleImportKind(ID->getImportKind(), *actualKind)) {
      diagnose(ID, diag::imported_decl_is_wrong_kind,
               declPath.front().first,
               getImportKindString(ID->getImportKind()),
               static_cast<unsigned>(*actualKind))
        .fixItReplace(SourceRange(ID->getKindLoc()),
                      getImportKindString(*actualKind));

      if (decls.size() == 1)
        diagnose(decls.front(), diag::decl_declared_here,
                 decls.front()->getName());
    }
  }

  return result;
}

/// performAutoImport - When a translation unit is first set up, this handles
/// setting up any auto imports of the standard library.
void swift::performAutoImport(TranslationUnit *TU) {
  // If we're building the standard library, import the magic Builtin module,
  // otherwise, import the standard library.
  Module *M;
  if (TU->HasBuiltinModuleAccess)
    M = TU->Ctx.TheBuiltinModule;
  else
    M = TU->Ctx.getModule(std::make_pair(TU->Ctx.getIdentifier("swift"),
                                         SourceLoc()));

  auto Import = std::make_pair(ImportedModule({}, M), false);
  TU->setImports(TU->Ctx.AllocateCopy(llvm::makeArrayRef(Import)));
}

//===----------------------------------------------------------------------===//
// performNameBinding
//===----------------------------------------------------------------------===//

template<typename OP_DECL>
static void insertOperatorDecl(NameBinder &Binder,
                               llvm::StringMap<OP_DECL*> &Operators,
                               OP_DECL *OpDecl) {
  auto previousDecl = Operators.find(OpDecl->getName().get());
  if (previousDecl != Operators.end()) {
    Binder.diagnose(OpDecl->getLoc(), diag::operator_redeclared);
    Binder.diagnose(previousDecl->getValue(), diag::previous_operator_decl);
    return;
  }
  
  Operators[OpDecl->getName().get()] = OpDecl;
}

namespace {
  /// \brief AST mutation listener that captures any added declarations and
  /// types, then adds them to the translation unit.
  class CaptureExternalsListener : public ASTMutationListener {
    TranslationUnit *TU;

    CaptureExternalsListener(const CaptureExternalsListener &) = delete;

    CaptureExternalsListener &
    operator=(const CaptureExternalsListener &) = delete;

  public:
    explicit CaptureExternalsListener(TranslationUnit *TU) : TU(TU) {
      TU->getASTContext().addMutationListener(*this);
    }

    ~CaptureExternalsListener() {
      TU->getASTContext().removeMutationListener(*this);
    }

    /// \brief A new declaration was added to the AST.
    virtual void addedExternalDecl(Decl *decl) {
      TU->getASTContext().ExternalDefinitions.insert(decl);
    }

    /// \brief A new type was added to the AST.
    virtual void addedExternalType(Type type) {
      TU->getASTContext().ExternalTypes.push_back(type);
    }
};
}

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this parsing has been performed, but we still have UnresolvedDeclRefExpr
/// nodes for unresolved value names, and we may have unresolved type names as
/// well.  This handles import directives and forward references.
void swift::performNameBinding(TranslationUnit *TU, unsigned StartElem) {
  // Make sure we skip adding the standard library imports if the
  // translation unit is empty.
  if (TU->Decls.empty()) {
    TU->ASTStage = TranslationUnit::NameBound;
    return;
  }

  CaptureExternalsListener Capture(TU);
  
  // Reset the name lookup cache so we find new decls.
  // FIXME: This is inefficient.
  TU->clearLookupCache();

  NameBinder Binder(TU);

  SmallVector<std::pair<ImportedModule, bool>, 8> ImportedModules;
  ImportedModules.append(TU->getImports().begin(),
                         TU->getImports().end());

  // Do a prepass over the declarations to find and load the imported modules
  // and map operator decls.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    if (ImportDecl *ID = dyn_cast<ImportDecl>(TU->Decls[i])) {
      if (auto import = Binder.addImport(ID))
        ImportedModules.push_back(*import);
    } else if (auto *OD = dyn_cast<PrefixOperatorDecl>(TU->Decls[i]))
      insertOperatorDecl(Binder, TU->PrefixOperators, OD);
    else if (auto *OD = dyn_cast<PostfixOperatorDecl>(TU->Decls[i]))
      insertOperatorDecl(Binder, TU->PostfixOperators, OD);
    else if (auto *OD = dyn_cast<InfixOperatorDecl>(TU->Decls[i]))
      insertOperatorDecl(Binder, TU->InfixOperators, OD);
  }

  if (ImportedModules.size() > TU->getImports().size())
    TU->setImports(TU->Ctx.AllocateCopy(ImportedModules));

  // FIXME: This algorithm has quadratic memory usage.  (In practice,
  // import statements after the first "chunk" should be rare, though.)
  // FIXME: Can we make this more efficient?

  llvm::DenseMap<Identifier, ValueDecl*> CheckTypes;
  for (unsigned i = 0, e = TU->Decls.size(); i != e; ++i) {
    Decl *D = TU->Decls[i];
    if (D->isInvalid())
      // No need to diagnose redeclarations of invalid declarations, we have
      // already complained about them in some other way.
      continue;

    if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
      // Check for declarations with the same name which aren't overloaded
      // vars/funcs.
      // FIXME: We don't have enough information to do this properly here,
      // because we need resolved types to find duplicates.
      if (VD->getName().empty())
        continue;
      ValueDecl *&LookupD = CheckTypes[VD->getName()];
      ValueDecl *PrevD = LookupD;
      LookupD = VD;
      if (i >= StartElem) {
        if (PrevD && !((isa<VarDecl>(VD)    || isa<FuncDecl>(VD)) &&
                       (isa<VarDecl>(PrevD) || isa<FuncDecl>(PrevD)))) {
          Binder.diagnose(VD->getStartLoc(), diag::invalid_redecl);
          Binder.diagnose(PrevD, diag::invalid_redecl_prev,
                          VD->getName());
        }
      }
    }
  }

  TU->ASTStage = TranslationUnit::NameBound;
  verify(TU);
}


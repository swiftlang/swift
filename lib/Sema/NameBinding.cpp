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
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/Path.h"
#include <algorithm>
using namespace swift;

//===----------------------------------------------------------------------===//
// NameBinder
//===----------------------------------------------------------------------===//

typedef TranslationUnit::ImportedModule ImportedModule;
typedef llvm::PointerUnion<const ImportedModule*, OneOfType*> BoundScope;

namespace {  
  class NameBinder {    
  public:
    TranslationUnit *TU;
    ASTContext &Context;

    NameBinder(TranslationUnit *TU) : TU(TU), Context(TU->Ctx) {
      for (auto M : TU->getImportedModules()) {
        // Don't add the builtin module to the LoadedModules list.
        if (isa<BuiltinModule>(M.second))
          continue;
        
        Module *&ref = Context.LoadedModules[M.second->Name.str()];
        if (ref)
          assert(ref == M.second ||
                 isa<TranslationUnit>(ref) && isa<ClangModule>(M.second));
        else
          ref = M.second;
      }
    }
    ~NameBinder() {
    }
    
    template<typename ...ArgTypes>
    InFlightDiagnostic diagnose(ArgTypes... Args) {
      return Context.Diags.diagnose(Args...);
    }
    
    void addImport(ImportDecl *ID, SmallVectorImpl<ImportedModule> &Result);

    /// Load a module referenced by an import statement.
    ///
    /// Returns null if no module can be loaded.
    Module *getModule(llvm::ArrayRef<std::pair<Identifier,SourceLoc>> ModuleID,
                      bool isStdlibImport);
  };
}

Module *
NameBinder::getModule(ArrayRef<std::pair<Identifier, SourceLoc>> modulePath,
                      bool isStdlibImport) {
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
      return importer->loadModule(moduleID.second, modulePath, isStdlibImport);
    return nullptr;
  }
  
  return Context.getModule(modulePath, isStdlibImport);
}


void NameBinder::addImport(ImportDecl *ID, 
                           SmallVectorImpl<ImportedModule> &Result) {
  ArrayRef<ImportDecl::AccessPathElement> Path = ID->getAccessPath();

  // FIXME: This is a hack to allow /either/ Clang submodules /or/ importing
  // declarations from within Swift translation units...but not both. We may
  // need to design this carefully: what does "Foundation.NSString" refer to?
  auto importPath = Path;
  if (!Context.getClangModuleLoader())
    importPath = importPath.slice(0, 1);

  Module *M = getModule(importPath, ID->isStdlibImport());
  if (M == 0) {
    diagnose(Path[0].second, diag::sema_no_import, Path[0].first.str());
    return;
  }
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (Path.size() > 2) {
    diagnose(Path[2].second, diag::invalid_declaration_imported);
    return;
  }
  
  Result.push_back(ImportedModule(Path.slice(1), M));

  // If the module we loaded is a translation unit that imports a Clang
  // module with the same name, add that Clang module to our own set of imports.
  // FIXME: This is a horrible, horrible way to provide transitive inclusion.
  // We really need a re-export syntax.
  if (Context.getClangModuleLoader()) {
    if (auto tu = dyn_cast<TranslationUnit>(M)) {
      for (auto imported : tu->getImportedModules()) {
        // Only check for Clang modules.
        auto clangMod = dyn_cast<ClangModule>(imported.second);
        if (!clangMod)
          continue;

        // With the same name as the module we imported.
        if (clangMod->Name != Path[0].first)
          continue;

        Result.push_back(ImportedModule(Path.slice(1), clangMod));
        break;
      }
    }
  }
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
                                         SourceLoc()), true);

  SmallVector<ImportedModule, 1> ImportedModules;
  ImportedModules.push_back({Module::AccessPathTy(), M});

  TU->setImportedModules(TU->Ctx.AllocateCopy(ImportedModules));
}

//===----------------------------------------------------------------------===//
// performNameBinding
//===----------------------------------------------------------------------===//

/// \brief Collect all of the Clang modules exported from this module
/// and its implicit submodules.
static void collectExportedClangModules(clang::Module *mod, SourceLoc importLoc,
              SmallVectorImpl<std::pair<clang::Module *, SourceLoc>> &results,
              llvm::SmallPtrSet<clang::Module *, 8> &visited) {
  // Collect Clang modules exported from this particular module.
  SmallVector<clang::Module *, 4> exported;
  mod->getExportedModules(exported);
  for (auto exportedModule : exported) {
    if (visited.insert(exportedModule)) {
      results.push_back({ exportedModule, importLoc });

      collectExportedClangModules(exportedModule, importLoc, results, visited);
    }
  }

  // Recurse into available, implicit submodules.
  for (auto sm = mod->submodule_begin(), smEnd = mod->submodule_end();
       sm != smEnd; ++sm) {
    if ((*sm)->IsExplicit || !(*sm)->IsAvailable)
      continue;

    if (visited.insert(*sm))
      collectExportedClangModules(*sm, importLoc, results, visited);
  }
}

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

  SmallVector<ImportedModule, 8> ImportedModules;
  ImportedModules.append(TU->getImportedModules().begin(),
                         TU->getImportedModules().end());

  // Do a prepass over the declarations to find and load the imported modules
  // and map operator decls.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    if (ImportDecl *ID = dyn_cast<ImportDecl>(TU->Decls[i]))
      Binder.addImport(ID, ImportedModules);
    else if (auto *OD = dyn_cast<PrefixOperatorDecl>(TU->Decls[i]))
      insertOperatorDecl(Binder, TU->PrefixOperators, OD);
    else if (auto *OD = dyn_cast<PostfixOperatorDecl>(TU->Decls[i]))
      insertOperatorDecl(Binder, TU->PostfixOperators, OD);
    else if (auto *OD = dyn_cast<InfixOperatorDecl>(TU->Decls[i]))
      insertOperatorDecl(Binder, TU->InfixOperators, OD);
  }

  // Walk the dependencies of imported Clang modules. For any imported
  // dependencies, also load the corresponding Swift module, if it exists.
  // FIXME: The Swift modules themselves should re-export their dependencies,
  // if they show up in the interface.
  {
    SmallVector<std::pair<clang::Module*, SourceLoc>, 4> importedClangModules;
    llvm::SmallPtrSet<clang::Module*, 8> visited;
    for (auto imported : ImportedModules) {
      auto mod = dyn_cast<ClangModule>(imported.second);
      if (!mod)
        continue;

      SourceLoc loc;
      if (imported.first.empty())
        loc = TU->Decls[0]->getStartLoc();
      else
        loc = imported.first[0].second;
      
      auto clangMod = mod->getClangModule();
      if (visited.insert(clangMod))
        collectExportedClangModules(clangMod, loc, importedClangModules,
                                    visited);
    }

    ASTContext &ctx = TU->getASTContext();
    llvm::DenseMap<clang::Module *, bool> topModules;
    for (auto clangImport : importedClangModules) {
      // Look at the top-level module.
      auto topClangMod = clangImport.first->getTopLevelModule();
      auto knownTop = topModules.find(topClangMod);
      bool hasSwiftModule;
      if (knownTop == topModules.end()) {
        // If we haven't looked for a Swift module corresponding to the
        // top-level module yet, do so now.

        ImportDecl::AccessPathElement importPair =
          { ctx.getIdentifier(topClangMod->Name), clangImport.second };

        // It's a little wasteful to load the module here and then again below,
        // but the one below should pick up the same (already-loaded) module.
        auto module = ctx.getModule(importPair, false);

        hasSwiftModule = !isa<ClangModule>(module);
        topModules[topClangMod] = hasSwiftModule;
      } else {
        hasSwiftModule = knownTop->second;
      }

      if (!hasSwiftModule)
        continue;

      // Form an implicit import of the Swift module.
      SmallVector<ImportDecl::AccessPathElement, 4> accessPath;
      for (auto mod = clangImport.first; mod; mod = mod->Parent) {
        accessPath.push_back({ ctx.getIdentifier(mod->Name),
                               clangImport.second });
      }
      std::reverse(accessPath.begin(), accessPath.end());

      // Create an implicit import declaration and import it.
      // FIXME: Mark as implicit!
      // FIXME: Actually pass through the whole access path.
      auto import = ImportDecl::create(ctx, TU, clangImport.second,
                                       accessPath[0], false);
      TU->Decls.push_back(import);
      Binder.addImport(import, ImportedModules);
    }
  }

  // FIXME: This algorithm has quadratic memory usage.  (In practice,
  // import statements after the first "chunk" should be rare, though.)
  if (ImportedModules.size() > TU->getImportedModules().size())
    TU->setImportedModules(TU->Ctx.AllocateCopy(ImportedModules));

  // FIXME: This is quadratic time for TUs with multiple chunks.
  // FIXME: Can we make this more efficient?

  llvm::DenseMap<Identifier, ValueDecl*> CheckTypes;
  for (unsigned i = 0, e = TU->Decls.size(); i != e; ++i) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(TU->Decls[i])) {
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


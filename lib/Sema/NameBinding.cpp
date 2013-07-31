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
typedef llvm::PointerUnion<const ImportedModule*, UnionType*> BoundScope;

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
          assert(ref == M.second || isa<ClangModule>(M.second));
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
    Module *getModule(llvm::ArrayRef<std::pair<Identifier,SourceLoc>> ModuleID);
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


void NameBinder::addImport(ImportDecl *ID, 
                           SmallVectorImpl<ImportedModule> &Result) {
  ArrayRef<ImportDecl::AccessPathElement> Path = ID->getAccessPath();

  // FIXME: This is a hack to allow /either/ Clang submodules /or/ importing
  // declarations from within Swift translation units...but not both. We may
  // need to design this carefully: what does "Foundation.NSString" refer to?
  auto importPath = Path;
  if (!Context.getClangModuleLoader())
    importPath = importPath.slice(0, 1);

  Module *M = getModule(importPath);
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

  M->forAllVisibleModules(Path.slice(1), [&](const ImportedModule &import) {
    Result.push_back(import);
    return true;
  });
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

  SmallVector<ImportedModule, 1> ImportedModules;
  ImportedModules.push_back({Module::AccessPathTy(), M});

  TU->setImportedModules(TU->Ctx.AllocateCopy(ImportedModules));
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

  if (ImportedModules.size() > TU->getImportedModules().size())
    TU->setImportedModules(TU->Ctx.AllocateCopy(ImportedModules));

  // FIXME: This algorithm has quadratic memory usage.  (In practice,
  // import statements after the first "chunk" should be rare, though.)
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


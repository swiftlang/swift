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
    bool ImportedBuiltinModule;

    NameBinder(TranslationUnit *TU)
    : TU(TU), Context(TU->Ctx), ImportedBuiltinModule(false) {
      for (auto M : TU->getImportedModules()) {
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
    void addStandardLibraryImport(SmallVectorImpl<ImportedModule> &Result);

    /// resolveIdentifierType - Perform name binding for a IdentifierType,
    /// resolving it or diagnosing the error as appropriate and return true on
    /// failure.  On failure, this leaves IdentifierType alone, otherwise it
    /// fills in the Components.
    bool resolveIdentifierType(IdentifierType *DNT, DeclContext *DC);

    llvm::error_code findModule(StringRef Module,
                                SourceLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer);

  private:
    /// getModule - Load a module referenced by an import statement,
    /// emitting an error at the specified location and returning null on
    /// failure.
    Module *getModule(llvm::ArrayRef<std::pair<Identifier,SourceLoc>> ModuleID);
  };
}

llvm::error_code NameBinder::findModule(StringRef Module, 
                                        SourceLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer) {
  std::string ModuleFilename = Module.str() + std::string(".swift");
  
  llvm::SmallString<128> InputFilename;
  
  // First, search in the directory corresponding to the import location.
  // FIXME: This screams for a proper FileManager abstraction.
  llvm::SourceMgr &SourceMgr = Context.SourceMgr;
  int CurrentBufferID = SourceMgr.FindBufferContainingLoc(ImportLoc.Value);
  if (CurrentBufferID >= 0) {
    const llvm::MemoryBuffer *ImportingBuffer 
      = SourceMgr.getBufferInfo(CurrentBufferID).Buffer;
    StringRef CurrentDirectory 
      = llvm::sys::path::parent_path(ImportingBuffer->getBufferIdentifier());
    if (!CurrentDirectory.empty()) {
      InputFilename = CurrentDirectory;
      llvm::sys::path::append(InputFilename, ModuleFilename);
      llvm::error_code Err = llvm::MemoryBuffer::getFile(InputFilename, Buffer);
      if (!Err)
        return Err;
    }
  }
  
  // Second, search in the current directory.
  llvm::error_code Err = llvm::MemoryBuffer::getFile(ModuleFilename, Buffer);
  if (!Err)
    return Err;

  // If we fail, search each import search path.
  for (auto Path : Context.ImportSearchPaths) {
    InputFilename = Path;
    llvm::sys::path::append(InputFilename, ModuleFilename);
    Err = llvm::MemoryBuffer::getFile(InputFilename, Buffer);
    if (!Err)
      return Err;
  }

  return Err;
}

Module *NameBinder::getModule(
                        ArrayRef<std::pair<Identifier, SourceLoc>> ModulePath) {
  // TODO: Swift submodules.
  assert(ModulePath.size() >= 1 && "empty import path");
  auto ModuleID = ModulePath[0];
  
  // TODO: We currently just recursively parse referenced modules.  This works
  // fine for now since they are each a single file.  Ultimately we'll want a
  // compiled form of AST's like clang's that support lazy deserialization.

  // FIXME: We shouldn't really allow arbitrary modules to import Builtin.
  if (ModuleID.first.str() == "Builtin") {
    ImportedBuiltinModule = true;
    return TU->Ctx.TheBuiltinModule;
  }

  // If the imported module name is the same as the current translation unit,
  // skip the Swift module loader and use the Clang module loader instead.
  // This allows a Swift module to extend a Clang module of the same name.
  bool useClangModule = false;
  if (ModuleID.first == TU->Name && Context.hasModuleLoader())
    useClangModule = true;

  Module *M = nullptr;
  
  // FIXME: For now, ignore submodule paths except for Clang modules.
  if (ModulePath.size() > 1 && Context.hasModuleLoader()) {
    useClangModule = true;
  } else {
    M = Context.LoadedModules.lookup(ModuleID.first.str());
    if (M && !(useClangModule && !isa<ClangModule>(M)))
      return M;
  }

  // Open the input file.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (!useClangModule) {
    if (llvm::error_code Err = findModule(ModuleID.first.str(), ModuleID.second,
                                          InputFile)) {
      if (Err.value() != llvm::errc::no_such_file_or_directory ||
          !Context.hasModuleLoader()) {
        diagnose(ModuleID.second, diag::sema_opening_import,
                 ModuleID.first.str(), Err.message());
        return 0;
      }

      // There was no Swift module with this name, so try a Clang module.
      useClangModule = true;
    }
  }

  if (useClangModule) {
    // FIXME: We're assuming that 'externally loaded module' == 'clang module',
    // which is clearly nonsense. We almost certainy want to have a chain
    // of external module loaders, with the Swift one first and the Clang one
    // as a fallback.
    // FIXME: Bad location info?
    return Context.getModuleLoader().loadModule(ModuleID.second,
                                                ModulePath);
  }

  unsigned BufferID =
    Context.SourceMgr.AddNewSourceBuffer(InputFile.take(),
                                         ModuleID.second.Value);

  // FIXME: Turn off the constraint-based type checker for the imported 'swift'
  // module.
  llvm::SaveAndRestore<bool> saveUseCS(Context.LangOpts.UseConstraintSolver,
                                       (Context.LangOpts.UseConstraintSolver &&
                                        ModuleID.first.str() != "swift"));

  // For now, treat all separate modules as unique components.
  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  TranslationUnit *ImportedTU;
  ImportedTU = new (Context) TranslationUnit(ModuleID.first, Comp, Context,
                                             /*IsMainModule*/false,
                                             /*IsReplModule*/false);

  Context.LoadedModules[ModuleID.first.str()] = ImportedTU;
  parseIntoTranslationUnit(ImportedTU, BufferID);

  // We have to do name binding on it to ensure that types are fully resolved.
  // This should eventually be eliminated by having actual fully resolved binary
  // dumps of the code instead of reparsing though.
  // FIXME: We also need to deal with circular imports!
  performNameBinding(ImportedTU);
  performTypeChecking(ImportedTU);
  
  return ImportedTU;
}


void NameBinder::addImport(ImportDecl *ID, 
                           SmallVectorImpl<ImportedModule> &Result) {
  ArrayRef<ImportDecl::AccessPathElement> Path = ID->getAccessPath();
  Module *M = getModule(Path);
  if (M == 0) return;
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (Path.size() > 2) {
    diagnose(Path[2].second, diag::invalid_declaration_imported);
    return;
  }
  
  Result.push_back(std::make_pair(Path.slice(1), M));

  // If the module we loaded is a translation unit that imports a Clang
  // module with the same name, add that Clang module to our own set of imports.
  // FIXME: This is a horrible, horrible way to provide transitive inclusion.
  // We really need a re-export syntax.
  if (Context.hasModuleLoader()) {
    if (auto tu = dyn_cast<TranslationUnit>(M)) {
      for (auto imported : tu->getImportedModules()) {
        // Only check for Clang modules.
        auto clangMod = dyn_cast<ClangModule>(imported.second);
        if (!clangMod)
          continue;

        // With the same name as the module we imported.
        if (clangMod->Name != Path[0].first)
          continue;

        Result.push_back(std::make_pair(Path.slice(1), clangMod));
        break;
      }
    }
  }
}

void NameBinder::
addStandardLibraryImport(SmallVectorImpl<ImportedModule> &Result) {
  // FIXME: The current model is that if a module doesn't explicitly import
  // Builtin or swift, we implicitly import swift.  This isn't really ideal.
  if (ImportedBuiltinModule)
    return;
  for (ImportedModule M : Result)
    if (M.second->Name.str() == "swift")
      return;
  if (TU->Name.str() == "swift")
    return;

  Identifier SwiftID = Context.getIdentifier("swift");
  Module *M = getModule(std::make_pair(SwiftID, TU->Decls[0]->getStartLoc()));
  if (M == 0) return;
  Result.push_back(std::make_pair(Module::AccessPathTy(), M));
}

/// resolveIdentifierType - Perform name binding for a IdentifierType,
/// resolving it or diagnosing the error as appropriate and return true on
/// failure.  On failure, this leaves IdentifierType alone, otherwise it fills
/// in the Components.
bool NameBinder::resolveIdentifierType(IdentifierType *DNT, DeclContext *DC) {
  MutableArrayRef<IdentifierType::Component> Components = DNT->Components;

  // If name lookup for the base of the type didn't get resolved in the
  // parsing phase, perform lookup on it.
  if (Components[0].Value.isNull()) {
    Identifier Name = Components[0].Id;
    SourceLoc Loc = Components[0].Loc;

    // Perform an unqualified lookup.
    UnqualifiedLookup Globals(Name, DC, SourceLoc(), /*IsTypeLookup*/true);

    if (Globals.Results.size() > 1) {
      diagnose(Loc, diag::abiguous_type_base, Name)
        << SourceRange(Loc, Components.back().Loc);
      for (auto Result : Globals.Results) {
        if (Globals.Results[0].hasValueDecl())
          diagnose(Result.getValueDecl(), diag::found_candidate);
        else
          diagnose(Loc, diag::found_candidate);
      }
      return true;
    }

    if (Globals.Results.empty()) {
      diagnose(Loc, Components.size() == 1 ? 
                 diag::use_undeclared_type : diag::unknown_name_in_type, Name)
        << SourceRange(Loc, Components.back().Loc);
      return true;
    }

    switch (Globals.Results[0].Kind) {
    case UnqualifiedLookupResult::ModuleMember:
    case UnqualifiedLookupResult::LocalDecl:
    case UnqualifiedLookupResult::MemberProperty:
    case UnqualifiedLookupResult::MemberFunction:
    case UnqualifiedLookupResult::MetatypeMember:
    case UnqualifiedLookupResult::ExistentialMember:
    case UnqualifiedLookupResult::ArchetypeMember:
      Components[0].Value = Globals.Results[0].getValueDecl();
      break;
    case UnqualifiedLookupResult::ModuleName:
      Components[0].Value = Globals.Results[0].getNamedModule();
      break;

    case UnqualifiedLookupResult::MetaArchetypeMember:
      // FIXME: This is actually possible in protocols.
      llvm_unreachable("meta-archetype member in unqualified name lookup");
      break;
    }
  }

  assert(!Components[0].Value.isNull() && "Failed to get a base");

  // Now that we have a base, iteratively resolve subsequent member entries.
  auto LastOne = Components[0];
  for (auto &C : Components.slice(1, Components.size()-1)) {
    if (auto M = LastOne.Value.dyn_cast<Module*>()) {
      // Lookup into a named module.
      SmallVector<ValueDecl*, 8> Decls;
      M->lookupValue(Module::AccessPathTy(), C.Id, 
                     NLKind::QualifiedLookup, Decls);
      if (Decls.size() == 1 && isa<TypeDecl>(Decls.back()))
        C.Value = cast<TypeDecl>(Decls.back());
    } else if (LastOne.Value.is<ValueDecl*>()) {
      diagnose(C.Loc, diag::cannot_resolve_extension_dot)
        << SourceRange(Components[0].Loc, Components.back().Loc);
      return true;
    } else {
      diagnose(C.Loc, diag::unknown_dotted_type_base, LastOne.Id)
        << SourceRange(Components[0].Loc, Components.back().Loc);
      return true;
    }

    if (C.Value.isNull()) {
      diagnose(C.Loc, diag::invalid_member_type, C.Id, LastOne.Id)
        << SourceRange(Components[0].Loc, Components.back().Loc);
      return true;
    }

    LastOne = C;
  }

  // Finally, sanity check that the last value is a type.
  if (ValueDecl *Last = Components.back().Value.dyn_cast<ValueDecl*>()) {
    auto GenericArgs = Components.back().GenericArgs;
    if (!GenericArgs.empty()) {
      if (auto NTD = dyn_cast<NominalTypeDecl>(Last)) {
        SmallVector<Type, 4> GenericArgTypes;
        for (TypeLoc T : GenericArgs)
          GenericArgTypes.push_back(T.getType());
        Components.back().Value = BoundGenericType::get(NTD, Type(),
                                                        GenericArgTypes);
        return false;
      }
      // FIXME: Need better diagnostic here
    } else if (auto TD = dyn_cast<TypeDecl>(Last)) {
      Components.back().Value = TD->getDeclaredType();
      return false;
    }
  }

  diagnose(Components.back().Loc,
           Components.size() == 1 ? diag::named_definition_isnt_type :
             diag::dotted_reference_not_type, Components.back().Id)
    << SourceRange(Components[0].Loc, Components.back().Loc);
  return true;
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

static void bindFuncDeclToOperator(NameBinder &Binder,
                                   TranslationUnit *TU,
                                   FuncDecl *FD) {
  OperatorDecl *op;
  if (FD->getAttrs().isPrefix()) {
    if (auto maybeOp = TU->lookupPrefixOperator(FD->getName(), FD->getLoc()))
      op = *maybeOp;
    else
      return;
  } else if (FD->getAttrs().isPostfix()) {
    if (auto maybeOp = TU->lookupPostfixOperator(FD->getName(), FD->getLoc()))
      op = *maybeOp;
    else
      return;
  } else {
    if (auto maybeOp = TU->lookupInfixOperator(FD->getName(), FD->getLoc()))
      op = *maybeOp;
    else
      return;
  }
  
  if (!op) {
    Binder.diagnose(FD->getLoc(), diag::declared_operator_without_operator_decl);
  } else {
    FD->setOperatorDecl(op);
  }
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

  bool IsInitialNameBinding = TU->getImportedModules().empty();

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
    SmallVector<std::pair<clang::Module *, SourceLoc>, 4> importedClangModules;
    llvm::SmallPtrSet<clang::Module *, 8> visited;
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

    llvm::DenseMap<clang::Module *, bool> topModules;
    for (auto clangImport : importedClangModules) {
      // Look at the top-level module.
      auto topClangMod = clangImport.first->getTopLevelModule();
      auto knownTop = topModules.find(topClangMod);
      bool hasSwiftModule;
      if (knownTop == topModules.end()) {
        // If we haven't looked for a Swift module corresponding to the
        // top-level module yet, do so now.
        llvm::OwningPtr<llvm::MemoryBuffer> buffer; // FIXME: wasteful!
        if (Binder.findModule(topClangMod->Name, clangImport.second, buffer)) {
          hasSwiftModule = false;
        } else {
          hasSwiftModule = true;
        }

        topModules[topClangMod] = hasSwiftModule;
      } else {
        hasSwiftModule = knownTop->second;
      }

      if (!hasSwiftModule)
        continue;

      // Form an implicit import of the Swift module.
      SmallVector<ImportDecl::AccessPathElement, 4> accessPath;
      ASTContext &context = TU->getASTContext();
      for (auto mod = clangImport.first; mod; mod = mod->Parent) {
        accessPath.push_back({ context.getIdentifier(mod->Name),
                               clangImport.second });
      }
      std::reverse(accessPath.begin(), accessPath.end());

      // Create an implicit import declaration and import it.
      // FIXME: Mark as implicit!
      // FIXME: Actually pass through the whole access path.
      auto import = ImportDecl::create(context, TU, clangImport.second,
                                       accessPath[0]);
      TU->Decls.push_back(import);
      Binder.addImport(import, ImportedModules);
    }
  }

  // Add the standard library import.
  // FIXME: The semantics here are sort of strange if an import statement is
  // after the first "chunk" in the main module.
  if (IsInitialNameBinding)
    Binder.addStandardLibraryImport(ImportedModules);

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
      // FIXME: I'm not sure this check is really correct.
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

  // FIXME:
  // The rule we want:
  // If a type is defined in an extension in the current module, and you're
  // trying to refer to it from an extension declaration, and the number of
  // dots you're using to refer to it is greater than its natural nesting depth,
  // it is an error.
  //
  // The current rule:
  // If you're trying to refer to a type from an extension declaration, and you
  // need to resolve any dots to find the type, it's an error.
  //
  // The really tricky part about making everything work correctly is
  // shadowing: if an extension defines a type which shadows a type in an
  // imported module, we have to make sure we don't choose a type which shouldn't
  // be visible.
  //
  // After this loop finishes, we can perform normal member lookup.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(TU->Decls[i])) {
      auto DNT = cast<IdentifierType>(ED->getExtendedType().getPointer());
      while (true) {
        if (Binder.resolveIdentifierType(DNT, TU)) {
          for (auto &C : DNT->Components)
            C.Value = TU->Ctx.TheErrorType;

          break;
        } else {
          // We need to make sure the extended type is canonical. There are
          // three possibilities here:
          // 1. The type is already canonical, because it's either a NominalType
          // or comes from an imported module.
          // 2. The type is a NameAliasType, which
          // we need to resolve immediately because we can't leave a
          // non-canonical type here in the AST.
          // 3. The type is a BoundGenericType; it's illegal to extend
          // such a type.
          Type FoundType = DNT->Components.back().Value.get<Type>();
          if (FoundType->hasCanonicalTypeComputed())
            break;

          if (isa<BoundGenericType>(FoundType.getPointer())) {
            Binder.diagnose(ED->getLoc(), diag::non_nominal_extension,
                            false, ED->getExtendedType());
            ED->getExtendedTypeLoc().setInvalidType(Binder.Context);
            break;
          }

          TypeAliasDecl *TAD =
              cast<NameAliasType>(FoundType.getPointer())->getDecl();
          TypeBase *underlying;
          
          do {
            underlying = TAD->getUnderlyingType().getPointer();
          
            if (auto *underlyingAlias = dyn_cast<NameAliasType>(underlying)) {
              TAD = underlyingAlias->getDecl();
              continue;
            }
            break;
          } while (true);
          
          if (isa<NominalType>(underlying))
            break;
          
          DNT = dyn_cast<IdentifierType>(underlying);

          if (!DNT) {
            Binder.diagnose(ED->getLoc(), diag::non_nominal_extension,
                            false, ED->getExtendedType());
            ED->getExtendedTypeLoc().setInvalidType(Binder.Context);
            break;
          }
        }
      }
    } else if (ClassDecl *CD = dyn_cast<ClassDecl>(TU->Decls[i])) {
      auto inherited = CD->getInherited();
      if (!inherited.empty()) {
        auto DNT = cast<IdentifierType>(inherited[0].getType().getPointer());
        Type foundType;
        while (true) {
          if (Binder.resolveIdentifierType(DNT, TU)) {
            for (auto &C : DNT->Components)
              C.Value = TU->Ctx.TheErrorType;

            foundType = Type();
            break;
          } else {
            // We need to make sure the extended type is canonical. There are
            // three possibilities here:
            // 1. The type is already canonical, because it's either a NominalType
            // or comes from an imported module.
            // 2. The type is a NameAliasType from the current module, which
            // we need to resolve immediately because we can't leave a
            // non-canonical type here in the AST.
            // 3. The type is a BoundGenericType; such a type isn't going to be
            // canonical, but name lookup doesn't actually care about generic
            // arguments, so we can ignore the fact that they're unresolved.
            foundType = DNT->Components.back().Value.get<Type>();
            if (foundType->hasCanonicalTypeComputed())
              break;

            if (isa<BoundGenericType>(foundType.getPointer()))
              break;

            TypeAliasDecl *TAD =
                cast<NameAliasType>(foundType.getPointer())->getDecl();
            Type curUnderlying = TAD->getUnderlyingType();
            DNT = dyn_cast<IdentifierType>(curUnderlying.getPointer());

            if (!DNT) {
              if (isa<ProtocolCompositionType>(curUnderlying.getPointer())) {
                // We don't need to resolve ProtocolCompositionTypes here; it's
                // enough to know that the type in question isn't a class type.
                foundType = Type();
                break;
              }
              // FIXME: Handling for other types?
              Binder.diagnose(CD->getLoc(), diag::non_nominal_extension,
                              false, inherited[0].getType());
              inherited[0].setInvalidType(Binder.Context);
              foundType = Type();
              break;
            }
          }
        }

        if (!foundType)
          continue;

        // Check that the base type is a class.
        TypeLoc baseClass;
        if (foundType->getClassOrBoundGenericClass())
          baseClass = inherited[0];

        if (baseClass.getType()) {
          CD->setBaseClassLoc(baseClass);
          inherited = inherited.slice(1);
          CD->setInherited(inherited);
        }
      }
    } else if (FuncDecl *FD = dyn_cast<FuncDecl>(TU->Decls[i])) {
      // If this is an operator implementation, bind it to an operator decl.
      if (FD->isOperator())
        bindFuncDeclToOperator(Binder, TU, FD);
    }
  }

  // FIXME: Check for cycles in class inheritance here?

  TU->ASTStage = TranslationUnit::NameBound;
  verify(TU);
}


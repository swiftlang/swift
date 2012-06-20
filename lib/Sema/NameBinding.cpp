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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
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
    llvm::error_code findModule(StringRef Module, 
                                SourceLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer);
    
  public:
    TranslationUnit *TU;
    ASTContext &Context;
    bool ImportedBuiltinModule;

    NameBinder(TranslationUnit *TU)
    : TU(TU), Context(TU->Ctx), ImportedBuiltinModule(false) {
      for (auto M : TU->getImportedModules())
        Context.LoadedModules[M.second->Name.str()] = M.second;
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
    bool resolveIdentifierType(IdentifierType *DNT, DeclContext *DC,
                               bool NoMemberLookup = false);

  private:
    /// getModule - Load a module referenced by an import statement,
    /// emitting an error at the specified location and returning null on
    /// failure.
    Module *getModule(std::pair<Identifier,SourceLoc> ModuleID);
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

Module *NameBinder::getModule(std::pair<Identifier, SourceLoc> ModuleID) {
  // TODO: We currently just recursively parse referenced modules.  This works
  // fine for now since they are each a single file.  Ultimately we'll want a
  // compiled form of AST's like clang's that support lazy deserialization.

  // FIXME: We shouldn't really allow arbitrary modules to import Builtin.
  if (ModuleID.first.str() == "Builtin") {
    ImportedBuiltinModule = true;
    return TU->Ctx.TheBuiltinModule;
  }

  Module *M = Context.LoadedModules.lookup(ModuleID.first.str());
  if (M) return M;

  // Open the input file.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (llvm::error_code Err = findModule(ModuleID.first.str(), ModuleID.second,
                                        InputFile)) {
    diagnose(ModuleID.second, diag::sema_opening_import,
             ModuleID.first.str(), Err.message());
    return 0;
  }

  unsigned BufferID =
    Context.SourceMgr.AddNewSourceBuffer(InputFile.take(),
                                         ModuleID.second.Value);

  // For now, treat all separate modules as unique components.
  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  TranslationUnit *ImportedTU;
  ImportedTU = new (Context) TranslationUnit(ModuleID.first, Comp, Context,
                                             /*IsMainModule*/false,
                                             /*IsReplModule*/false);

  parseIntoTranslationUnit(ImportedTU, BufferID);
  Context.LoadedModules[ModuleID.first.str()] = ImportedTU;

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
  Module *M = getModule(Path[0]);
  if (M == 0) return;
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (Path.size() > 2) {
    diagnose(Path[2].second, diag::invalid_declaration_imported);
    return;
  }
  
  Result.push_back(std::make_pair(Path.slice(1), M));
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
bool NameBinder::resolveIdentifierType(IdentifierType *DNT, DeclContext *DC,
                                       bool NoMemberLookup) {
  MutableArrayRef<IdentifierType::Component> Components = DNT->Components;

  // If name lookup for the base of the type didn't get resolved in the
  // parsing phase, perform lookup on it.
  if (Components[0].Value.isNull()) {
    Identifier Name = Components[0].Id;
    SourceLoc Loc = Components[0].Loc;

    // Perform an unqualified lookup.
    UnqualifiedLookup Globals(Name, DC);

    if (Globals.Results.size() > 1) {
      diagnose(Loc, diag::abiguous_type_base, Name)
        << SourceRange(Loc, Components.back().Loc);
      for (auto Result : Globals.Results) {
        if (Globals.Results[0].hasValueDecl())
          diagnose(Result.getValueDecl()->getStartLoc(), diag::found_candidate);
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
    } else if (auto VD = LastOne.Value.dyn_cast<ValueDecl*>()) {
      // Lookup into a type.
      if (auto TD = dyn_cast<TypeDecl>(VD)) {
        if (NoMemberLookup) {
          diagnose(C.Loc, diag::cannot_resolve_extension_dot)
            << SourceRange(Components[0].Loc, Components.back().Loc);
          return true;
        }

        MemberLookup ML(TD->getDeclaredType(), C.Id, *TU);
        if (ML.Results.size() == 1 && ML.Results.back().hasDecl() &&
            isa<TypeDecl>(ML.Results.back().D))
          C.Value = cast<TypeDecl>(ML.Results.back().D);
      }
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
  if (ValueDecl *Last = Components.back().Value.dyn_cast<ValueDecl*>())
    if (auto TAD = dyn_cast<TypeDecl>(Last)) {
      Components[Components.size()-1].Value = TAD->getDeclaredType();
      return false;
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

  // Do a prepass over the declarations to find and load the imported modules.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i)
    if (ImportDecl *ID = dyn_cast<ImportDecl>(TU->Decls[i]))
      Binder.addImport(ID, ImportedModules);

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
  // Check for declarations with the same name which aren't overloaded
  // vars/funcs.
  llvm::DenseMap<Identifier, ValueDecl*> CheckTypes;
  for (unsigned i = 0, e = TU->Decls.size(); i != e; ++i) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(TU->Decls[i])) {
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
          Binder.diagnose(PrevD->getStartLoc(), diag::invalid_redecl_prev,
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
      IdentifierType *DNT = dyn_cast<IdentifierType>(ED->getExtendedType());
      while (true) {
        if (Binder.resolveIdentifierType(DNT, TU, /*NoMemberLookup*/true)) {
          for (auto &C : DNT->Components)
            C.Value = TU->Ctx.TheErrorType;

          break;
        } else {
          // We need to make sure the extended type is canonical. There are
          // two possibilities here:
          // 1. The type is already canonical, because it's either a NominalType
          // or comes from an imported module.
          // 2. The type is a NameAliasType from the current module, which
          // we need to resolve immediately because we can't leave a
          // non-canonical type here in the AST.
          Type FoundType = DNT->Components.back().Value.get<Type>();
          if (FoundType->hasCanonicalTypeComputed())
            break;

          TypeAliasDecl *TAD = cast<NameAliasType>(FoundType)->getDecl();
          DNT = dyn_cast<IdentifierType>(TAD->getUnderlyingType());

          if (!DNT) {
            Binder.diagnose(ED->getLoc(), diag::non_nominal_extension,
                            false, ED->getExtendedType());
            ED->setExtendedType(ErrorType::get(Binder.Context));
            break;
          }
        }
      }
    }
  }

  typedef TranslationUnit::IdentTypeAndContext IdentTypeAndContext;
  for (auto IdAndContext : TU->getUnresolvedIdentifierTypes()) {
    IdentifierType *DNT = IdAndContext.first;
    DeclContext *DC = IdAndContext.second;

    // Make sure we don't try to resolve a type twice.
    if (DNT->Components.back().Value.is<Type>())
      continue;

    if (Binder.resolveIdentifierType(DNT, DC)) {
      for (auto &C : DNT->Components)
        C.Value = TU->Ctx.TheErrorType;
    }
  }

  TU->ASTStage = TranslationUnit::NameBound;
  verify(TU);
}


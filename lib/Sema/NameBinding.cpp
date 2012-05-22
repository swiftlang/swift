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
#include "NameLookup.h"
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
    bool resolveIdentifierType(IdentifierType *DNT);
    
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
  Module *M = getModule(std::make_pair(SwiftID, TU->Decls[0]->getLocStart()));
  if (M == 0) return;
  Result.push_back(std::make_pair(Module::AccessPathTy(), M));
}

/// resolveIdentifierType - Perform name binding for a IdentifierType,
/// resolving it or diagnosing the error as appropriate and return true on
/// failure.  On failure, this leaves IdentifierType alone, otherwise it fills
/// in the Components.
bool NameBinder::resolveIdentifierType(IdentifierType *DNT) {
  const MutableArrayRef<IdentifierType::Component> &Components =DNT->Components;
  
  // If name lookup for the base of the type didn't get resolved in the
  // parsing phase, do a global lookup for it.
  if (Components[0].Value.isNull()) {
    Identifier Name = Components[0].Id;
    SourceLoc Loc = Components[0].Loc;
    
    // Perform an unqualified lookup.
    SmallVector<ValueDecl*, 4> Decls;
    TU->lookupGlobalValue(Name, NLKind::UnqualifiedLookup, Decls);
    
    // If we find multiple results, we have an ambiguity error.
    // FIXME: This should be reevaluated and probably turned into a new NLKind.
    // Certain matches (e.g. of a function) should just be filtered out/ignored.
    if (Decls.size() > 1) {
      diagnose(Loc, diag::abiguous_type_base, Name)
        << SourceRange(Loc, Components.back().Loc);
      for (ValueDecl *D : Decls)
        diagnose(D->getLocStart(), diag::found_candidate);
      return true;
    }
    
    if (!Decls.empty()) {
      Components[0].Value = Decls[0];
    } else {
      // If that fails, this may be the name of a module, try looking that up.
      for (const ImportedModule &ImpEntry : TU->getImportedModules())
        if (ImpEntry.second->Name == Name) {
          Components[0].Value = ImpEntry.second;
          break;
        }
    
      // If we still don't have anything, we fail.
      if (Components[0].Value.isNull()) {
        diagnose(Loc, Components.size() == 1 ? 
                   diag::use_undeclared_type : diag::unknown_name_in_type, Name)
          << SourceRange(Loc, Components.back().Loc);
        return true;
      }
    }
  }
  
  assert(!Components[0].Value.isNull() && "Failed to get a base");
  
  // Now that we have a base, iteratively resolve subsequent member entries.
  auto LastOne = Components[0];
  for (auto &C : Components.slice(1, Components.size()-1)) {
    // TODO: Only support digging into modules so far.
    if (auto M = LastOne.Value.dyn_cast<Module*>()) {
      SmallVector<ValueDecl*, 8> Decls;
      M->lookupValue(Module::AccessPathTy(), C.Id, 
                     NLKind::QualifiedLookup, Decls);
      if (Decls.size() == 1 && isa<TypeDecl>(Decls.back()))
        C.Value = cast<TypeDecl>(Decls.back());
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

/// BindNameToIVar - We have an unresolved reference to an identifier in some
/// FuncDecl.  Check to see if this is a reference to an instance variable,
/// and return an AST for the reference if so.  If not, return null with no
/// error emitted.
static Expr *BindNameToIVar(UnresolvedDeclRefExpr *UDRE, FuncDecl *CurFD, 
                            NameBinder &Binder) {
  Type ExtendedType = CurFD->getExtensionType();
  if (ExtendedType.isNull()) return 0;

  // For a static method, we perform name lookup in the corresponding metatype.
  TypeDecl *StaticAlias = 0;
  if (CurFD->isStatic()) {
    if (NominalType *Nominal = ExtendedType->getAs<NominalType>())
      StaticAlias = Nominal->getDecl();
    else
      return 0;
    
    ExtendedType = MetaTypeType::get(StaticAlias);
  }
  
  // Do a full "dot syntax" name lookup with the implicit 'this' base.
  MemberLookup Lookup(ExtendedType, UDRE->getName(), *Binder.TU);
  
  // On failure, this isn't an member reference.
  if (!Lookup.isSuccess()) return 0;
  
  // On success, this is a member reference. Build either a reference to the
  // implicit 'this' VarDecl (for instance methods) or to the metaclass
  // instance (for static methods).
  Expr *BaseExpr;
  if (StaticAlias) {
    BaseExpr = new (Binder.Context) DeclRefExpr(StaticAlias, SourceLoc(),
                                      StaticAlias->getTypeOfReference());
  } else {
    VarDecl *ThisDecl = CurFD->getImplicitThisDecl();
    assert(ThisDecl && "Couldn't find decl for 'this'");
    BaseExpr = new (Binder.Context) DeclRefExpr(ThisDecl, SourceLoc(),
                                                ThisDecl->getTypeOfReference());
  }
  
  return Lookup.createResultAST(BaseExpr, SourceLoc(), UDRE->getLoc(),
                                Binder.Context);
}

/// BindName - Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression.  If this reference is inside of a
/// FuncDecl (i.e. in a function body, not at global scope) then CurFD is the
/// innermost function, otherwise null.
static Expr *BindName(UnresolvedDeclRefExpr *UDRE, FuncDecl *CurFD,
                      NameBinder &Binder) {
  
  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  if (CurFD)
    if (Expr *E = BindNameToIVar(UDRE, CurFD, Binder))
      return E;

  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  Identifier Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();
  SmallVector<ValueDecl*, 4> Decls;
  // Perform standard value name lookup.
  Binder.TU->lookupGlobalValue(Name, NLKind::UnqualifiedLookup, Decls);

  // If that fails, this may be the name of a module, try looking that up.
  if (Decls.empty()) {
    for (const ImportedModule &ImpEntry : Binder.TU->getImportedModules())
      if (ImpEntry.second->Name == Name) {
        ModuleType *MT = ModuleType::get(ImpEntry.second);
        return new (Binder.Context) ModuleExpr(Loc, MT);
      }
  }

  if (Decls.empty()) {
    Binder.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return new (Binder.Context) ErrorExpr(Loc);
  }

  return OverloadedDeclRefExpr::createWithCopy(Decls, Loc);
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
  if (TU->Decls.empty())
    return;

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
          Binder.diagnose(VD->getLocStart(), diag::invalid_redecl);
          Binder.diagnose(PrevD->getLocStart(), diag::invalid_redecl_prev,
                          VD->getName());
        }
      }
    }
  }

  // Loop over all the unresolved dotted types in the translation unit,
  // resolving them if possible.
  for (auto IdAndContext : TU->getUnresolvedIdentifierTypes()) {
    if (Binder.resolveIdentifierType(IdAndContext.first)) {
      // This IdentifierType resolved to the error type.
      for (auto &C : IdAndContext.first->Components)
        C.Value = TU->Ctx.TheErrorType;
    }
  }

  struct NameBindingWalker : public ASTWalker {
    NameBinder &Binder;
    NameBindingWalker(NameBinder &binder) : Binder(binder) {}
    
    /// CurFuncDecls - This is the stack of FuncDecls that we're nested in.
    SmallVector<FuncDecl*, 4> CurFuncDecls;
    
    virtual bool walkToDeclPre(Decl *D) {
      if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
        CurFuncDecls.push_back(FD);
        
        // If this is an instance method with a body, set the type of it's
        // implicit 'this' variable.
        if (FD->getBody())
          if (Type ThisTy = FD->computeThisType()) {
            // References to the 'this' declaration will add back the lvalue
            // type as appropriate.
            if (LValueType *LValue = ThisTy->getAs<LValueType>())
              ThisTy = LValue->getObjectType();
            
            FD->getImplicitThisDecl()->setType(ThisTy);
          }
      }
      return true;
    }
    
    virtual bool walkToDeclPost(Decl *D) {
      if (isa<FuncDecl>(D)) {
        assert(CurFuncDecls.back() == D && "Decl misbalance");
        CurFuncDecls.pop_back();
      }
      return true;
    }
    
    Expr *walkToExprPost(Expr *E) {
      if (UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
        return BindName(UDRE, CurFuncDecls.empty() ? 0 : CurFuncDecls.back(),
                        Binder);
      }
      return E;
    }
  };

  NameBindingWalker walker(Binder);

  // Now that we know the top-level value names, go through and resolve any
  // UnresolvedDeclRefExprs that exist.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i)
    TU->Decls[i]->walk(walker);

  // Also resolve UnresolvedDeclRefExprs in TupleTypes.
  // FIXME: This code doesn't handle TupleTypes declared in the
  // lexical context of an extension correctly yet.
  for (auto TypeAndContext : TU->getTypesWithDefaultValues()) {
    TupleType *TT = TypeAndContext.first;
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      const TupleTypeElt& Elt = TT->getFields()[i];
      if (Elt.hasInit()) {
        Expr *Init = Elt.getInit();
        Init = Init->walk(walker);
        TT->updateInitializedElementType(i, Elt.getType(), Init);
      }
    }
  }
  TU->clearTypesWithDefaultValues();

  TU->ASTStage = TranslationUnit::NameBound;
  verify(TU);
}


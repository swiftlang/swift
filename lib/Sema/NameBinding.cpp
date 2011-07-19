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

// FIXME: Entrypoint declared in Parser.h
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/system_error.h"
#include "llvm/Support/Path.h"
using namespace swift;

/// NLKind - This is the kind of name lookup we're performing.
enum NLKind {
  NLK_UnqualifiedLookup,
  NLK_DotLookup
};

namespace {
  class ReferencedModule {
    // FIXME: A module can be more than one translation unit eventually.
    TranslationUnitDecl *TUD;
    
    llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*> > TopLevelValues;
    llvm::DenseMap<Identifier, TypeAliasDecl *> TopLevelTypes;

  public:
    ReferencedModule(TranslationUnitDecl *tud) : TUD(tud) {}
    ~ReferencedModule() {
      // Nothing to destroy here, TU is ASTContext allocated.
    }

    /// lookupType - Resolve a reference to a type name that found this module
    /// with the specified import declaration.
    TypeAliasDecl *lookupType(ImportDecl *ID, Identifier Name);

    /// lookupValue - Resolve a reference to a value name that found this module
    /// through the specified import declaration.
    void lookupValue(ImportDecl *ID, Identifier Name,
                     SmallVectorImpl<ValueDecl*> &Result,
                     NLKind LookupKind);
  };
} // end anonymous namespace.


/// lookupType - Resolve a reference to a type name that found this module
/// with the specified import declaration.
TypeAliasDecl *ReferencedModule::lookupType(ImportDecl *ID, Identifier Name) {
  assert(ID->AccessPath.size() <= 2 && "Don't handle this yet");
  
  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (ID->AccessPath.size() == 2 && ID->AccessPath[1].first != Name)
    return 0;
  
  if (TopLevelTypes.empty()) {
    for (unsigned i = 0, e = TUD->Body->NumElements; i != e; ++i)
      if (Decl *D = TUD->Body->Elements[i].dyn_cast<Decl*>())
        if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
          if (!TAD->Name.empty())
            TopLevelTypes[TAD->Name] = TAD;
  }
  
  llvm::DenseMap<Identifier, TypeAliasDecl*>::iterator I =
    TopLevelTypes.find(Name);
  return I != TopLevelTypes.end() ? I->second : 0;
}

/// lookupValue - Resolve a reference to a value name that found this module
/// through the specified import declaration.
void ReferencedModule::lookupValue(ImportDecl *ID, Identifier Name,
                                   SmallVectorImpl<ValueDecl*> &Result,
                                   NLKind LookupKind) {
  // TODO: ImportDecls cannot specified namespaces or individual entities
  // yet, so everything is just a lookup at the top-level.
  assert(ID->AccessPath.size() <= 2 && "Don't handle this yet");

  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (ID->AccessPath.size() == 2 && ID->AccessPath[1].first != Name)
    return;
    
  // If we haven't built a map of the top-level values, do so now.
  if (TopLevelValues.empty()) {
    for (unsigned i = 0, e = TUD->Body->NumElements; i != e; ++i)
      if (Decl *D = TUD->Body->Elements[i].dyn_cast<Decl*>())
        if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
          if (!VD->Name.empty())
            TopLevelValues[VD->Name].push_back(VD);
  }
   
  llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*> >::iterator I =
    TopLevelValues.find(Name);
  if (I == TopLevelValues.end()) return;
  
  Result.reserve(I->second.size());
  for (unsigned i = 0, e = I->second.size(); i != e; ++i) {
    // Dot Lookup ignores values with non-function types.
    if (LookupKind == NLK_DotLookup && !I->second[i]->Ty->is<FunctionType>())
      continue;
    
    Result.push_back(I->second[i]);
  }
}


namespace {  
  class NameBinder {
    std::vector<ReferencedModule *> LoadedModules;
    
    /// TopLevelValues - This is the list of top-level declarations we have.
    llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*> > TopLevelValues;
    SmallVector<std::pair<ImportDecl*, ReferencedModule*>, 4> Imports;
    
    llvm::error_code findModule(StringRef Module, 
                                SMLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer);
    
  public:
    ASTContext &Context;
    NameBinder(ASTContext &C) : Context(C) {}
    ~NameBinder() {
      for (unsigned i = 0, e = LoadedModules.size(); i != e; ++i)
        delete LoadedModules[i];      
    }
    
    void addNamedTopLevelDecl(ValueDecl *VD) {
      TopLevelValues[VD->Name].push_back(VD);
    }
    
    void addImport(ImportDecl *ID);

    void bindValueName(Identifier I, SmallVectorImpl<ValueDecl*> &Result,
                       NLKind LookupKind);
    
    TypeAliasDecl *lookupTypeName(Identifier I);
    
    void note(SMLoc Loc, const Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "note");
    }
    void warning(SMLoc Loc, const Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "warning");
    }
    void error(SMLoc Loc, const Twine &Message) {
      Context.setHadError();
      Context.SourceMgr.PrintMessage(Loc, Message, "error");
    }
  private:
    /// getReferencedModule - Load a module referenced by an import statement,
    /// emitting an error at the specified location and returning null on
    /// failure.
    ReferencedModule *getReferencedModule(std::pair<Identifier,SMLoc> ModuleID);
  };
}

llvm::error_code NameBinder::findModule(StringRef Module, 
                                        SMLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer) {
  std::string ModuleFilename = Module.str() + std::string(".swift");
  
  // First, search in the directory corresponding to the import location.
  // FIXME: This screams for a proper FileManager abstraction.
  llvm::SourceMgr &SourceMgr = Context.SourceMgr;
  int CurrentBufferID = SourceMgr.FindBufferContainingLoc(ImportLoc);
  if (CurrentBufferID >= 0) {
    const llvm::MemoryBuffer *ImportingBuffer 
      = SourceMgr.getBufferInfo(CurrentBufferID).Buffer;
    StringRef CurrentDirectory 
      = llvm::sys::path::parent_path(ImportingBuffer->getBufferIdentifier());
    if (!CurrentDirectory.empty()) {
      llvm::SmallString<128> InputFilename(CurrentDirectory);
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

  // FIXME: Search in the include directories.
  return Err;
}

ReferencedModule *NameBinder::
getReferencedModule(std::pair<Identifier, SMLoc> ModuleID) {
  // TODO: We currently just recursively parse referenced modules.  This works
  // fine for now since they are each a single file.  Ultimately we'll want a
  // compiled form of AST's like clang's that support lazy deserialization.
  
  // Open the input file.
  llvm::OwningPtr<llvm::MemoryBuffer> InputFile;
  if (llvm::error_code Err = findModule(ModuleID.first.str(), ModuleID.second,
                                        InputFile)) {
    error(ModuleID.second,
          "opening import file '" + ModuleID.first.str() + ".swift': " 
          + Err.message());
    return 0;
  }

  unsigned BufferID =
    Context.SourceMgr.AddNewSourceBuffer(InputFile.take(), ModuleID.second);

  // Parse the translation unit, but don't do name binding or type checking.
  // This can produce new errors etc if the input is erroneous.
  TranslationUnitDecl *TUD = Parser(BufferID, Context).parseTranslationUnit();
  if (TUD == 0)
    return 0;
  
  // We have to do name binding on it to ensure that types are fully resolved.
  // This should eventually be eliminated by having actual fully resolved binary
  // dumps of the code instead of reparsing though.
  //performNameBinding(TUD, Context);
  
  ReferencedModule *RM = new ReferencedModule(TUD);
  LoadedModules.push_back(RM);
  return RM;
}


void NameBinder::addImport(ImportDecl *ID) {
  ReferencedModule *RM = getReferencedModule(ID->AccessPath[0]);
  if (RM == 0) return;
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (ID->AccessPath.size() > 2)
    return error(ID->AccessPath[2].second,
                 "invalid declaration referenced in import");
  
  Imports.push_back(std::make_pair(ID, RM));
}

/// lookupTypeName - Lookup the specified type name in imports.  We know that it
/// has already been resolved within the current translation unit.  This returns
/// null if there is no match found.
TypeAliasDecl *NameBinder::lookupTypeName(Identifier Name) {
  for (unsigned i = 0, e = Imports.size(); i != e; ++i)
    if (TypeAliasDecl *D = Imports[i].second->lookupType(Imports[i].first,Name))
      return D;

  return 0;
}



/// bindValueName - This is invoked for each name reference in the AST, and
/// returns a decl if found or null if not.  If OnlyReturnFunctions is true,
/// then this will only return a decl if it has function type.
void NameBinder::bindValueName(Identifier Name, 
                               SmallVectorImpl<ValueDecl*> &Result,
                               NLKind LookupKind) {
  // Resolve forward references defined within the module.
  llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*> >::iterator I =
    TopLevelValues.find(Name);
  // If we found a match, return the decls.
  if (I != TopLevelValues.end()) {
    Result.reserve(I->second.size());
    for (unsigned i = 0, e = I->second.size(); i != e; ++i) {
      ValueDecl *VD = I->second[i];
      
      // Dot Lookup ignores values with non-function types.
      if (LookupKind == NLK_DotLookup && !VD->Ty->is<FunctionType>())
        continue;

      Result.push_back(VD);
    }
    if (!Result.empty())
      return;
  }

  // If we still haven't found it, scrape through all of the imports, taking the
  // first match of the name.
  for (unsigned i = 0, e = Imports.size(); i != e; ++i) {
    Imports[i].second->lookupValue(Imports[i].first, Name, Result, LookupKind);
    if (!Result.empty()) return;  // If we found a match, return the decls.
  }
}

static Expr *BindNames(Expr *E, Expr::WalkOrder Order, void *binder) {
  NameBinder &Binder = *static_cast<NameBinder*>(binder);
  
  // Ignore the preorder walk.
  if (Order == Expr::Walk_PreOrder)
    return E;
  
  // Process UnresolvedDeclRefExpr.
  if (UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
    SmallVector<ValueDecl*, 4> Decls;
    Binder.bindValueName(UDRE->Name, Decls, NLK_UnqualifiedLookup);
    if (Decls.empty()) {
      Binder.error(UDRE->Loc, "use of unresolved identifier '" +
                   UDRE->Name.str() + "'");
      return 0;
    }
    if (Decls.size() == 1)
      return new (Binder.Context) DeclRefExpr(Decls[0], UDRE->Loc);
    
    // Copy the overload set into ASTContext memory.
    ArrayRef<ValueDecl*> DeclList = Binder.Context.AllocateCopy(Decls);
    
    return new (Binder.Context) OverloadSetRefExpr(DeclList, UDRE->Loc);
  }
  
  // A reference to foo.bar may be an application ("bar foo"), so look up bar.
  // It may also be a tuple field reference, so don't report an error here if we
  // don't find anything juicy.
  if (UnresolvedDotExpr *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
    SmallVector<ValueDecl*, 4> Decls;
    // Perform .-style name lookup.
    Binder.bindValueName(UDE->Name, Decls, NLK_DotLookup);

    // Copy the overload set into ASTContext memory.
    if (!Decls.empty())
      UDE->ResolvedDecls = Binder.Context.AllocateCopy(Decls);
    return UDE;
  }
  
  // Otherwise, not something that needs name binding.
  return E;
}

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this parsing has been performed, but we still have UnresolvedDeclRefExpr
/// nodes for unresolved value names, and we may have unresolved type names as
/// well.  This handles import directives and forward references.
void swift::performNameBinding(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  NameBinder Binder(Ctx);
  
  // Do a prepass over the declarations to find the list of top-level value
  // declarations.
  for (unsigned i = 0, e = TUD->Body->NumElements; i != e; ++i)
    if (Decl *D = TUD->Body->Elements[i].dyn_cast<Decl*>()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        if (!VD->Name.empty())
          Binder.addNamedTopLevelDecl(VD);
    
      if (ImportDecl *ID = dyn_cast<ImportDecl>(D))
        Binder.addImport(ID);
    }
  
  // Type binding.  Loop over all of the unresolved types in the translation
  // unit, resolving them with imports.
  for (unsigned i = 0, e = TUD->UnresolvedTypesForParser.size(); i != e; ++i) {
    TypeAliasDecl *TA = TUD->UnresolvedTypesForParser[i];

    if (TypeAliasDecl *Result = Binder.lookupTypeName(TA->Name)) {
      assert(isa<UnresolvedType>(TA->UnderlyingTy.getPointer()) &&
             "Not an unresolved type");
      // Update the decl we already have to be the correct type.
      TA->TypeAliasLoc = Result->TypeAliasLoc;
      TA->UnderlyingTy = Result->UnderlyingTy;
      continue;
    }
    
    Binder.error(TA->getLocStart(),
                 "use of undeclared type '" + TA->Name.str() + "'");
  }

  // Now that we know the top-level value names, go through and resolve any
  // UnresolvedDeclRefExprs that exist.
  for (unsigned i = 0, e = TUD->Body->NumElements; i != e; ++i)
    if (Decl *D = TUD->Body->Elements[i].dyn_cast<Decl*>()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        if (VD->Init)
          VD->Init = VD->Init->WalkExpr(BindNames, &Binder);
    } else {
      TUD->Body->Elements[i] =
        TUD->Body->Elements[i].get<Expr*>()->WalkExpr(BindNames, &Binder);
     
      // Fill in null results with a dummy expression.
      if (TUD->Body->Elements[i] == 0)
        TUD->Body->Elements[i] =
          new (Ctx) TupleExpr(SMLoc(), 0, 0, 0, SMLoc(), false, false,
                              Ctx.TheEmptyTupleType);
    }
}

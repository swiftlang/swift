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
#include "swift/AST/AST.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Verifier.h"
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
    
    NameBinder(TranslationUnit *TU);
    ~NameBinder() {
    }
    
    template<typename ...ArgTypes>
    void diagnose(ArgTypes... Args) {
      Context.Diags.diagnose(Args...);
    }
    
    void addImport(ImportDecl *ID, SmallVectorImpl<ImportedModule> &Result);

    BoundScope bindScopeName(TypeAliasDecl *TypeFromScope,
                             Identifier Name, SourceLoc NameLoc);

    void lookupGlobalValue(Identifier I, NLKind LookupKind,
                           SmallVectorImpl<ValueDecl*> &Result);
    
    TypeAliasDecl *lookupTypeName(Identifier I);
    
  private:
    /// getModule - Load a module referenced by an import statement,
    /// emitting an error at the specified location and returning null on
    /// failure.
    Module *getModule(std::pair<Identifier,SourceLoc> ModuleID);
  };
}

NameBinder::NameBinder(TranslationUnit *TU) : TU(TU), Context(TU->Ctx) {
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

  // Parse the translation unit, but don't do name binding or type checking.
  // This can produce new errors etc if the input is erroneous.
  TranslationUnit *TU = parseTranslationUnit(BufferID, Context);
  if (TU == 0)
    return 0;
  
  // We have to do name binding on it to ensure that types are fully resolved.
  // This should eventually be eliminated by having actual fully resolved binary
  // dumps of the code instead of reparsing though.
  performNameBinding(TU, Context);
  
  return TU;
}


void NameBinder::addImport(ImportDecl *ID, 
                           SmallVectorImpl<ImportedModule> &Result) {
  Module *M = getModule(ID->AccessPath[0]);
  if (M == 0) return;
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (ID->AccessPath.size() > 2) {
    diagnose(ID->AccessPath[2].second, diag::invalid_declaration_imported);
    return;
  }
  
  Result.push_back(std::make_pair(ID->AccessPath.slice(1), M));
}

/// lookupTypeName - Lookup the specified type name in imports.  We know that it
/// has already been resolved within the current translation unit.  This returns
/// null if there is no match found.
TypeAliasDecl *NameBinder::lookupTypeName(Identifier Name) {
  for (auto &ImpEntry : TU->ImportedModules)
    if (TypeAliasDecl *D = ImpEntry.second->lookupType(
                              ImpEntry.first, Name, NLKind::UnqualifiedLookup))
      return D;

  return 0;
}



/// lookupGlobalValue - Perform a name lookup from within the context of the
/// current module.
void NameBinder::lookupGlobalValue(Identifier Name, NLKind LookupKind,
                                   SmallVectorImpl<ValueDecl*> &Result) {
  // Look in the current module.
  TU->lookupValue(Module::AccessPathTy(), Name, LookupKind, Result);
  if (!Result.empty())
    return;

  // If we still haven't found it, scrape through all of the imports, taking the
  // first match of the name.
  for (auto &ImpEntry : TU->ImportedModules) {
    ImpEntry.second->lookupValue(ImpEntry.first, Name, LookupKind, Result);
    if (!Result.empty()) return;  // If we found a match, return the decls.
  }
}

/// Try to bind an unqualified name into something usable as a scope.
BoundScope NameBinder::bindScopeName(TypeAliasDecl *TypeFromScope,
                                     Identifier Name, SourceLoc NameLoc) {
  // Check whether the "optimistic" type from scope is still
  // undefined.  If not, use that as the actual type; otherwise we'll
  // need to do a lookup from the imports.
  TypeAliasDecl *Type;
  if (!TypeFromScope->UnderlyingTy.isNull()) {
    Type = TypeFromScope;
  } else {
    Type = lookupTypeName(Name);
  }

  // If that failed, look for a module name.
  if (!Type) {
    for (const ImportedModule &ImpEntry : TU->ImportedModules)
      if (ImpEntry.second->Name == Name)
        return &ImpEntry;
    
    diagnose(NameLoc, diag::no_module_or_type);
    return BoundScope();
  }

  // Otherwise, at least cache the type we found.
  assert(!Type->UnderlyingTy.isNull());
  if (TypeFromScope->UnderlyingTy.isNull()) {
    TypeFromScope->UnderlyingTy = Type->UnderlyingTy;
  }

  // Try to convert that to a type scope.
  TypeBase *Ty = Type->UnderlyingTy->getCanonicalType();

  // Silently fail if we have an error type.
  if (isa<ErrorType>(Ty)) return BoundScope();
    
  // Reject things like int::x.
  OneOfType *DT = dyn_cast<OneOfType>(Ty);
  if (DT == 0) {
    diagnose(NameLoc, diag::invalid_type_scoped_access, Name);
    return BoundScope();
  }
    
  if (DT->Elements.empty()) {
    diagnose(NameLoc, diag::incomplete_or_empty_oneof, Name);
    return BoundScope();
  }

  return DT;
}

//===----------------------------------------------------------------------===//
// performNameBinding
//===----------------------------------------------------------------------===//

static Expr *BindNames(Expr *E, WalkOrder Order, NameBinder &Binder) {
  
  // Ignore the preorder walk.
  if (Order == WalkOrder::PreOrder)
    return E;

  // A reference to foo.bar may be an application ("bar foo"), so look up bar.
  // It may also be a tuple field reference, so don't report an error here if we
  // don't find anything juicy.
  if (UnresolvedDotExpr *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
    SmallVector<ValueDecl*, 4> Decls;
    // Perform .-style name lookup.
    Binder.lookupGlobalValue(UDE->getName(), NLKind::DotLookup, Decls);

    // Copy the overload set into ASTContext memory.
    if (!Decls.empty())
      UDE->setResolvedDecls(Binder.Context.AllocateCopy(Decls));
    return UDE;
  }
  
  Identifier Name;
  SourceLoc Loc;
  SmallVector<ValueDecl*, 4> Decls;
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  if (UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
    Name = UDRE->getName();
    Loc = UDRE->getLoc();
    Binder.lookupGlobalValue(Name, NLKind::UnqualifiedLookup, Decls);

  // Process UnresolvedScopedIdentifierExpr by doing a qualified lookup.
  } else if (UnresolvedScopedIdentifierExpr *USIE =
               dyn_cast<UnresolvedScopedIdentifierExpr>(E)) {
    Name = USIE->getName();
    Loc = USIE->getNameLoc();

    Identifier BaseName = USIE->getBaseName();
    SourceLoc BaseNameLoc = USIE->getBaseNameLoc();
    BoundScope Scope =
      Binder.bindScopeName(USIE->getBaseTypeFromScope(), BaseName, BaseNameLoc);
    if (!Scope) return nullptr;

    if (OneOfType *Ty = Scope.dyn_cast<OneOfType*>()) {
      OneOfElementDecl *Elt = Ty->getElement(Name);
      if (Elt == 0) {
        Binder.diagnose(Loc, diag::invalid_member, Name, BaseName);
        return 0;
      }
      Decls.push_back(Elt);
    } else {
      auto Module = Scope.get<const ImportedModule*>();
      Module->second->lookupValue(Module->first, Name,
                                  NLKind::QualifiedLookup, Decls);
    }

  // Otherwise, not something that needs name binding.
  } else {
    return E;
  }

  if (Decls.empty()) {
    Binder.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return 0;
  }

  if (Decls.size() == 1)
    return new (Binder.Context) DeclRefExpr(Decls[0], Loc);
    
  // Copy the overload set into ASTContext memory.
  ArrayRef<ValueDecl*> DeclList = Binder.Context.AllocateCopy(Decls);
    
  return new (Binder.Context) OverloadSetRefExpr(DeclList, Loc);
}

/// performNameBinding - Once parsing is complete, this walks the AST to
/// resolve names and do other top-level validation.
///
/// At this parsing has been performed, but we still have UnresolvedDeclRefExpr
/// nodes for unresolved value names, and we may have unresolved type names as
/// well.  This handles import directives and forward references.
void swift::performNameBinding(TranslationUnit *TU, ASTContext &Ctx) {
  NameBinder Binder(TU);

  SmallVector<ImportedModule, 8> ImportedModules;
  
  // Import the builtin library as an implicit import.
  // FIXME: This should only happen for translation units in the standard
  // library.
  ImportedModules.push_back(std::make_pair(Module::AccessPathTy(),
                                           Ctx.TheBuiltinModule));
  
  // FIXME: For translation units not in the standard library, we should import
  // swift.swift implicitly.  We need a way for swift.swift itself to not
  // recursively import itself though.

  // Do a prepass over the declarations to find and load the imported modules.
  for (auto Elt : TU->Body->getElements())
    if (Decl *D = Elt.dyn_cast<Decl*>()) {
      if (ImportDecl *ID = dyn_cast<ImportDecl>(D))
        Binder.addImport(ID, ImportedModules);
    }
  
  TU->ImportedModules = Ctx.AllocateCopy(ImportedModules);
  
  // Type binding.  Loop over all of the unresolved types in the translation
  // unit, resolving them with imports.
  for (TypeAliasDecl *TA : TU->UnresolvedTypesForParser) {
    if (TypeAliasDecl *Result = Binder.lookupTypeName(TA->Name)) {
      assert(TA->UnderlyingTy.isNull() && "Not an unresolved type");
      // Update the decl we already have to be the correct type.
      TA->TypeAliasLoc = Result->TypeAliasLoc;
      TA->UnderlyingTy = Result->UnderlyingTy;
      continue;
    }
    
    Binder.diagnose(TA->getLocStart(), diag::use_undeclared_type, TA->Name);
    
    TA->UnderlyingTy = ErrorType::get(Ctx);
  }

  // Loop over all the unresolved scoped types in the translation
  // unit, resolving them if possible.
  for (auto BaseAndType : TU->UnresolvedScopedTypesForParser) {
    BoundScope Scope = Binder.bindScopeName(BaseAndType.first,
                                            BaseAndType.first->Name,
                                            BaseAndType.first->TypeAliasLoc);
    if (!Scope) continue;

    Identifier Name = BaseAndType.second->Name;
    SourceLoc NameLoc = BaseAndType.second->TypeAliasLoc;

    TypeAliasDecl *Alias = nullptr;

    if (auto Module = Scope.dyn_cast<const ImportedModule*>()) {
      Alias = Module->second->lookupType(Module->first, Name,
                                         NLKind::QualifiedLookup);
    }
    if (Alias) {
      BaseAndType.second->UnderlyingTy = Alias->getAliasType(Binder.Context);
    } else {
      Binder.diagnose(NameLoc, diag::invalid_member_type,
                      Name, BaseAndType.first->Name);
      BaseAndType.second->UnderlyingTy = Binder.Context.TheErrorType;
    }
  }

  NameBinder *NBPtr = &Binder;
  auto BinderBlock = ^(Expr *E, WalkOrder Order) {
    return BindNames(E, Order, *NBPtr);
  };
  
  // Now that we know the top-level value names, go through and resolve any
  // UnresolvedDeclRefExprs that exist.
  for (unsigned i = 0, e = TU->Body->getNumElements(); i != e; ++i) {
    BraceStmt::ExprStmtOrDecl Elt = TU->Body->getElement(i);
    if (Decl *D = Elt.dyn_cast<Decl*>()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        if (VD->Init)
          VD->Init = VD->Init->walk(BinderBlock);
    } else if (Stmt *S = Elt.dyn_cast<Stmt*>()) {
      Elt = S->walk(BinderBlock);
    } else {
      Elt = Elt.get<Expr*>()->walk(BinderBlock);
    }
    
    // Fill in null results with a dummy expression.
    if (Elt.isNull())
      Elt = new (Ctx) TupleExpr(SourceLoc(), 0, 0, 0, SourceLoc(), false,
                                TypeJudgement(TupleType::getEmpty(Ctx),
                                              ValueKind::RValue));
    TU->Body->setElement(i, Elt);
  }

  verify(TU, VerificationKind::BoundNames);
}


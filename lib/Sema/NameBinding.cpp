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
#include "swift/AST/Builtins.h"
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
enum class NLKind {
  UnqualifiedLookup,
  QualifiedLookup,
  DotLookup
};

namespace {
  /// An abstract class for something that provides module data.
  class ModuleProvider {
  public:
    virtual ~ModuleProvider() {}
    virtual TypeAliasDecl *lookupType(ImportDecl *ID, Identifier Name,
                                      NLKind LookupKind) = 0;
    virtual void lookupValue(ImportDecl *ID, Identifier Name,
                             SmallVectorImpl<ValueDecl*> &Result,
                             NLKind LookupKind) = 0;
  };

  /// A concrete implementation of ModuleProvider which holds a
  /// fully-loaded module.
  class InMemoryModule : public ModuleProvider {
    // FIXME: A module can be more than one translation unit eventually.
    TranslationUnit *TU;
    
    llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*>> TopLevelValues;
    llvm::DenseMap<Identifier, TypeAliasDecl *> TopLevelTypes;

  public:
    InMemoryModule(TranslationUnit *tu) : TU(tu) {}
    ~InMemoryModule() {
      // Nothing to destroy here, TU is ASTContext allocated.
    }

    /// lookupType - Resolve a reference to a type name that found this module
    /// with the specified import declaration.
    TypeAliasDecl *lookupType(ImportDecl *ID, Identifier Name,
                              NLKind LookupKind) override;

    /// lookupValue - Resolve a reference to a value name that found this module
    /// through the specified import declaration.
    void lookupValue(ImportDecl *ID, Identifier Name,
                     SmallVectorImpl<ValueDecl*> &Result,
                     NLKind LookupKind) override;
  };

  /// An implementation of ModuleProvider for builtin types and functions.
  class BuiltinModule : public ModuleProvider {
    ASTContext &Context;

    /// The cache of identifiers we've already looked up.  We use a
    /// single hashtable for both types and values as a minor
    /// optimization; this prevents us from having both a builtin type
    /// and a builtin value with the same name, but that's okay.
    llvm::DenseMap<Identifier, NamedDecl*> Cache;
  public:
    BuiltinModule(ASTContext &Context) : Context(Context) {}
    TypeAliasDecl *lookupType(ImportDecl *ID, Identifier Name,
                              NLKind LookupKind) override;
    void lookupValue(ImportDecl *ID, Identifier Name,
                     SmallVectorImpl<ValueDecl*> &Result,
                     NLKind LookupKind) override;
  };
} // end anonymous namespace.


/// lookupType - Resolve a reference to a type name that found this module
/// with the specified import declaration.
TypeAliasDecl *InMemoryModule::lookupType(ImportDecl *ID, Identifier Name,
                                          NLKind LookupKind) {
  assert(ID->AccessPath.size() <= 2 && "Don't handle this yet");
  
  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (ID->AccessPath.size() == 2 && ID->AccessPath[1].first != Name)
    return 0;
  
  if (TopLevelTypes.empty()) {
    for (unsigned i = 0, e = TU->Body->NumElements; i != e; ++i)
      if (Decl *D = TU->Body->Elements[i].dyn_cast<Decl*>())
        if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
          if (!TAD->Name.empty())
            TopLevelTypes[TAD->Name] = TAD;
  }
  
  auto I = TopLevelTypes.find(Name);
  return I != TopLevelTypes.end() ? I->second : 0;
}

/// lookupValue - Resolve a reference to a value name that found this module
/// through the specified import declaration.
void InMemoryModule::lookupValue(ImportDecl *ID, Identifier Name,
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
    for (unsigned i = 0, e = TU->Body->NumElements; i != e; ++i)
      if (Decl *D = TU->Body->Elements[i].dyn_cast<Decl*>())
        if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
          if (!VD->Name.empty())
            TopLevelValues[VD->Name].push_back(VD);
  }
   
  auto I = TopLevelValues.find(Name);
  if (I == TopLevelValues.end()) return;
  
  Result.reserve(I->second.size());
  for (ValueDecl *Elt : I->second) {
    // Dot Lookup ignores values with non-function types.
    if (LookupKind == NLKind::DotLookup && !Elt->Ty->is<FunctionType>())
      continue;
    
    Result.push_back(Elt);
  }
}


typedef std::pair<ImportDecl*, ModuleProvider*> Import;
typedef llvm::PointerUnion<Import*, OneOfType*> BoundScope;

namespace {  
  class NameBinder {
    std::vector<ModuleProvider *> LoadedModules;
    
    /// TopLevelValues - This is the list of top-level declarations we have.
    llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*>> TopLevelValues;
    SmallVector<Import, 4> Imports;
    
    llvm::error_code findModule(StringRef Module, 
                                SMLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer);
    
  public:
    ASTContext &Context;
    NameBinder(ASTContext &C) : Context(C) {}
    ~NameBinder() {
      for (ModuleProvider *M : LoadedModules)
        delete M;
    }
    
    void addNamedTopLevelDecl(ValueDecl *VD) {
      TopLevelValues[VD->Name].push_back(VD);
    }
    
    void addImport(ImportDecl *ID);
    void addBuiltinImport(ImportDecl *ID);

    BoundScope bindScopeName(TypeAliasDecl *TypeFromScope,
                             Identifier Name, SMLoc NameLoc);

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
    /// getModuleProvider - Load a module referenced by an import statement,
    /// emitting an error at the specified location and returning null on
    /// failure.
    ModuleProvider *getModuleProvider(std::pair<Identifier,SMLoc> ModuleID);
  };
}

llvm::error_code NameBinder::findModule(StringRef Module, 
                                        SMLoc ImportLoc,
                                llvm::OwningPtr<llvm::MemoryBuffer> &Buffer) {
  std::string ModuleFilename = Module.str() + std::string(".swift");
  
  llvm::SmallString<128> InputFilename;
  
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

ModuleProvider *NameBinder::
getModuleProvider(std::pair<Identifier, SMLoc> ModuleID) {
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
  TranslationUnit *TU = parseTranslationUnit(BufferID, Context);
  if (TU == 0)
    return 0;
  
  // We have to do name binding on it to ensure that types are fully resolved.
  // This should eventually be eliminated by having actual fully resolved binary
  // dumps of the code instead of reparsing though.
  performNameBinding(TU, Context);
  
  ModuleProvider *MP = new InMemoryModule(TU);
  LoadedModules.push_back(MP);
  return MP;
}


void NameBinder::addImport(ImportDecl *ID) {
  ModuleProvider *MP = getModuleProvider(ID->AccessPath[0]);
  if (MP == 0) return;
  
  // FIXME: Validate the access path against the module.  Reject things like
  // import swift.aslkdfja
  if (ID->AccessPath.size() > 2)
    return error(ID->AccessPath[2].second,
                 "invalid declaration referenced in import");
  
  Imports.push_back(std::make_pair(ID, MP));
}

void NameBinder::addBuiltinImport(ImportDecl *ID) {
  ModuleProvider *MP = new BuiltinModule(Context);
  Imports.push_back(std::make_pair(ID, MP));
}

/// lookupTypeName - Lookup the specified type name in imports.  We know that it
/// has already been resolved within the current translation unit.  This returns
/// null if there is no match found.
TypeAliasDecl *NameBinder::lookupTypeName(Identifier Name) {
  for (auto &ImpEntry : Imports)
    if (TypeAliasDecl *D = ImpEntry.second->lookupType(ImpEntry.first, Name,
                                                  NLKind::UnqualifiedLookup))
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
  auto I = TopLevelValues.find(Name);
  // If we found a match, return the decls.
  if (I != TopLevelValues.end()) {
    Result.reserve(I->second.size());
    for (ValueDecl *VD : I->second) {
      // Dot Lookup ignores values with non-function types.
      if (LookupKind == NLKind::DotLookup && !VD->Ty->is<FunctionType>())
        continue;

      Result.push_back(VD);
    }
    if (!Result.empty())
      return;
  }

  // If we still haven't found it, scrape through all of the imports, taking the
  // first match of the name.
  for (auto &ImpEntry : Imports) {
    ImpEntry.second->lookupValue(ImpEntry.first, Name, Result, LookupKind);
    if (!Result.empty()) return;  // If we found a match, return the decls.
  }
}

/// Try to bind an unqualified name into something usable as a scope.
BoundScope NameBinder::bindScopeName(TypeAliasDecl *TypeFromScope,
                                     Identifier Name, SMLoc NameLoc) {
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
    for (Import &ImpEntry : Imports)
      if (ImpEntry.first->AccessPath.back().first == Name)
        return &ImpEntry;
    
    error(NameLoc, "no such module or type");
    return BoundScope();
  }

  // Otherwise, at least cache the type we found.
  assert(!Type->UnderlyingTy.isNull());
  if (TypeFromScope->UnderlyingTy.isNull()) {
    TypeFromScope->UnderlyingTy = Type->UnderlyingTy;
  }

  // Try to convert that to a type scope.
  TypeBase *Ty = Type->UnderlyingTy->getCanonicalType(Context);

  // Silently fail if we have an error type.
  if (isa<ErrorType>(Ty)) return BoundScope();
    
  // Reject things like int::x.
  OneOfType *DT = dyn_cast<OneOfType>(Ty);
  if (DT == 0) {
    error(NameLoc, "invalid type '" + Name.str() + "' for scoped access");
    return BoundScope();
  }
    
  if (DT->Elements.empty()) {
    error(NameLoc, "oneof '" + Name.str() +
          "' is not complete or has no elements");
    return BoundScope();
  }

  return DT;
}

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
    Binder.bindValueName(UDE->Name, Decls, NLKind::DotLookup);

    // Copy the overload set into ASTContext memory.
    if (!Decls.empty())
      UDE->ResolvedDecls = Binder.Context.AllocateCopy(Decls);
    return UDE;
  }
  
  Identifier Name;
  SMLoc Loc;
  SmallVector<ValueDecl*, 4> Decls;
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  if (UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
    Name = UDRE->Name;
    Loc = UDRE->Loc;
    Binder.bindValueName(Name, Decls, NLKind::UnqualifiedLookup);

  // Process UnresolvedScopedIdentifierExpr by doing a qualified lookup.
  } else if (UnresolvedScopedIdentifierExpr *USIE =
               dyn_cast<UnresolvedScopedIdentifierExpr>(E)) {
    Name = USIE->Name;
    Loc = USIE->NameLoc;

    Identifier BaseName = USIE->BaseName;
    SMLoc BaseNameLoc = USIE->BaseNameLoc;
    BoundScope Scope =
      Binder.bindScopeName(USIE->BaseTypeFromScope, BaseName, BaseNameLoc);
    if (!Scope) return nullptr;

    if (OneOfType *Ty = Scope.dyn_cast<OneOfType*>()) {
      OneOfElementDecl *Elt = Ty->getElement(Name);
      if (Elt == 0) {
        Binder.error(Loc, "'" + Name.str() + "' is not a member of '" +
                     BaseName.str() + "'");
        return 0;
      }
      Decls.push_back(Elt);
    } else {
      Import *Module = Scope.get<Import*>();
      Module->second->lookupValue(Module->first, Name, Decls,
                                  NLKind::QualifiedLookup);
    }

  // Otherwise, not something that needs name binding.
  } else {
    return E;
  }

  if (Decls.empty()) {
    Binder.error(Loc, "use of unresolved identifier '" + Name.str() + "'");
    return 0;
  }

  if (Decls.size() == 1) {
    return new (Binder.Context) DeclRefExpr(Decls[0], Loc);
  }
    
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
  NameBinder Binder(Ctx);

  std::pair<Identifier,SMLoc> BuiltinPath[] {
    std::make_pair(Ctx.getIdentifier("Builtin"), SMLoc())
  };
  ImportDecl BuiltinImport(SMLoc(), BuiltinPath, nullptr);
  Binder.addBuiltinImport(&BuiltinImport);
  
  // Do a prepass over the declarations to find the list of top-level value
  // declarations.
  for (unsigned i = 0, e = TU->Body->NumElements; i != e; ++i)
    if (Decl *D = TU->Body->Elements[i].dyn_cast<Decl*>()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        if (!VD->Name.empty())
          Binder.addNamedTopLevelDecl(VD);
    
      if (ImportDecl *ID = dyn_cast<ImportDecl>(D))
        Binder.addImport(ID);
    }
  
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
    
    Binder.error(TA->getLocStart(),
                 "use of undeclared type '" + TA->Name.str() + "'");
    
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
    SMLoc NameLoc = BaseAndType.second->TypeAliasLoc;

    TypeAliasDecl *Alias = nullptr;

    if (Import *Module = Scope.dyn_cast<Import*>()) {
      Alias = Module->second->lookupType(Module->first, Name,
                                         NLKind::QualifiedLookup);
    }
    if (Alias) {
      BaseAndType.second->UnderlyingTy = Alias->getAliasType(Binder.Context);
    } else {
      Binder.error(NameLoc, "'" + Name.str() + "' is not a member type of '" +
                   BaseAndType.first->Name.str() + "'");
      BaseAndType.second->UnderlyingTy = Binder.Context.TheErrorType;
    }
  }

  NameBinder *NBPtr = &Binder;
  auto BinderBlock = ^(Expr *E, WalkOrder Order) {
    return BindNames(E, Order, *NBPtr);
  };
  
  // Now that we know the top-level value names, go through and resolve any
  // UnresolvedDeclRefExprs that exist.
  for (unsigned i = 0, e = TU->Body->NumElements; i != e; ++i) {
    BraceStmt::ExprStmtOrDecl &Elt = TU->Body->Elements[i];
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
      Elt = new (Ctx) TupleExpr(SMLoc(), 0, 0, 0, SMLoc(), false,
                                TupleType::getEmpty(Ctx));
  }
}

TypeAliasDecl *
BuiltinModule::lookupType(ImportDecl *ID, Identifier Name, NLKind LookupKind) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return nullptr;

  auto I = Cache.find(Name);
  if (I != Cache.end())
    return dyn_cast<TypeAliasDecl>(I->second);

  Type Ty = getBuiltinType(Context, Name);
  if (!Ty) return nullptr;

  TypeAliasDecl *Alias
    = new (Context) TypeAliasDecl(SMLoc(), Name, Ty,
                                  DeclAttributes(),
                                  nullptr);
  Cache.insert(std::make_pair(Name, Alias));
  return Alias;
}

void BuiltinModule::lookupValue(ImportDecl *ID, Identifier Name,
                                SmallVectorImpl<ValueDecl*> &Result,
                                NLKind LookupKind) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return;

  auto I = Cache.find(Name);
  if (I != Cache.end()) {
    if (ValueDecl *V = dyn_cast<ValueDecl>(I->second))
      Result.push_back(V);
    return;
  }

  ValueDecl *V = getBuiltinValue(Context, Name);
  if (!V) return;

  Cache.insert(std::make_pair(Name, V));
  Result.push_back(V);
}

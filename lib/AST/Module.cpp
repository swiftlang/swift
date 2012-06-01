//===--- Module.cpp - Swift Language Module Implementation ----------------===//
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
//  This file implements the Module class and subclasses.
//
//===----------------------------------------------------------------------===//
  
#include "swift/AST/Module.h"
#include "swift/AST/AST.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/SmallPtrSet.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// Builtin Module Name lookup
//===----------------------------------------------------------------------===//

namespace {
  /// BuiltinModuleCache - This is the type of the cache for the BuiltinModule.
  /// This is lazily created on its first use an hangs off
  /// Module::LookupCachePimpl.
  class BuiltinModuleCache {
    /// The cache of identifiers we've already looked up.  We use a
    /// single hashtable for both types and values as a minor
    /// optimization; this prevents us from having both a builtin type
    /// and a builtin value with the same name, but that's okay.
    llvm::DenseMap<Identifier, ValueDecl*> Cache;
  public:

    void lookupValue(Identifier Name, NLKind LookupKind, BuiltinModule &M, 
                     SmallVectorImpl<ValueDecl*> &Result);
  };
} // end anonymous namespace.

static BuiltinModuleCache &getBuiltinCachePimpl(void *&Ptr) {
  // FIXME: This leaks.  Sticking this into ASTContext isn't enough because then
  // the DenseMap will leak.
  if (Ptr == 0)
    Ptr = new BuiltinModuleCache();
  return *(BuiltinModuleCache*)Ptr;
}

void BuiltinModuleCache::lookupValue(Identifier Name, NLKind LookupKind,
                                     BuiltinModule &M,
                                     SmallVectorImpl<ValueDecl*> &Result) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return;
  
  ValueDecl *&Entry = Cache[Name];

  if (Entry == 0)
    if (Type Ty = getBuiltinType(M.Ctx, Name.str()))
      Entry = new (M.Ctx) TypeAliasDecl(SourceLoc(), Name, SourceLoc(), Ty,
                                        M.Ctx.TheBuiltinModule);

  if (Entry == 0)
    Entry = getBuiltinValue(M.Ctx, Name);
      
  if (ValueDecl *VD = dyn_cast_or_null<ValueDecl>(Entry))
    Result.push_back(VD);
}
                       
//===----------------------------------------------------------------------===//
// Normal Module Name Lookup
//===----------------------------------------------------------------------===//

namespace {
  /// TUModuleCache - This is the type of the cache for the TranslationUnit.
  /// This is lazily created on its first use an hangs off
  /// Module::LookupCachePimpl.
  class TUModuleCache {
    llvm::DenseMap<Identifier, TinyPtrVector<ValueDecl*>> TopLevelValues;
  public:
    typedef Module::AccessPathTy AccessPathTy;
    
    TUModuleCache(TranslationUnit &TU);
    
    void lookupValue(AccessPathTy AccessPath, Identifier Name, 
                     NLKind LookupKind, TranslationUnit &TU, 
                     SmallVectorImpl<ValueDecl*> &Result);
  };
} // end anonymous namespace.

static TUModuleCache &getTUCachePimpl(void *&Ptr, TranslationUnit &TU) {
  // FIXME: This leaks.  Sticking this into ASTContext isn't enough because then
  // the DenseMap will leak.
  if (Ptr == 0)
    Ptr = new TUModuleCache(TU);
  return *(TUModuleCache*)Ptr;
}

static void freeTUCachePimpl(void *&Ptr) {
  delete (TUModuleCache*)Ptr;
  Ptr = 0;
}


/// Populate our cache on the first name lookup.
TUModuleCache::TUModuleCache(TranslationUnit &TU) {
  for (Decl *D : TU.Decls) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      if (!VD->getName().empty())
        TopLevelValues[VD->getName()].push_back(VD);
  }
}


void TUModuleCache::lookupValue(AccessPathTy AccessPath, Identifier Name, 
                                NLKind LookupKind, TranslationUnit &TU, 
                                SmallVectorImpl<ValueDecl*> &Result) {
  // TODO: ImportDecls cannot specified namespaces or individual entities
  // yet, so everything is just a lookup at the top-level.
  assert(AccessPath.size() <= 1 && "Don't handle this yet");
  
  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (AccessPath.size() == 1 && AccessPath[0].first != Name)
    return;
  
  auto I = TopLevelValues.find(Name);
  if (I == TopLevelValues.end()) return;
  
  Result.reserve(I->second.size());
  for (ValueDecl *Elt : I->second)
    Result.push_back(Elt);
}

//===----------------------------------------------------------------------===//
// Module Extension Name Lookup
//===----------------------------------------------------------------------===//

namespace {
  class TUExtensionCache {
    llvm::DenseMap<CanType, TinyPtrVector<ExtensionDecl*>> Extensions;
  public:

    TUExtensionCache(TranslationUnit &TU);
    
    ArrayRef<ExtensionDecl*> getExtensions(CanType T) const{
      auto I = Extensions.find(T);
      if (I == Extensions.end())
        return ArrayRef<ExtensionDecl*>();
      return I->second;
    }
  };
}

static TUExtensionCache &getTUExtensionCachePimpl(void *&Ptr,
                                                  TranslationUnit &TU) {
  // FIXME: This leaks.  Sticking this into ASTContext isn't enough because then
  // the DenseMap will leak.
  if (Ptr == 0)
    Ptr = new TUExtensionCache(TU);
  return *(TUExtensionCache*)Ptr;
}

static void freeTUExtensionCachePimpl(void *&Ptr) {
  delete (TUExtensionCache*)Ptr;
  Ptr = 0;
}

TUExtensionCache::TUExtensionCache(TranslationUnit &TU) {
  for (Decl *D : TU.Decls) {
    if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
      // Ignore failed name lookups.
      if (ED->getExtendedType()->is<ErrorType>()) continue;
      
      Extensions[ED->getExtendedType()->getCanonicalType()].push_back(ED);
    }
  }
}


/// lookupExtensions - Look up all of the extensions in the module that are
/// extending the specified type and return a list of them.
ArrayRef<ExtensionDecl*> Module::lookupExtensions(Type T) {
  assert(ASTStage >= Parsed &&
         "Extensions should only be looked up after name binding is underway");
  
  // The builtin module just has free functions, not extensions.
  if (isa<BuiltinModule>(this)) return ArrayRef<ExtensionDecl*>();
  
  TUExtensionCache &Cache =
    getTUExtensionCachePimpl(ExtensionCachePimpl, *cast<TranslationUnit>(this));
  
  return Cache.getExtensions(T->getCanonicalType());
}

//===----------------------------------------------------------------------===//
// Module Implementation
//===----------------------------------------------------------------------===//

/// lookupValue - Look up a (possibly overloaded) value set at top-level scope
/// (but with the specified access path, which may come from an import decl)
/// within the current module. This does a simple local lookup, not
/// recursively looking through imports.  
void Module::lookupValue(AccessPathTy AccessPath, Identifier Name,
                         NLKind LookupKind, 
                         SmallVectorImpl<ValueDecl*> &Result) {
  if (BuiltinModule *BM = dyn_cast<BuiltinModule>(this)) {
    assert(AccessPath.empty() && "builtin module's access path always empty!");
    return getBuiltinCachePimpl(LookupCachePimpl)
      .lookupValue(Name, LookupKind, *BM, Result);
  }
  
  // Otherwise must be TranslationUnit.  Someday we should generalize this to
  // allow modules with multiple translation units.
  TranslationUnit &TU = *cast<TranslationUnit>(this);
  return getTUCachePimpl(LookupCachePimpl, TU)
    .lookupValue(AccessPath, Name, LookupKind, TU, Result);
}

static void DoGlobalExtensionLookup(Type BaseType, Identifier Name,
                                    ArrayRef<ValueDecl*> BaseMembers,
                                    Module *CurModule,
                                    Module *BaseModule,
                                    SmallVectorImpl<ValueDecl*> &Result) {
  bool CurModuleHasTypeDecl = false;
  llvm::SmallPtrSet<CanType, 8> CurModuleTypes;

  bool NameBindingLookup = CurModule->ASTStage == Module::Parsed;

  // Find all extensions in this module.
  for (ExtensionDecl *ED : CurModule->lookupExtensions(BaseType)) {
    for (Decl *Member : ED->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
        if (VD->getName() == Name) {
          Result.push_back(VD);
          if (!NameBindingLookup)
            CurModuleTypes.insert(VD->getType()->getCanonicalType());
          CurModuleHasTypeDecl |= isa<MetaTypeType>(VD->getType());
        }
      }
    }
  }

  if (BaseModule == CurModule) {
    for (ValueDecl *VD : BaseMembers) {
      if (VD->getName() == Name) {
        Result.push_back(VD);
        if (!NameBindingLookup)
          CurModuleTypes.insert(VD->getType()->getCanonicalType());
        CurModuleHasTypeDecl |= isa<MetaTypeType>(VD->getType());
      }
    }
  }

  // The builtin module has no imports.
  if (isa<BuiltinModule>(CurModule)) return;

  // If we find a type in the current module, don't look into any
  // imported modules.
  if (CurModuleHasTypeDecl) return;

  TranslationUnit &TU = *cast<TranslationUnit>(CurModule);

  // Otherwise, check our imported extensions as well.
  // FIXME: Implement DAG-based shadowing rules.
  llvm::SmallPtrSet<Module *, 16> Visited;
  for (auto &ImpEntry : TU.getImportedModules()) {
    if (!Visited.insert(ImpEntry.second))
      continue;
    
    for (ExtensionDecl *ED : ImpEntry.second->lookupExtensions(BaseType)) {
      for (Decl *Member : ED->getMembers()) {
        if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
          if (VD->getName() == Name &&
              (NameBindingLookup || isa<TypeDecl>(VD) ||
               !CurModuleTypes.count(VD->getType()->getCanonicalType()))) {
            Result.push_back(VD);
          }
        }
      }
    }
  }

  if (BaseModule != CurModule) {
    for (ValueDecl *VD : BaseMembers) {
      if (VD->getName() == Name &&
          (NameBindingLookup || isa<TypeDecl>(VD) ||
           !CurModuleTypes.count(VD->getType()->getCanonicalType()))) {
        Result.push_back(VD);
      }
    }
  }
}

/// lookupGlobalExtensionMethods - Lookup the extensions members for the
/// specified BaseType with the specified type, and return them in Result.
void Module::lookupMembers(Type BaseType, Identifier Name,
                           SmallVectorImpl<ValueDecl*> &Result) {
  assert(Result.empty() &&
         "This expects that the input list is empty, could be generalized");

  TypeDecl *D;
  ArrayRef<ValueDecl*> BaseMembers;
  SmallVector<ValueDecl*, 2> BaseMembersStorage;
  if (StructType *ST = BaseType->getAs<StructType>()) {
    // FIXME: Refuse to look up "constructors" until we have real constructors.
    if (ST->getDecl()->getName() == Name)
      return;

    D = ST->getDecl();
    for (Decl* Member : ST->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else if (ClassType *CT = BaseType->getAs<ClassType>()) {
    D = CT->getDecl();
    for (Decl* Member : CT->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else if (OneOfType *OOT = BaseType->getAs<OneOfType>()) {
    D = OOT->getDecl();
    for (Decl* Member : OOT->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else {
    return;
  }

  DeclContext *DC = D->getDeclContext();
  while (!DC->isModuleContext())
    DC = DC->getParent();

  DoGlobalExtensionLookup(BaseType, Name, BaseMembers, this, cast<Module>(DC),
                          Result);
}

/// lookupGlobalExtensionMethods - Lookup the extensions members for the
/// specified BaseType with the specified type, and return them in Result.
void Module::lookupValueConstructors(Type BaseType,
                                     SmallVectorImpl<ValueDecl*> &Result) {
  assert(Result.empty() &&
         "This expects that the input list is empty, could be generalized");

  TypeDecl *D;
  Identifier Name;
  ArrayRef<ValueDecl*> BaseMembers;
  SmallVector<ValueDecl*, 2> BaseMembersStorage;
  if (StructType *ST = BaseType->getAs<StructType>()) {
    D = ST->getDecl();
    Name = ST->getDecl()->getName();
    for (Decl* Member : ST->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else if (OneOfType *OOT = BaseType->getAs<OneOfType>()) {
    D = OOT->getDecl();
    Name = OOT->getDecl()->getName();
    for (Decl* Member : OOT->getDecl()->getMembers()) {
      // FIXME: We shouldn't be injecting OneOfElementDecls into the results
      // like this.
      if (OneOfElementDecl *OOED = dyn_cast<OneOfElementDecl>(Member))
        Result.push_back(OOED);
      else if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
  } else {
    return;
  }

  DeclContext *DC = D->getDeclContext();
  if (!DC->isModuleContext()) {
    for (ValueDecl *VD : BaseMembers) {
      if (VD->getName() == Name)
        Result.push_back(VD);
    }
    return;
  }

  DoGlobalExtensionLookup(BaseType, Name, BaseMembers, this, cast<Module>(DC),
                          Result);
}

//===----------------------------------------------------------------------===//
// TranslationUnit Implementation
//===----------------------------------------------------------------------===//

void TranslationUnit::clearLookupCache() {
  freeTUCachePimpl(LookupCachePimpl);
  freeTUExtensionCachePimpl(ExtensionCachePimpl);
}

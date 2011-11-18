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
#include "llvm/ADT/TinyPtrVector.h"
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
    llvm::DenseMap<Identifier, NamedDecl*> Cache;
  public:

    TypeAliasDecl *lookupType(Identifier Name, NLKind LookupKind,
                              BuiltinModule &M);
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

TypeAliasDecl *BuiltinModuleCache::lookupType(Identifier Name, 
                                              NLKind LookupKind,
                                              BuiltinModule &M) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return nullptr;
  
  NamedDecl *&Entry = Cache[Name];
  if (Entry == 0)
    if (Type Ty = getBuiltinType(M.Ctx, Name))
      Entry = new (M.Ctx) TypeAliasDecl(SourceLoc(), Name, Ty, DeclAttributes(),
                                        M.Ctx.TheBuiltinModule);
    
  return dyn_cast_or_null<TypeAliasDecl>(Entry);
}

void BuiltinModuleCache::lookupValue(Identifier Name, NLKind LookupKind,
                                     BuiltinModule &M,
                                     SmallVectorImpl<ValueDecl*> &Result) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return;
  
  NamedDecl *&Entry = Cache[Name];
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
    llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl*>> TopLevelValues;
    llvm::DenseMap<Identifier, TypeAliasDecl *> TopLevelTypes;
  public:
    typedef Module::AccessPathTy AccessPathTy;
    
    TUModuleCache(TranslationUnit &TU);
    
    TypeAliasDecl *lookupType(AccessPathTy AccessPath, Identifier Name,
                              NLKind LookupKind, TranslationUnit &TU);
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


/// Populate our cache on the first name lookup.
TUModuleCache::TUModuleCache(TranslationUnit &TU) {
  for (auto Elt : TU.Body->getElements())
    if (Decl *D = Elt.dyn_cast<Decl*>()) {
      if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
        if (!TAD->getName().empty())
          TopLevelTypes[TAD->getName()] = TAD;
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        if (!VD->getName().empty())
          TopLevelValues[VD->getName()].push_back(VD);
    }
}


TypeAliasDecl *TUModuleCache::lookupType(AccessPathTy AccessPath,
                                         Identifier Name, NLKind LookupKind,
                                         TranslationUnit &TU) {
  assert(AccessPath.size() <= 1 && "Don't handle this yet");
  
  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (AccessPath.size() == 1 && AccessPath[0].first != Name)
    return 0;
  
  auto I = TopLevelTypes.find(Name);
  return I != TopLevelTypes.end() ? I->second : 0;
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
  for (ValueDecl *Elt : I->second) {
    // Dot Lookup ignores values with non-function types.
    if (LookupKind == NLKind::DotLookup && !Elt->Ty->is<FunctionType>())
      continue;
    
    Result.push_back(Elt);
  }
}


//===----------------------------------------------------------------------===//
// Module Implementation
//===----------------------------------------------------------------------===//

/// lookupType - Look up a type at top-level scope (but with the specified 
/// access path, which may come from an import decl) within the current
/// module. This does a simple local lookup, not recursively looking  through
/// imports.  
TypeAliasDecl *Module::lookupType(AccessPathTy AccessPath, Identifier Name,
                                  NLKind LookupKind) {
  if (BuiltinModule *BM = dyn_cast<BuiltinModule>(this)) {
    assert(AccessPath.empty() && "builtin module's access path always empty!");
    return getBuiltinCachePimpl(LookupCachePimpl)
      .lookupType(Name, LookupKind, *BM);
  }
  
  // Otherwise must be TranslationUnit.  Someday we should generalize this to
  // allow modules with multiple translation units.
  TranslationUnit &TU = *cast<TranslationUnit>(this);
  return getTUCachePimpl(LookupCachePimpl, TU)
    .lookupType(AccessPath, Name, LookupKind, TU);
}

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


/// lookupGlobalType - Perform a type lookup within the current Module.
/// Unlike lookupType, this does look through import declarations to resolve
/// the name.
TypeAliasDecl *Module::lookupGlobalType(Identifier Name, NLKind LookupKind) {
  // Do a local lookup within the current module.
  TypeAliasDecl *TAD = lookupType(AccessPathTy(), Name, LookupKind);
  
  // If we get a hit, we're done.  Also, the builtin module never has
  // imports, so it is always done at this point.
  if (TAD || isa<BuiltinModule>(this)) return TAD;
  
  TranslationUnit &TU = *cast<TranslationUnit>(this);
  
  // If we still haven't found it, scrape through all of the imports, taking the
  // first match of the name.
  for (auto &ImpEntry : TU.ImportedModules) {
    TAD = ImpEntry.second->lookupType(ImpEntry.first, Name, LookupKind);
    if (TAD) return TAD;  // If we found a match, return the decl.
  }
  return 0;
}

/// lookupGlobalValue - Perform a value lookup within the current Module.
/// Unlike lookupValue, this does look through import declarations to resolve
/// the name.
void Module::lookupGlobalValue(Identifier Name, NLKind LookupKind, 
                               SmallVectorImpl<ValueDecl*> &Result) {
  // Do a local lookup within the current module.
  lookupValue(AccessPathTy(), Name, LookupKind, Result);

  // If we get any hits, we're done.  Also, the builtin module never has
  // imports, so it is always done at this point.
  if (!Result.empty() || isa<BuiltinModule>(this)) return;
  
  TranslationUnit &TU = *cast<TranslationUnit>(this);

  // If we still haven't found it, scrape through all of the imports, taking the
  // first match of the name.
  for (auto &ImpEntry : TU.ImportedModules) {
    ImpEntry.second->lookupValue(ImpEntry.first, Name, LookupKind, Result);
    if (!Result.empty()) return;  // If we found a match, return the decls.
  }
}


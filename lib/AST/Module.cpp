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
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/PrintOptions.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/raw_ostream.h"

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
      Entry = new (M.Ctx) TypeAliasDecl(SourceLoc(), Name, SourceLoc(),
                                        TypeLoc::withoutLoc(Ty),
                                        M.Ctx.TheBuiltinModule,
                                        MutableArrayRef<TypeLoc>());

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
    void doPopulateCache(ArrayRef<Decl*> decls, bool onlyOperators);
  public:
    typedef Module::AccessPathTy AccessPathTy;
    
    TUModuleCache(TranslationUnit &TU);
    
    void lookupValue(AccessPathTy AccessPath, Identifier Name, 
                     NLKind LookupKind, TranslationUnit &TU, 
                     SmallVectorImpl<ValueDecl*> &Result);
    
    void lookupVisibleDecls(AccessPathTy AccessPath,
                            VisibleDeclConsumer &Consumer,
                            NLKind LookupKind,
                            TranslationUnit &TU);
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

void TUModuleCache::doPopulateCache(ArrayRef<Decl*> decls, bool onlyOperators) {
  for (Decl *D : decls) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      if (onlyOperators ? VD->getName().isOperator() : !VD->getName().empty())
        TopLevelValues[VD->getName()].push_back(VD);
    if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D))
      doPopulateCache(NTD->getMembers(), true);
    if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D))
      doPopulateCache(ED->getMembers(), true);
  }
}

/// Populate our cache on the first name lookup.
TUModuleCache::TUModuleCache(TranslationUnit &TU) {
  doPopulateCache(TU.Decls, false);
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

void TUModuleCache::lookupVisibleDecls(AccessPathTy AccessPath,
                                       VisibleDeclConsumer &Consumer,
                                       NLKind LookupKind,
                                       TranslationUnit &TU) {
  // TODO: ImportDecls cannot specified namespaces or individual entities
  // yet, so everything is just a lookup at the top-level.
  assert(AccessPath.size() <= 1 && "Don't handle this yet");
  
  // TODO: If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  for (auto &tlv : TopLevelValues) {
    for (ValueDecl *vd : tlv.second)
      Consumer.foundDecl(vd);
  }
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

  if (auto tu = dyn_cast<TranslationUnit>(this)) {
    TUExtensionCache &Cache = getTUExtensionCachePimpl(ExtensionCachePimpl,*tu);
    return Cache.getExtensions(T->getCanonicalType());
  }

  return Ctx.getModuleLoader().lookupExtensions(cast<ClangModule>(this), T);
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

  if (auto TU = dyn_cast<TranslationUnit>(this)) {
    // Look in the translation unit.
    return getTUCachePimpl(LookupCachePimpl, *TU)
      .lookupValue(AccessPath, Name, LookupKind, *TU, Result);
  }

  return Ctx.getModuleLoader().lookupValue(cast<ClangModule>(this), AccessPath,
                                           Name, LookupKind, Result);
}

/// lookupVisibleDecls - Find ValueDecls in the module and pass them to the
/// given consumer object.
void Module::lookupVisibleDecls(AccessPathTy AccessPath,
                                VisibleDeclConsumer &Consumer,
                                NLKind LookupKind) {
  if (BuiltinModule *BM = dyn_cast<BuiltinModule>(this)) {
    // TODO Look through the Builtin module.
    (void)BM;
    return;
  }

  if (auto TU = dyn_cast<TranslationUnit>(this)) {
    return getTUCachePimpl(LookupCachePimpl, *TU)
      .lookupVisibleDecls(AccessPath, Consumer, LookupKind, *TU);
  }

  // TODO Delegate to Clang lookupVisibleDecls.
  return;
}

//===----------------------------------------------------------------------===//
// TranslationUnit Implementation
//===----------------------------------------------------------------------===//

void TranslationUnit::print(raw_ostream &os) {
  print(os, PrintOptions::printEverything());
}

void TranslationUnit::print(raw_ostream &os, const PrintOptions &options) {
  for (auto decl : Decls) {
    if (!decl->shouldPrintInContext())
      continue;

    decl->print(os, options);
    os << "\n";
  }
}

void TranslationUnit::clearLookupCache() {
  freeTUCachePimpl(LookupCachePimpl);
  freeTUExtensionCachePimpl(ExtensionCachePimpl);
}

//===----------------------------------------------------------------------===//
// ClangModule Implementation
//===----------------------------------------------------------------------===//
ClangModule::ClangModule(ASTContext &ctx, Component *comp,
                         clang::Module *clangModule)
  : Module(DeclContextKind::ClangModule,
           ctx.getIdentifier(clangModule->getFullModuleName()),
           comp, ctx),
    clangModule(clangModule)
{
  // Clang modules are always well-formed.
  ASTStage = TypeChecked;
}

//===----------------------------------------------------------------------===//
// ModuleLoader Implementation
//===----------------------------------------------------------------------===//
ModuleLoader::~ModuleLoader() {}

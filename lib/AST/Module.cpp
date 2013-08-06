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

#include "swift/AST/Diagnostics.h"
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

  if (Entry)
    Result.push_back(Entry);
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
    
    TUModuleCache(const TranslationUnit &TU);
    
    void lookupValue(AccessPathTy AccessPath, Identifier Name, 
                     NLKind LookupKind, TranslationUnit &TU, 
                     SmallVectorImpl<ValueDecl*> &Result);
    
    void lookupVisibleDecls(AccessPathTy AccessPath,
                            VisibleDeclConsumer &Consumer,
                            NLKind LookupKind,
                            const TranslationUnit &TU);
  };
} // end anonymous namespace.

static TUModuleCache &getTUCachePimpl(void *&Ptr, const TranslationUnit &TU) {
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
TUModuleCache::TUModuleCache(const TranslationUnit &TU) {
  doPopulateCache(TU.Decls, false);
}


void TUModuleCache::lookupValue(AccessPathTy AccessPath, Identifier Name, 
                                NLKind LookupKind, TranslationUnit &TU, 
                                SmallVectorImpl<ValueDecl*> &Result) {
  assert(AccessPath.size() <= 1 && "can only refer to top-level decls");
  
  // If this import is specific to some named type or decl ("import swift.int")
  // then filter out any lookups that don't match.
  if (AccessPath.size() == 1 && AccessPath.front().first != Name)
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
                                       const TranslationUnit &TU) {
  assert(AccessPath.size() <= 1 && "can only refer to top-level decls");

  if (!AccessPath.empty()) {
    auto I = TopLevelValues.find(AccessPath.front().first);
    if (I == TopLevelValues.end()) return;

    for (auto vd : I->second)
      Consumer.foundDecl(vd);
    return;
  }

  for (auto &tlv : TopLevelValues) {
    for (ValueDecl *vd : tlv.second)
      Consumer.foundDecl(vd);
  }
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

  return cast<LoadedModule>(this)->lookupValue(AccessPath, Name, LookupKind,
                                               Result);
}

/// lookupVisibleDecls - Find ValueDecls in the module and pass them to the
/// given consumer object.
void Module::lookupVisibleDecls(AccessPathTy AccessPath,
                                VisibleDeclConsumer &Consumer,
                                NLKind LookupKind) const {
  if (auto BM = dyn_cast<BuiltinModule>(this)) {
    // TODO Look through the Builtin module.
    (void)BM;
    return;
  }

  if (auto TU = dyn_cast<TranslationUnit>(this)) {
    return getTUCachePimpl(LookupCachePimpl, *TU)
      .lookupVisibleDecls(AccessPath, Consumer, LookupKind, *TU);
  }

  return cast<LoadedModule>(this)->lookupVisibleDecls(AccessPath, Consumer,
                                                      LookupKind);
}

namespace {
// Returns Nothing on error, Optional(nullptr) if no operator decl found, or
// Optional(decl) if decl was found.
template<typename OP_DECL>
Optional<OP_DECL *> lookupOperatorDeclForName(Module *M,
                          SourceLoc Loc,
                          Identifier Name,
                          llvm::StringMap<OP_DECL *> TranslationUnit::*OP_MAP)
{
  if (auto loadedModule = dyn_cast<LoadedModule>(M))
    return loadedModule->lookupOperator<OP_DECL>(Name);

  auto *TU = dyn_cast<TranslationUnit>(M);
  if (!TU)
    return nullptr;

  // Look for an operator declaration in the current module.
  auto found = (TU->*OP_MAP).find(Name.get());
  if (found != (TU->*OP_MAP).end())
    return found->getValue();
  
  // Look for imported operator decls.
  
  llvm::DenseSet<OP_DECL*> importedOperators;
  for (auto &imported : TU->getImportedModules()) {
    Optional<OP_DECL *> maybeOp
      = lookupOperatorDeclForName(imported.second, Loc, Name, OP_MAP);
    if (!maybeOp)
      return Nothing;
    
    if (OP_DECL *op = *maybeOp)
      importedOperators.insert(op);
  }
  
  // If we found a single import, use it.
  if (importedOperators.empty()) {
    // Cache the mapping so we don't need to troll imports next time.
    (TU->*OP_MAP)[Name.get()] = nullptr;
    return nullptr;
  }
  if (importedOperators.size() == 1) {
    // Cache the mapping so we don't need to troll imports next time.
    OP_DECL *result = *importedOperators.begin();
    (TU->*OP_MAP)[Name.get()] = result;
    return result;
  }
  
  // Otherwise, check for conflicts.
  auto i = importedOperators.begin(), end = importedOperators.end();
  OP_DECL *first = *i;
  for (++i; i != end; ++i) {
    if ((*i)->conflictsWith(first)) {
      if (Loc.isValid()) {
        ASTContext &C = M->getASTContext();
        C.Diags.diagnose(Loc, diag::ambiguous_operator_decls);
        C.Diags.diagnose(first->getLoc(), diag::found_this_operator_decl);
        C.Diags.diagnose((*i)->getLoc(), diag::found_this_operator_decl);
      }
      return Nothing;
    }
  }
  // Cache the mapping so we don't need to troll imports next time.
  (TU->*OP_MAP)[Name.get()] = first;
  return first;
}
} // end anonymous namespace

Optional<PrefixOperatorDecl *> Module::lookupPrefixOperator(Identifier name,
                                                            SourceLoc diagLoc) {
  return lookupOperatorDeclForName(this, diagLoc, name,
                                   &TranslationUnit::PrefixOperators);
}

Optional<PostfixOperatorDecl *> Module::lookupPostfixOperator(Identifier name,
                                                            SourceLoc diagLoc) {
  return lookupOperatorDeclForName(this, diagLoc, name,
                                   &TranslationUnit::PostfixOperators);
}

Optional<InfixOperatorDecl *> Module::lookupInfixOperator(Identifier name,
                                                          SourceLoc diagLoc) {
  return lookupOperatorDeclForName(this, diagLoc, name,
                                   &TranslationUnit::InfixOperators);
}

void
Module::getReexportedModules(SmallVectorImpl<ImportedModule> &modules) const {
  if (isa<BuiltinModule>(this))
    return;

  if (auto TU = dyn_cast<TranslationUnit>(this)) {
    // A translation unit doesn't really re-export all of its imported modules,
    // but for the purposes of lookup a TU is always top-level, so we want to
    // look at regular imports as well as re-exports.
    modules.append(TU->getImportedModules().begin(),
                   TU->getImportedModules().end());
    return;
  }

  return cast<LoadedModule>(this)->getReexportedModules(modules);
}

namespace {
  /// Arbitrarily orders ImportedModule records, for inclusion in sets and such.
  class OrderImportedModules {
    using ImportedModule = Module::ImportedModule;
    using AccessPathTy = Module::AccessPathTy;
  public:
    bool operator()(const ImportedModule &lhs, const ImportedModule &rhs) {
      if (lhs.second != rhs.second)
        return std::less<const Module *>()(lhs.second, rhs.second);
      if (lhs.first.data() != rhs.first.data())
        return std::less<AccessPathTy::iterator>()(lhs.first.begin(),
                                                   rhs.first.begin());
      return lhs.first.size() < rhs.first.size();
    }
  };
}

/// Returns true if the two access paths contain the same chain of identifiers.
///
/// Source locations are ignored here.
static bool isSameAccessPath(Module::AccessPathTy lhs,
                             Module::AccessPathTy rhs) {
  using AccessPathElem = std::pair<Identifier, SourceLoc>;
  if (lhs.size() != rhs.size())
    return false;
  auto iters = std::mismatch(lhs.begin(), lhs.end(), rhs.begin(),
                             [](const AccessPathElem &lElem,
                                const AccessPathElem &rElem) {
    return lElem.first == rElem.first;
  });
  return iters.first == lhs.end();
}

void Module::forAllVisibleModules(Optional<AccessPathTy> thisPath,
                                  std::function<bool(ImportedModule)> fn) {
  llvm::SmallSet<ImportedModule, 32, OrderImportedModules> visited;
  SmallVector<ImportedModule, 32> queue;

  AccessPathTy overridingPath;
  if (thisPath.hasValue()) {
    overridingPath = thisPath.getValue();
    queue.push_back(ImportedModule(overridingPath, this));
  } else {
    visited.insert(ImportedModule({}, this));
    getReexportedModules(queue);
  }

  while (!queue.empty()) {
    auto next = queue.pop_back_val();

    // Filter any whole-module imports, and skip specific-decl imports if the
    // import path doesn't match exactly.
    if (next.first.empty())
      next.first = overridingPath;
    else if (!overridingPath.empty() &&
             !isSameAccessPath(next.first, overridingPath)) {
      // If we ever allow importing non-top-level decls, it's possible the rule
      // above isn't what we want.
      assert(next.first.size() == 1 && "import of non-top-level decl");
      continue;
    }

    if (!visited.insert(next))
      continue;

    if (!fn(next))
      break;
    next.second->getReexportedModules(queue);
  }
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
}

//===----------------------------------------------------------------------===//
// LoadedModule Implementation
//===----------------------------------------------------------------------===//

void LoadedModule::lookupValue(AccessPathTy accessPath, Identifier name,
                               NLKind lookupKind,
                               SmallVectorImpl<ValueDecl*> &result) {
  return getOwner().lookupValue(this, accessPath, name, lookupKind, result);
}

OperatorDecl *LoadedModule::lookupOperator(Identifier name, DeclKind fixity) {
  return getOwner().lookupOperator(this, name, fixity);
}

template<>
PrefixOperatorDecl *
LoadedModule::lookupOperator<PrefixOperatorDecl>(Identifier name) {
  auto result = lookupOperator(name, DeclKind::PrefixOperator);
  return cast_or_null<PrefixOperatorDecl>(result);
}

template<>
PostfixOperatorDecl *
LoadedModule::lookupOperator<PostfixOperatorDecl>(Identifier name) {
  auto result = lookupOperator(name, DeclKind::PostfixOperator);
  return cast_or_null<PostfixOperatorDecl>(result);
}

template<>
InfixOperatorDecl *
LoadedModule::lookupOperator<InfixOperatorDecl>(Identifier name) {
  auto result = lookupOperator(name, DeclKind::InfixOperator);
  return cast_or_null<InfixOperatorDecl>(result);
}

void LoadedModule::getReexportedModules(
    SmallVectorImpl<ImportedModule> &exports) const {
  return getOwner().getReexportedModules(this, exports);
}

void LoadedModule::lookupVisibleDecls(AccessPathTy accessPath,
                                      VisibleDeclConsumer &consumer,
                                      NLKind lookupKind) const {
  return getOwner().lookupVisibleDecls(this, accessPath, consumer, lookupKind);
}


//===----------------------------------------------------------------------===//
// ModuleLoader Implementation
//===----------------------------------------------------------------------===//
ModuleLoader::~ModuleLoader() {}

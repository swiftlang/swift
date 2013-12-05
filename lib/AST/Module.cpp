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

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/PrintOptions.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// Builtin Module Name lookup
//===----------------------------------------------------------------------===//

class BuiltinUnit::LookupCache {
  /// The cache of identifiers we've already looked up.  We use a
  /// single hashtable for both types and values as a minor
  /// optimization; this prevents us from having both a builtin type
  /// and a builtin value with the same name, but that's okay.
  llvm::DenseMap<Identifier, ValueDecl*> Cache;

public:
  void lookupValue(Identifier Name, NLKind LookupKind, const BuiltinUnit &M,
                   SmallVectorImpl<ValueDecl*> &Result);
};

BuiltinUnit::LookupCache &BuiltinUnit::getCache() const {
  // FIXME: This leaks. Sticking this into ASTContext isn't enough because then
  // the DenseMap will leak.
  if (!Cache)
    const_cast<BuiltinUnit *>(this)->Cache.reset(new LookupCache());
  return *Cache;
}

void BuiltinUnit::LookupCache::lookupValue(Identifier Name, NLKind LookupKind,
                                           const BuiltinUnit &M,
                                           SmallVectorImpl<ValueDecl*> &Result) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return;
  
  ValueDecl *&Entry = Cache[Name];
  ASTContext &Ctx = M.getParentModule()->Ctx;
  if (Entry == 0)
    if (Type Ty = getBuiltinType(Ctx, Name.str()))
      Entry = new (Ctx) TypeAliasDecl(SourceLoc(), Name, SourceLoc(),
                                      TypeLoc::withoutLoc(Ty),
                                      const_cast<BuiltinUnit*>(&M), {});

  if (Entry == 0)
    Entry = getBuiltinValueDecl(Ctx, Name);

  if (Entry)
    Result.push_back(Entry);
}

// Out-of-line because std::unique_ptr wants LookupCache to be complete.
BuiltinUnit::BuiltinUnit(Module &M)
   : FileUnit(FileUnitKind::Builtin, M) {
}

//===----------------------------------------------------------------------===//
// Normal Module Name Lookup
//===----------------------------------------------------------------------===//

class SourceFile::LookupCache {
  llvm::DenseMap<Identifier, TinyPtrVector<ValueDecl*>> TopLevelValues;
  llvm::DenseMap<Identifier, TinyPtrVector<ValueDecl*>> ClassMembers;
  bool MemberCachePopulated = false;
  void doPopulateCache(ArrayRef<Decl*> decls, bool onlyOperators);
  void addToMemberCache(ArrayRef<Decl*> decls);
  void populateMemberCache(const SourceFile &SF);
public:
  typedef Module::AccessPathTy AccessPathTy;
  
  LookupCache(const SourceFile &SF);
  
  void lookupValue(AccessPathTy AccessPath, Identifier Name, 
                   NLKind LookupKind, SmallVectorImpl<ValueDecl*> &Result);
  
  void lookupVisibleDecls(AccessPathTy AccessPath,
                          VisibleDeclConsumer &Consumer,
                          NLKind LookupKind);
  
  void lookupClassMembers(AccessPathTy AccessPath,
                          VisibleDeclConsumer &consumer,
                          const SourceFile &SF);
                          
  void lookupClassMember(AccessPathTy accessPath,
                         Identifier name,
                         SmallVectorImpl<ValueDecl*> &results,
                         const SourceFile &SF);

  SmallVector<ValueDecl *, 0> AllVisibleValues;
};
using SourceLookupCache = SourceFile::LookupCache;

SourceLookupCache &SourceFile::getCache() const {
  // FIXME: This leaks.  Sticking this into ASTContext isn't enough because then
  // the DenseMap will leak.
  if (!Cache)
    const_cast<SourceFile *>(this)->Cache.reset(new SourceLookupCache(*this));
  return *Cache;
}

void SourceLookupCache::doPopulateCache(ArrayRef<Decl*> decls,
                                        bool onlyOperators) {
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

void SourceLookupCache::populateMemberCache(const SourceFile &SF) {
  for (const Decl *D : SF.Decls) {
    if (const NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D)) {
      addToMemberCache(NTD->getMembers());
    } else if (const ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D)) {
      addToMemberCache(ED->getMembers());
    }
  }
}

void SourceLookupCache::addToMemberCache(ArrayRef<Decl*> decls) {
  for (Decl *D : decls) {
    auto VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
      assert(!VD->canBeAccessedByDynamicLookup() &&
             "inner types cannot be accessed by dynamic lookup");
      addToMemberCache(NTD->getMembers());
    } else if (VD->canBeAccessedByDynamicLookup()) {
      ClassMembers[VD->getName()].push_back(VD);
    }
  }
}

/// Populate our cache on the first name lookup.
SourceLookupCache::LookupCache(const SourceFile &SF) {
  doPopulateCache(SF.Decls, false);
}


void SourceLookupCache::lookupValue(AccessPathTy AccessPath, Identifier Name,
                                    NLKind LookupKind,
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

void SourceLookupCache::lookupVisibleDecls(AccessPathTy AccessPath,
                                           VisibleDeclConsumer &Consumer,
                                           NLKind LookupKind) {
  assert(AccessPath.size() <= 1 && "can only refer to top-level decls");

  if (!AccessPath.empty()) {
    auto I = TopLevelValues.find(AccessPath.front().first);
    if (I == TopLevelValues.end()) return;

    for (auto vd : I->second)
      Consumer.foundDecl(vd, DeclVisibilityKind::VisibleAtTopLevel);
    return;
  }

  for (auto &tlv : TopLevelValues) {
    for (ValueDecl *vd : tlv.second)
      Consumer.foundDecl(vd, DeclVisibilityKind::VisibleAtTopLevel);
  }
}

void SourceLookupCache::lookupClassMembers(AccessPathTy accessPath,
                                           VisibleDeclConsumer &consumer,
                                           const SourceFile &SF) {
  if (!MemberCachePopulated)
    populateMemberCache(SF);
  
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");
  
  if (!accessPath.empty()) {
    for (auto &member : ClassMembers) {
      for (ValueDecl *vd : member.second) {
        Type ty = vd->getDeclContext()->getDeclaredTypeOfContext();
        if (auto nominal = ty->getAnyNominal())
          if (nominal->getName() == accessPath.front().first)
            consumer.foundDecl(vd, DeclVisibilityKind::DynamicLookup);
      }
    }
    return;
  }

  for (auto &member : ClassMembers) {
    for (ValueDecl *vd : member.second)
      consumer.foundDecl(vd, DeclVisibilityKind::DynamicLookup);
  }
}

void SourceLookupCache::lookupClassMember(AccessPathTy accessPath,
                                          Identifier name,
                                          SmallVectorImpl<ValueDecl*> &results,
                                          const SourceFile &SF) {
  if (!MemberCachePopulated)
    populateMemberCache(SF);
  
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");
  
  auto iter = ClassMembers.find(name);
  if (iter == ClassMembers.end())
    return;
  
  if (!accessPath.empty()) {
    for (ValueDecl *vd : iter->second) {
      Type ty = vd->getDeclContext()->getDeclaredTypeOfContext();
      if (auto nominal = ty->getAnyNominal())
        if (nominal->getName() == accessPath.front().first)
          results.push_back(vd);
    }
    return;
  }

  results.append(iter->second.begin(), iter->second.end());
}

//===----------------------------------------------------------------------===//
// Module Implementation
//===----------------------------------------------------------------------===//

void Module::addFile(FileUnit &newFile) {
  // Require Main and REPL files to be the first file added.
  assert(Files.empty() ||
         !isa<SourceFile>(newFile) ||
         cast<SourceFile>(newFile).Kind == SourceFileKind::Library ||
         cast<SourceFile>(newFile).Kind == SourceFileKind::SIL);
  Files.push_back(&newFile);
}

void Module::removeFile(FileUnit &existingFile) {
  // Do a reverse search; usually the file to be deleted will be at the end.
  std::reverse_iterator<decltype(Files)::iterator> I(Files.end()),
  E(Files.begin());
  I = std::find(I, E, &existingFile);
  assert(I != E);

  // Adjust for the std::reverse_iterator offset.
  ++I;
  Files.erase(I.base());
}

#define FORWARD(name, args) \
  for (const FileUnit *file : getFiles()) \
    file->name args;

void Module::lookupValue(AccessPathTy AccessPath, Identifier Name,
                         NLKind LookupKind, 
                         SmallVectorImpl<ValueDecl*> &Result) const {
  FORWARD(lookupValue, (AccessPath, Name, LookupKind, Result));
}

void BuiltinUnit::lookupValue(Module::AccessPathTy accessPath, Identifier name,
                              NLKind lookupKind,
                              SmallVectorImpl<ValueDecl*> &result) const {
  getCache().lookupValue(name, lookupKind, *this, result);
}

void SourceFile::lookupValue(Module::AccessPathTy accessPath, Identifier name,
                             NLKind lookupKind,
                             SmallVectorImpl<ValueDecl*> &result) const {
  getCache().lookupValue(accessPath, name, lookupKind, result);
}

void Module::lookupVisibleDecls(AccessPathTy AccessPath,
                                VisibleDeclConsumer &Consumer,
                                NLKind LookupKind) const {
  FORWARD(lookupVisibleDecls, (AccessPath, Consumer, LookupKind));
}

void SourceFile::lookupVisibleDecls(Module::AccessPathTy AccessPath,
                                    VisibleDeclConsumer &Consumer,
                                    NLKind LookupKind) const {
  getCache().lookupVisibleDecls(AccessPath, Consumer, LookupKind);
}

void Module::lookupClassMembers(AccessPathTy accessPath,
                                VisibleDeclConsumer &consumer) const {
  FORWARD(lookupClassMembers, (accessPath, consumer));
}

void SourceFile::lookupClassMembers(Module::AccessPathTy accessPath,
                                    VisibleDeclConsumer &consumer) const {
  getCache().lookupClassMembers(accessPath, consumer, *this);
}

void Module::lookupClassMember(AccessPathTy accessPath,
                               Identifier name,
                               SmallVectorImpl<ValueDecl*> &results) const {
  FORWARD(lookupClassMember, (accessPath, name, results));
}

void SourceFile::lookupClassMember(Module::AccessPathTy accessPath,
                                   Identifier name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  getCache().lookupClassMember(accessPath, name, results, *this);
}

void Module::getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const {
  FORWARD(getTopLevelDecls, (Results));
}

void SourceFile::getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const {
  Results.append(Decls.begin(), Decls.end());
}

ArrayRef<Substitution> BoundGenericType::getSubstitutions(
                                           Module *module,
                                           LazyResolver *resolver) {
  // FIXME: If there is no module, infer one. This is a hack for callers that
  // don't have access to the module. It will have to go away once we're
  // properly differentiating bound generic types based on the protocol
  // conformances visible from a given module.
  if (!module) {
    module = getDecl()->getParentModule();
  }

  // If we already have a cached copy of the substitutions, return them.
  auto *canon = getCanonicalType()->castTo<BoundGenericType>();
  const ASTContext &ctx = canon->getASTContext();
  if (auto known = ctx.getSubstitutions(canon))
    return *known;

  // Compute the set of substitutions.
  llvm::SmallPtrSet<ArchetypeType *, 8> knownArchetypes;
  SmallVector<ArchetypeType *, 8> archetypeStack;
  TypeSubstitutionMap substitutions;
  auto genericParams = canon->getDecl()->getGenericParams();
  unsigned index = 0;
  for (Type arg : canon->getGenericArgs()) {
    auto gp = genericParams->getParams()[index++];
    auto archetype = gp.getAsTypeParam()->getArchetype();
    substitutions[archetype] = arg;
  }

  // Collect all of the archetypes.
  SmallVector<ArchetypeType *, 2> allArchetypesList;
  ArrayRef<ArchetypeType *> allArchetypes = genericParams->getAllArchetypes();
  if (genericParams->getOuterParameters()) {
    SmallVector<const GenericParamList *, 2> allGenericParams;
    unsigned numArchetypes = 0;
    for (; genericParams; genericParams = genericParams->getOuterParameters()) {
      allGenericParams.push_back(genericParams);
      numArchetypes += genericParams->getAllArchetypes().size();
    }
    allArchetypesList.reserve(numArchetypes);
    for (auto gp = allGenericParams.rbegin(), gpEnd = allGenericParams.rend();
         gp != gpEnd; ++gp) {
      allArchetypesList.append((*gp)->getAllArchetypes().begin(),
                               (*gp)->getAllArchetypes().end());
    }
    allArchetypes = allArchetypesList;
  }

  // For each of the archetypes, compute the substitution.
  bool hasTypeVariables = canon->hasTypeVariable();
  SmallVector<Substitution, 4> resultSubstitutions;
  resultSubstitutions.resize(allArchetypes.size());
  index = 0;
  for (auto archetype : allArchetypes) {
    // Substitute into the type.
    auto type = Type(archetype).subst(module, substitutions,
                                      /*ignoreMissing=*/hasTypeVariables,
                                      resolver);
    assert(type && "Unable to perform type substitution");

    SmallVector<ProtocolConformance *, 4> conformances;
    if (type->hasTypeVariable()) {
      // If the type involves a type variable, just fill in null conformances.
      // FIXME: It seems like we should record these as requirements (?).
      conformances.assign(archetype->getConformsTo().size(), nullptr);
    } else {
      // Find the conformances.
      for (auto proto : archetype->getConformsTo()) {
        auto conforms = module->lookupConformance(type, proto, resolver);
        switch (conforms.getInt()) {
        case ConformanceKind::Conforms:
          conformances.push_back(conforms.getPointer());
          break;

        case ConformanceKind::DoesNotConform:
        case ConformanceKind::UncheckedConforms:
          llvm_unreachable("Couldn't find conformance");
        }
      }
    }

    // Record this substitution.
    resultSubstitutions[index].Archetype = archetype;
    resultSubstitutions[index].Replacement = type;
    resultSubstitutions[index].Conformance = ctx.AllocateCopy(conformances);
    ++index;
  }

  // Copy and record the substitutions.
  auto permanentSubs = ctx.AllocateCopy(resultSubstitutions,
                                        hasTypeVariables
                                          ? AllocationArena::ConstraintSolver
                                          : AllocationArena::Permanent);
  ctx.setSubstitutions(canon, permanentSubs);
  return permanentSubs;
}

/// Gather the set of substitutions required to map from the generic form of
/// the given type to the specialized form.
static ArrayRef<Substitution> gatherSubstitutions(Module *module, Type type,
                                                  LazyResolver *resolver) {
  assert(type->isSpecialized() && "Type is not specialized");
  SmallVector<ArrayRef<Substitution>, 2> allSubstitutions;

  while (type) {
    // Record the substitutions in a bound generic type.
    if (auto boundGeneric = type->getAs<BoundGenericType>()) {
      allSubstitutions.push_back(boundGeneric->getSubstitutions(module,
                                                                resolver));
      type = boundGeneric->getParent();
      continue;
    }

    // Skip to the parent of a nominal type.
    if (auto nominal = type->getAs<NominalType>()) {
      type = nominal->getParent();
      continue;
    }

    llvm_unreachable("Not a nominal or bound generic type");
  }
  assert(!allSubstitutions.empty() && "No substitutions?");

  // If there is only one list of substitutions, return it. There's no
  // need to copy it.
  if (allSubstitutions.size() == 1)
    return allSubstitutions.front();

  SmallVector<Substitution, 4> flatSubstitutions;
  for (auto substitutions : allSubstitutions)
    flatSubstitutions.append(substitutions.begin(), substitutions.end());
  auto &ctx = module->getASTContext();
  return ctx.AllocateCopy(flatSubstitutions);
}

/// Given a type witness map and a set of substitutions, produce the specialized
/// type witness map by applying the substitutions to each type witness.
static TypeWitnessMap
specializeTypeWitnesses(ASTContext &ctx,
                        Module *module,
                        const TypeWitnessMap &witnesses,
                        ArrayRef<Substitution> substitutions,
                        LazyResolver *resolver) {
  // Compute the substitution map, which is needed for substType().
  TypeSubstitutionMap substitutionMap;
  for (const auto &substitution : substitutions) {
    substitutionMap[substitution.Archetype] = substitution.Replacement;
  }

  // Substitute into each of the type witnesses.
  TypeWitnessMap result;
  for (const auto &genericWitness : witnesses) {
    // Substitute into the type witness to produce the type witness for
    // the specialized type.
    auto specializedType
      = genericWitness.second.Replacement.subst(module, substitutionMap,
                                                /*ignoreMissing=*/false,
                                                resolver);

    // If the type witness was unchanged, just copy it directly.
    if (specializedType.getPointer() ==
          genericWitness.second.Replacement.getPointer()) {
      result.insert(genericWitness);
      continue;
    }

    // Gather the conformances for the type witness. These should never fail.
    SmallVector<ProtocolConformance *, 4> conformances;
    auto archetype = genericWitness.second.Archetype;
    for (auto proto : archetype->getConformsTo()) {
      auto conforms = module->lookupConformance(specializedType, proto,
                                                resolver);
      switch (conforms.getInt()) {
      case ConformanceKind::Conforms:
        conformances.push_back(conforms.getPointer());
        break;

      case ConformanceKind::DoesNotConform:
      case ConformanceKind::UncheckedConforms:
        // FIXME: Signal errors in a more sane way.
        return TypeWitnessMap();
      }
    }

    result[genericWitness.first]
      = Substitution{archetype, specializedType,
                     ctx.AllocateCopy(conformances)};
  }

  return result;
}

/// Retrieve the explicit conformance of the given nominal type declaration
/// to the given protocol.
static std::tuple<NominalTypeDecl *, Decl *, ProtocolConformance *>
findExplicitConformance(NominalTypeDecl *nominal, ProtocolDecl *protocol,
                        LazyResolver *resolver) {
  // FIXME: Introduce a cache/lazy lookup structure to make this more efficient?

  // Walk the nominal type, its extensions, superclasses, and so on.
  llvm::SmallPtrSet<ProtocolDecl *, 4> visitedProtocols;
  SmallVector<std::tuple<NominalTypeDecl *, NominalTypeDecl *, Decl *>,4> stack;
  NominalTypeDecl *owningNominal = nullptr;
  Decl *declaresConformance = nullptr;
  ProtocolConformance *nominalConformance = nullptr;

  // Local function that checks for our protocol in the given array of
  // protocols.
  auto isProtocolInList
    = [&](NominalTypeDecl *currentNominal,
          Decl *currentOwner,
          ArrayRef<ProtocolDecl *> protocols,
          ArrayRef<ProtocolConformance *> conformances) -> bool {
      for (unsigned i = 0, n = protocols.size(); i != n; ++i) {
        auto testProto = protocols[i];
        if (testProto == protocol) {
          owningNominal = currentNominal;
          declaresConformance = currentOwner;
          if (i < conformances.size())
            nominalConformance = conformances[i];
          return true;
        }

        if (visitedProtocols.insert(testProto))
          stack.push_back(
              std::make_tuple(testProto, currentNominal, currentOwner));
      }

      return false;
    };

  resolver->resolveDeclSignature(nominal);

  // Walk the stack of types to find a conformance.
  stack.push_back(std::make_tuple(nominal, nominal, nominal));
  while (!stack.empty()) {
    NominalTypeDecl *current;
    NominalTypeDecl *currentNominal;
    Decl *currentOwner;
    std::tie(current, currentNominal, currentOwner) = stack.back();
    stack.pop_back();

    // Visit the superclass of a class.
    if (auto classDecl = dyn_cast<ClassDecl>(current)) {
      if (auto superclassTy = classDecl->getSuperclass()) {
        auto nominal = superclassTy->getAnyNominal();
        stack.push_back(std::make_tuple(nominal, nominal, nominal));
      }
    }

    // Visit the protocols this type conforms to directly.
    if (isProtocolInList(currentNominal, currentOwner,
                         current->getProtocols(),
                         current->getConformances()))
      break;

    // Visit the extensions of this type.
    for (auto ext : current->getExtensions()) {
      if (isProtocolInList(currentNominal, ext, ext->getProtocols(),
                           ext->getConformances()))
        break;
    }
  }

  // If we didn't find the protocol, we don't conform. Cache the negative result
  // and return.
  if (!owningNominal) {
    return std::make_tuple(nullptr, nullptr, nullptr);
  }

  // If we don't have a nominal conformance, but we do have a resolver, try
  // to resolve the nominal conformance now.
  if (!nominalConformance && resolver) {
    nominalConformance = resolver->resolveConformance(
                           owningNominal,
                           protocol,
                           dyn_cast<ExtensionDecl>(declaresConformance));
  }

  // If we have a nominal conformance, we're done.
  if (nominalConformance) {
    return std::make_tuple(owningNominal, declaresConformance,
                           nominalConformance);
  }

  return std::make_tuple(nullptr, nullptr, nullptr);
}

LookupConformanceResult Module::lookupConformance(Type type,
                                                  ProtocolDecl *protocol,
                                                  LazyResolver *resolver) {
  ASTContext &ctx = getASTContext();

  // An archetype conforms to a protocol if the protocol is listed in the
  // archetype's list of conformances.
  if (auto archetype = type->getAs<ArchetypeType>()) {
    for (auto ap : archetype->getConformsTo()) {
      if (ap == protocol || ap->inheritsFrom(protocol))
        return { nullptr, ConformanceKind::Conforms };
    }

    return { nullptr, ConformanceKind::DoesNotConform };
  }

  // An archetype conforms to a protocol if the protocol is listed in the
  // existential's list of conformances and the existential conforms to
  // itself.
  if (type->isExistentialType()) {
    // If the protocol doesn't conform to itself, there's no point in looking
    // further.
    auto known = protocol->existentialConformsToSelf();
    if (!known && resolver) {
      resolver->resolveExistentialConformsToItself(protocol);
      known = protocol->existentialConformsToSelf();
    }

    // If we know that protocol doesn't conform to itself, we're done.
    if (known && !*known)
      return { nullptr, ConformanceKind::DoesNotConform };

    // Look for this protocol within the existential's list of conformances.
    SmallVector<ProtocolDecl *, 4> protocols;
    type->isExistentialType(protocols);
    for (auto ap : protocols) {
      if (ap == protocol || ap->inheritsFrom(protocol)) {
        return { nullptr,
                 known? ConformanceKind::Conforms
                      : ConformanceKind::UncheckedConforms };
      }
    }

    // We didn't find our protocol in the existential's list; it doesn't
    // conform.
    return { nullptr, ConformanceKind::DoesNotConform };
  }

  // Check whether we have already cached an answer to this query.
  ASTContext::ConformsToMap::key_type key(type->getCanonicalType(), protocol);
  auto known = ctx.ConformsTo.find(key);
  if (known != ctx.ConformsTo.end()) {
    // If we conform, return the conformance.
    if (known->second.getInt()) {
      return { known->second.getPointer(), ConformanceKind::Conforms };
    }

    // We don't conform.
    return { nullptr, ConformanceKind::DoesNotConform };
  }

  auto nominal = type->getAnyNominal();

  // If we don't have a nominal type, there are no conformances.
  // FIXME: We may have implicit conformances for some cases. Handle those
  // here.
  if (!nominal) {
    return { nullptr, ConformanceKind::DoesNotConform };
  }

  // Find the explicit conformance.
  NominalTypeDecl *owningNominal = nullptr;
  Decl *declaresConformance = nullptr;
  ProtocolConformance *nominalConformance = nullptr;
  std::tie(owningNominal, declaresConformance, nominalConformance)
    = findExplicitConformance(nominal, protocol, resolver);

  // If we didn't find an owning nominal, we don't conform. Cache the negative
  // result and return.
  if (!owningNominal) {
    ctx.ConformsTo[key] = ConformanceEntry(nullptr, false);
    return { nullptr, ConformanceKind::DoesNotConform };
  }

  // If we found an owning nominal but didn't have a conformance, this is
  // an unchecked conformance.
  if (!nominalConformance) {
    return { nullptr, ConformanceKind::UncheckedConforms };
  }

  // If the nominal type in which we found the conformance is not the same
  // as the type we asked for, it's an inherited type.
  if (owningNominal != nominal) {
    // Find the superclass type
    Type superclassTy = type->getSuperclass(resolver);
    while (superclassTy->getAnyNominal() != owningNominal)
      superclassTy = superclassTy->getSuperclass(resolver);

    // Compute the conformance for the inherited type.
    auto inheritedConformance = lookupConformance(superclassTy, protocol,
                                                  resolver);
    switch (inheritedConformance.getInt()) {
    case ConformanceKind::DoesNotConform:
      llvm_unreachable("We already found the inherited conformance");

    case ConformanceKind::UncheckedConforms:
      return inheritedConformance;

    case ConformanceKind::Conforms:
      // Create inherited conformance below.
      break;
    }

    // Create the inherited conformance entry.
    auto result
      = ctx.getInheritedConformance(type, inheritedConformance.getPointer());
    ctx.ConformsTo[key] = ConformanceEntry(result, true);
    return { result, ConformanceKind::Conforms };
  }

  // If the type is specialized, find the conformance for the generic type.
  if (type->isSpecialized()) {
    // Figure out the type that's explicitly conforming to this protocol.
    Type explicitConformanceType;
    if (auto nominal = dyn_cast<NominalTypeDecl>(declaresConformance)) {
      explicitConformanceType = nominal->getDeclaredTypeInContext();
    } else {
      explicitConformanceType = cast<ExtensionDecl>(declaresConformance)
        ->getDeclaredTypeInContext();
    }

    // If the explicit conformance is associated with a type that is different
    // from the type we're checking, retrieve generic conformance.
    if (!explicitConformanceType->isEqual(type)) {
      // Gather the substitutions we need to map the generic conformance to
      // the specialized conformance.
      auto substitutions = gatherSubstitutions(this, type, resolver);

      // The type witnesses for the specialized conformance.
      TypeWitnessMap typeWitnesses
        = specializeTypeWitnesses(ctx, this,
                                  nominalConformance->getTypeWitnesses(),
                                  substitutions,
                                  resolver);

      // Create the specialized conformance entry.
      ctx.ConformsTo[key] = ConformanceEntry(nullptr, false);
      auto result = ctx.getSpecializedConformance(type, nominalConformance,
                                                  substitutions,
                                                  std::move(typeWitnesses));
      ctx.ConformsTo[key] = ConformanceEntry(result, true);
      return { result, ConformanceKind::Conforms };
    }
  }

  // Record and return the simple conformance.
  ctx.ConformsTo[key] = ConformanceEntry(nominalConformance, true);
  return { nominalConformance, ConformanceKind::Conforms };
}

void Module::getDisplayDecls(SmallVectorImpl<Decl*> &results) const {
  // FIXME: Include decls from a shadowed module.
  // FIXME: Should this do extra access control filtering?
  getTopLevelDecls(results);
}

namespace {
  template <typename T>
  using OperatorMap = SourceFile::OperatorMap<T>;

  template <typename T>
  struct OperatorKind {
    static_assert(static_cast<T*>(nullptr), "Only usable with operators");
  };

  template <>
  struct OperatorKind<PrefixOperatorDecl> {
    static const auto value = DeclKind::PrefixOperator;
  };

  template <>
  struct OperatorKind<InfixOperatorDecl> {
    static const auto value = DeclKind::InfixOperator;
  };

  template <>
  struct OperatorKind<PostfixOperatorDecl> {
    static const auto value = DeclKind::PostfixOperator;
  };
}

template <typename Op, typename T>
static Op *lookupOperator(T &container, Identifier name) {
  return cast_or_null<Op>(container.lookupOperator(name,
                                                   OperatorKind<Op>::value));
}

template<typename OP_DECL>
static Optional<OP_DECL *>
lookupOperatorDeclForName(Module *M, SourceLoc Loc, Identifier Name,
                          OperatorMap<OP_DECL *> SourceFile::*OP_MAP);

// Returns Nothing on error, Optional(nullptr) if no operator decl found, or
// Optional(decl) if decl was found.
template<typename OP_DECL>
static Optional<OP_DECL *>
lookupOperatorDeclForName(const FileUnit &File, SourceLoc Loc, Identifier Name,
                          bool includePrivate,
                          OperatorMap<OP_DECL *> SourceFile::*OP_MAP)
{
  switch (File.getKind()) {
  case FileUnitKind::Builtin:
    // The Builtin module declares no operators.
    return nullptr;
  case FileUnitKind::Source:
    break;
  case FileUnitKind::SerializedAST:
  case FileUnitKind::ClangModule:
    return lookupOperator<OP_DECL>(cast<LoadedFile>(File), Name);
  }

  auto &SF = cast<SourceFile>(File);
  assert(SF.ASTStage >= SourceFile::NameBound);

  // Look for an operator declaration in the current module.
  auto found = (SF.*OP_MAP).find(Name);
  if (found != (SF.*OP_MAP).end() && (includePrivate || found->second.getInt()))
    return found->second.getPointer();

  // Look for imported operator decls.
  // Record whether they come from re-exported modules.
  // FIXME: We ought to prefer operators elsewhere in this module before we
  // check imports.
  llvm::SmallDenseMap<OP_DECL*, bool, 16> importedOperators;
  for (auto &imported : SF.getImports()) {
    if (!includePrivate && !imported.second)
      continue;

    Optional<OP_DECL *> maybeOp
      = lookupOperatorDeclForName(imported.first.second, Loc, Name, OP_MAP);
    if (!maybeOp)
      return Nothing;
    
    if (OP_DECL *op = *maybeOp)
      importedOperators[op] |= imported.second;
  }

  typename OperatorMap<OP_DECL *>::mapped_type result = { nullptr, true };
  
  if (!importedOperators.empty()) {
    // Check for conflicts.
    auto i = importedOperators.begin(), end = importedOperators.end();
    auto start = i;
    for (++i; i != end; ++i) {
      if (i->first->conflictsWith(start->first)) {
        if (Loc.isValid()) {
          ASTContext &C = SF.getASTContext();
          C.Diags.diagnose(Loc, diag::ambiguous_operator_decls);
          C.Diags.diagnose(start->first->getLoc(),
                           diag::found_this_operator_decl);
          C.Diags.diagnose(i->first->getLoc(), diag::found_this_operator_decl);
        }
        return Nothing;
      }
    }
    result = { start->first, start->second };
  }

  if (includePrivate) {
    // Cache the mapping so we don't need to troll imports next time.
    // It's not safe to cache the non-private results because we didn't search
    // private imports there, but in most non-private cases the result will
    // be cached in the final lookup.
    auto &mutableOpMap = const_cast<OperatorMap<OP_DECL *> &>(SF.*OP_MAP);
    mutableOpMap[Name] = result;
  }

  if (includePrivate || result.getInt())
    return result.getPointer();
  return nullptr;
}

template<typename OP_DECL>
static Optional<OP_DECL *>
lookupOperatorDeclForName(Module *M, SourceLoc Loc, Identifier Name,
                          OperatorMap<OP_DECL *> SourceFile::*OP_MAP)
{
  OP_DECL *result = nullptr;
  for (const FileUnit *File : M->getFiles()) {
    auto next = lookupOperatorDeclForName(*File, Loc, Name, false, OP_MAP);
    if (!next.hasValue())
      return next;

    // FIXME: Diagnose ambiguity.
    if (*next && result)
      return Nothing;
    if (*next)
      result = *next;
  }
  return result;
}

#define LOOKUP_OPERATOR(Kind) \
Kind##OperatorDecl * \
Module::lookup##Kind##Operator(Identifier name, SourceLoc loc) { \
  auto result = lookupOperatorDeclForName(this, loc, name, \
                                          &SourceFile::Kind##Operators); \
  return result ? *result : nullptr; \
} \
Kind##OperatorDecl * \
SourceFile::lookup##Kind##Operator(Identifier name, SourceLoc loc) { \
  auto result = lookupOperatorDeclForName(*this, loc, name, true, \
                                          &SourceFile::Kind##Operators); \
  if (result.hasValue() && !result.getValue()) \
    result = lookupOperatorDeclForName(getParentModule(), loc, name, \
                                       &SourceFile::Kind##Operators); \
  return result ? *result : nullptr; \
}

LOOKUP_OPERATOR(Prefix)
LOOKUP_OPERATOR(Infix)
LOOKUP_OPERATOR(Postfix)
#undef LOOKUP_OPERATOR

void Module::getImportedModules(SmallVectorImpl<ImportedModule> &modules,
                                bool includePrivate) const {
  // FIXME: Audit uses of this function and make sure they make sense in a
  // multi-file world.
  FORWARD(getImportedModules, (modules, includePrivate));
}

void
SourceFile::getImportedModules(SmallVectorImpl<Module::ImportedModule> &modules,
                               bool includePrivate) const {
  for (auto importPair : getImports())
    if (includePrivate || importPair.second)
      modules.push_back(importPair.first);
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

bool Module::isSameAccessPath(AccessPathTy lhs, AccessPathTy rhs) {
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

StringRef Module::getModuleFilename() const {
  // FIXME: Audit uses of this function and figure out how to migrate them to
  // per-file names. Modules can consist of more than one file.
  if (getFiles().size() == 1) {
    if (auto SF = dyn_cast<SourceFile>(getFiles().front()))
      return SF->getFilename();
    if (auto LF = dyn_cast<LoadedFile>(getFiles().front()))
      return LF->getFilename();
  }
  return StringRef();
}

bool Module::isStdlibModule() const {
  return !getParent() && Name == Ctx.StdlibModuleName;
}

template<bool respectVisibility, typename Callback>
static bool forAllImportedModules(Module *topLevel,
                                  Optional<Module::AccessPathTy> thisPath,
                                  const Callback &fn) {
  using ImportedModule = Module::ImportedModule;
  using AccessPathTy = Module::AccessPathTy;
  
  llvm::SmallSet<ImportedModule, 32, OrderImportedModules> visited;
  SmallVector<ImportedModule, 32> queue;

  AccessPathTy overridingPath;
  if (thisPath.hasValue()) {
    if (respectVisibility)
      overridingPath = thisPath.getValue();
    queue.push_back(ImportedModule(overridingPath, topLevel));
  } else {
    visited.insert(ImportedModule({}, topLevel));
  }

  // Even if we're processing the top-level module like any other, we still want
  // to include non-exported modules.
  topLevel->getImportedModules(queue, !respectVisibility);

  while (!queue.empty()) {
    auto next = queue.pop_back_val();

    // Filter any whole-module imports, and skip specific-decl imports if the
    // import path doesn't match exactly.
    if (next.first.empty() || !respectVisibility)
      next.first = overridingPath;
    else if (!overridingPath.empty() &&
             !Module::isSameAccessPath(next.first, overridingPath)) {
      // If we ever allow importing non-top-level decls, it's possible the rule
      // above isn't what we want.
      assert(next.first.size() == 1 && "import of non-top-level decl");
      continue;
    }

    if (!visited.insert(next))
      continue;

    if (!fn(next))
      return false;
    next.second->getImportedModules(queue, !respectVisibility);
  }

  return true;
}

bool Module::forAllVisibleModules(Optional<AccessPathTy> thisPath,
                                  std::function<bool(ImportedModule)> fn) {
  return forAllImportedModules<true>(this, thisPath, fn);
}

bool
FileUnit::forAllVisibleModules(std::function<bool(Module::ImportedModule)> fn) {
  if (!getParentModule()->forAllVisibleModules(Module::AccessPathTy(), fn))
    return false;

  if (auto SF = dyn_cast<SourceFile>(this)) {
    // Handle privately visible modules as well.
    for (auto importPair : SF->getImports()) {
      if (importPair.second)
        continue;
      Module *M = importPair.first.second;
      if (!M->forAllVisibleModules(importPair.first.first, fn))
        return false;
    }
  }

  return true;
}

void Module::collectLinkLibraries(LinkLibraryCallback callback) {
  // FIXME: The proper way to do this depends on the decls used.
  FORWARD(collectLinkLibraries, (callback));
}

void
SourceFile::collectLinkLibraries(Module::LinkLibraryCallback callback) const {
  for (auto importPair : Imports)
    importPair.first.second->collectLinkLibraries(callback);
}

bool Module::walk(ASTWalker &Walker) {
  llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(Walker.Parent, this);
  for (auto SF : getFiles())
    if (SF->walk(Walker))
      return true;
  return false;
}

//===----------------------------------------------------------------------===//
// SourceFile Implementation
//===----------------------------------------------------------------------===//

void SourceFile::print(raw_ostream &os) {
  print(os, PrintOptions::printEverything());
}

void SourceFile::print(raw_ostream &os, const PrintOptions &options) {
  for (auto decl : Decls) {
    if (!decl->shouldPrintInContext())
      continue;

    decl->print(os, options);
    os << "\n";
  }
}

void SourceFile::clearLookupCache() {
  Cache.reset();
}

void
SourceFile::cacheVisibleDecls(SmallVectorImpl<ValueDecl*> &&globals) const {
  SmallVectorImpl<ValueDecl*> &cached = getCache().AllVisibleValues;
  cached = std::move(globals);
}

const SmallVectorImpl<ValueDecl *> &
SourceFile::getCachedVisibleDecls() const {
  return getCache().AllVisibleValues;
}

static void performAutoImport(SourceFile &SF, bool hasBuiltinModuleAccess) {
  if (SF.Kind == SourceFileKind::SIL)
    return;

  // If we're building the standard library, import the magic Builtin module,
  // otherwise, import the standard library.
  ASTContext &Ctx = SF.getASTContext();
  Module *M;
  if (hasBuiltinModuleAccess)
    M = Ctx.TheBuiltinModule;
  else
    M = Ctx.getModule({ {Ctx.StdlibModuleName, SourceLoc()} });

  if (!M)
    return;

  // FIXME: These will be the same for most source files, but we copy them
  // over and over again.
  std::pair<Module::ImportedModule, bool> Imports[] = {
    std::make_pair(Module::ImportedModule({}, M), false)
  };
  SF.setImports(Ctx.AllocateCopy(Imports));
}

SourceFile::SourceFile(Module &M, SourceFileKind K,
                       Optional<unsigned> bufferID, bool hasBuiltinModuleAccess)
  : FileUnit(FileUnitKind::Source, M),
    BufferID(bufferID ? *bufferID : -1), Kind(K) {
  performAutoImport(*this, hasBuiltinModuleAccess);
}


bool FileUnit::walk(ASTWalker &walker) {
  SmallVector<Decl *, 64> Decls;
  getTopLevelDecls(Decls);
  llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(walker.Parent,
                                                getParentModule());
  for (Decl *D : Decls) {
    if (D->walk(walker))
      return true;
  }
  return false;
}

bool SourceFile::walk(ASTWalker &walker) {
  llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(walker.Parent,
                                                getParentModule());
  for (Decl *D : Decls) {
    if (D->walk(walker))
      return true;
  }
  return false;
}

StringRef SourceFile::getFilename() const  {
  if (BufferID == -1)
    return "";
  SourceManager &SM = getASTContext().SourceMgr;
  return SM->getMemoryBuffer(BufferID)->getBufferIdentifier();
}


//===----------------------------------------------------------------------===//
// Miscellaneous
//===----------------------------------------------------------------------===//

void FileUnit::anchor() {}
void *FileUnit::operator new(size_t Bytes, ASTContext &C, unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

StringRef LoadedFile::getFilename() const {
  return "";
}

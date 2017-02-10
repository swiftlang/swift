//===--- Module.cpp - Swift Language Module Implementation ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Module class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Module.h"
#include "swift/AST/AccessScope.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTScope.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Compiler.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/MD5.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
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
    const_cast<BuiltinUnit *>(this)->Cache = llvm::make_unique<LookupCache>();
  return *Cache;
}

void BuiltinUnit::LookupCache::lookupValue(
       Identifier Name, NLKind LookupKind, const BuiltinUnit &M,
       SmallVectorImpl<ValueDecl*> &Result) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return;
  
  ValueDecl *&Entry = Cache[Name];
  ASTContext &Ctx = M.getParentModule()->getASTContext();
  if (!Entry) {
    if (Type Ty = getBuiltinType(Ctx, Name.str())) {
      auto *TAD = new (Ctx) TypeAliasDecl(SourceLoc(), Name, SourceLoc(),
                                          /*genericparams*/nullptr,
                                          const_cast<BuiltinUnit*>(&M));
      TAD->setUnderlyingType(Ty);
      TAD->setAccessibility(Accessibility::Public);
      Entry = TAD;
    }
  }

  if (!Entry)
    Entry = getBuiltinValueDecl(Ctx, Name);

  if (Entry)
    Result.push_back(Entry);
}

// Out-of-line because std::unique_ptr wants LookupCache to be complete.
BuiltinUnit::BuiltinUnit(ModuleDecl &M)
   : FileUnit(FileUnitKind::Builtin, M) {
  M.getASTContext().addDestructorCleanup(*this);
}

//===----------------------------------------------------------------------===//
// Normal Module Name Lookup
//===----------------------------------------------------------------------===//

class SourceFile::LookupCache {
  /// A lookup map for value decls. When declarations are added they are added
  /// under all variants of the name they can be found under.
  class DeclMap {
    llvm::DenseMap<DeclName, TinyPtrVector<ValueDecl*>> Members;

  public:
    void add(ValueDecl *VD) {
      if (!VD->hasName()) return;
      VD->getFullName().addToLookupTable(Members, VD);
    }

    void clear() {
      Members.shrink_and_clear();
    }

    decltype(Members)::const_iterator begin() const { return Members.begin(); }
    decltype(Members)::const_iterator end() const { return Members.end(); }
    decltype(Members)::const_iterator find(DeclName Name) const {
      return Members.find(Name);
    }
  };

  DeclMap TopLevelValues;
  DeclMap ClassMembers;
  bool MemberCachePopulated = false;

  template<typename Range>
  void doPopulateCache(Range decls, bool onlyOperators);
  void addToMemberCache(DeclRange decls);
  void populateMemberCache(const SourceFile &SF);
public:
  typedef ModuleDecl::AccessPathTy AccessPathTy;
  
  LookupCache(const SourceFile &SF);

  /// Throw away as much memory as possible.
  void invalidate();
  
  void lookupValue(AccessPathTy AccessPath, DeclName Name,
                   NLKind LookupKind, SmallVectorImpl<ValueDecl*> &Result);
  
  void lookupVisibleDecls(AccessPathTy AccessPath,
                          VisibleDeclConsumer &Consumer,
                          NLKind LookupKind);
  
  void lookupClassMembers(AccessPathTy AccessPath,
                          VisibleDeclConsumer &consumer,
                          const SourceFile &SF);
                          
  void lookupClassMember(AccessPathTy accessPath,
                         DeclName name,
                         SmallVectorImpl<ValueDecl*> &results,
                         const SourceFile &SF);

  SmallVector<ValueDecl *, 0> AllVisibleValues;
};
using SourceLookupCache = SourceFile::LookupCache;

SourceLookupCache &SourceFile::getCache() const {
  if (!Cache) {
    const_cast<SourceFile *>(this)->Cache =
        llvm::make_unique<SourceLookupCache>(*this);
  }
  return *Cache;
}

template<typename Range>
void SourceLookupCache::doPopulateCache(Range decls,
                                        bool onlyOperators) {
  for (Decl *D : decls) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      if (onlyOperators ? VD->getName().isOperator() : VD->hasName()) {
        // Cache the value under both its compound name and its full name.
        TopLevelValues.add(VD);
      }
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

  MemberCachePopulated = true;
}

void SourceLookupCache::addToMemberCache(DeclRange decls) {
  for (Decl *D : decls) {
    auto VD = dyn_cast<ValueDecl>(D);
    if (!VD)
      continue;

    if (auto NTD = dyn_cast<NominalTypeDecl>(VD)) {
      assert(!VD->canBeAccessedByDynamicLookup() &&
             "inner types cannot be accessed by dynamic lookup");
      addToMemberCache(NTD->getMembers());
    } else if (VD->canBeAccessedByDynamicLookup()) {
      ClassMembers.add(VD);
    }
  }
}

/// Populate our cache on the first name lookup.
SourceLookupCache::LookupCache(const SourceFile &SF) {
  doPopulateCache(llvm::makeArrayRef(SF.Decls), false);
}

void SourceLookupCache::lookupValue(AccessPathTy AccessPath, DeclName Name,
                                    NLKind LookupKind,
                                    SmallVectorImpl<ValueDecl*> &Result) {
  // If this import is specific to some named type or decl ("import Swift.int")
  // then filter out any lookups that don't match.
  if (!ModuleDecl::matchesAccessPath(AccessPath, Name))
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
    for (ValueDecl *vd : tlv.second) {
      // Declarations are added under their full and simple names.  Skip the
      // entry for the simple name so that we report each declaration once.
      if (tlv.first.isSimpleName() && !vd->getFullName().isSimpleName())
        continue;
      Consumer.foundDecl(vd, DeclVisibilityKind::VisibleAtTopLevel);
    }
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
      // Non-simple names are also stored under their simple name, so make
      // sure to only report them once.
      if (!member.first.isSimpleName())
        continue;

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
    // Non-simple names are also stored under their simple name, so make sure to
    // only report them once.
    if (!member.first.isSimpleName())
      continue;

    for (ValueDecl *vd : member.second)
      consumer.foundDecl(vd, DeclVisibilityKind::DynamicLookup);
  }
}

void SourceLookupCache::lookupClassMember(AccessPathTy accessPath,
                                          DeclName name,
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

void SourceLookupCache::invalidate() {
  TopLevelValues.clear();
  ClassMembers.clear();
  MemberCachePopulated = false;

  // std::move AllVisibleValues into a temporary to destroy its contents.
  using SameSizeSmallVector = decltype(AllVisibleValues);
  (void)SameSizeSmallVector{std::move(AllVisibleValues)};
}

//===----------------------------------------------------------------------===//
// Module Implementation
//===----------------------------------------------------------------------===//

ModuleDecl::ModuleDecl(Identifier name, ASTContext &ctx)
  : TypeDecl(DeclKind::Module, &ctx, name, SourceLoc(), { }),
    DeclContext(DeclContextKind::Module, nullptr),
    Flags({0, 0, 0}), DSOHandle(nullptr) {
  ctx.addDestructorCleanup(*this);
  setImplicit();
  setInterfaceType(ModuleType::get(this));
  setAccessibility(Accessibility::Public);
}

void ModuleDecl::addFile(FileUnit &newFile) {
  // Require Main and REPL files to be the first file added.
  assert(Files.empty() ||
         !isa<SourceFile>(newFile) ||
         cast<SourceFile>(newFile).Kind == SourceFileKind::Library ||
         cast<SourceFile>(newFile).Kind == SourceFileKind::SIL);
  Files.push_back(&newFile);
}

void ModuleDecl::removeFile(FileUnit &existingFile) {
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

void ModuleDecl::lookupValue(AccessPathTy AccessPath, DeclName Name,
                             NLKind LookupKind, 
                             SmallVectorImpl<ValueDecl*> &Result) const {
  FORWARD(lookupValue, (AccessPath, Name, LookupKind, Result));
}

TypeDecl * ModuleDecl::lookupLocalType(StringRef MangledName) const {
  for (auto file : getFiles()) {
    auto TD = file->lookupLocalType(MangledName);
    if (TD)
      return TD;
  }
  return nullptr;
}

void ModuleDecl::lookupMember(SmallVectorImpl<ValueDecl*> &results,
                              DeclContext *container, DeclName name,
                              Identifier privateDiscriminator) const {
  size_t oldSize = results.size();
  bool alreadyInPrivateContext = false;

  switch (container->getContextKind()) {
  case DeclContextKind::SerializedLocal:
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::SubscriptDecl:
    llvm_unreachable("This context does not support lookup.");

  case DeclContextKind::FileUnit:
    llvm_unreachable("Use FileUnit::lookupValue instead.");

  case DeclContextKind::ExtensionDecl:
    llvm_unreachable("Use ExtensionDecl::lookupDirect instead.");

  case DeclContextKind::Module: {
    assert(container == this);
    this->lookupValue({}, name, NLKind::QualifiedLookup, results);
    break;
  }

  case DeclContextKind::GenericTypeDecl: {
    auto nominal = dyn_cast<NominalTypeDecl>(container);
    if (!nominal) break;

    auto lookupResults = nominal->lookupDirect(name);

    // Filter out declarations from other modules.
    std::copy_if(lookupResults.begin(), lookupResults.end(),
                 std::back_inserter(results),
                 [this](const ValueDecl *VD) -> bool {
      return VD->getModuleContext() == this;
    });

    auto AS = nominal->getFormalAccessScope();
    if (AS.isPrivate() || AS.isFileScope())
      alreadyInPrivateContext = true;

    break;
  }
  }

  // Filter by private-discriminator, or filter out private decls if there isn't
  // one...unless we're already in a private context, in which case everything
  // is private and a discriminator is unnecessary.
  if (alreadyInPrivateContext) {
    assert(privateDiscriminator.empty() && "unnecessary private-discriminator");
    // Don't remove anything; everything here is private anyway.

  } else if (privateDiscriminator.empty()) {
    auto newEnd = std::remove_if(results.begin()+oldSize, results.end(),
                                 [](const ValueDecl *VD) -> bool {
      return VD->getFormalAccess() <= Accessibility::FilePrivate;
    });
    results.erase(newEnd, results.end());

  } else {
    auto newEnd = std::remove_if(results.begin()+oldSize, results.end(),
                                 [=](const ValueDecl *VD) -> bool {
      if (VD->getFormalAccess() > Accessibility::FilePrivate)
        return true;
      auto enclosingFile =
        cast<FileUnit>(VD->getDeclContext()->getModuleScopeContext());
      auto discriminator = enclosingFile->getDiscriminatorForPrivateValue(VD);
      return discriminator != privateDiscriminator;
    });
    results.erase(newEnd, results.end());
  }
}

void ModuleDecl::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  FORWARD(lookupObjCMethods, (selector, results));
}

void BuiltinUnit::lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
                              NLKind lookupKind,
                              SmallVectorImpl<ValueDecl*> &result) const {
  getCache().lookupValue(name.getBaseName(), lookupKind, *this, result);
}

void BuiltinUnit::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  // No @objc methods in the Builtin module.
}

void SourceFile::lookupValue(ModuleDecl::AccessPathTy accessPath, DeclName name,
                             NLKind lookupKind,
                             SmallVectorImpl<ValueDecl*> &result) const {
  getCache().lookupValue(accessPath, name, lookupKind, result);
}

void ModuleDecl::lookupVisibleDecls(AccessPathTy AccessPath,
                                VisibleDeclConsumer &Consumer,
                                NLKind LookupKind) const {
  FORWARD(lookupVisibleDecls, (AccessPath, Consumer, LookupKind));
}

void SourceFile::lookupVisibleDecls(ModuleDecl::AccessPathTy AccessPath,
                                    VisibleDeclConsumer &Consumer,
                                    NLKind LookupKind) const {
  getCache().lookupVisibleDecls(AccessPath, Consumer, LookupKind);
}

void ModuleDecl::lookupClassMembers(AccessPathTy accessPath,
                                VisibleDeclConsumer &consumer) const {
  FORWARD(lookupClassMembers, (accessPath, consumer));
}

void SourceFile::lookupClassMembers(ModuleDecl::AccessPathTy accessPath,
                                    VisibleDeclConsumer &consumer) const {
  getCache().lookupClassMembers(accessPath, consumer, *this);
}

void ModuleDecl::lookupClassMember(AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  FORWARD(lookupClassMember, (accessPath, name, results));
}

void SourceFile::lookupClassMember(ModuleDecl::AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  getCache().lookupClassMember(accessPath, name, results, *this);
}

void SourceFile::lookupObjCMethods(
       ObjCSelector selector,
       SmallVectorImpl<AbstractFunctionDecl *> &results) const {
  // FIXME: Make sure this table is complete, somehow.
  auto known = ObjCMethods.find(selector);
  if (known == ObjCMethods.end()) return;
  results.append(known->second.begin(), known->second.end());
}

void ModuleDecl::getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &Results) const {
  FORWARD(getLocalTypeDecls, (Results));
}

void ModuleDecl::getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const {
  FORWARD(getTopLevelDecls, (Results));
}

void SourceFile::getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const {
  Results.append(Decls.begin(), Decls.end());
}

void SourceFile::getLocalTypeDecls(SmallVectorImpl<TypeDecl*> &Results) const {
  Results.append(LocalTypeDecls.begin(), LocalTypeDecls.end());
}

void ModuleDecl::getDisplayDecls(SmallVectorImpl<Decl*> &Results) const {
  // FIXME: Should this do extra access control filtering?
  FORWARD(getDisplayDecls, (Results));
}

SubstitutionList
TypeBase::gatherAllSubstitutions(ModuleDecl *module,
                                 LazyResolver *resolver,
                                 DeclContext *gpContext) {
  // FIXME: If there is no module, infer one. This is a hack for callers that
  // don't have access to the module. It will have to go away once we're
  // properly differentiating bound generic types based on the protocol
  // conformances visible from a given module.
  if (!module)
    module = getAnyNominal()->getParentModule();

  // Check the context, introducing the default if needed.
  if (!gpContext)
    gpContext = getAnyNominal();

  assert(gpContext->getAsNominalTypeOrNominalTypeExtensionContext()
         == getAnyNominal() && "not a valid context");

  auto *genericSig = gpContext->getGenericSignatureOfContext();
  if (genericSig == nullptr)
    return { };

  // If we already have a cached copy of the substitutions, return them.
  const ASTContext &ctx = getASTContext();
  if (auto known = ctx.getSubstitutions(this, gpContext))
    return *known;

  // Compute the set of substitutions.
  TypeSubstitutionMap substitutions;

  // The type itself contains substitutions up to the innermost
  // non-type context.
  Type parent = this;
  ArrayRef<GenericTypeParamType *> genericParams =
    genericSig->getGenericParams();
  unsigned lastGenericIndex = genericParams.size();
  while (parent) {
    if (auto boundGeneric = parent->getAs<BoundGenericType>()) {
      unsigned index = lastGenericIndex - boundGeneric->getGenericArgs().size();
      for (Type arg : boundGeneric->getGenericArgs()) {
        auto paramTy = genericParams[index++];
        substitutions[
          paramTy->getCanonicalType()->castTo<GenericTypeParamType>()] = arg;
      }
      lastGenericIndex -= boundGeneric->getGenericArgs().size();

      parent = boundGeneric->getParent();
      continue;
    }

    if (auto protocol = parent->getAs<ProtocolType>()) {
      parent = protocol->getParent();
      lastGenericIndex--;
      continue;
    }

    if (auto nominal = parent->getAs<NominalType>()) {
      parent = nominal->getParent();
      continue;
    }

    llvm_unreachable("Not a nominal or bound generic type");
  }

  auto *genericEnv = gpContext->getGenericEnvironmentOfContext();

  // Add forwarding substitutions from the outer context if we have
  // a type nested inside a generic function.
  auto *parentDC = gpContext;
  while (parentDC->isTypeContext())
    parentDC = parentDC->getParent();
  if (auto *outerSig = parentDC->getGenericSignatureOfContext()) {
    for (auto gp : outerSig->getGenericParams()) {
      auto result = substitutions.insert(
                      {gp->getCanonicalType()->castTo<GenericTypeParamType>(),
                       genericEnv->mapTypeIntoContext(gp)});
      assert(result.second);
    }
  }

  SmallVector<Substitution, 4> result;
  genericSig->getSubstitutions(substitutions,
                               LookUpConformanceInModule(module),
                               result);

  // Before recording substitutions, make sure we didn't end up doing it
  // recursively.
  if (auto known = ctx.getSubstitutions(this, gpContext))
    return *known;

  // Copy and record the substitutions.
  auto permanentSubs = ctx.AllocateCopy(result,
                                        hasTypeVariable()
                                          ? AllocationArena::ConstraintSolver
                                          : AllocationArena::Permanent);
  ctx.setSubstitutions(this, gpContext, permanentSubs);
  return permanentSubs;
}

Optional<ProtocolConformanceRef>
ModuleDecl::lookupConformance(Type type, ProtocolDecl *protocol,
                              LazyResolver *resolver) {
  ASTContext &ctx = getASTContext();

  // A dynamic Self type conforms to whatever its underlying type
  // conforms to.
  if (auto selfType = type->getAs<DynamicSelfType>())
    type = selfType->getSelfType();

  // An archetype conforms to a protocol if the protocol is listed in the
  // archetype's list of conformances, or if the archetype has a superclass
  // constraint and the superclass conforms to the protocol.
  if (auto archetype = type->getAs<ArchetypeType>()) {

    // The generic signature builder drops conformance requirements that are made
    // redundant by a superclass requirement, so check for a concrete
    // conformance first, since an abstract conformance might not be
    // able to be resolved by a substitution that makes the archetype
    // concrete.
    if (auto super = archetype->getSuperclass()) {
      if (auto inheritedConformance = lookupConformance(super, protocol,
                                                        resolver)) {
        return ProtocolConformanceRef(
                 ctx.getInheritedConformance(
                   type,
                   inheritedConformance->getConcrete()));
      }
    }

    if (protocol->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
      if (archetype->requiresClass())
        return ProtocolConformanceRef(protocol);

      return None;
    }

    for (auto ap : archetype->getConformsTo()) {
      if (ap == protocol || ap->inheritsFrom(protocol))
        return ProtocolConformanceRef(protocol);
    }

    return None;
  }

  // An existential conforms to a protocol if the protocol is listed in the
  // existential's list of conformances and the existential conforms to
  // itself.
  if (type->isExistentialType()) {
    SmallVector<ProtocolDecl *, 4> protocols;
    type->getAnyExistentialTypeProtocols(protocols);

    // Due to an IRGen limitation, witness tables cannot be passed from an
    // existential to an archetype parameter, so for now we restrict this to
    // @objc protocols.
    for (auto proto : protocols) {
      if (!proto->isObjC() &&
          !proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
        return None;
    }

    // If the existential type cannot be represented or the protocol does not
    // conform to itself, there's no point in looking further.
    if (!protocol->existentialConformsToSelf() ||
        !protocol->existentialTypeSupported(resolver))
      return None;

    // Special-case AnyObject, which may not be in the list of conformances.
    if (protocol->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
      if (type->isClassExistentialType())
        return ProtocolConformanceRef(protocol);

      return None;
    }

    // Look for this protocol within the existential's list of conformances.
    for (auto proto : protocols) {
      if (proto == protocol || proto->inheritsFrom(protocol))
        return ProtocolConformanceRef(protocol);
    }

    // We didn't find our protocol in the existential's list; it doesn't
    // conform.
    return None;
  }

  // Type variables have trivial conformances.
  if (type->isTypeVariableOrMember())
    return ProtocolConformanceRef(protocol);

  // UnresolvedType is a placeholder for an unknown type used when generating
  // diagnostics.  We consider it to conform to all protocols, since the
  // intended type might have.
  if (type->is<UnresolvedType>())
    return ProtocolConformanceRef(protocol);

  auto nominal = type->getAnyNominal();

  // If we don't have a nominal type, there are no conformances.
  if (!nominal) return None;

  // Find the (unspecialized) conformance.
  SmallVector<ProtocolConformance *, 2> conformances;
  if (!nominal->lookupConformance(this, protocol, conformances))
    return None;

  // FIXME: Ambiguity resolution.
  auto conformance = conformances.front();

  // Rebuild inherited conformances based on the root normal conformance.
  // FIXME: This is a hack to work around our inability to handle multiple
  // levels of substitution through inherited conformances elsewhere in the
  // compiler.
  if (auto inherited = dyn_cast<InheritedProtocolConformance>(conformance)) {
    // Dig out the conforming nominal type.
    auto rootConformance = inherited->getRootNormalConformance();
    auto conformingNominal
      = rootConformance->getType()->getClassOrBoundGenericClass();

    // Map up to our superclass's type.
    Type superclassTy = type->getSuperclass(resolver);
    while (superclassTy->getAnyNominal() != conformingNominal)
      superclassTy = superclassTy->getSuperclass(resolver);

    // Compute the conformance for the inherited type.
    auto inheritedConformance = lookupConformance(superclassTy, protocol,
                                                  resolver);
    assert(inheritedConformance &&
           "We already found the inherited conformance");

    // Create the inherited conformance entry.
    conformance
      = ctx.getInheritedConformance(type, inheritedConformance->getConcrete());
    return ProtocolConformanceRef(conformance);
  }

  // If the type is specialized, find the conformance for the generic type.
  if (type->isSpecialized()) {
    // Figure out the type that's explicitly conforming to this protocol.
    Type explicitConformanceType = conformance->getType();
    DeclContext *explicitConformanceDC = conformance->getDeclContext();

    // If the explicit conformance is associated with a type that is different
    // from the type we're checking, retrieve generic conformance.
    if (!explicitConformanceType->isEqual(type)) {
      // Gather the substitutions we need to map the generic conformance to
      // the specialized conformance.
      auto substitutions = type->gatherAllSubstitutions(this, resolver,
                                                        explicitConformanceDC);

      // Create the specialized conformance entry.
      auto result = ctx.getSpecializedConformance(type, conformance,
                                                  substitutions);
      return ProtocolConformanceRef(result);
    }
  }

  // Record and return the simple conformance.
  return ProtocolConformanceRef(conformance);
}

namespace {
  template <typename T>
  using OperatorMap = SourceFile::OperatorMap<T>;

  template <typename T>
  struct OperatorLookup {
    // Don't fold this into the static_assert: this would trigger an MSVC bug
    // that causes the assertion to fail.
    static constexpr T* ptr = static_cast<T*>(nullptr);
    static_assert(ptr, "Only usable with operators");
  };

  template <>
  struct OperatorLookup<PrefixOperatorDecl> {
    template <typename T>
    static PrefixOperatorDecl *lookup(T &container, Identifier name) {
      return cast_or_null<PrefixOperatorDecl>(
               container.lookupOperator(name, DeclKind::PrefixOperator));
    }
  };

  template <>
  struct OperatorLookup<InfixOperatorDecl> {
    template <typename T>
    static InfixOperatorDecl *lookup(T &container, Identifier name) {
      return cast_or_null<InfixOperatorDecl>(
               container.lookupOperator(name, DeclKind::InfixOperator));
    }
  };

  template <>
  struct OperatorLookup<PostfixOperatorDecl> {
    template <typename T>
    static PostfixOperatorDecl *lookup(T &container, Identifier name) {
      return cast_or_null<PostfixOperatorDecl>(
               container.lookupOperator(name, DeclKind::PostfixOperator));
    }
  };

  template <>
  struct OperatorLookup<PrecedenceGroupDecl> {
    template <typename T>
    static PrecedenceGroupDecl *lookup(T &container, Identifier name) {
      return container.lookupPrecedenceGroup(name);
    }
  };
} // end anonymous namespace

/// A helper class to sneak around C++ access control rules.
class SourceFile::Impl {
public:
  /// Only intended for use by lookupOperatorDeclForName.
  static ArrayRef<std::pair<ModuleDecl::ImportedModule, SourceFile::ImportOptions>>
  getImportsForSourceFile(const SourceFile &SF) {
    return SF.Imports;
  }
};


template<typename OP_DECL>
static Optional<OP_DECL *>
lookupOperatorDeclForName(ModuleDecl *M, SourceLoc Loc, Identifier Name,
                          OperatorMap<OP_DECL *> SourceFile::*OP_MAP);

template<typename OP_DECL>
using ImportedOperatorsMap = llvm::SmallDenseMap<OP_DECL*, bool, 16>;

template<typename OP_DECL>
static typename ImportedOperatorsMap<OP_DECL>::iterator
checkOperatorConflicts(const SourceFile &SF, SourceLoc loc,
                       ImportedOperatorsMap<OP_DECL> &importedOperators) {
  // Check for conflicts.
  auto i = importedOperators.begin(), end = importedOperators.end();
  auto start = i;
  for (++i; i != end; ++i) {
    if (i->first->conflictsWith(start->first)) {
      if (loc.isValid()) {
        ASTContext &C = SF.getASTContext();
        C.Diags.diagnose(loc, diag::ambiguous_operator_decls);
        C.Diags.diagnose(start->first->getLoc(),
                         diag::found_this_operator_decl);
        C.Diags.diagnose(i->first->getLoc(), diag::found_this_operator_decl);
      }
      return end;
    }
  }
  return start;
}

template<>
typename ImportedOperatorsMap<PrecedenceGroupDecl>::iterator
checkOperatorConflicts(const SourceFile &SF, SourceLoc loc,
               ImportedOperatorsMap<PrecedenceGroupDecl> &importedGroups) {
  if (importedGroups.size() == 1)
    return importedGroups.begin();

  // Any sort of ambiguity is an error.
  if (loc.isValid()) {
    ASTContext &C = SF.getASTContext();
    C.Diags.diagnose(loc, diag::ambiguous_precedence_groups);
    for (auto &entry : importedGroups) {
      C.Diags.diagnose(entry.first->getLoc(),
                       diag::found_this_precedence_group);
    }
  }
  return importedGroups.end();
}

// Returns None on error, Optional(nullptr) if no operator decl found, or
// Optional(decl) if decl was found.
template<typename OP_DECL>
static Optional<OP_DECL *>
lookupOperatorDeclForName(const FileUnit &File, SourceLoc Loc, Identifier Name,
                          bool includePrivate,
                          OperatorMap<OP_DECL *> SourceFile::*OP_MAP)
{
  switch (File.getKind()) {
  case FileUnitKind::Builtin:
  case FileUnitKind::Derived:
    // The Builtin module declares no operators, nor do derived units.
    return nullptr;
  case FileUnitKind::Source:
    break;
  case FileUnitKind::SerializedAST:
  case FileUnitKind::ClangModule:
    return OperatorLookup<OP_DECL>::lookup(cast<LoadedFile>(File), Name);
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
  auto ownModule = SF.getParentModule();
  ImportedOperatorsMap<OP_DECL> importedOperators;
  for (auto &imported : SourceFile::Impl::getImportsForSourceFile(SF)) {
    // Protect against source files that contrive to import their own modules.
    if (imported.first.second == ownModule)
      continue;

    bool isExported =
        imported.second.contains(SourceFile::ImportFlags::Exported);
    if (!includePrivate && !isExported)
      continue;

    Optional<OP_DECL *> maybeOp
      = lookupOperatorDeclForName(imported.first.second, Loc, Name, OP_MAP);
    if (!maybeOp)
      return None;
    
    if (OP_DECL *op = *maybeOp)
      importedOperators[op] |= isExported;
  }

  typename OperatorMap<OP_DECL *>::mapped_type result = { nullptr, true };
  
  if (!importedOperators.empty()) {
    auto start = checkOperatorConflicts(SF, Loc, importedOperators);
    if (start == importedOperators.end())
      return None;
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
lookupOperatorDeclForName(ModuleDecl *M, SourceLoc Loc, Identifier Name,
                          OperatorMap<OP_DECL *> SourceFile::*OP_MAP)
{
  OP_DECL *result = nullptr;
  for (const FileUnit *File : M->getFiles()) {
    auto next = lookupOperatorDeclForName(*File, Loc, Name, false, OP_MAP);
    if (!next.hasValue())
      return next;

    // FIXME: Diagnose ambiguity.
    if (*next && result)
      return None;
    if (*next)
      result = *next;
  }
  return result;
}

#define LOOKUP_OPERATOR(Kind) \
Kind##Decl * \
ModuleDecl::lookup##Kind(Identifier name, SourceLoc loc) { \
  auto result = lookupOperatorDeclForName(this, loc, name, \
                                          &SourceFile::Kind##s); \
  return result ? *result : nullptr; \
} \
Kind##Decl * \
SourceFile::lookup##Kind(Identifier name, bool isCascading, SourceLoc loc) { \
  auto result = lookupOperatorDeclForName(*this, loc, name, true, \
                                          &SourceFile::Kind##s); \
  if (!result.hasValue()) \
    return nullptr; \
  if (ReferencedNames) {\
    if (!result.getValue() || \
        result.getValue()->getDeclContext()->getModuleScopeContext() != this) {\
      ReferencedNames->addTopLevelName(name, isCascading); \
    } \
  } \
  if (!result.getValue()) { \
    result = lookupOperatorDeclForName(getParentModule(), loc, name, \
                                       &SourceFile::Kind##s); \
  } \
  return result.hasValue() ? result.getValue() : nullptr; \
}

LOOKUP_OPERATOR(PrefixOperator)
LOOKUP_OPERATOR(InfixOperator)
LOOKUP_OPERATOR(PostfixOperator)
LOOKUP_OPERATOR(PrecedenceGroup)
#undef LOOKUP_OPERATOR

void ModuleDecl::getImportedModules(SmallVectorImpl<ImportedModule> &modules,
                                    ModuleDecl::ImportFilter filter) const {
  FORWARD(getImportedModules, (modules, filter));
}

void
SourceFile::getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &modules,
                               ModuleDecl::ImportFilter filter) const {
  assert(ASTStage >= Parsed || Kind == SourceFileKind::SIL);
  for (auto importPair : Imports) {
    switch (filter) {
    case ModuleDecl::ImportFilter::All:
      break;
    case ModuleDecl::ImportFilter::Public:
      if (!importPair.second.contains(ImportFlags::Exported))
        continue;
      break;
    case ModuleDecl::ImportFilter::Private:
      if (importPair.second.contains(ImportFlags::Exported))
        continue;
      break;
    }

    modules.push_back(importPair.first);
  }
}

void ModuleDecl::getImportedModulesForLookup(
    SmallVectorImpl<ImportedModule> &modules) const {
  FORWARD(getImportedModulesForLookup, (modules));
}

bool ModuleDecl::isSameAccessPath(AccessPathTy lhs, AccessPathTy rhs) {
  using AccessPathElem = std::pair<Identifier, SourceLoc>;
  if (lhs.size() != rhs.size())
    return false;
  return std::equal(lhs.begin(), lhs.end(), rhs.begin(),
                    [](const AccessPathElem &lElem,
                       const AccessPathElem &rElem) {
    return lElem.first == rElem.first;
  });
}

StringRef ModuleDecl::getModuleFilename() const {
  // FIXME: Audit uses of this function and figure out how to migrate them to
  // per-file names. Modules can consist of more than one file.
  StringRef Result;
  for (auto F : getFiles()) {
    if (auto SF = dyn_cast<SourceFile>(F)) {
      if (!Result.empty())
        return StringRef();
      Result = SF->getFilename();
      continue;
    }
    if (auto LF = dyn_cast<LoadedFile>(F)) {
      if (!Result.empty())
        return StringRef();
      Result = LF->getFilename();
      continue;
    }
    return StringRef();
  }
  return Result;
}

bool ModuleDecl::isStdlibModule() const {
  return !getParent() && getName() == getASTContext().StdlibModuleName;
}

bool ModuleDecl::isSwiftShimsModule() const {
  return !getParent() && getName() == getASTContext().SwiftShimsModuleName;
}

bool ModuleDecl::isBuiltinModule() const {
  return this == getASTContext().TheBuiltinModule;
}

bool SourceFile::registerMainClass(ClassDecl *mainClass, SourceLoc diagLoc) {
  if (mainClass == MainClass)
    return false;

  ArtificialMainKind kind = mainClass->getArtificialMainKind();
  if (getParentModule()->registerEntryPointFile(this, diagLoc, kind))
    return true;

  MainClass = mainClass;
  MainClassDiagLoc = diagLoc;
  return false;
}

bool ModuleDecl::registerEntryPointFile(FileUnit *file, SourceLoc diagLoc,
                                        Optional<ArtificialMainKind> kind) {
  if (!EntryPointInfo.hasEntryPoint()) {
    EntryPointInfo.setEntryPointFile(file);
    return false;
  }

  if (diagLoc.isInvalid())
    return true;

  assert(kind.hasValue() && "multiple entry points without attributes");

  // %select indices for UI/NSApplication-related diagnostics.
  enum : unsigned {
    UIApplicationMainClass = 0,
    NSApplicationMainClass = 1,
  } mainClassDiagKind;

  switch (kind.getValue()) {
  case ArtificialMainKind::UIApplicationMain:
    mainClassDiagKind = UIApplicationMainClass;
    break;
  case ArtificialMainKind::NSApplicationMain:
    mainClassDiagKind = NSApplicationMainClass;
    break;
  }

  FileUnit *existingFile = EntryPointInfo.getEntryPointFile();
  const ClassDecl *existingClass = existingFile->getMainClass();
  SourceLoc existingDiagLoc;

  if (auto *sourceFile = dyn_cast<SourceFile>(existingFile)) {
    if (existingClass) {
      existingDiagLoc = sourceFile->getMainClassDiagLoc();
    } else {
      if (auto bufID = sourceFile->getBufferID())
        existingDiagLoc = getASTContext().SourceMgr.getLocForBufferStart(*bufID);
    }
  }

  if (existingClass) {
    if (EntryPointInfo.markDiagnosedMultipleMainClasses()) {
      // If we already have a main class, and we haven't diagnosed it,
      // do so now.
      if (existingDiagLoc.isValid()) {
        getASTContext().Diags.diagnose(existingDiagLoc, diag::attr_ApplicationMain_multiple,
                           mainClassDiagKind);
      } else {
        getASTContext().Diags.diagnose(existingClass, diag::attr_ApplicationMain_multiple,
                           mainClassDiagKind);
      }
    }

    // Always diagnose the new class.
    getASTContext().Diags.diagnose(diagLoc, diag::attr_ApplicationMain_multiple,
                       mainClassDiagKind);

  } else {
    // We don't have an existing class, but we /do/ have a file in script mode.
    // Diagnose that.
    if (EntryPointInfo.markDiagnosedMainClassWithScript()) {
      getASTContext().Diags.diagnose(diagLoc, diag::attr_ApplicationMain_with_script,
                         mainClassDiagKind);

      if (existingDiagLoc.isValid()) {
        getASTContext().Diags.diagnose(existingDiagLoc,
                           diag::attr_ApplicationMain_script_here);
      }
    }
  }

  return true;
}

bool ModuleDecl::isSystemModule() const {
  if (isStdlibModule())
    return true;
  for (auto F : getFiles()) {
    if (auto LF = dyn_cast<LoadedFile>(F))
      return LF->isSystemModule();
  }
  return false;
}

template<bool respectVisibility, typename Callback>
static bool forAllImportedModules(ModuleDecl *topLevel,
                                  ModuleDecl::AccessPathTy thisPath,
                                  bool includePrivateTopLevelImports,
                                  const Callback &fn) {
  using ImportedModule = ModuleDecl::ImportedModule;
  using AccessPathTy = ModuleDecl::AccessPathTy;
  
  llvm::SmallSet<ImportedModule, 32, ModuleDecl::OrderImportedModules> visited;
  SmallVector<ImportedModule, 32> stack;

  // Even if we're processing the top-level module like any other, we may
  // still want to include non-exported modules.
  ModuleDecl::ImportFilter filter = respectVisibility ? ModuleDecl::ImportFilter::Public
                                                      : ModuleDecl::ImportFilter::All;
  ModuleDecl::ImportFilter topLevelFilter =
    includePrivateTopLevelImports ? ModuleDecl::ImportFilter::All : filter;
  topLevel->getImportedModules(stack, topLevelFilter);

  // Make sure the top-level module is first; we want pre-order-ish traversal.
  AccessPathTy overridingPath;
  if (respectVisibility)
    overridingPath = thisPath;
  stack.push_back(ImportedModule(overridingPath, topLevel));

  while (!stack.empty()) {
    auto next = stack.pop_back_val();

    // Filter any whole-module imports, and skip specific-decl imports if the
    // import path doesn't match exactly.
    if (next.first.empty() || !respectVisibility)
      next.first = overridingPath;
    else if (!overridingPath.empty() &&
             !ModuleDecl::isSameAccessPath(next.first, overridingPath)) {
      // If we ever allow importing non-top-level decls, it's possible the rule
      // above isn't what we want.
      assert(next.first.size() == 1 && "import of non-top-level decl");
      continue;
    }

    if (!visited.insert(next).second)
      continue;

    if (!fn(next))
      return false;

    if (respectVisibility)
      next.second->getImportedModulesForLookup(stack);
    else
      next.second->getImportedModules(stack, filter);
  }

  return true;
}

bool ModuleDecl::forAllVisibleModules(AccessPathTy thisPath,
                                      bool includePrivateTopLevelImports,
                                  llvm::function_ref<bool(ImportedModule)> fn) {
  return forAllImportedModules<true>(this, thisPath,
                                     includePrivateTopLevelImports, fn);
}

bool FileUnit::forAllVisibleModules(
    llvm::function_ref<bool(ModuleDecl::ImportedModule)> fn) {
  if (!getParentModule()->forAllVisibleModules(ModuleDecl::AccessPathTy(), fn))
    return false;

  if (auto SF = dyn_cast<SourceFile>(this)) {
    // Handle privately visible modules as well.
    // FIXME: Should this apply to all FileUnits?
    SmallVector<ModuleDecl::ImportedModule, 4> imports;
    SF->getImportedModules(imports, ModuleDecl::ImportFilter::Private);
    for (auto importPair : imports)
      if (!importPair.second->forAllVisibleModules(importPair.first, fn))
        return false;
  }

  return true;
}

void ModuleDecl::collectLinkLibraries(LinkLibraryCallback callback) {
  // FIXME: The proper way to do this depends on the decls used.
  FORWARD(collectLinkLibraries, (callback));
}

void
SourceFile::collectLinkLibraries(ModuleDecl::LinkLibraryCallback callback) const {
  for (auto importPair : Imports)
    importPair.first.second->collectLinkLibraries(callback);
}

bool ModuleDecl::walk(ASTWalker &Walker) {
  llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(Walker.Parent, this);
  for (auto SF : getFiles())
    if (SF->walk(Walker))
      return true;
  return false;
}

const clang::Module *ModuleDecl::findUnderlyingClangModule() {
  for (auto *FU : getFiles()) {
    if (auto *Mod = FU->getUnderlyingClangModule())
      return Mod;
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// SourceFile Implementation
//===----------------------------------------------------------------------===//

void SourceFile::print(raw_ostream &OS, const PrintOptions &PO) {
  StreamPrinter Printer(OS);
  print(Printer, PO);
}

void SourceFile::print(ASTPrinter &Printer, const PrintOptions &PO) {
  std::set<DeclKind> MajorDeclKinds = {DeclKind::Class, DeclKind::Enum,
    DeclKind::Extension, DeclKind::Protocol, DeclKind::Struct};
  for (auto decl : Decls) {
    if (!decl->shouldPrintInContext(PO))
      continue;
    // For a major decl, we print an empty line before it.
    if (MajorDeclKinds.find(decl->getKind()) != MajorDeclKinds.end())
      Printer << "\n";
    if (decl->print(Printer, PO))
      Printer << "\n";
  }
}

void SourceFile::addImports(
    ArrayRef<std::pair<ModuleDecl::ImportedModule, ImportOptions>> IM) {
  using ImportPair = std::pair<ModuleDecl::ImportedModule, ImportOptions>;
  if (IM.empty())
    return;
  ASTContext &ctx = getASTContext();
  auto newBuf =
     ctx.AllocateUninitialized<ImportPair>(Imports.size() + IM.size());

  auto iter = newBuf.begin();
  iter = std::uninitialized_copy(Imports.begin(), Imports.end(), iter);
  iter = std::uninitialized_copy(IM.begin(), IM.end(), iter);
  assert(iter == newBuf.end());

  Imports = newBuf;
}

bool SourceFile::hasTestableImport(const swift::ModuleDecl *module) const {
  using ImportPair = std::pair<ModuleDecl::ImportedModule, ImportOptions>;
  return std::any_of(Imports.begin(), Imports.end(),
                     [module](ImportPair importPair) -> bool {
    return importPair.first.second == module &&
        importPair.second.contains(ImportFlags::Testable);
  });
}

void SourceFile::clearLookupCache() {
  if (!Cache)
    return;

  // Abandon any current cache. We'll rebuild it on demand.
  Cache->invalidate();
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

static void performAutoImport(SourceFile &SF,
                              SourceFile::ImplicitModuleImportKind modImpKind) {
  if (SF.Kind == SourceFileKind::SIL)
    assert(modImpKind == SourceFile::ImplicitModuleImportKind::None);

  ASTContext &Ctx = SF.getASTContext();
  ModuleDecl *M = nullptr;

  switch (modImpKind) {
  case SourceFile::ImplicitModuleImportKind::None:
    return;
  case SourceFile::ImplicitModuleImportKind::Builtin:
    M = Ctx.TheBuiltinModule;
    break;
  case SourceFile::ImplicitModuleImportKind::Stdlib:
    M = Ctx.getStdlibModule(true);
    break;
  }

  assert(M && "unable to auto-import module");

  // FIXME: These will be the same for most source files, but we copy them
  // over and over again.
  auto Imports =
    std::make_pair(ModuleDecl::ImportedModule({}, M), SourceFile::ImportOptions());
  SF.addImports(Imports);
}

SourceFile::SourceFile(ModuleDecl &M, SourceFileKind K,
                       Optional<unsigned> bufferID,
                       ImplicitModuleImportKind ModImpKind)
  : FileUnit(FileUnitKind::Source, M),
    BufferID(bufferID ? *bufferID : -1), Kind(K) {
  M.getASTContext().addDestructorCleanup(*this);
  performAutoImport(*this, ModImpKind);

  if (isScriptMode()) {
    bool problem = M.registerEntryPointFile(this, SourceLoc(), None);
    assert(!problem && "multiple main files?");
    (void)problem;
  }
}

SourceFile::~SourceFile() {}

bool FileUnit::walk(ASTWalker &walker) {
  SmallVector<Decl *, 64> Decls;
  getTopLevelDecls(Decls);
  llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(walker.Parent,
                                                getParentModule());
  for (Decl *D : Decls) {
#ifndef NDEBUG
    PrettyStackTraceDecl debugStack("walking into decl", D);
#endif
    
    if (D->walk(walker))
      return true;
  }
  return false;
}

bool SourceFile::walk(ASTWalker &walker) {
  llvm::SaveAndRestore<ASTWalker::ParentTy> SAR(walker.Parent,
                                                getParentModule());
  for (Decl *D : Decls) {
#ifndef NDEBUG
    PrettyStackTraceDecl debugStack("walking into decl", D);
#endif

    if (D->walk(walker))
      return true;
  }
  return false;
}

StringRef SourceFile::getFilename() const {
  if (BufferID == -1)
    return "";
  SourceManager &SM = getASTContext().SourceMgr;
  return SM.getIdentifierForBuffer(BufferID);
}

ASTScope &SourceFile::getScope() {
  if (!Scope) Scope = ASTScope::createRoot(this);
  return *Scope;
}

Identifier
SourceFile::getDiscriminatorForPrivateValue(const ValueDecl *D) const {
  assert(D->getDeclContext()->getModuleScopeContext() == this);

  if (!PrivateDiscriminator.empty())
    return PrivateDiscriminator;

  StringRef name = getFilename();
  if (name.empty()) {
    assert(1 == count_if(getParentModule()->getFiles(),
                         [](const FileUnit *FU) -> bool {
                           return isa<SourceFile>(FU) &&
                                  cast<SourceFile>(FU)->getFilename().empty();
                         }) &&
           "can't promise uniqueness if multiple source files are nameless");

    // We still need a discriminator, so keep going.
  }

  // Use a hash of the basename of the source file as our discriminator.
  // This keeps us from leaking information about the original filename
  // while still providing uniqueness. Using the basename makes the
  // discriminator invariant across source checkout locations.
  // FIXME: Use a faster hash here? We don't need security, just uniqueness.
  llvm::MD5 hash;
  hash.update(getParentModule()->getName().str());
  hash.update(llvm::sys::path::filename(name));
  llvm::MD5::MD5Result result;
  hash.final(result);

  // Make sure the whole thing is a valid identifier.
  SmallString<33> buffer{"_"};

  // Write the hash as a hex string.
  // FIXME: This should go into llvm/ADT/StringExtras.h.
  // FIXME: And there are more compact ways to encode a 16-byte value.
  buffer.reserve(buffer.size() + 2*llvm::array_lengthof(result));
  for (uint8_t byte : result) {
    buffer.push_back(llvm::hexdigit(byte >> 4, /*LowerCase=*/false));
    buffer.push_back(llvm::hexdigit(byte & 0xF, /*LowerCase=*/false));
  }

  PrivateDiscriminator = getASTContext().getIdentifier(buffer);
  return PrivateDiscriminator;
}

TypeRefinementContext *SourceFile::getTypeRefinementContext() {
  return TRC;
}

void SourceFile::setTypeRefinementContext(TypeRefinementContext *Root) {
  TRC = Root;
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

static const clang::Module *
getClangModule(llvm::PointerUnion<const ModuleDecl *, const void *> Union) {
  return static_cast<const clang::Module *>(Union.get<const void *>());
}

StringRef ModuleEntity::getName() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const ModuleDecl*>())
    return SwiftMod->getName().str();
  return getClangModule(Mod)->Name;
}

std::string ModuleEntity::getFullName() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const ModuleDecl*>())
    return SwiftMod->getName().str();
  return getClangModule(Mod)->getFullModuleName();
}

bool ModuleEntity::isSystemModule() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const ModuleDecl*>())
    return SwiftMod->isSystemModule();
  return getClangModule(Mod)->IsSystem;
}

bool ModuleEntity::isBuiltinModule() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const ModuleDecl*>())
    return SwiftMod->isBuiltinModule();
  return false;
}

const ModuleDecl* ModuleEntity::getAsSwiftModule() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const ModuleDecl*>())
    return SwiftMod;
  return nullptr;
}

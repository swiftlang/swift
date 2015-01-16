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
#include "swift/AST/ASTPrinter.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ReferencedNameTracker.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/TypeRefinementContext.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/Module.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringExtras.h"
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
  if (!Cache) {
    const_cast<BuiltinUnit *>(this)->Cache = new LookupCache();
    getASTContext().addDestructorCleanup(*Cache);
  }
  return *Cache;
}

void BuiltinUnit::LookupCache::lookupValue(Identifier Name, NLKind LookupKind,
                                           const BuiltinUnit &M,
                                           SmallVectorImpl<ValueDecl*> &Result) {
  // Only qualified lookup ever finds anything in the builtin module.
  if (LookupKind != NLKind::QualifiedLookup) return;
  
  ValueDecl *&Entry = Cache[Name];
  ASTContext &Ctx = M.getParentModule()->Ctx;
  if (Entry == 0) {
    if (Type Ty = getBuiltinType(Ctx, Name.str())) {
      Entry = new (Ctx) TypeAliasDecl(SourceLoc(), Name, SourceLoc(),
                                      TypeLoc::withoutLoc(Ty),
                                      const_cast<BuiltinUnit*>(&M));
      Entry->setAccessibility(Accessibility::Public);
    }
  }

  if (Entry == 0)
    Entry = getBuiltinValueDecl(Ctx, Name);

  if (Entry)
    Result.push_back(Entry);
}

// Out-of-line because std::unique_ptr wants LookupCache to be complete.
BuiltinUnit::BuiltinUnit(Module &M)
   : FileUnit(FileUnitKind::Builtin, M) {
  M.Ctx.addDestructorCleanup(*this);
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

    decltype(Members)::const_iterator begin() const  { return Members.begin(); }
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
  typedef Module::AccessPathTy AccessPathTy;
  
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
    const_cast<SourceFile *>(this)->Cache = new SourceLookupCache(*this);
    getASTContext().addDestructorCleanup(*Cache);
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
  if (!Module::matchesAccessPath(AccessPath, Name))
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

Module::Module(Identifier name, ASTContext &ctx)
    : DeclContext(DeclContextKind::Module, nullptr), Ctx(ctx), Name(name),
      DiagnosedMultipleMainClasses(false),
      DiagnosedMainClassWithScript(false)
{
  ctx.addDestructorCleanup(*this);
}


void Module::addFile(FileUnit &newFile) {
  assert(!isa<DerivedFileUnit>(newFile) &&
         "DerivedFileUnits are added automatically");

  // Require Main and REPL files to be the first file added.
  assert(Files.empty() ||
         !isa<SourceFile>(newFile) ||
         cast<SourceFile>(newFile).Kind == SourceFileKind::Library ||
         cast<SourceFile>(newFile).Kind == SourceFileKind::SIL);
  Files.push_back(&newFile);

  switch (newFile.getKind()) {
  case FileUnitKind::Source:
  case FileUnitKind::ClangModule: {
    for (auto File : Files) {
      if (isa<DerivedFileUnit>(File))
        return;
    }
    auto DFU = new (Ctx) DerivedFileUnit(*this);
    Files.push_back(DFU);
    break;
  }

  case FileUnitKind::Builtin:
  case FileUnitKind::SerializedAST:
    break;

  case FileUnitKind::Derived:
    llvm_unreachable("DerivedFileUnits are added automatically");
  }
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

DerivedFileUnit &Module::getDerivedFileUnit() const {
  for (auto File : Files) {
    if (auto DFU = dyn_cast<DerivedFileUnit>(File))
      return *DFU;
  }
  llvm_unreachable("the client should not be calling this function if "
                   "there is no DerivedFileUnit");
}

VarDecl *Module::getDSOHandle() {
  if (DSOHandle)
    return DSOHandle;

  auto unsafeMutablePtr = Ctx.getUnsafeMutablePointerDecl();
  if (!unsafeMutablePtr)
    return nullptr;

  Type arg;
  if (auto voidDecl = Ctx.getVoidDecl()) {
    arg = voidDecl->getDeclaredInterfaceType();
  } else {
    arg = TupleType::getEmpty(Ctx);
  }
  
  Type type = BoundGenericType::get(unsafeMutablePtr, Type(), { arg });
  DSOHandle = new (Ctx) VarDecl(/*IsStatic=*/false, /*IsLet=*/false,
                                SourceLoc(), 
                                Ctx.getIdentifier("__dso_handle"),
                                type, Files[0]);
  DSOHandle->setImplicit(true);
  DSOHandle->getAttrs().add(
    new (Ctx) AsmnameAttr("__dso_handle", /*Implicit=*/true));
  return DSOHandle;
}

#define FORWARD(name, args) \
  for (const FileUnit *file : getFiles()) \
    file->name args;

void Module::lookupValue(AccessPathTy AccessPath, DeclName Name,
                         NLKind LookupKind, 
                         SmallVectorImpl<ValueDecl*> &Result) const {
  FORWARD(lookupValue, (AccessPath, Name, LookupKind, Result));
}

void Module::lookupMember(SmallVectorImpl<ValueDecl*> &results,
                          DeclContext *container, DeclName name,
                          Identifier privateDiscriminator) const {
  size_t oldSize = results.size();
  bool alreadyInPrivateContext = false;

  switch (container->getContextKind()) {
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::Initializer:
  case DeclContextKind::TopLevelCodeDecl:
  case DeclContextKind::AbstractFunctionDecl:
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

  case DeclContextKind::NominalTypeDecl: {
    auto nominal = cast<NominalTypeDecl>(container);
    auto lookupResults = nominal->lookupDirect(name);

    // Filter out declarations from other modules.
    std::copy_if(lookupResults.begin(), lookupResults.end(),
                 std::back_inserter(results),
                 [this](const ValueDecl *VD) -> bool {
      return VD->getModuleContext() == this;
    });

    alreadyInPrivateContext =
      (nominal->getAccessibility() == Accessibility::Private);

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
      return VD->getAccessibility() <= Accessibility::Private;
    });
    results.erase(newEnd, results.end());

  } else {
    auto newEnd = std::remove_if(results.begin()+oldSize, results.end(),
                                 [=](const ValueDecl *VD) -> bool {
      if (VD->getAccessibility() > Accessibility::Private)
        return true;
      auto enclosingFile =
        cast<FileUnit>(VD->getDeclContext()->getModuleScopeContext());
      auto discriminator = enclosingFile->getDiscriminatorForPrivateValue(VD);
      return discriminator != privateDiscriminator;
    });
    results.erase(newEnd, results.end());
  }
}

void BuiltinUnit::lookupValue(Module::AccessPathTy accessPath, DeclName name,
                              NLKind lookupKind,
                              SmallVectorImpl<ValueDecl*> &result) const {
  getCache().lookupValue(name.getBaseName(), lookupKind, *this, result);
}

DerivedFileUnit::DerivedFileUnit(Module &M)
    : FileUnit(FileUnitKind::Derived, M) {
  M.Ctx.addDestructorCleanup(*this);
}

void DerivedFileUnit::lookupValue(Module::AccessPathTy accessPath,
                                  DeclName name,
                                  NLKind lookupKind,
                                  SmallVectorImpl<ValueDecl*> &result) const {
  // If this import is specific to some named type or decl ("import Swift.int")
  // then filter out any lookups that don't match.
  if (!Module::matchesAccessPath(accessPath, name))
    return;
  
  for (auto D : DerivedDecls) {
    if (D->getFullName().matchesRef(name))
      result.push_back(D);
  }
}

void DerivedFileUnit::lookupVisibleDecls(Module::AccessPathTy accessPath,
                                         VisibleDeclConsumer &consumer,
                                         NLKind lookupKind) const {
  assert(accessPath.size() <= 1 && "can only refer to top-level decls");
  
  Identifier Id;
  if (!accessPath.empty()) {
    Id = accessPath.front().first;
  }

  for (auto D : DerivedDecls) {
    if (Id.empty() || D->getName() == Id)
      consumer.foundDecl(D, DeclVisibilityKind::VisibleAtTopLevel);
  }
}

void DerivedFileUnit::getTopLevelDecls(SmallVectorImpl<swift::Decl *> &results)
const {
  results.append(DerivedDecls.begin(), DerivedDecls.end());
}

void SourceFile::lookupValue(Module::AccessPathTy accessPath, DeclName name,
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
                               DeclName name,
                               SmallVectorImpl<ValueDecl*> &results) const {
  FORWARD(lookupClassMember, (accessPath, name, results));
}

void SourceFile::lookupClassMember(Module::AccessPathTy accessPath,
                                   DeclName name,
                                   SmallVectorImpl<ValueDecl*> &results) const {
  getCache().lookupClassMember(accessPath, name, results, *this);
}

void Module::getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const {
  FORWARD(getTopLevelDecls, (Results));
}

void SourceFile::getTopLevelDecls(SmallVectorImpl<Decl*> &Results) const {
  Results.append(Decls.begin(), Decls.end());
}

void Module::getDisplayDecls(SmallVectorImpl<Decl*> &Results) const {
  // FIXME: Should this do extra access control filtering?
  FORWARD(getDisplayDecls, (Results));
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
    auto archetype = gp->getArchetype();
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
    if (type->is<TypeVariableType>() || type->isDependentType()) {
      // If the type is a type variable or is dependent, just fill in null
      // conformances. FIXME: It seems like we should record these as
      // requirements (?).
      conformances.assign(archetype->getConformsTo().size(), nullptr);
    } else {
      // Find the conformances.
      for (auto proto : archetype->getConformsTo()) {
        auto conforms = module->lookupConformance(type, proto, resolver);
        switch (conforms.getInt()) {
        case ConformanceKind::Conforms:
          conformances.push_back(conforms.getPointer());
          break;
        case ConformanceKind::UncheckedConforms:
          conformances.push_back(nullptr);
          break;
        case ConformanceKind::DoesNotConform:
          llvm_unreachable("Couldn't find conformance");
        }
      }
    }

    // Record this substitution.
    resultSubstitutions[index] = {archetype, type,
                                  ctx.AllocateCopy(conformances)};
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

/// Retrieve the explicit conformance of the given nominal type declaration
/// to the given protocol.
static std::tuple<NominalTypeDecl *, Decl *, ProtocolConformance *>
findExplicitConformance(NominalTypeDecl *nominal, ProtocolDecl *protocol,
                        LazyResolver *resolver) {
  // FIXME: Introduce a cache/lazy lookup structure to make this more efficient?

  using NominalOrConformance =
    llvm::PointerUnion<NominalTypeDecl *, ProtocolConformance *>;

  if (!nominal->hasType())
    resolver->resolveDeclSignature(nominal);

  // Walk the nominal type, its extensions, superclasses, and so on.
  llvm::SmallPtrSet<ProtocolDecl *, 4> visitedProtocols;
  SmallVector<std::pair<NominalOrConformance, Decl *>, 4> stack;
  Decl *declaresConformance = nullptr;
  ProtocolConformance *foundConformance = nullptr;

  // Local function that checks for our protocol in the given array of
  // protocols.
  auto isProtocolInList
    = [&](Decl *currentOwner,
          ArrayRef<ProtocolDecl *> protocols,
          ArrayRef<ProtocolConformance *> nominalConformances) -> bool {
      for (unsigned i = 0, n = protocols.size(); i != n; ++i) {
        auto testProto = protocols[i];
        if (testProto == protocol) {
          declaresConformance = currentOwner;
          if (i < nominalConformances.size())
            foundConformance = nominalConformances[i];
          return true;
        }

        if (visitedProtocols.insert(testProto).second) {
          NominalOrConformance next = {};
          if (i < nominalConformances.size())
            next = nominalConformances[i];
          if (next.isNull())
            next = testProto;
          stack.push_back({next, currentOwner});
        }
      }

      return false;
    };

  // Walk the stack of types to find a conformance.
  stack.push_back({nominal, nominal});
  while (!stack.empty()) {
    NominalOrConformance current;
    Decl *currentOwner;
    std::tie(current, currentOwner) = stack.pop_back_val();
    assert(!current.isNull());

    if (auto currentNominal = current.dyn_cast<NominalTypeDecl *>()) {
      // Visit the superclass of a class.
      if (auto classDecl = dyn_cast<ClassDecl>(currentNominal)) {
        if (auto superclassTy = classDecl->getSuperclass()) {
          auto super = superclassTy->getAnyNominal();
          stack.push_back({super, super});
        }
      }

      // Visit the protocols this type conforms to directly.
      if (isProtocolInList(currentOwner,
                           currentNominal->getProtocols(),
                           currentNominal->getConformances()))
        break;

      // Visit the extensions of this type.
      for (auto ext : currentNominal->getExtensions()) {
        if (resolver)
          resolver->resolveExtension(ext);

        if (isProtocolInList(ext, ext->getProtocols(), ext->getConformances())) {
          // Break outer loop as well.
          stack.clear();
          break;
        }
      }
    } else {
      auto currentConformance = current.get<ProtocolConformance *>();
      for (auto inherited : currentConformance->getInheritedConformances()) {
        if (inherited.first == protocol) {
          declaresConformance = currentOwner;
          foundConformance = inherited.second;
          // Break outer loop as well.
          stack.clear();
          break;
        }

        if (visitedProtocols.insert(inherited.first).second)
          stack.push_back({inherited.second, currentOwner});
      }
    }
  }

  // If we didn't find the protocol, we don't conform. Cache the negative result
  // and return.
  if (!declaresConformance)
    return std::make_tuple(nullptr, nullptr, nullptr);

  NominalTypeDecl *owningNominal;
  if (auto ext = dyn_cast<ExtensionDecl>(declaresConformance))
    owningNominal = ext->getExtendedType()->getAnyNominal();
  else
    owningNominal = cast<NominalTypeDecl>(declaresConformance);
  assert(owningNominal);

  // If we don't have a nominal conformance, but we do have a resolver, try
  // to resolve the nominal conformance now.
  if (!foundConformance && resolver) {
    foundConformance = resolver->resolveConformance(
                           owningNominal,
                           protocol,
                           dyn_cast<ExtensionDecl>(declaresConformance));
  }

  // If we have a nominal conformance, we're done.
  if (foundConformance) {
    return std::make_tuple(owningNominal, declaresConformance,
                           foundConformance);
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
    if (protocol->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
      if (archetype->requiresClass())
        return { nullptr, ConformanceKind::Conforms };
      return { nullptr, ConformanceKind::DoesNotConform };
    }

    for (auto ap : archetype->getConformsTo()) {
      if (ap == protocol || ap->inheritsFrom(protocol))
        return { nullptr, ConformanceKind::Conforms };
    }

    return { nullptr, ConformanceKind::DoesNotConform };
  }

  // An existential conforms to a protocol if the protocol is listed in the
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

    // Special-case AnyObject, which may not be in the list of conformances.
    if (protocol->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
      if (type->isClassExistentialType()) {
        return { nullptr, known ? ConformanceKind::Conforms
                                : ConformanceKind::UncheckedConforms };
      }
      return { nullptr, ConformanceKind::DoesNotConform };
    }

    // Look for this protocol within the existential's list of conformances.
    SmallVector<ProtocolDecl *, 4> protocols;
    type->getAnyExistentialTypeProtocols(protocols);
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
  CanType canType = type->getCanonicalType();
  if (auto entry = ctx.getConformsTo(canType, protocol)) {
    // If we conform, return the conformance.
    if (entry->getInt()) {
      return { entry->getPointer(), ConformanceKind::Conforms };
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
    ctx.setConformsTo(canType, protocol, ConformanceEntry(nullptr, false));
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
    ctx.setConformsTo(canType, protocol, ConformanceEntry(result, true));
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
        ->getExtendedType()->getAnyNominal()->getDeclaredTypeInContext();
    }

    // If the explicit conformance is associated with a type that is different
    // from the type we're checking, retrieve generic conformance.
    if (!explicitConformanceType->isEqual(type)) {
      // Gather the substitutions we need to map the generic conformance to
      // the specialized conformance.
      SmallVector<Substitution, 4> substitutionsVec;
      auto substitutions = type->gatherAllSubstitutions(this, substitutionsVec,
                                                        resolver);

      // Create the specialized conformance entry.
      ctx.setConformsTo(canType, protocol, ConformanceEntry(nullptr, false));
      auto result = ctx.getSpecializedConformance(type, nominalConformance,
                                                  substitutions);
      ctx.setConformsTo(canType, protocol, ConformanceEntry(result, true));
      return { result, ConformanceKind::Conforms };
    }
  }

  // Record and return the simple conformance.
  ctx.setConformsTo(canType, protocol, 
                    ConformanceEntry(nominalConformance, true));
  return { nominalConformance, ConformanceKind::Conforms };
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
      return None;
    
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
        return None;
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
      return None;
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
SourceFile::lookup##Kind##Operator(Identifier name, bool isCascading, \
                                   SourceLoc loc) { \
  auto result = lookupOperatorDeclForName(*this, loc, name, true, \
                                          &SourceFile::Kind##Operators); \
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
                                       &SourceFile::Kind##Operators); \
  } \
  return result.hasValue() ? result.getValue() : nullptr; \
}

LOOKUP_OPERATOR(Prefix)
LOOKUP_OPERATOR(Infix)
LOOKUP_OPERATOR(Postfix)
#undef LOOKUP_OPERATOR

void Module::getImportedModules(SmallVectorImpl<ImportedModule> &modules,
                                Module::ImportFilter filter) const {
  // FIXME: Audit uses of this function and make sure they make sense in a
  // multi-file world.
  FORWARD(getImportedModules, (modules, filter));
}

void
SourceFile::getImportedModules(SmallVectorImpl<Module::ImportedModule> &modules,
                               Module::ImportFilter filter) const {
  for (auto importPair : getImports())
    if (filter == Module::ImportFilter::All ||
        (filter == Module::ImportFilter::Private) ^ importPair.second)
      modules.push_back(importPair.first);
}

bool Module::isSameAccessPath(AccessPathTy lhs, AccessPathTy rhs) {
  using AccessPathElem = std::pair<Identifier, SourceLoc>;
  if (lhs.size() != rhs.size())
    return false;
  return std::equal(lhs.begin(), lhs.end(), rhs.begin(),
                    [](const AccessPathElem &lElem,
                       const AccessPathElem &rElem) {
    return lElem.first == rElem.first;
  });
}

StringRef Module::getModuleFilename() const {
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
    if (isa<DerivedFileUnit>(F))
      continue;
    return StringRef();
  }
  return Result;
}

bool Module::isStdlibModule() const {
  return !getParent() && Name == Ctx.StdlibModuleName;
}

bool Module::isBuiltinModule() const {
  return this == Ctx.TheBuiltinModule;
}

bool Module::registerMainClass(ClassDecl *mainClass, SourceLoc diagLoc) {
  // %select indices for UI/NSApplication-related diagnostics.
  enum : unsigned {
    UIApplicationMainClass = 0,
    NSApplicationMainClass = 1,
  };
  
  unsigned mainClassDiagKind;
  if (mainClass->getAttrs().hasAttribute<UIApplicationMainAttr>())
    mainClassDiagKind = UIApplicationMainClass;
  else if (mainClass->getAttrs().hasAttribute<NSApplicationMainAttr>())
    mainClassDiagKind = NSApplicationMainClass;
  else
    llvm_unreachable("main class has no @ApplicationMain attribute?!");
  
  if (mainClass == MainClass)
    return false;
  
  if (MainClass) {
    // If we already have a main class, and we haven't diagnosed it, do so now.
    if (!DiagnosedMultipleMainClasses) {
      getASTContext().Diags.diagnose(MainClassDiagLoc,
                                     diag::attr_ApplicationMain_multiple,
                                     mainClassDiagKind);
      DiagnosedMultipleMainClasses = true;
    }
    getASTContext().Diags.diagnose(diagLoc,
                                   diag::attr_ApplicationMain_multiple,
                                   mainClassDiagKind);
    return true;
  }
  
  // Complain if there is also a script file in this module.
  if (!DiagnosedMainClassWithScript) {
    DiagnosedMainClassWithScript = true;
    for (auto file : getFiles()) {
      auto sf = dyn_cast<SourceFile>(file);
      if (!sf)
        continue;
      if (sf->isScriptMode()) {
        getASTContext().Diags.diagnose(diagLoc,
                                     diag::attr_ApplicationMain_with_script,
                                     mainClassDiagKind);
        // Note the source file we're reading top-level code from.
        if (auto bufID = sf->getBufferID()) {
          auto fileLoc = getASTContext().SourceMgr.getLocForBufferStart(*bufID);
          getASTContext().Diags.diagnose(fileLoc,
                                     diag::attr_ApplicationMain_script_here,
                                     mainClassDiagKind);
        }
        break;
      }
    }
  }
  
  MainClass = mainClass;
  MainClassDiagLoc = diagLoc;
  return false;
}

bool Module::isSystemModule() const {
  if (isStdlibModule())
    return true;
  for (auto F : getFiles()) {
    if (auto LF = dyn_cast<LoadedFile>(F))
      return LF->isSystemModule();
  }
  return false;
}

template<bool respectVisibility, typename Callback>
static bool forAllImportedModules(Module *topLevel,
                                  Module::AccessPathTy thisPath,
                                  bool includePrivateTopLevelImports,
                                  const Callback &fn) {
  using ImportedModule = Module::ImportedModule;
  using AccessPathTy = Module::AccessPathTy;
  
  llvm::SmallSet<ImportedModule, 32, Module::OrderImportedModules> visited;
  SmallVector<ImportedModule, 32> stack;

  // Even if we're processing the top-level module like any other, we may
  // still want to include non-exported modules.
  Module::ImportFilter filter = respectVisibility ? Module::ImportFilter::Public
                                                  : Module::ImportFilter::All;
  Module::ImportFilter topLevelFilter =
    includePrivateTopLevelImports ? Module::ImportFilter::All : filter;
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
             !Module::isSameAccessPath(next.first, overridingPath)) {
      // If we ever allow importing non-top-level decls, it's possible the rule
      // above isn't what we want.
      assert(next.first.size() == 1 && "import of non-top-level decl");
      continue;
    }

    if (!visited.insert(next).second)
      continue;

    if (!fn(next))
      return false;
    next.second->getImportedModules(stack, filter);
  }

  return true;
}

bool Module::forAllVisibleModules(AccessPathTy thisPath,
                                  bool includePrivateTopLevelImports,
                                  llvm::function_ref<bool(ImportedModule)> fn) {
  return forAllImportedModules<true>(this, thisPath,
                                     includePrivateTopLevelImports, fn);
}

bool FileUnit::forAllVisibleModules(
    llvm::function_ref<bool(Module::ImportedModule)> fn) {
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

const clang::Module *Module::findUnderlyingClangModule() {
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
  for (auto decl : Decls) {
    if (!decl->shouldPrintInContext(PO))
      continue;

    decl->print(Printer, PO);
    Printer << "\n";
  }
}

void SourceFile::clearLookupCache() {
  if (!Cache)
    return;

  // Abandon any current cache. We'll rebuild it on demand.
  Cache->invalidate();
  Cache = nullptr;
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
  Module *M = nullptr;

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
  std::pair<Module::ImportedModule, bool> Imports[] = {
    std::make_pair(Module::ImportedModule({}, M), false)
  };
  SF.setImports(Ctx.AllocateCopy(Imports));
}

SourceFile::SourceFile(Module &M, SourceFileKind K,
                       Optional<unsigned> bufferID,
                       ImplicitModuleImportKind ModImpKind)
  : FileUnit(FileUnitKind::Source, M),
    BufferID(bufferID ? *bufferID : -1), Kind(K) {
  M.Ctx.addDestructorCleanup(*this);
  performAutoImport(*this, ModImpKind);
      
  // The root type refinement context reflects the fact that all parts of
  // the source file are guaranteed to be executing on at least the minimum
  // platform version.
  auto VersionRange =
    VersionRange::allGTE(M.Ctx.LangOpts.getMinPlatformVersion());
  TRC = TypeRefinementContext::createRoot(M.Ctx, this, VersionRange);
}

SourceFile::~SourceFile() {}

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
  return SM.getIdentifierForBuffer(BufferID);
}

bool SourceFile::hasMainClass() const {
  auto mainClass = getParentModule()->getMainClass();
  if (!mainClass) return false;
  return mainClass->getParentSourceFile() == this;
}

ArtificialMainKind SourceFile::getArtificialMainKind() const {
  if (hasMainClass()) {
    auto &attrs = getParentModule()->getMainClass()->getAttrs();
    if (attrs.hasAttribute<UIApplicationMainAttr>())
      return ArtificialMainKind::UIApplicationMain;
    if (attrs.hasAttribute<NSApplicationMainAttr>())
      return ArtificialMainKind::NSApplicationMain;
    llvm_unreachable("main class has no @ApplicationMain attr?!");
  }
  return ArtificialMainKind::None;
}

Identifier
SourceFile::getDiscriminatorForPrivateValue(const ValueDecl *D) const {
  assert(D->getDeclContext()->getModuleScopeContext() == this);

  if (!PrivateDiscriminator.empty())
    return PrivateDiscriminator;

  StringRef name = getFilename();
  if (name.empty()) {
    assert(1 == std::count_if(getParentModule()->getFiles().begin(),
                              getParentModule()->getFiles().end(),
                              [](const FileUnit *FU) -> bool {
      return isa<SourceFile>(FU) && cast<SourceFile>(FU)->getFilename().empty();
    }) && "can't promise uniqueness if multiple source files are nameless");

    // We still need a discriminator, so keep going.
  }

  // Use a hash of the basename of the source file as our discriminator.
  // This keeps us from leaking information about the original filename
  // while still providing uniqueness. Using the basename makes the
  // discriminator invariant across source checkout locations.
  // FIXME: Use a faster hash here? We don't need security, just uniqueness.
  llvm::MD5 hash;
  hash.update(getParentModule()->Name.str());
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
    buffer.push_back(llvm::hexdigit(byte >> 4, /*lowercase=*/false));
    buffer.push_back(llvm::hexdigit(byte & 0xF, /*lowercase=*/false));
  }

  PrivateDiscriminator = getASTContext().getIdentifier(buffer);
  return PrivateDiscriminator;
}

TypeRefinementContext *SourceFile::getTypeRefinementContext() {
  return TRC;
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

StringRef ModuleEntity::getName() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const Module*>())
    return SwiftMod->getName().str();
  return Mod.get<const clang::Module*>()->Name;
}

std::string ModuleEntity::getFullName() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const Module*>())
    return SwiftMod->getName().str();
  return Mod.get<const clang::Module*>()->getFullModuleName();
}

bool ModuleEntity::isSystemModule() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const Module*>())
    return SwiftMod->isSystemModule();
  return Mod.get<const clang::Module*>()->IsSystem;
}

bool ModuleEntity::isBuiltinModule() const {
  assert(!Mod.isNull());
  if (auto SwiftMod = Mod.dyn_cast<const Module*>())
    return SwiftMod->isBuiltinModule();
  return false;
}

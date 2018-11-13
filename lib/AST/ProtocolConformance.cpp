//===--- ProtocolConformance.cpp - AST Protocol Conformance ---------------===//
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
// This file implements the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//

#include "ConformanceLookupTable.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Statistic.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/SaveAndRestore.h"

#define DEBUG_TYPE "AST"

STATISTIC(NumConformanceLookupTables, "# of conformance lookup tables built");

using namespace swift;

Witness::Witness(ValueDecl *decl, SubstitutionMap substitutions,
                 GenericEnvironment *syntheticEnv,
                 SubstitutionMap reqToSynthesizedEnvSubs) {
  if (!syntheticEnv && substitutions.empty() &&
      reqToSynthesizedEnvSubs.empty()) {
    storage = decl;
    return;
  }

  auto &ctx = decl->getASTContext();
  auto declRef = ConcreteDeclRef(decl, substitutions);
  auto storedMem = ctx.Allocate(sizeof(StoredWitness), alignof(StoredWitness));
  auto stored = new (storedMem) StoredWitness{declRef, syntheticEnv,
                                              reqToSynthesizedEnvSubs};

  storage = stored;
}

void Witness::dump() const { dump(llvm::errs()); }

void Witness::dump(llvm::raw_ostream &out) const {
  // FIXME: Implement!
}

ProtocolConformanceRef::ProtocolConformanceRef(ProtocolDecl *protocol,
                                               ProtocolConformance *conf) {
  assert(protocol != nullptr &&
         "cannot construct ProtocolConformanceRef with null protocol");
  if (conf) {
    assert(protocol == conf->getProtocol() && "protocol conformance mismatch");
    Union = conf;
  } else {
    Union = protocol;
  }
}

ProtocolDecl *ProtocolConformanceRef::getRequirement() const {
  assert(!isInvalid());

  if (isConcrete()) {
    return getConcrete()->getProtocol();
  } else {
    return getAbstract();
  }
}

ProtocolConformanceRef
ProtocolConformanceRef::subst(Type origType,
                              SubstitutionMap subMap) const {
  return subst(origType,
               QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap));
}

ProtocolConformanceRef
ProtocolConformanceRef::subst(Type origType,
                              TypeSubstitutionFn subs,
                              LookupConformanceFn conformances) const {
  if (isInvalid())
    return *this;

  // If we have a concrete conformance, we need to substitute the
  // conformance to apply to the new type.
  if (isConcrete())
    return ProtocolConformanceRef(getConcrete()->subst(subs, conformances));

  // Otherwise, compute the substituted type.
  auto substType = origType.subst(subs, conformances,
                                  SubstFlags::UseErrorType);

  // Opened existentials trivially conform and do not need to go through
  // substitution map lookup.
  if (substType->isOpenedExistential())
    return *this;

  // If the substituted type is an existential, we have a self-conforming
  // existential being substituted in place of itself. There's no
  // conformance information in this case, so just return.
  if (substType->isObjCExistentialType())
    return *this;

  auto *proto = getRequirement();

  // Check the conformance map.
  if (auto result = conformances(origType->getCanonicalType(),
                                 substType, proto)) {
    return *result;
  }

  llvm_unreachable("Invalid conformance substitution");
}

Type
ProtocolConformanceRef::getTypeWitnessByName(Type type,
                                             ProtocolConformanceRef conformance,
                                             Identifier name,
                                             LazyResolver *resolver) {
  assert(!conformance.isInvalid());

  // Find the named requirement.
  AssociatedTypeDecl *assocType = nullptr;
  auto members = conformance.getRequirement()->lookupDirect(name);
  for (auto member : members) {
    assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (assocType)
      break;
  }

  // FIXME: Shouldn't this be a hard error?
  if (!assocType)
    return nullptr;

  if (conformance.isAbstract()) {
    // For an archetype, retrieve the nested type with the appropriate
    // name. There are no conformance tables.
    if (auto archetype = type->getAs<ArchetypeType>()) {
      return archetype->getNestedType(name);
    }

    return DependentMemberType::get(type, assocType);
  }

  auto concrete = conformance.getConcrete();
  if (!concrete->hasTypeWitness(assocType, resolver)) {
    return nullptr;
  }
  return concrete->getTypeWitness(assocType, resolver);
}

void *ProtocolConformance::operator new(size_t bytes, ASTContext &context,
                                        AllocationArena arena,
                                        unsigned alignment) {
  return context.Allocate(bytes, alignment, arena);

}

#define CONFORMANCE_SUBCLASS_DISPATCH(Method, Args)                          \
switch (getKind()) {                                                         \
  case ProtocolConformanceKind::Normal:                                      \
    static_assert(&ProtocolConformance::Method !=                            \
                    &NormalProtocolConformance::Method,                      \
                  "Must override NormalProtocolConformance::" #Method);      \
    return cast<NormalProtocolConformance>(this)->Method Args;               \
  case ProtocolConformanceKind::Specialized:                                 \
    static_assert(&ProtocolConformance::Method !=                            \
                    &SpecializedProtocolConformance::Method,                 \
                  "Must override SpecializedProtocolConformance::" #Method); \
    return cast<SpecializedProtocolConformance>(this)->Method Args;          \
  case ProtocolConformanceKind::Inherited:                                   \
    static_assert(&ProtocolConformance::Method !=                            \
                    &InheritedProtocolConformance::Method,                   \
                  "Must override InheritedProtocolConformance::" #Method);   \
    return cast<InheritedProtocolConformance>(this)->Method Args;            \
}                                                                            \
llvm_unreachable("bad ProtocolConformanceKind");

/// Get the protocol being conformed to.
ProtocolDecl *ProtocolConformance::getProtocol() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getProtocol, ())
}

DeclContext *ProtocolConformance::getDeclContext() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getDeclContext, ())
}

/// Retrieve the state of this conformance.
ProtocolConformanceState ProtocolConformance::getState() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getState, ())
}

ConformanceEntryKind ProtocolConformance::getSourceKind() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getSourceKind, ())
}
NormalProtocolConformance *ProtocolConformance::getImplyingConformance() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getImplyingConformance, ())
}

bool
ProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                    LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(hasTypeWitness, (assocType, resolver));
}

std::pair<Type, TypeDecl *>
ProtocolConformance::getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                                           LazyResolver *resolver,
                                           SubstOptions options) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getTypeWitnessAndDecl,
                                (assocType, resolver, options))
}

Type ProtocolConformance::getTypeWitness(AssociatedTypeDecl *assocType,
                                         LazyResolver *resolver,
                                         SubstOptions options) const {
  return getTypeWitnessAndDecl(assocType, resolver, options).first;
}

ConcreteDeclRef
ProtocolConformance::getWitnessDeclRef(ValueDecl *requirement,
                                       LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getWitnessDeclRef, (requirement, resolver))
}

ValueDecl *ProtocolConformance::getWitnessDecl(ValueDecl *requirement,
                                               LazyResolver *resolver) const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    return cast<NormalProtocolConformance>(this)->getWitness(requirement,
                                                             resolver)
      .getDecl();

  case ProtocolConformanceKind::Inherited:
    return cast<InheritedProtocolConformance>(this)
      ->getInheritedConformance()->getWitnessDecl(requirement, resolver);

  case ProtocolConformanceKind::Specialized:
    return cast<SpecializedProtocolConformance>(this)
      ->getGenericConformance()->getWitnessDecl(requirement, resolver);
  }
  llvm_unreachable("unhandled kind");
}

/// Determine whether the witness for the given requirement
/// is either the default definition or was otherwise deduced.
bool ProtocolConformance::
usesDefaultDefinition(AssociatedTypeDecl *requirement) const {
  CONFORMANCE_SUBCLASS_DISPATCH(usesDefaultDefinition, (requirement))
}

GenericEnvironment *ProtocolConformance::getGenericEnvironment() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Inherited:
  case ProtocolConformanceKind::Normal:
    // If we have a normal or inherited protocol conformance, look for its
    // generic parameters.
    return getDeclContext()->getGenericEnvironmentOfContext();

  case ProtocolConformanceKind::Specialized:
    // If we have a specialized protocol conformance, since we do not support
    // currently partial specialization, we know that it cannot have any open
    // type variables.
    //
    // FIXME: We could return a meaningful GenericEnvironment here
    return nullptr;
  }

  llvm_unreachable("Unhandled ProtocolConformanceKind in switch.");
}

GenericSignature *ProtocolConformance::getGenericSignature() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Inherited:
  case ProtocolConformanceKind::Normal:
    // If we have a normal or inherited protocol conformance, look for its
    // generic signature.
    return getDeclContext()->getGenericSignatureOfContext();

  case ProtocolConformanceKind::Specialized:
    // If we have a specialized protocol conformance, since we do not support
    // currently partial specialization, we know that it cannot have any open
    // type variables.
    return nullptr;
  }

  llvm_unreachable("Unhandled ProtocolConformanceKind in switch.");
}

SubstitutionMap ProtocolConformance::getSubstitutions(ModuleDecl *M) const {
  // Walk down to the base NormalProtocolConformance.
  SubstitutionMap subMap;
  const ProtocolConformance *parent = this;
  while (!isa<NormalProtocolConformance>(parent)) {
    switch (parent->getKind()) {
    case ProtocolConformanceKind::Normal:
      llvm_unreachable("should have exited the loop?!");
    case ProtocolConformanceKind::Inherited:
      parent =
          cast<InheritedProtocolConformance>(parent)->getInheritedConformance();
      break;
    case ProtocolConformanceKind::Specialized: {
      auto SC = cast<SpecializedProtocolConformance>(parent);
      parent = SC->getGenericConformance();
      assert(subMap.empty() && "multiple conformance specializations?!");
      subMap = SC->getSubstitutionMap();
      break;
    }
    }
  }

  // Found something; we're done!
  if (!subMap.empty())
    return subMap;

  // If the normal conformance is for a generic type, and we didn't hit a
  // specialized conformance, collect the substitutions from the generic type.
  // FIXME: The AST should do this for us.
  const NormalProtocolConformance *normalC =
      cast<NormalProtocolConformance>(parent);

  if (!normalC->getType()->isSpecialized())
    return SubstitutionMap();

  auto *DC = normalC->getDeclContext();
  return normalC->getType()->getContextSubstitutionMap(M, DC);
}

bool ProtocolConformance::isBehaviorConformance() const {
  return getRootNormalConformance()->isBehaviorConformance();
}

AbstractStorageDecl *ProtocolConformance::getBehaviorDecl() const {
  return getRootNormalConformance()->getBehaviorDecl();
}

bool NormalProtocolConformance::isRetroactive() const {
  auto module = getDeclContext()->getParentModule();

  // If the conformance occurs in the same module as the protocol definition,
  // this is not a retroactive conformance.
  auto protocolModule = getProtocol()->getDeclContext()->getParentModule();
  if (module == protocolModule)
    return false;

  // If the conformance occurs in the same module as the conforming type
  // definition, this is not a retroactive conformance.
  if (auto nominal = getType()->getAnyNominal()) {
    auto nominalModule = nominal->getParentModule();

    // Consider the overlay module to be the "home" of a nominal type
    // defined in a Clang module.
    if (auto nominalClangModule =
          dyn_cast<ClangModuleUnit>(nominal->getModuleScopeContext())) {
      if (auto clangLoader = nominal->getASTContext().getClangModuleLoader()) {
        if (auto overlayModule = nominalClangModule->getAdapterModule())
          nominalModule = overlayModule;
      }
    }

    if (module == nominalModule)
      return false;
  }

  // Everything else is retroactive.
  return true;
}

bool NormalProtocolConformance::isSynthesizedNonUnique() const {
  return isa<ClangModuleUnit>(getDeclContext()->getModuleScopeContext());
}

bool NormalProtocolConformance::isResilient() const {
  // If the type is non-resilient or the module we're in is non-resilient, the
  // conformance is non-resilient.
  // FIXME: Looking at the type is not the right long-term solution. We need an
  // explicit mechanism for declaring conformances as 'fragile', or even
  // individual witnesses.
  if (!getType()->getAnyNominal()->isResilient())
    return false;

  switch (getDeclContext()->getParentModule()->getResilienceStrategy()) {
  case ResilienceStrategy::Resilient:
    return true;
  case ResilienceStrategy::Default:
    return false;
  }
}

Optional<ArrayRef<Requirement>>
ProtocolConformance::getConditionalRequirementsIfAvailable() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getConditionalRequirementsIfAvailable, ());
}

ArrayRef<Requirement> ProtocolConformance::getConditionalRequirements() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getConditionalRequirements, ());
}

Optional<ArrayRef<Requirement>>
ProtocolConformanceRef::getConditionalRequirementsIfAvailable() const {
  if (isConcrete())
    return getConcrete()->getConditionalRequirementsIfAvailable();
  else
    // An abstract conformance is never conditional: any conditionality in the
    // concrete types that will eventually pass through this at runtime is
    // completely pre-checked and packaged up.
    return ArrayRef<Requirement>();
}

ArrayRef<Requirement>
ProtocolConformanceRef::getConditionalRequirements() const {
  if (isConcrete())
    return getConcrete()->getConditionalRequirements();
  else
    // An abstract conformance is never conditional, as above.
    return {};
}

ProtocolConformanceRef
ProtocolConformanceRef::getInheritedConformanceRef(ProtocolDecl *base) const {
  if (isAbstract()) {
    assert(getRequirement()->inheritsFrom(base));
    return ProtocolConformanceRef(base);
  }

  auto concrete = getConcrete();
  auto proto = concrete->getProtocol();
  auto path =
    proto->getGenericSignature()->getConformanceAccessPath(
                                            proto->getSelfInterfaceType(), base);
  ProtocolConformanceRef result = *this;
  Type resultType = concrete->getType();
  bool first = true;
  for (const auto &step : path) {
    if (first) {
      assert(step.first->isEqual(proto->getSelfInterfaceType()));
      assert(step.second == proto);
      first = false;
      continue;
    }

    result =
        result.getAssociatedConformance(resultType, step.first, step.second);
    resultType = result.getAssociatedType(resultType, step.first);
  }

  return result;
}

void NormalProtocolConformance::differenceAndStoreConditionalRequirements()
    const {
  switch (CRState) {
  case ConditionalRequirementsState::Complete:
    // already done!
    return;
  case ConditionalRequirementsState::Computing:
    // recursive
    return;
  case ConditionalRequirementsState::Uncomputed:
    // try to compute it!
    break;
  };

  CRState = ConditionalRequirementsState::Computing;
  auto success = [this](ArrayRef<Requirement> reqs) {
    ConditionalRequirements = reqs;
    assert(CRState == ConditionalRequirementsState::Computing);
    CRState = ConditionalRequirementsState::Complete;
  };
  auto failure = [this] {
    assert(CRState == ConditionalRequirementsState::Computing);
    CRState = ConditionalRequirementsState::Uncomputed;
  };

  auto &ctxt = getProtocol()->getASTContext();
  auto DC = getDeclContext();
  // A non-extension conformance won't have conditional requirements.
  if (!isa<ExtensionDecl>(DC)) {
    success({});
    return;
  }

  auto *ext = cast<ExtensionDecl>(DC);
  auto nominal = ext->getExtendedNominal();
  auto typeSig = nominal->getGenericSignature();

  // A non-generic type won't have conditional requirements.
  if (!typeSig) {
    success({});
    return;
  }

  auto extensionSig = ext->getGenericSignature();
  if (!extensionSig) {
    if (auto lazyResolver = ctxt.getLazyResolver()) {
      lazyResolver->resolveExtension(ext);
      extensionSig = ext->getGenericSignature();
    }
  }

  // The type is generic, but the extension doesn't have a signature yet, so
  // we might be in a recursive validation situation.
  if (!extensionSig) {
    // If the extension is invalid, it won't ever get a signature, so we
    // "succeed" with an empty result instead.
    if (ext->isInvalid()) {
      success({});
      return;
    }

    // Otherwise we'll try again later.
    failure();
    return;
  }

  auto canExtensionSig = extensionSig->getCanonicalSignature();
  auto canTypeSig = typeSig->getCanonicalSignature();
  if (canTypeSig == canExtensionSig) {
    success({});
    return;
  }

  // The extension signature should be a superset of the type signature, meaning
  // every thing in the type signature either is included too or is implied by
  // something else. The most important bit is having the same type
  // parameters. (NB. if/when Swift gets parameterized extensions, this needs to
  // change.)
  assert(canTypeSig.getGenericParams() == canExtensionSig.getGenericParams());

  // Find the requirements in the extension that aren't proved by the original
  // type, these are the ones that make the conformance conditional.
  success(ctxt.AllocateCopy(extensionSig->requirementsNotSatisfiedBy(typeSig)));
}

void NormalProtocolConformance::setSignatureConformances(
                               ArrayRef<ProtocolConformanceRef> conformances) {
  if (conformances.empty()) {
    SignatureConformances = { };
    return;
  }

  auto &ctx = getProtocol()->getASTContext();
  SignatureConformances = ctx.AllocateCopy(conformances);

#if !NDEBUG
  unsigned idx = 0;
  for (const auto &req : getProtocol()->getRequirementSignature()) {
    if (req.getKind() == RequirementKind::Conformance) {
      assert(!conformances[idx].isConcrete() ||
             !conformances[idx].getConcrete()->getType()->hasArchetype() &&
             "Should have interface types here");
      assert(idx < conformances.size());
      assert(conformances[idx].getRequirement() ==
               req.getSecondType()->castTo<ProtocolType>()->getDecl());
      ++idx;
    }
  }
  assert(idx == conformances.size() && "Too many conformances");
#endif
}

std::function<void(ProtocolConformanceRef)>
NormalProtocolConformance::populateSignatureConformances() {
  assert(SignatureConformances.empty());

  class Writer {
    NormalProtocolConformance *self;
    ArrayRef<Requirement> requirementSignature;
    MutableArrayRef<ProtocolConformanceRef> buffer;
    mutable bool owning = true;

    /// Skip any non-conformance requirements in the requirement signature.
    void skipNonConformanceRequirements() {
      while (!requirementSignature.empty() &&
             requirementSignature.front().getKind()
               != RequirementKind::Conformance)
        requirementSignature = requirementSignature.drop_front();
    }

  public:
    Writer(NormalProtocolConformance *self) : self(self) {
      requirementSignature = self->getProtocol()->getRequirementSignature();

      // Determine the number of conformance requirements we need.
      unsigned numConformanceRequirements = 0;
      for (const auto &req : requirementSignature) {
        if (req.getKind() == RequirementKind::Conformance)
          ++numConformanceRequirements;
      }

      // Allocate the buffer of conformance requirements.
      auto &ctx = self->getProtocol()->getASTContext();
      buffer = ctx.AllocateUninitialized<ProtocolConformanceRef>(
          numConformanceRequirements);

      // Skip over any non-conformance requirements in the requirement
      // signature.
      skipNonConformanceRequirements();
    };

    Writer(Writer &&other)
      : self(other.self),
        requirementSignature(other.requirementSignature),
        buffer(other.buffer)
    {
      other.owning = false;
    }

    Writer(const Writer &other)
      : self(other.self),
        requirementSignature(other.requirementSignature),
        buffer(other.buffer) {
      other.owning = false;
    }

    ~Writer() {
      if (!owning)
        return;
      while (!requirementSignature.empty())
        (*this)(ProtocolConformanceRef::forInvalid());
    }

    void operator()(ProtocolConformanceRef conformance){
      // Make sure we have the right conformance.
      assert(!requirementSignature.empty() && "Too many conformances?");
      assert(conformance.isInvalid() ||
             conformance.getRequirement() ==
                 requirementSignature.front().getSecondType()
                     ->castTo<ProtocolType>()->getDecl());
      assert((!conformance.isConcrete() ||
              !conformance.getConcrete()->getType()->hasArchetype()) &&
             "signature conformances must use interface types");
      // Add this conformance to the known signature conformances.
      requirementSignature = requirementSignature.drop_front();
      new (&buffer[self->SignatureConformances.size()])
        ProtocolConformanceRef(conformance);
      self->SignatureConformances =
        buffer.slice(0, self->SignatureConformances.size() + 1);

      // Skip over any non-conformance requirements.
      skipNonConformanceRequirements();
    }
  };

  return Writer(this);
}

void NormalProtocolConformance::resolveLazyInfo() const {
  assert(Loader);

  auto *loader = Loader;
  auto *mutableThis = const_cast<NormalProtocolConformance *>(this);
  mutableThis->Loader = nullptr;
  loader->finishNormalConformance(mutableThis, LoaderContextData);
}

void NormalProtocolConformance::setLazyLoader(LazyConformanceLoader *loader,
                                              uint64_t contextData) {
  assert(!Loader && "already has a loader");
  Loader = loader;
  LoaderContextData = contextData;
}

namespace {
  class PrettyStackTraceRequirement : public llvm::PrettyStackTraceEntry {
    const char *Action;
    const ProtocolConformance *Conformance;
    ValueDecl *Requirement;
  public:
    PrettyStackTraceRequirement(const char *action,
                                const ProtocolConformance *conformance,
                                ValueDecl *requirement)
      : Action(action), Conformance(conformance), Requirement(requirement) { }

    void print(llvm::raw_ostream &out) const override {
      out << "While " << Action << " requirement ";
      Requirement->dumpRef(out);
      out << " in conformance ";
      Conformance->printName(out);
      out << "\n";
    }
  };
} // end anonymous namespace

bool NormalProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                               LazyResolver *resolver) const {
  if (Loader)
    resolveLazyInfo();

  auto found = TypeWitnesses.find(assocType);
  if (found != TypeWitnesses.end()) {
    return !found->getSecond().first.isNull();
  }
  if (resolver) {
    PrettyStackTraceRequirement trace("resolving", this, assocType);
    resolver->resolveTypeWitness(this, assocType);
    if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
      return true;
    }
  }
  return false;
}

using TypeWitnessAndDecl = std::pair<Type, TypeDecl *>;
TypeWitnessAndDecl
NormalProtocolConformance::getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                                                 LazyResolver *resolver,
                                                 SubstOptions options) const {
  if (Loader)
    resolveLazyInfo();

  // Check whether we already have a type witness.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end())
    return known->second;

  // If there is a tentative-type-witness function, use it.
  if (options.getTentativeTypeWitness) {
   if (Type witnessType =
         Type(options.getTentativeTypeWitness(this, assocType)))
     return { witnessType, nullptr };
  }

  // If this conformance is in a state where it is inferring type witnesses but
  // we didn't find anything, fail.
  if (getState() == ProtocolConformanceState::CheckingTypeWitnesses) {
    return { Type(), nullptr };
  }

  // If the conditional requirements aren't known, we can't properly run
  // inference.
  if (!getConditionalRequirementsIfAvailable()) {
    return {Type(), nullptr};
  }

  // Otherwise, resolve the type witness.
  PrettyStackTraceRequirement trace("resolving", this, assocType);
  if (!resolver) resolver = assocType->getASTContext().getLazyResolver();
  assert(resolver && "Unable to resolve type witness");

  // Block recursive resolution of this type witness.
  TypeWitnesses[assocType] = { Type(), nullptr };
  resolver->resolveTypeWitness(this, assocType);

  known = TypeWitnesses.find(assocType);
  assert(known != TypeWitnesses.end() && "Didn't resolve witness?");
  return known->second;
}

void NormalProtocolConformance::setTypeWitness(AssociatedTypeDecl *assocType,
                                               Type type,
                                               TypeDecl *typeDecl) const {
  assert(getProtocol() == cast<ProtocolDecl>(assocType->getDeclContext()) &&
         "associated type in wrong protocol");
  assert((TypeWitnesses.count(assocType) == 0 ||
          TypeWitnesses[assocType].first.isNull()) &&
         "Type witness already known");
  assert((!isComplete() || isInvalid()) && "Conformance already complete?");
  assert(!type->hasArchetype() && "type witnesses must be interface types");
  TypeWitnesses[assocType] = std::make_pair(type, typeDecl);
}

Type ProtocolConformance::getAssociatedType(Type assocType,
                                            LazyResolver *resolver) const {
  assert(assocType->isTypeParameter() &&
         "associated type must be a type parameter");

  ProtocolConformanceRef ref(const_cast<ProtocolConformance*>(this));
  return ref.getAssociatedType(getType(), assocType, resolver);
}

Type ProtocolConformanceRef::getAssociatedType(Type conformingType,
                                               Type assocType,
                                               LazyResolver *resolver) const {
  assert(!isConcrete() || getConcrete()->getType()->isEqual(conformingType));

  auto type = assocType->getCanonicalType();
  auto proto = getRequirement();

  // Fast path for generic parameters.
  if (isa<GenericTypeParamType>(type)) {
    assert(type->isEqual(proto->getSelfInterfaceType()) &&
           "type parameter in protocol was not Self");
    return conformingType;
  }

  // Fast path for dependent member types on 'Self' of our associated types.
  auto memberType = cast<DependentMemberType>(type);
  if (memberType.getBase()->isEqual(proto->getSelfInterfaceType()) &&
      memberType->getAssocType()->getProtocol() == proto &&
      isConcrete())
    return getConcrete()->getTypeWitness(memberType->getAssocType(), resolver);

  // General case: consult the substitution map.
  auto substMap =
    SubstitutionMap::getProtocolSubstitutions(proto, conformingType, *this);
  return type.subst(substMap);
}

ProtocolConformanceRef
ProtocolConformanceRef::getAssociatedConformance(Type conformingType,
                                                 Type assocType,
                                                 ProtocolDecl *protocol,
                                                 LazyResolver *resolver) const {
  // If this is a concrete conformance, look up the associated conformance.
  if (isConcrete()) {
    auto conformance = getConcrete();
    assert(conformance->getType()->isEqual(conformingType));
    return conformance->getAssociatedConformance(assocType, protocol, resolver);
  }

  // Otherwise, apply the substitution {self -> conformingType}
  // to the abstract conformance requirement laid upon the dependent type
  // by the protocol.
  auto subMap =
    SubstitutionMap::getProtocolSubstitutions(getRequirement(),
                                              conformingType, *this);
  auto abstractConf = ProtocolConformanceRef(protocol);
  return abstractConf.subst(assocType, subMap);
}

ProtocolConformanceRef
ProtocolConformance::getAssociatedConformance(Type assocType,
                                               ProtocolDecl *protocol,
                                               LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getAssociatedConformance,
                                (assocType, protocol, resolver))
}

ProtocolConformanceRef
NormalProtocolConformance::getAssociatedConformance(Type assocType,
                                                    ProtocolDecl *protocol,
                                                LazyResolver *resolver) const {
  assert(assocType->isTypeParameter() &&
         "associated type must be a type parameter");

  // Fill in the signature conformances, if we haven't done so yet.
  if (getSignatureConformances().empty()) {
    assocType->getASTContext().getLazyResolver()
      ->checkConformanceRequirements(
        const_cast<NormalProtocolConformance *>(this));
  }

  assert(!getSignatureConformances().empty() &&
         "signature conformances not yet computed");

  unsigned conformanceIndex = 0;
  for (const auto &reqt : getProtocol()->getRequirementSignature()) {
    if (reqt.getKind() == RequirementKind::Conformance) {
      // Is this the conformance we're looking for?
      if (reqt.getFirstType()->isEqual(assocType) &&
          reqt.getSecondType()->castTo<ProtocolType>()->getDecl() == protocol)
        return getSignatureConformances()[conformanceIndex];

      ++conformanceIndex;
    }
  }

  llvm_unreachable(
    "requested conformance was not a direct requirement of the protocol");
}

/// Retrieve the value witness corresponding to the given requirement.
Witness NormalProtocolConformance::getWitness(ValueDecl *requirement,
                                              LazyResolver *resolver) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  assert(requirement->isProtocolRequirement() && "Not a requirement");

  if (Loader)
    resolveLazyInfo();

  auto known = Mapping.find(requirement);
  if (known == Mapping.end()) {
    if (!resolver) resolver = requirement->getASTContext().getLazyResolver();
    assert(resolver && "Unable to resolve witness without resolver");
    resolver->resolveWitness(this, requirement);
    known = Mapping.find(requirement);
  }
  if (known != Mapping.end()) {
    return known->second;
  } else {
    assert((!isComplete() || isInvalid()) &&
           "Resolver did not resolve requirement");
    return Witness();
  }
}

ConcreteDeclRef
NormalProtocolConformance::getWitnessDeclRef(ValueDecl *requirement,
                                             LazyResolver *resolver) const {
  if (auto witness = getWitness(requirement, resolver))
    return witness.getDeclRef();
  return ConcreteDeclRef();
}

void NormalProtocolConformance::setWitness(ValueDecl *requirement,
                                           Witness witness) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  assert(getProtocol() == cast<ProtocolDecl>(requirement->getDeclContext()) &&
         "requirement in wrong protocol");
  assert(Mapping.count(requirement) == 0 && "Witness already known");
  assert((!isComplete() || isInvalid() ||
          requirement->getAttrs().hasAttribute<OptionalAttr>() ||
          requirement->getAttrs().isUnavailable(
                                        requirement->getASTContext())) &&
         "Conformance already complete?");
  Mapping[requirement] = witness;
}

SpecializedProtocolConformance::SpecializedProtocolConformance(
    Type conformingType,
    ProtocolConformance *genericConformance,
    SubstitutionMap substitutions)
  : ProtocolConformance(ProtocolConformanceKind::Specialized, conformingType),
    GenericConformance(genericConformance),
    GenericSubstitutions(substitutions)
{
  assert(genericConformance->getKind() != ProtocolConformanceKind::Specialized);
}

void SpecializedProtocolConformance::computeConditionalRequirements() const {
  // already computed?
  if (ConditionalRequirements)
    return;

  auto parentCondReqs =
      GenericConformance->getConditionalRequirementsIfAvailable();
  if (!parentCondReqs)
    return;

  if (!parentCondReqs->empty()) {
    // Substitute the conditional requirements so that they're phrased in
    // terms of the specialized types, not the conformance-declaring decl's
    // types.
    auto nominal = GenericConformance->getType()->getAnyNominal();
    auto module = nominal->getModuleContext();
    auto subMap = getType()->getContextSubstitutionMap(module, nominal);

    SmallVector<Requirement, 4> newReqs;
    for (auto oldReq : *parentCondReqs) {
      if (auto newReq = oldReq.subst(QuerySubstitutionMap{subMap},
                                     LookUpConformanceInModule(module)))
        newReqs.push_back(*newReq);
    }
    auto &ctxt = getProtocol()->getASTContext();
    ConditionalRequirements = ctxt.AllocateCopy(newReqs);
  } else {
    ConditionalRequirements = ArrayRef<Requirement>();
  }
}

bool SpecializedProtocolConformance::hasTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  return TypeWitnesses.find(assocType) != TypeWitnesses.end() ||
         GenericConformance->hasTypeWitness(assocType, resolver);
}

std::pair<Type, TypeDecl *>
SpecializedProtocolConformance::getTypeWitnessAndDecl(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver,
                      SubstOptions options) const {
  // If we've already created this type witness, return it.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end()) {
    return known->second;
  }

  // Otherwise, perform substitutions to create this witness now.

  // Local function to determine whether we will end up referring to a
  // tentative witness that may not be chosen.
  auto normal = GenericConformance->getRootNormalConformance();
  auto isTentativeWitness = [&] {
    if (normal->getState() != ProtocolConformanceState::CheckingTypeWitnesses)
      return false;

    return !normal->hasTypeWitness(assocType, nullptr);
  };

  auto genericWitnessAndDecl
    = GenericConformance->getTypeWitnessAndDecl(assocType, resolver, options);

  auto genericWitness = genericWitnessAndDecl.first;
  if (!genericWitness)
    return { Type(), nullptr };

  auto *typeDecl = genericWitnessAndDecl.second;

  // Form the substitution.
  auto substitutionMap = getSubstitutionMap();
  if (substitutionMap.empty())
    return {Type(), nullptr};

  // Apply the substitution we computed above
  auto specializedType = genericWitness.subst(substitutionMap, options);
  if (!specializedType) {
    if (isTentativeWitness())
      return { Type(), nullptr };

    specializedType = ErrorType::get(genericWitness);
  }

  // If we aren't in a case where we used the tentative type witness
  // information, cache the result.
  auto specializedWitnessAndDecl = std::make_pair(specializedType, typeDecl);
  if (!isTentativeWitness() && !specializedType->hasError())
    TypeWitnesses[assocType] = specializedWitnessAndDecl;

  return specializedWitnessAndDecl;
}

ProtocolConformanceRef
SpecializedProtocolConformance::getAssociatedConformance(Type assocType,
                                                ProtocolDecl *protocol,
                                                LazyResolver *resolver) const {
  ProtocolConformanceRef conformance =
    GenericConformance->getAssociatedConformance(assocType, protocol, resolver);

  auto subMap = getSubstitutionMap();

  Type origType =
    (conformance.isConcrete()
       ? conformance.getConcrete()->getType()
       : GenericConformance->getAssociatedType(assocType, resolver));

  return conformance.subst(origType, subMap);
}

ConcreteDeclRef
SpecializedProtocolConformance::getWitnessDeclRef(
                                              ValueDecl *requirement,
                                              LazyResolver *resolver) const {
  auto baseWitness = GenericConformance->getWitnessDeclRef(requirement, resolver);
  if (!baseWitness || !baseWitness.isSpecialized())
    return baseWitness;

  auto specializationMap = getSubstitutionMap();

  auto witnessDecl = baseWitness.getDecl();
  auto witnessMap = baseWitness.getSubstitutions();

  auto combinedMap = witnessMap.subst(specializationMap);

  // Fast path if the substitutions didn't change.
  if (combinedMap == baseWitness.getSubstitutions())
    return baseWitness;

  return ConcreteDeclRef(witnessDecl, combinedMap);
}

ProtocolConformanceRef
InheritedProtocolConformance::getAssociatedConformance(Type assocType,
                         ProtocolDecl *protocol,
                         LazyResolver *resolver) const {
  auto underlying =
    InheritedConformance->getAssociatedConformance(assocType, protocol,
                                                   resolver);


  // If the conformance is for Self, return an inherited conformance.
  if (underlying.isConcrete() &&
      assocType->isEqual(getProtocol()->getSelfInterfaceType())) {
    auto subclassType = getType();
    ASTContext &ctx = subclassType->getASTContext();
    return ProtocolConformanceRef(
             ctx.getInheritedConformance(subclassType,
                                         underlying.getConcrete()));
  }

  return underlying;
}

ConcreteDeclRef
InheritedProtocolConformance::getWitnessDeclRef(ValueDecl *requirement,
                                                LazyResolver *resolver) const {
  // FIXME: substitutions?
  return InheritedConformance->getWitnessDeclRef(requirement, resolver);
}

const NormalProtocolConformance *
ProtocolConformance::getRootNormalConformance() const {
  const ProtocolConformance *C = this;
  while (!isa<NormalProtocolConformance>(C)) {
    switch (C->getKind()) {
    case ProtocolConformanceKind::Normal:
      llvm_unreachable("should have broken out of loop");
    case ProtocolConformanceKind::Inherited:
      C = cast<InheritedProtocolConformance>(C)
          ->getInheritedConformance();
      break;
    case ProtocolConformanceKind::Specialized:
      C = cast<SpecializedProtocolConformance>(C)
        ->getGenericConformance();
      break;
    }
  }
  return cast<NormalProtocolConformance>(C);
}

bool ProtocolConformance::isVisibleFrom(const DeclContext *dc) const {
  // FIXME: Implement me!
  return true;
}

ProtocolConformance *
ProtocolConformance::subst(SubstitutionMap subMap) const {
  return subst(QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap));
}

ProtocolConformance *
ProtocolConformance::subst(TypeSubstitutionFn subs,
                           LookupConformanceFn conformances) const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    auto origType = getType();
    if (!origType->hasTypeParameter() &&
        !origType->hasArchetype())
      return const_cast<ProtocolConformance *>(this);

    auto subMap = SubstitutionMap::get(getGenericSignature(),
                                       subs, conformances);
    auto substType = origType.subst(subMap, SubstFlags::UseErrorType);
    if (substType->isEqual(origType))
      return const_cast<ProtocolConformance *>(this);

    return substType->getASTContext()
        .getSpecializedConformance(substType,
                                   const_cast<ProtocolConformance *>(this),
                                   subMap);
  }
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    auto inheritedConformance
      = cast<InheritedProtocolConformance>(this)->getInheritedConformance();

    auto origType = getType();
    if (!origType->hasTypeParameter() &&
        !origType->hasArchetype()) {
      return const_cast<ProtocolConformance *>(this);
    }

    auto origBaseType = inheritedConformance->getType();
    if (origBaseType->hasTypeParameter() ||
        origBaseType->hasArchetype()) {
      // Substitute into the superclass.
      inheritedConformance = inheritedConformance->subst(subs, conformances);
    }

    auto substType = origType.subst(subs, conformances,
                                    SubstFlags::UseErrorType);
    return substType->getASTContext()
      .getInheritedConformance(substType, inheritedConformance);
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto genericConformance = spec->getGenericConformance();
    auto subMap = spec->getSubstitutionMap();

    auto origType = getType();
    auto substType = origType.subst(subs, conformances,
                                    SubstFlags::UseErrorType);
    return substType->getASTContext()
      .getSpecializedConformance(substType, genericConformance,
                                 subMap.subst(subs, conformances));
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

ProtocolConformance *
ProtocolConformance::getInheritedConformance(ProtocolDecl *protocol) const {
  auto result =
    getAssociatedConformance(getProtocol()->getSelfInterfaceType(), protocol);
  return result.isConcrete() ? result.getConcrete() : nullptr;
}

#pragma mark Protocol conformance lookup
void NominalTypeDecl::prepareConformanceTable() const {
  if (ConformanceTable)
    return;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  ASTContext &ctx = getASTContext();
  ConformanceTable = new (ctx) ConformanceLookupTable(ctx);
  ++NumConformanceLookupTables;

  // If this type declaration was not parsed from source code or introduced
  // via the Clang importer, don't add any synthesized conformances.
  auto *file = cast<FileUnit>(getModuleScopeContext());
  if (file->getKind() != FileUnitKind::Source &&
      file->getKind() != FileUnitKind::ClangModule) {
    return;
  }

  SmallPtrSet<ProtocolDecl *, 2> protocols;

  auto addSynthesized = [&](KnownProtocolKind kind) {
    if (auto *proto = getASTContext().getProtocol(kind)) {
      if (protocols.count(proto) == 0) {
        ConformanceTable->addSynthesizedConformance(mutableThis, proto);
        protocols.insert(proto);
      }
    }
  };

  // Add protocols for any synthesized protocol attributes.
  for (auto attr : getAttrs().getAttributes<SynthesizedProtocolAttr>()) {
    addSynthesized(attr->getProtocolKind());
  }

  // Add any implicit conformances.
  if (auto theEnum = dyn_cast<EnumDecl>(mutableThis)) {
    if (theEnum->hasCases() && theEnum->hasOnlyCasesWithoutAssociatedValues()) {
      // Simple enumerations conform to Equatable.
      addSynthesized(KnownProtocolKind::Equatable);

      // Simple enumerations conform to Hashable.
      addSynthesized(KnownProtocolKind::Hashable);
    }

    // Enumerations with a raw type conform to RawRepresentable.
    if (theEnum->hasRawType()) {
      addSynthesized(KnownProtocolKind::RawRepresentable);
    }
  }
}

bool NominalTypeDecl::lookupConformance(
       ModuleDecl *module, ProtocolDecl *protocol,
       SmallVectorImpl<ProtocolConformance *> &conformances) const {
  prepareConformanceTable();
  return ConformanceTable->lookupConformance(
           module,
           const_cast<NominalTypeDecl *>(this),
           protocol,
           conformances);
}

SmallVector<ProtocolDecl *, 2> NominalTypeDecl::getAllProtocols() const {
  prepareConformanceTable();
  SmallVector<ProtocolDecl *, 2> result;
  ConformanceTable->getAllProtocols(const_cast<NominalTypeDecl *>(this),
                                    result);
  return result;
}

SmallVector<ProtocolConformance *, 2> NominalTypeDecl::getAllConformances(
                                        bool sorted) const
{
  prepareConformanceTable();
  SmallVector<ProtocolConformance *, 2> result;
  ConformanceTable->getAllConformances(const_cast<NominalTypeDecl *>(this),
                                       sorted,
                                       result);
  return result;
}

void NominalTypeDecl::getImplicitProtocols(
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  prepareConformanceTable();
  ConformanceTable->getImplicitProtocols(this, protocols);
}

void NominalTypeDecl::registerProtocolConformance(
       ProtocolConformance *conformance) {
  prepareConformanceTable();
  ConformanceTable->registerProtocolConformance(conformance);
}

ArrayRef<ValueDecl *>
NominalTypeDecl::getSatisfiedProtocolRequirementsForMember(
                                             const ValueDecl *member,
                                             bool sorted) const {
  assert(member->getDeclContext()->getSelfNominalTypeDecl() == this);
  assert(!isa<ProtocolDecl>(this));
  prepareConformanceTable();
  return ConformanceTable->getSatisfiedProtocolRequirementsForMember(member,
                                           const_cast<NominalTypeDecl *>(this),
                                           sorted);
}

SmallVector<ProtocolDecl *, 2>
DeclContext::getLocalProtocols(
  ConformanceLookupKind lookupKind,
  SmallVectorImpl<ConformanceDiagnostic> *diagnostics,
  bool sorted) const
{
  SmallVector<ProtocolDecl *, 2> result;

  // Dig out the nominal type.
  NominalTypeDecl *nominal = getSelfNominalTypeDecl();
  if (!nominal)
    return result;

  // Update to record all potential conformances.
  nominal->prepareConformanceTable();
  nominal->ConformanceTable->lookupConformances(
    nominal,
    const_cast<DeclContext *>(this),
    lookupKind,
    &result,
    nullptr,
    diagnostics);

  // Sort if required.
  if (sorted) {
    llvm::array_pod_sort(result.begin(), result.end(), TypeDecl::compare);
  }

  return result;
}

SmallVector<ProtocolConformance *, 2>
DeclContext::getLocalConformances(
  ConformanceLookupKind lookupKind,
  SmallVectorImpl<ConformanceDiagnostic> *diagnostics,
  bool sorted) const
{
  SmallVector<ProtocolConformance *, 2> result;

  // Dig out the nominal type.
  NominalTypeDecl *nominal = getSelfNominalTypeDecl();
  if (!nominal)
    return result;

  // Protocols don't have conformances.
  if (isa<ProtocolDecl>(nominal))
    return { };

  // Update to record all potential conformances.
  nominal->prepareConformanceTable();
  nominal->ConformanceTable->lookupConformances(
    nominal,
    const_cast<DeclContext *>(this),
    lookupKind,
    nullptr,
    &result,
    diagnostics);

  // If requested, sort the results.
  if (sorted) {
    llvm::array_pod_sort(result.begin(), result.end(),
                         &ConformanceLookupTable::compareProtocolConformances);
  }

  return result;
}

/// Check of all types used by the conformance are canonical.
bool ProtocolConformance::isCanonical() const {
  // Normal conformances are always canonical by construction.
  if (getKind() == ProtocolConformanceKind::Normal)
    return true;

  if (!getType()->isCanonical())
    return false;

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    return true;
  }
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    auto inheritedConformance
      = cast<InheritedProtocolConformance>(this);
    return inheritedConformance->getInheritedConformance()->isCanonical();
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto genericConformance = spec->getGenericConformance();
    if (!genericConformance->isCanonical())
      return false;
    if (!spec->getSubstitutionMap().isCanonical()) return false;
    return true;
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

/// Check of all types used by the conformance are canonical.
ProtocolConformance *ProtocolConformance::getCanonicalConformance() {
  if (isCanonical())
    return this;

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    // Normal conformances are always canonical by construction.
    return this;
  }

  case ProtocolConformanceKind::Inherited: {
    auto &Ctx = getType()->getASTContext();
    auto inheritedConformance = cast<InheritedProtocolConformance>(this);
    return Ctx.getInheritedConformance(
        getType()->getCanonicalType(),
        inheritedConformance->getInheritedConformance()
            ->getCanonicalConformance());
  }

  case ProtocolConformanceKind::Specialized: {
    auto &Ctx = getType()->getASTContext();
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto genericConformance = spec->getGenericConformance();
    return Ctx.getSpecializedConformance(
                                getType()->getCanonicalType(),
                                genericConformance->getCanonicalConformance(),
                                spec->getSubstitutionMap().getCanonical());
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

/// Check of all types used by the conformance are canonical.
bool ProtocolConformanceRef::isCanonical() const {
  if (isAbstract() || isInvalid())
    return true;
  return getConcrete()->isCanonical();
}

ProtocolConformanceRef
ProtocolConformanceRef::getCanonicalConformanceRef() const {
  if (isAbstract() || isInvalid())
    return *this;
  return ProtocolConformanceRef(getConcrete()->getCanonicalConformance());
}

// See swift/Basic/Statistic.h for declaration: this enables tracing
// ProtocolConformances, is defined here to avoid too much layering violation /
// circular linkage dependency.

struct ProtocolConformanceTraceFormatter
    : public UnifiedStatsReporter::TraceFormatter {
  void traceName(const void *Entity, raw_ostream &OS) const {
    if (!Entity)
      return;
    const ProtocolConformance *C =
        static_cast<const ProtocolConformance *>(Entity);
    C->printName(OS);
  }
  void traceLoc(const void *Entity, SourceManager *SM,
                clang::SourceManager *CSM, raw_ostream &OS) const {
    if (!Entity)
      return;
    const ProtocolConformance *C =
        static_cast<const ProtocolConformance *>(Entity);
    if (auto const *NPC = dyn_cast<NormalProtocolConformance>(C)) {
      NPC->getLoc().print(OS, *SM);
    } else if (auto const *DC = C->getDeclContext()) {
      if (auto const *D = DC->getAsDecl())
        D->getLoc().print(OS, *SM);
    }
  }
};

static ProtocolConformanceTraceFormatter TF;

template<>
const UnifiedStatsReporter::TraceFormatter*
FrontendStatsTracer::getTraceFormatter<const ProtocolConformance *>() {
  return &TF;
}

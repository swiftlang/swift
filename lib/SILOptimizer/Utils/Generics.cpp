//===--- Generics.cpp ---- Utilities for transforming generics ------------===//
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

#define DEBUG_TYPE "generic-specializer"

#include "swift/Strings.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/GenericCloner.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/GenericEnvironment.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;

llvm::cl::opt<bool> EnablePartialSpecialization(
    "enable-partial-specialization", llvm::cl::init(true),
    llvm::cl::desc("Enable partial specialization of generic functions"));


/// If set, then generic specialization tries to specialize using
/// all substitutions, even if their replacement types are generic.
/// Otherwise specialize only those generic type parameters which are
/// substituted by concrete types.
static bool CanSpecializeConcreteAndGenericSubstitutions = true;

/// If set, generic substitutions are substituted into function signatures.
/// Given a substitution, U = G<V>, function signature 'func foo<U>(x: U)'
/// becomes 'func foo<V>(x: G<V>)'.
///
/// Only has impact if SpecializeConcreteAndGenericSubstitutions is set.
/// FIXME: This is a temporary flag that will be removed once the implementation
/// of the partial specialization is ready.
static bool SupportGenericSubstitutions = false;

/// Apply certain heuristics to decide of a given generic substitution
/// should be used for the generic specialization. As an optimization, the
/// compiler may decide that a given generic substitution is not profitable
/// and can decide to avoid substituting it into a function signature.
///
/// Only has impact if SpecializeConcreteAndGenericSubstitutions and
/// SupportGenericSubstitutions are set.
/// FIXME: This is a temporary flag that will be removed once the implementation
/// of the partial specialization is ready.
static bool OptimizeGenericSubstitutions = false;

/// If set to false, fully concrete substitutions use the same code path
/// as partial specializations, which eliminates a need to special case
/// the full specialization.
/// Currently it is disabled, because ArchetypeBuidler cannot properly
/// handle same-type requirements to concrete types in certain cases, which
/// occur in the stdlib.
/// rdar://29333056
/// FIXME: This is a temporary flag that will be removed once the implementation
/// of the partial specialization is ready.
static bool ShortcutFullSpecialization = true;

// Max depth of a bound generic which can be processed by the generic
// specializer.
// E.g. the depth of Array<Array<Array<T>>> is 3.
// No specializations will be produced, if any of generic parameters contains
// a bound generic type with the depth higher than this threshold 
static const unsigned BoundGenericDepthThreshold = 50;

class QueryConformance {
  ModuleDecl *const Mod;
public:
  QueryConformance(ModuleDecl* Mod): Mod(Mod) {}

  Optional<ProtocolConformanceRef>
  operator()(CanType original, Type replacement, ProtocolType *protoType) {
    auto proto = protoType->getDecl();
    auto C = Mod->lookupConformance(replacement, proto, nullptr);
    if (C.hasValue())
      return C.getValue();

    if (!replacement->hasTypeParameter() && !replacement->hasArchetype()) {
      // It is a concrete type.
      auto nominal = replacement->getNominalOrBoundGenericNominal();
      if (nominal) {
        SmallVector<ProtocolConformance *, 2> conformances;
        nominal->lookupConformance(Mod, proto, conformances);
        assert(conformances.size() == 1);
        return ProtocolConformanceRef(conformances.front())
            .subst(Mod, replacement);
        // auto concreteSubs = replacement->gatherAllSubstitutions(
        //     nominal->getModuleContext(), nullptr);
        // if (!concreteSubs.empty()) {
        //   auto specialized = Mod->getASTContext().getSpecializedConformance(
        //       replacement, conformances.front(), concreteSubs);
        //   return ProtocolConformanceRef(specialized);
        // }
        // return ProtocolConformanceRef(conformances.front());
      }
    }
    return ProtocolConformanceRef(protoType->getDecl());
  }
};

static unsigned getBoundGenericDepth(Type t) {
  unsigned Depth = 0;
  if (auto BGT = t->getAs<BoundGenericType>()) {
    Depth++;
    auto GenericArgs = BGT->getGenericArgs();
    unsigned MaxGenericArgDepth = 0;
    for (auto GenericArg : GenericArgs) {
      auto ArgDepth = getBoundGenericDepth(GenericArg);
      if (ArgDepth > MaxGenericArgDepth)
        MaxGenericArgDepth = ArgDepth;
    }
    Depth += MaxGenericArgDepth;
  }
  return Depth;
}

/// For a dependent type of a generic type returns its base
/// generic type. For a generic type parameter type, returns this
/// type.
GenericTypeParamType *getBaseGenericTypeParamType(Type Ty) {
  auto BaseTy = Ty;
  while (auto DepTy = dyn_cast<DependentMemberType>(BaseTy.getPointer())) {
    BaseTy = DepTy->getBase();
  };
  return BaseTy->castTo<GenericTypeParamType>();
}

// =============================================================================
// ReabstractionInfo
// =============================================================================

/// Prepares the ReabstractionInfo object for further processing and checks
/// if the current function can be specialized at all.
/// Returns false, if the current function cannot be specialized.
/// Returns true otherwise.
bool ReabstractionInfo::prepareAndCheck(ApplySite Apply, SILFunction *Callee,
                                        ArrayRef<Substitution> ParamSubs) {
  if (!Callee->shouldOptimize()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize function " << Callee->getName()
                       << " marked to be excluded from optimizations.\n");
    return false;
  }

  SpecializedGenericEnv = nullptr;
  SpecializedGenericSig = nullptr;
  OriginalParamSubs = ParamSubs;
  CallerParamSubs = {};
  ClonerParamSubs = ParamSubs;

  OriginalF = Callee;
  this->Apply = Apply;

  SubstitutionMap InterfaceSubs;

  // Get the original substitution map.
  if (Callee->getLoweredFunctionType()->getGenericSignature())
    InterfaceSubs = Callee->getLoweredFunctionType()->getGenericSignature()
      ->getSubstitutionMap(ParamSubs);

  if (hasDynamicSelfTypes(InterfaceSubs)) {
    DEBUG(llvm::dbgs() << "    Cannot specialize with dynamic self.\n");
    return false;
  }

  // Check if the substitution contains any generic types that are too deep.
  // If this is the case, bail to avoid the explosion in the number of 
  // generated specializations.
  for (auto Sub : ParamSubs) {
    auto Replacement = Sub.getReplacement();
    if (Replacement.findIf([](Type ty) -> bool {
          return getBoundGenericDepth(ty) >= BoundGenericDepthThreshold;
        })) {
      DEBUG(llvm::dbgs()
            << "    Cannot specialize because the generic type is too deep.\n");
      return false;
    }
  }

  // Check if we have substitutions which replace generic type parameters with
  // concrete types or unbound generic types.
  bool HasConcreteGenericParams = false;
  HasUnboundGenericParams = false;
  for (auto &entry : InterfaceSubs.getMap()) {
    // Check only the substitutions for the generic parameters.
    // Ignore any dependent types, etc.
    if (!isa<GenericTypeParamType>(entry.first))
      continue;
    auto Replacement = entry.second->getCanonicalType();
    if (Replacement->hasArchetype()) {
      HasUnboundGenericParams = true;
      continue;
    }
    HasConcreteGenericParams = true;
  }

  if (!EnablePartialSpecialization && HasUnboundGenericParams) {
    DEBUG(llvm::dbgs() <<
          "    Partial specialization is not enabled.\n");
    return false;
  }

  // Do not partially specialize any print.*unlock methods, because
  // they are very big and are not important for performance.
  // TODO: Could it be that they are important for string interpolation?
  // TODO: Introduce a proper way to tell the compiler that certain
  // functions should not be (auto)-(partially)-specialized.
  if (Callee->getName().find("_unlock", 0) != StringRef::npos)
    return false;

  // We need a generic environment for the partial specialization.
  // if (HasUnboundGenericParams && !Callee->getGenericEnvironment())
  //   return false;

  return true;
}

/// Copy an entry from one substitution map into the other substitution map.
static void copySubstitutionMapEntry(Type FromTy, SubstitutionMap &FromMap,
                                     Type ToTy, SubstitutionMap &ToMap,
                                     bool SkipSubstIfExists = false,
                                     Type Replacement = nullptr) {
  auto FromCanTy = FromTy->getCanonicalType();
  auto ToCanTy = ToTy->getCanonicalType();
  assert(isa<SubstitutableType>(FromCanTy));
  auto FromSubTy = cast<SubstitutableType>(FromCanTy);
  auto ToSubTy = cast<SubstitutableType>(ToCanTy);
  if (!SkipSubstIfExists || !ToMap.getMap().lookup(ToSubTy)) {
    auto FromSubstTy =
        (Replacement) ? Replacement : FromMap.getMap().lookup(FromSubTy);
    ToMap.addSubstitution(ToSubTy, FromSubstTy);
  }
}

bool ReabstractionInfo::canBeSpecialized() const {
  return getSpecializedType();
}

bool ReabstractionInfo::isFullSpecialization() const {
  return !hasUnboundGenericTypes(getOriginalParamSubstitutions());
}

bool ReabstractionInfo::isPartialSpecialization() const {
  return hasUnboundGenericTypes(getOriginalParamSubstitutions());
}

void ReabstractionInfo::createSubstitutedAndSpecializedTypes() {
  auto &M = OriginalF->getModule();

  if (ShortcutFullSpecialization && !HasUnboundGenericParams) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
  }

  // Find out how the function type looks like after applying the provided
  // substitutions.
  if (!SubstitutedType) {
    SubstitutedType = createSubstitutedType(
        OriginalF, CallerInterfaceSubs, HasUnboundGenericParams);
  }
  assert(!SubstitutedType->hasArchetype() &&
         "Substituted function type should not contain archetypes");

  // Check which parameters and results can be converted from
  // indirect to direct ones.
  NumResults = SubstitutedType->getNumIndirectResults();
  Conversions.resize(NumResults + SubstitutedType->getParameters().size());

  CanGenericSignature CanSig;
  if (SpecializedGenericSig)
    CanSig = SpecializedGenericSig->getCanonicalSignature();
  Lowering::GenericContextScope GenericScope(M.Types, CanSig);

  if (SubstitutedType->getNumDirectResults() == 0) {
    // The original function has no direct result yet. Try to convert the first
    // indirect result to a direct result.
    // TODO: We could also convert multiple indirect results by returning a
    // tuple type and created tuple_extract instructions at the call site.
    unsigned IdxForResult = 0;
    for (SILResultInfo RI : SubstitutedType->getIndirectResults()) {
      assert(RI.isIndirect());
      if (RI.getSILType().isLoadable(M) && !RI.getType()->isVoid()) {
        Conversions.set(IdxForResult);
        break;
      }
      ++IdxForResult;
    }
  }

  // Try to convert indirect incoming parameters to direct parameters.
  unsigned IdxForParam = NumResults;
  for (SILParameterInfo PI : SubstitutedType->getParameters()) {
    if (PI.getSILType().isLoadable(M) &&
        PI.getConvention() == ParameterConvention::Indirect_In) {
      Conversions.set(IdxForParam);
    }
    ++IdxForParam;
  }

  // Produce a specialized type, which is the substituted type with
  // the parameters/results passing conventions adjusted according
  // to the converions selected above.
  SpecializedType = createSpecializedType(SubstitutedType, M);
}

/// Replace dependent types with their archetypes or concrete types.
static Type substConcreteTypesForDependentTypes(ModuleDecl &SM,
                                                SubstitutionMap &SubsMap,
                                                Type type) {
  // Cannot use type.subst(SubsMap) here, because it requires a proper
  // set of conformances to be present in SubsMap, which is not the case.

  return type.transform([&](Type type) -> Type {
      if (auto depMemTy = type->getAs<DependentMemberType>()) {
        auto newBase = substConcreteTypesForDependentTypes(SM,
                                                           SubsMap,
                                                           depMemTy->getBase());
        return depMemTy->substBaseType(&SM, newBase);
      }

      if (auto typeParam = type->getAs<GenericTypeParamType>()) {
        return SubsMap.getMap().lookup(typeParam);
      }

      return type;
  });
}

static void remapRequirements(
    GenericSignature *GenSig,
    GenericEnvironment *GenEnv,
    SubstitutionMap &SubstMap,
    bool ResolveArchetypes,
    ArchetypeBuilder &Builder,
    Module *SM) {
  if (!GenSig)
    return;

  auto *SigBuilder = SM->getASTContext().getOrCreateArchetypeBuilder(
      GenSig->getCanonicalSignature(), SM);

  // Next, add each of the requirements (mapped from the requirement's
  // interface types into the specialized interface type parameters).
  RequirementSource source(RequirementSource::Explicit, SourceLoc());
  SourceLoc sourceLoc;

  // Add requirements derived from the caller signature for the
  // caller's archetypes mapped to the specialized signature.
  for (auto &reqReq : GenSig->getRequirements()) {
    SubstitutableType *FirstTy =
        dyn_cast<SubstitutableType>(reqReq.getFirstType()->getCanonicalType());
    // If this generic parameter is not mapped, no need to handle its requirements.
    if (FirstTy && !SubstMap.getMap().lookup(FirstTy)) {
      assert(!isa<SubstitutableType>(reqReq.getSecondType()->getCanonicalType()));
      continue;
    }

    switch (reqReq.getKind()) {
    case RequirementKind::Conformance: {
      // Substitute the constrained types.
      auto first = substConcreteTypesForDependentTypes(
          *SM, SubstMap, reqReq.getFirstType()->getCanonicalType());
      if (!first)
        continue;
      if (!first->isTypeParameter())
        break;

      auto Req = RequirementRepr::getTypeConstraint(
          TypeLoc::withoutLoc(first), sourceLoc,
          TypeLoc::withoutLoc(reqReq.getSecondType()));
      auto Failure = Builder.addRequirement(Req);

      assert(!Failure);
      break;
    }

    case RequirementKind::Layout: {
      // Substitute the constrained types.
      auto first = substConcreteTypesForDependentTypes(
          *SM, SubstMap, reqReq.getFirstType()->getCanonicalType());
      if (!first)
        continue;
      if (!first->isTypeParameter())
        break;

      auto Req = RequirementRepr::getLayoutConstraint(
          TypeLoc::withoutLoc(first), sourceLoc,
          LayoutConstraintLoc::withoutLoc(reqReq.getLayoutConstraint()));
      auto Failure = Builder.addRequirement(Req);

      assert(!Failure);
      break;
    }

    case RequirementKind::Superclass: {
      // Substitute the constrained types.
      auto first = substConcreteTypesForDependentTypes(
          *SM, SubstMap, reqReq.getFirstType()->getCanonicalType());
      auto second = substConcreteTypesForDependentTypes(
          *SM, SubstMap, reqReq.getSecondType()->getCanonicalType());

      if (!first)
        continue;
      if (!first->isTypeParameter())
        break;

      auto Req = RequirementRepr::getTypeConstraint(
          TypeLoc::withoutLoc(first), sourceLoc, TypeLoc::withoutLoc(second));
      auto Failure = Builder.addRequirement(Req);

      assert(!Failure);
      break;
    }

    case RequirementKind::SameType: {
      if (SigBuilder->resolveArchetype(reqReq.getFirstType()) ==
              SigBuilder->resolveArchetype(reqReq.getSecondType()))
        continue;

      if (ResolveArchetypes &&
          GenEnv->mapTypeIntoContext(SM, reqReq.getFirstType())
                  ->getCanonicalType() ==
              GenEnv->mapTypeIntoContext(SM, reqReq.getSecondType())
                  ->getCanonicalType())
        continue;

      // Substitute the constrained types.
      auto first = substConcreteTypesForDependentTypes(
          *SM, SubstMap, reqReq.getFirstType()->getCanonicalType());
      auto second = substConcreteTypesForDependentTypes(
          *SM, SubstMap, reqReq.getSecondType()->getCanonicalType());

      if (!first->isTypeParameter()) {
        if (!second->isTypeParameter())
          break;
        std::swap(first, second);
      }

      if (first->is<GenericTypeParamType>() &&
          second->is<GenericTypeParamType>())
        continue;

      auto Req = RequirementRepr::getSameType(
          TypeLoc::withoutLoc(first), sourceLoc, TypeLoc::withoutLoc(second));
      auto Failure = Builder.addRequirement(Req);

      assert(!Failure);
      break;
    }
    }
  }
}

/// Returns true if a given substitution should participate in the
/// partial specialization.
///
/// TODO:
/// If a replacement is an archetype or a dependent type
/// of an archetype, then it does not make sense to substitute
/// it into the signature of the specialized function, because
/// it does not provide any benefits at runtime and may actually
/// lead to performance degradations.
///
/// If a replacement is a loadable type, it is most likely
/// rather beneficial to specialize using this substitution, because
/// it would allow for more efficient codegen for this type.
///
/// If a substitution simply replaces a generic parameter in the callee
/// by a generic parameter in the caller and this generic parameter
/// in the caller does have more "specific" conformances or requirements,
/// then it does name make any sense to perform this substitutions.
/// In particular, if the generic parameter in the callee is unconstrained
/// (i.e. just T), then providing a more specific generic parameter with some
/// conformances does not help, because the body of the callee does not invoke
/// any methods from any of these new conformances, unless these conformances
/// or requirements influence the layout of the generic type, e.g. "class",
/// "Trivial of size N", "HeapAllocationObject", etc.
/// (NOTE: It could be that additional conformances can still be used due
/// to conditional conformances or something like that, if the caller
/// has an invocation like: "G<T>().method(...)". In this case, G<T>().method()
/// and G<T:P>().method() may be resolved differently).
///
/// We may need to analyze the uses of the generic type inside
/// the function body (recursively). It is ever loaded/stored?
/// Do we create objects of this type? Which conformances are
/// really used?
static bool
shouldBePartiallySpecialized(Type Replacement,
                             ArrayRef<ProtocolConformanceRef> Conformances) {
  if (!SupportGenericSubstitutions) {
    // If replacement is a concrete type, this substitution
    // should participate.
    if (!Replacement->hasArchetype())
      return true;
    return false;
  }

  // We cannot handle opened existentials yet.
  if (Replacement->isOpenedExistential())
    return false;

  if (OptimizeGenericSubstitutions) {
    // Is it an unconstrained generic parameter?
    if (Conformances.empty()) {
      if (Replacement->is<ArchetypeType>() ||
          Replacement->is<DependentMemberType>()) {
        // TODO: If Replacement add a new layout constraint, then
        // it may be still useful to perform the partial specialization.
        return false;
      }
    }
  }

  return true;
}


/// Collect all used archetypes from all the substitutions.
static void
collectUsedArchetypes(ArrayRef<Substitution> ParamSubs,
                      llvm::SmallSetVector<CanType, 8> &UsedCallerArchetypes) {

  for (auto Sub : ParamSubs) {
    auto Replacement = Sub.getReplacement()->getCanonicalType();
    if (!Replacement->hasArchetype())
      continue;

    // If the substitution will not be performed in the specialized
    // function, there is no need to check for any archetypes inside
    // the replacement.
    if (!shouldBePartiallySpecialized(Replacement, Sub.getConformances()))
      continue;

    // Add used generic parameters/archetypes.
    Replacement.visit([&](Type Ty) {
      if (auto Archetype = dyn_cast<ArchetypeType>(Ty->getCanonicalType())) {
        UsedCallerArchetypes.insert(
            Archetype->getPrimary()->getCanonicalType());
      }
    });
  }
}

/// Overall idea:
/// Create a new generic signature based on the generic signature of the callee
/// and a set of apply substitutions.
///
/// The new signature should contain generic parameters for all the caller's
/// archetypes used in the apply's substitutions. It should also have all the
/// requirements derived from the generic signature of the callee.
///
/// This function also forms the substitution map for the cloner. It maps
/// from interface types of the callee function to the archetypes of the
/// specialized function.
bool ReabstractionInfo::SpecializeConcreteAndGenericSubstitutions(
    ApplySite Apply, SILFunction *Callee, ArrayRef<Substitution> ParamSubs) {

  SILModule &M = Callee->getModule();
  Module *SM = M.getSwiftModule();
  auto &Ctx = M.getASTContext();

  // Caller is the SILFunction containing the apply instruction.
  // Apply may be empty in case this function is invoked from the
  // EagerSpecializer.
  SILFunction *Caller = (Apply) ? Apply.getFunction() : nullptr;
  auto CallerGenericSig =
      Caller ? Caller->getLoweredFunctionType()->getGenericSignature()
             : nullptr;
  auto CallerGenericEnv = Caller ? Caller->getGenericEnvironment() : nullptr;

  // Callee is the generic function being called by the apply instruction.
  auto CalleeFnTy = Callee->getLoweredFunctionType();
  auto CalleeGenericSig = CalleeFnTy->getGenericSignature();
  auto CalleeGenericEnv = Callee->getGenericEnvironment();

  // Maps callee's interface types to caller's contextual types.
  auto CalleeInterfaceToCallerArchetypeMap =
      CalleeGenericSig->getSubstitutionMap(ParamSubs);

  // Map caller's interface types to the new specialized interface types.
  SubstitutionMap CallerInterfaceToSpecializedInterfaceMap;
  // Map callee's interface types to the new specialized interface types.
  SubstitutionMap CalleeInterfaceToSpecializedInterfaceMap;
  // Map callee's interface types to the new specialized contextual archetypes.
  // This is required for cloning the callee into a specialized function.
  SubstitutionMap CalleeInterfaceToSpecializedArchetypeMap;
  // Map caller's archetypes to the new specialized interface types.
  SubstitutionMap CallerArchetypeToSpecializedInterfaceMap;
  // Map new specialized interface types back to the caller's archetypes.
  // It is a reverse map for CallerArchetypeToSpecializedInterfaceMap.
  SubstitutionMap SpecializedInterfaceToCallerArchetypeMap;

  // Construct an archetype builder by collecting the constraints from the
  // requirements of the original generic function and substitutions,
  // because both define the capabilities of the requirement.

  // This is a builder for a new specialized generic signature.
  ArchetypeBuilder Builder(*SM);

  // Set of newly created generic type parameters.
  SmallVector<GenericTypeParamType*, 4> AllGenericParams;

  // Archetypes used in the substitutions of an apply instructions.
  // These are the contextual archetypes of the caller function, which
  // invokes a generic function that is being specialized.
  llvm::SmallSetVector<CanType, 8> UsedCallerArchetypes;

  // Collect all used caller's archetypes from all the substitutions.
  collectUsedArchetypes(ParamSubs, UsedCallerArchetypes);

  unsigned Depth = 0;
  unsigned Index = 0;

  // Add generic parameters that will come from the Callee.
  // These are those generic type parameters that will not be substituted,
  // because shouldBePartiallySpecialized decided to skip them.
  for (auto GP : CalleeGenericSig->getGenericParams()) {
    auto CanTy = GP->getCanonicalType();
    auto Replacement = CalleeInterfaceToCallerArchetypeMap.getMap().lookup(GP);
    if (!Replacement)
      continue;

    if (shouldBePartiallySpecialized(
            Replacement,
            CalleeInterfaceToCallerArchetypeMap.getConformances(CanTy)))
      continue;

    // This generic parameter is not to be partially specialized.
    // Create an equivalent generic parameter in the specialized
    // generic environment.
    auto SubstGenericParam = GenericTypeParamType::get(Depth, Index++, Ctx);
    auto SubstGenericParamCanTy = SubstGenericParam->getCanonicalType();

    AllGenericParams.push_back(SubstGenericParam);
    Builder.addGenericParameter(SubstGenericParam);

    CalleeInterfaceToSpecializedInterfaceMap.addSubstitution(
        CanSubstitutableType(GP), SubstGenericParamCanTy);

    copySubstitutionMapEntry(GP, CalleeInterfaceToCallerArchetypeMap,
                             SubstGenericParam,
                             SpecializedInterfaceToCallerArchetypeMap);
  }

  // None of the provided substitutions can be used for partial specialization.
  // Thus, the signature of the function won't change.
  if (AllGenericParams.size() == ParamSubs.size()) {
    return false;
  }

  // Generate a new generic type parameter for each used archetype from
  // the caller.
  for (auto CallerArchetype : UsedCallerArchetypes) {
    auto CallerGenericParam =
        CallerGenericEnv->mapTypeOutOfContext(CallerArchetype)
            ->getCanonicalType();
    // No need to introduce new generic parameters for dependent types.
    if (!CallerGenericParam->is<DependentMemberType>())
      continue;
    assert(CallerGenericParam);
    assert(CallerGenericParam->is<GenericTypeParamType>());

    // Create an equivalent generic parameter.
    auto SubstGenericParam = GenericTypeParamType::get(Depth, Index++, Ctx);
    auto SubstGenericParamCanTy = SubstGenericParam->getCanonicalType();

    AllGenericParams.push_back(SubstGenericParam);
    Builder.addGenericParameter(SubstGenericParam);

    // Map the caller archetype to the new generic parameter type.
    CallerArchetypeToSpecializedInterfaceMap.addSubstitution(
        cast<SubstitutableType>(CallerArchetype), SubstGenericParam);
    // Add a reverse mapping.
    SpecializedInterfaceToCallerArchetypeMap.addSubstitution(
        CanSubstitutableType(SubstGenericParam), CallerArchetype);

    // Map the original generic parameter type to the new generic parameter
    // type in the specialized generic environment.
    CallerInterfaceToSpecializedInterfaceMap.addSubstitution(
        cast<SubstitutableType>(CallerGenericParam), SubstGenericParamCanTy);
  }

  // Copy entries for the generic type parameters mapped to concrete types.
  for (auto GP : CalleeGenericSig->getGenericParams()) {
    // Skip if this generic parameter is not substituted.
    if (CalleeInterfaceToSpecializedInterfaceMap.getMap().lookup(GP))
      continue;
    auto Replacement = CalleeInterfaceToCallerArchetypeMap.getMap().lookup(GP);
    if (!Replacement)
      continue;
    auto ReplacementTy = Replacement->getCanonicalType();
    // Map the replacement to the interface type of the specialization.
    CanType SpecializedReplacementTy =
        ReplacementTy.subst(CallerArchetypeToSpecializedInterfaceMap)
            ->getCanonicalType();

    CalleeInterfaceToSpecializedInterfaceMap.addSubstitution(
        CanSubstitutableType(GP), SpecializedReplacementTy);
  }

  // Next, add each of the requirements (mapped from the requirement's
  // interface types into the specialized interface type parameters).
  // TODO: Do we need to add requirements of the caller's archetypes, which
  // stem from the caller's generic signature? If so, which ones? All of them?
  // Just some of them? Most likely we need to add only those which are not
  // present in the callee's signature.
  remapRequirements(CallerGenericSig, CallerGenericEnv,
                    CallerInterfaceToSpecializedInterfaceMap, true, Builder,
                    SM);

  remapRequirements(CalleeGenericSig, CalleeGenericEnv,
                    CalleeInterfaceToSpecializedInterfaceMap, false, Builder,
                    SM);

  // Finalize the archetype builder.
  Builder.finalize(SourceLoc());

  if (!AllGenericParams.empty()) {
    // Produce the generic signature and environment.
    auto GenPair = Builder.getGenericSignatureAndEnvironment();
    SpecializedGenericSig = GenPair.first->getCanonicalSignature();
    SpecializedGenericEnv = GenPair.second;
  }

  if (SpecializedGenericSig) {
    // Create the updated SubstitutionMap to be used by the cloner.
    for (auto GP : CalleeGenericSig->getGenericParams()) {
      auto Ty = GP;
      auto Replacement =
          CalleeInterfaceToCallerArchetypeMap.getMap().lookup(GP);
      if (!Replacement)
        continue;
      auto ReplacementTy = Replacement->getCanonicalType();

      auto SpecializedGP =
          CalleeInterfaceToSpecializedInterfaceMap.getMap().lookup(GP);

      if (SpecializedGP) {
        CalleeInterfaceToSpecializedArchetypeMap.addSubstitution(
            CanSubstitutableType(GP),
            SpecializedGenericEnv->mapTypeIntoContext(SM, SpecializedGP));
        continue;
      }

      if (!ReplacementTy->hasArchetype()) {
        // Copy the concrete substitution.
        copySubstitutionMapEntry(Ty, CalleeInterfaceToCallerArchetypeMap, Ty,
                                 CalleeInterfaceToSpecializedArchetypeMap);
        continue;
      }

      // It is a substitution where the replacement contains an archetype.
      auto SubstInterfaceReplacementTy =
          ReplacementTy.subst(CallerArchetypeToSpecializedInterfaceMap)
              ->getCanonicalType();
      auto SubstReplacementTy = SpecializedGenericEnv->mapTypeIntoContext(
          SM, SubstInterfaceReplacementTy);

      CalleeInterfaceToSpecializedArchetypeMap.addSubstitution(
          CanSubstitutableType(GP), SubstReplacementTy);
    }
  } else {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
    CalleeInterfaceToSpecializedArchetypeMap = CalleeInterfaceToCallerArchetypeMap;
  }

  // CalleeInterfaceToSpecializedInterfaceMap substitutes interface types
  // from the callee's generic environment by interface types from
  // the specialized generic environment. But substGenericArgs has some
  // issues with this kind of substitutions (rdar://29711782).

  // We want to map the interface type of the callee to the interface type
  // of the specialized callee by remapping the generic types.

  auto SpecializedSubstFnTy = CalleeFnTy->substGenericArgs(
      M,
      QueryTypeSubstitutionMap{
          CalleeInterfaceToSpecializedInterfaceMap.getMap()},
      QueryConformance(SM), SpecializedGenericSig);

  // Canonicalize the type.
  if (SpecializedGenericSig) {
    SpecializedSubstFnTy = CanSILFunctionType(
        SpecializedGenericSig
            ->getCanonicalTypeInContext(SpecializedSubstFnTy, *SM)
            ->getAs<SILFunctionType>());
  }

  if (SpecializedGenericSig && SpecializedGenericSig->areAllParamsConcrete()) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
  }

  SubstitutedType = SILFunctionType::get(
      SpecializedGenericSig, SpecializedSubstFnTy->getExtInfo(),
      SpecializedSubstFnTy->getCalleeConvention(),
      SpecializedSubstFnTy->getParameters(),
      SpecializedSubstFnTy->getAllResults(),
      SpecializedSubstFnTy->getOptionalErrorResult(), Ctx);

  assert(!SubstitutedType->hasArchetype() &&
         "Function type should not contain archetypes");

  SmallVector<Substitution, 8> ClonerSubsVector;

  // Form a substitution list to be used by the cloner when it clones the
  // body of the original function.
  CalleeGenericSig->getSubstitutions(
      CalleeInterfaceToSpecializedArchetypeMap.getMap(),
      QueryConformance(SM), ClonerSubsVector);

  ClonerParamSubs = Ctx.AllocateCopy(ClonerSubsVector);

  // Form a substitution list to be used by the caller when it invokes
  // the specialized function.
  if (SpecializedGenericSig && !SpecializedGenericSig->areAllParamsConcrete()) {
    SmallVector<Substitution, 8> CallerSubsVector;
    SpecializedGenericSig->getSubstitutions(
        SpecializedInterfaceToCallerArchetypeMap.getMap(), QueryConformance(SM),
        CallerSubsVector);

    CallerParamSubs = Ctx.AllocateCopy(CallerSubsVector);
  }

  createSubstitutedAndSpecializedTypes();
  if (getSubstitutedType() != Callee->getLoweredFunctionType()) {
    if (getSubstitutedType()->isPolymorphic()) {
      DEBUG(llvm::dbgs() << "Created new type: " << getSpecializedType()
                         << "\n";
            if (Apply) Apply.getInstruction()->dumpInContext());
    }
  }

  return true;
}

// This approach does not try to create a new generic signature with
// a different number of generic type parameters. Instead, it simply
// builds a new signature based on the old one and adds same type
// requirements for those generic type parameters that are concrete
// according to the partial substitution. This way, the signature
// has exactly the same generic parameter types, just with more
// requirements. It is much easier to create than using the other
// approach, because it does not require any complex re-mappings
// of generic types and archetypes.
bool ReabstractionInfo::SpecializeConcreteSubstitutions(
    ApplySite Apply, SILFunction *Callee, ArrayRef<Substitution> ParamSubs) {

  SILModule &M = Callee->getModule();
  Module *SM = M.getSwiftModule();
  auto &Ctx = M.getASTContext();

  SubstitutionMap InterfaceSubs;

  // Get the original substitution map.
  if (Callee->getLoweredFunctionType()->getGenericSignature())
    InterfaceSubs = Callee->getLoweredFunctionType()->getGenericSignature()
      ->getSubstitutionMap(ParamSubs);

  if (ShortcutFullSpecialization && !HasUnboundGenericParams) {
    CallerInterfaceSubs = InterfaceSubs;
  }

  // The overall approach for partial specializations is as follows:
  // - Form a new generic signature by starting with the old one and
  // adding new same-type requirements for all generic parameters
  // which are substituted by the concrete types in the apply instruction.
  // - Form two new substitution maps: one to be used by the apply instruction,
  // which will invoke a specialized function, and the other one to be used
  // by the SIL function cloner, when it clones the original generic function
  // into a partially specialized function.
  //
  // Current approach has some limitations: It does not support a partial
  // specialization if a generic type is substituted by a non-concrete type,
  // containing some unbound generic parameters. The reason for this limitation
  // is that it would require an additional implementation complexity, because
  // the usage of generic parameters in the replacement may require a creation
  // of a new generic signature for a different set of generic parameters than
  // in the original generic function. This would make the re-mapping of
  // generic parameters rather complex.
  // If we see a need for this more general form of partial specializaiton, we
  // may support it in the future.
  if (!ShortcutFullSpecialization || HasUnboundGenericParams) {
    auto CalleeGenericSig = Callee->getLoweredFunctionType()->getGenericSignature();
    auto CalleeGenericEnv = Callee->getGenericEnvironment();
    SpecializedGenericSig = CalleeGenericSig;

    // Form a new generic signature based on the old one.
    ArchetypeBuilder Builder(*SM);

    // First, add the old generic signature.
    Builder.addGenericSignature(CalleeGenericSig);

    // Checks if a given generic parameter type or a dependent type
    // is mapped to a non-concrete type, i.e. a type that has
    // archetypes. For dependent types, it also checks that its
    // base generic parameter type is also mapped to a non-concrete type.
    //
    // FIXME: This check is introduced, because without it we sometimes
    // run into situations, where the dependent type is mapped to a
    // concrete type, but its base generic parameter is mapped to a
    // non-concrete type. This crashed the compiler on stdlib.
    // It is not clear why this happens. Thus this check, which
    // ensures that we do not try to handle these failing cases for now.
    auto IsNonConcreteReplacementType =
        [&InterfaceSubs](CanType Ty, Type DefaultReplacementTy) -> bool {
      if ( DefaultReplacementTy && DefaultReplacementTy->getCanonicalType()->hasArchetype())
        return true;
      auto BaseTy = Ty;
      while (auto DepTy = dyn_cast<DependentMemberType>(BaseTy)) {
        BaseTy = DepTy->getBase()->getCanonicalType();
      };
      assert(isa<GenericTypeParamType>(BaseTy));
      return InterfaceSubs.getMap()
          .lookup(cast<SubstitutableType>(BaseTy->getCanonicalType()))
          ->getCanonicalType()
          ->hasArchetype();
    };

    // For each substitution with a concrete type as a replacement,
    // add a new concrete type equality requirement.
    for (auto &entry : InterfaceSubs.getMap()) {
      auto CanTy = entry.first->getCanonicalType();
      auto IsNonConcreteReplacement =
          IsNonConcreteReplacementType(CanTy, entry.second);
      if (!IsNonConcreteReplacement) {
        // If it is a dependent type and its parent generic
        // parameter type is concrete, no need to add
        // a requirement for the dependent type as it can be derived from the
        // parent generic parameter requirements.
        if (!isa<GenericTypeParamType>(CanTy)) {
          auto BaseGP = getBaseGenericTypeParamType(CanTy);
          if (!InterfaceSubs.getMap().lookup(BaseGP)->hasArchetype())
            continue;
        }

        auto CanTy = entry.first->getCanonicalType();
        auto OutTy = CanTy;
        auto EqualTy = entry.second->getCanonicalType();
        // TODO: For pre-specializations we need to add a different kind of a
        // requirement, e.g. that the type conforms to TrivialTypeOfSizeN or
        // to RefCountedObject.
        {
          if (Builder.addSameTypeRequirement(
              OutTy, EqualTy->getCanonicalType(),
              RequirementSource(RequirementSource::Explicit, SourceLoc()))) {
            // Bail if this requirement cannot be added;
            // llvm_unreachable("An impossible requirement?");
            return false;
          }
        }
      }
    }

    auto GenPair = Builder.getGenericSignatureAndEnvironment();
    SpecializedGenericSig = GenPair.first->getCanonicalSignature();
    SpecializedGenericEnv = GenPair.second;

    if (ShortcutFullSpecialization && !HasUnboundGenericParams) {
      createSubstitutedAndSpecializedTypes();
      return true;
    }

    // Substitution map to be used by the cloner.
    SubstitutionMap ClonerSubsMap;

    auto &InterfaceSubsMap = InterfaceSubs.getMap();
    auto ForwardingSubs = SpecializedGenericEnv->getForwardingSubstitutions();
    auto ForwardingInterfaceSubsMap =
        SpecializedGenericSig->getSubstitutionMap(ForwardingSubs);

    // Create new substitution maps. Simply copy the mappings for those types
    // which are mapped to concrete types in the original list of substitutions.
    // And if a generic parameter is mapped to a non-concrete type in the
    // original substitutions, simply map it to a corresponding archetype,
    // i.e. do the same what a forwarding substitution does.
    for (auto &entry : CalleeGenericSig->getAllDependentTypes()) {
      auto CanTy = entry->getCanonicalType();
      auto ST = dyn_cast<SubstitutableType>(CanTy);
      if (!ST || !isa<GenericTypeParamType>(ST))
        continue;
      auto Repl = ST ? InterfaceSubsMap.lookup(ST) : CanType();
      auto IsNonConcreteReplacement =
          IsNonConcreteReplacementType(CanTy, Repl);
      if (IsNonConcreteReplacement) {
        Type Ty;

        // Map generic parameter type to an interface type in the
        // old generic environment.
        Ty = CalleeGenericEnv->mapTypeOutOfContext(CanTy)->getCanonicalType();
        CallerInterfaceSubs.addSubstitution(ST, Ty);

        // Map generic parameter type to a contextual type in the
        // new generic environment. This will be used by the cloner.
        Ty = SpecializedGenericEnv->mapTypeIntoContext(SM, CanTy)
                 ->getCanonicalType();
        ClonerSubsMap.addSubstitution(ST, Ty);
      } else {
        // Copy entries if a replacement is a concrete type.
        copySubstitutionMapEntry(CanTy, InterfaceSubs, CanTy, CallerInterfaceSubs);
        copySubstitutionMapEntry(CanTy, InterfaceSubs, CanTy, ClonerSubsMap);
      }
    }

    // Create new substitutions lists for the caller and for the cloner.
    SmallVector<Substitution, 8> ClonerSubsVector;
    SmallVector<Substitution, 8> CallerSubsVector;

    // First, get a substitution map from the original signature.
    // It is based on the substitution list from the apply site.
    auto ParamMap = CalleeGenericSig->getSubstitutionMap(ParamSubs);
    // Then, use this map to form a list of substitutions for calling the
    // specialized function.
    // This new substitution list may contain less elements than the original one,
    // because generic parameter types with concrete type equality requirements
    // do not need a substitution.
    SpecializedGenericSig->getSubstitutions(ParamMap, CallerSubsVector);

    // Form a substitution list to be used by the cloner when it clones the
    // body of the original function.
    CalleeGenericSig->getSubstitutions(ClonerSubsMap.getMap(),
                                       QueryConformance(SM), ClonerSubsVector);

    CallerParamSubs = Ctx.AllocateCopy(CallerSubsVector);
    ClonerParamSubs = Ctx.AllocateCopy(ClonerSubsVector);
  }

  createSubstitutedAndSpecializedTypes();
  return true;
}

ReabstractionInfo::ReabstractionInfo(ApplySite Apply, SILFunction *Callee,
                                     ArrayRef<Substitution> ParamSubs) {
  if (!prepareAndCheck(Apply, Callee, ParamSubs))
    return;

  bool Success = false;
  if (CanSpecializeConcreteAndGenericSubstitutions) {
    Success = SpecializeConcreteAndGenericSubstitutions(Apply, Callee,
                                                        ParamSubs);
  } else {
    Success = SpecializeConcreteSubstitutions(Apply, Callee, ParamSubs);
  }

  // Some sanity checks.
  auto SpecializedFnTy = getSpecializedType();
  auto SpecializedSubstFnTy = SpecializedFnTy;

  if (Success && SpecializedFnTy->isPolymorphic() &&
      !getCallerParamSubstitutions().empty()) {
    auto CalleeFnTy = Callee->getLoweredFunctionType();
    assert(CalleeFnTy->isPolymorphic());
    auto CalleeSubstFnTy = CalleeFnTy->substGenericArgs(
        Callee->getModule(), getOriginalParamSubstitutions());
    assert(!CalleeSubstFnTy->isPolymorphic() &&
           "Substituted callee type should not be polymorphic");
    assert(!CalleeSubstFnTy->hasTypeParameter() &&
           "Substituted callee type should not have type parameters");

    SpecializedSubstFnTy = SpecializedFnTy->substGenericArgs(
        Callee->getModule(), getCallerParamSubstitutions());
    assert(!SpecializedSubstFnTy->isPolymorphic() &&
           "Substituted callee type should not be polymorphic");
    assert(!SpecializedSubstFnTy->hasTypeParameter() &&
           "Substituted callee type should not have type parameters");
    auto SpecializedCalleeSubstFnTy =
        createSpecializedType(CalleeSubstFnTy, Callee->getModule());

    if (SpecializedSubstFnTy != SpecializedCalleeSubstFnTy) {
      llvm::dbgs() << "SpecializedFnTy:\n" << SpecializedFnTy << "\n";
      llvm::dbgs() << "SpecializedSubstFnTy:\n" << SpecializedSubstFnTy << "\n";
      for (auto Sub : getCallerParamSubstitutions()) {
        llvm::dbgs() << "Sub:\n";
        Sub.dump();
      }
      llvm::dbgs() << "\n\n";

      llvm::dbgs() << "CalleeFnTy:\n" << CalleeFnTy << "\n";
      llvm::dbgs() << "SpecializedCalleeSubstFnTy:\n" << SpecializedCalleeSubstFnTy << "\n";
      for (auto Sub : ParamSubs) {
        llvm::dbgs() << "Sub:\n";
        Sub.dump();
      }
      llvm::dbgs() << "\n\n";
      assert(SpecializedSubstFnTy == SpecializedCalleeSubstFnTy &&
             "Substituted function types should be the same");
    }
  }

  // If the new type is the same, there is nothing to do and 
  // no specialization should be performed.
  if (!Success || getSubstitutedType() == Callee->getLoweredFunctionType()) {
    SpecializedType = CanSILFunctionType();
    SubstitutedType = CanSILFunctionType();
    SpecializedGenericSig = nullptr;
    return;
  }
}

/// Create a new substituted type with the updated signature.
CanSILFunctionType
ReabstractionInfo::createSubstitutedType(SILFunction *OrigF,
                                         const SubstitutionMap &SubstMap,
                                         bool HasUnboundGenericParams) {
  auto &M = OrigF->getModule();
  auto SM = M.getSwiftModule();
  auto OrigFnTy = OrigF->getLoweredFunctionType();

  // First substitute concrete types into the existing function type.
  auto FnTy = OrigFnTy->substGenericArgs(
      M, QueryTypeSubstitutionMap{SubstMap.getMap()}, QueryConformance(SM));

  if (ShortcutFullSpecialization && !HasUnboundGenericParams)
    return FnTy;

  if (SpecializedGenericSig)
    FnTy = CanSILFunctionType(
        SpecializedGenericSig
            ->getCanonicalTypeInContext(FnTy, *M.getSwiftModule())
            ->getAs<SILFunctionType>());

  if (SpecializedGenericSig && SpecializedGenericSig->areAllParamsConcrete()) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
  }

  // Use the new specialized generic signature.
  auto NewFnTy = SILFunctionType::get(
      SpecializedGenericSig, FnTy->getExtInfo(), FnTy->getCalleeConvention(),
      FnTy->getParameters(), FnTy->getAllResults(),
      FnTy->getOptionalErrorResult(), M.getASTContext());

  // This is an interface type. It should not have any type parameters or
  // archetypes.
  assert(!NewFnTy->hasTypeParameter() && !NewFnTy->hasArchetype());
  return NewFnTy;
}

Type ReabstractionInfo::mapTypeIntoContext(Type type) const {
  if (!type->hasTypeParameter())
    return type;
  assert(getSpecializedGenericEnvironment());
  return getSpecializedGenericEnvironment()->mapTypeIntoContext(
      getModule().getSwiftModule(), type);
}

SILType ReabstractionInfo::mapTypeIntoContext(SILType type) const {
  return doSubstDependentSILType(
      getModule(),
      [&](CanType t) { return mapTypeIntoContext(t)->getCanonicalType(); },
      type);
}

// Convert the substituted function type into a specialized function type based
// on the ReabstractionInfo.
CanSILFunctionType ReabstractionInfo::
createSpecializedType(CanSILFunctionType SubstFTy, SILModule &M) const {
  llvm::SmallVector<SILResultInfo, 8> SpecializedResults;
  llvm::SmallVector<SILParameterInfo, 8> SpecializedParams;

  unsigned ResultIdx = 0;
  for (SILResultInfo RI : SubstFTy->getAllResults()) {
    if (RI.isDirect()) {
      SpecializedResults.push_back(RI);
    } else {
      if (isResultConverted(ResultIdx++)) {
        // Convert the indirect result to a direct result.
        SILType SILResTy = SILType::getPrimitiveObjectType(RI.getType());
        // Indirect results are passed as owned, so we also need to pass the
        // direct result as owned (except it's a trivial type).
        auto C = (mapTypeIntoContext(SILResTy).isTrivial(M)
                      ? ResultConvention::Unowned
                      : ResultConvention::Owned);
        SpecializedResults.push_back(SILResultInfo(RI.getType(), C));
      } else {
        // No conversion: re-use the original result info.
        SpecializedResults.push_back(RI);
      }
    }
  }
  unsigned ParamIdx = 0;
  for (SILParameterInfo PI : SubstFTy->getParameters()) {
    if (isParamConverted(ParamIdx++)) {
      // Convert the indirect parameter to a direct parameter.
      SILType SILParamTy = SILType::getPrimitiveObjectType(PI.getType());
      // Indirect parameters are passed as owned, so we also need to pass the
      // direct parameter as owned (except it's a trivial type).
      auto C = mapTypeIntoContext(SILParamTy).isTrivial(M)
                    ? ParameterConvention::Direct_Unowned
                    : ParameterConvention::Direct_Owned;
      SpecializedParams.push_back(SILParameterInfo(PI.getType(), C));
    } else {
      // No conversion: re-use the original parameter info.
      SpecializedParams.push_back(PI);
    }
  }
  return
    SILFunctionType::get(SubstFTy->getGenericSignature(),
                         SubstFTy->getExtInfo(),
                         SubstFTy->getCalleeConvention(), SpecializedParams,
                         SpecializedResults, SubstFTy->getOptionalErrorResult(),
                         M.getASTContext());
}

std::pair<GenericEnvironment *, GenericSignature *>
getSignatureWithRequirements(GenericSignature *OrigGenSig,
                             GenericEnvironment *OrigGenericEnv,
                             ArrayRef<RequirementRepr> Requirements,
                             SILModule &M) {
  // Form a new generic signature based on the old one.
  ArchetypeBuilder Builder(*M.getSwiftModule());

  // First, add the old generic signature.
  Builder.addGenericSignature(OrigGenSig);

  // For each substitution with a concrete type as a replacement,
  // add a new concrete type equality requirement.
  for (auto &Req : Requirements) {
    Builder.addRequirement(Req);
  }

  auto *GenericSig = Builder.getGenericSignature();
  // Remember the new generic environment.
  auto *GenericEnv = Builder.getGenericEnvironment(GenericSig);

  return std::make_pair(GenericEnv, GenericSig);
}

/// Perform some sanity checks for the requirements
static void
checkSpecializationRequirements(ArrayRef<RequirementRepr> Requirements) {
  for (auto &Req : Requirements) {
    if (Req.getKind() == RequirementReprKind::SameType) {
      auto FirstType = Req.getFirstType();
      auto SecondType = Req.getSecondType();
      assert(FirstType && SecondType);

      bool isFirstTypeNonConcrete =
          FirstType->hasArchetype() || FirstType->hasTypeParameter();
      bool isSecondTypeNonConcrete =
          SecondType->hasArchetype() || SecondType->hasTypeParameter();
      // Only one of the types should be concrete.
      assert((isFirstTypeNonConcrete ^ isSecondTypeNonConcrete) &&
             "Only concrete type same-type requirements are supported by "
             "generic specialization");
      continue;
    }

    if (Req.getKind() == RequirementReprKind::LayoutConstraint) {
      // TODO: Check that it is one of the compiler known protocols used for
      // pre-specializations.
      continue;
    }

    // TODO: Extend it to support TypeConstraint requirements once it is
    // supported?
    llvm_unreachable("Unknown type of requirement in generic specialization");
  }
}

/// Remap Requirements to RequirementReps.
static void convertRequirements(ArrayRef<Requirement> From,
                                SmallVectorImpl<RequirementRepr> &To) {
  for (auto &Req : From) {
    if (Req.getKind() == RequirementKind::SameType) {
      To.push_back(RequirementRepr::getSameType(
          TypeLoc::withoutLoc(Req.getFirstType()),
          SourceLoc(),
          TypeLoc::withoutLoc(Req.getSecondType())));
      continue;
    }

    if (Req.getKind() == RequirementKind::Conformance) {
      To.push_back(RequirementRepr::getTypeConstraint(
          TypeLoc::withoutLoc(Req.getFirstType()), SourceLoc(),
          TypeLoc::withoutLoc(Req.getSecondType())));
      continue;
    }

    if (Req.getKind() == RequirementKind::Layout) {
      To.push_back(RequirementRepr::getLayoutConstraint(
          TypeLoc::withoutLoc(Req.getFirstType()), SourceLoc(),
            LayoutConstraintLoc::withoutLoc(Req.getLayoutConstraint())));
      continue;
    }

    llvm_unreachable("Unspoorted requirement kind");
  }
}

ReabstractionInfo::ReabstractionInfo(SILFunction *OrigF,
                                     ArrayRef<Requirement> Requirements) {
  if (!OrigF->shouldOptimize()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize function " << OrigF->getName()
                       << " marked to be excluded from optimizations.\n");
    return;
  }

  // Perform some sanity checks for the requirements
  SpecializedGenericEnv = nullptr;

  OriginalF = OrigF;
  SILModule &M = OrigF->getModule();
  Module *SM = M.getSwiftModule();
  auto &Ctx = M.getASTContext();

  auto OrigGenericSig = OrigF->getLoweredFunctionType()->getGenericSignature();
  auto OrigGenericEnv = OrigF->getGenericEnvironment();
  SpecializedGenericSig = OrigGenericSig;
  auto ForwardingSubs = OrigGenericEnv->getForwardingSubstitutions();
  auto ForwardingInterfaceSubsMap =
      OrigGenericSig->getSubstitutionMap(ForwardingSubs);

  SubstitutionMap ClonerInterfaceSubs;

  SmallVector<RequirementRepr, 4> RequirementReprs;
  convertRequirements(Requirements, RequirementReprs);

  for (auto &Req : Requirements) {
    if (Req.getKind() == RequirementKind::SameType) {
      // Remember that a given generic parameter is mapped
      // to a concrete type.
      CallerInterfaceSubs.addSubstitution(
          dyn_cast<SubstitutableType>(Req.getFirstType()->getCanonicalType()),
          Req.getSecondType()->getCanonicalType());

      ClonerInterfaceSubs.addSubstitution(
          dyn_cast<SubstitutableType>(Req.getFirstType()->getCanonicalType()),
          Req.getSecondType()->getCanonicalType());
      continue;
    }
  }

  // Perform some sanity checks for the requirements
  checkSpecializationRequirements(RequirementReprs);

  std::tie(SpecializedGenericEnv, SpecializedGenericSig) =
      getSignatureWithRequirements(OrigGenericSig, OrigGenericEnv,
                                   RequirementReprs, M);

  // Form the ClonerInterfaceSubs. This map will be used later by the cloner.
  for (auto GP : OrigGenericSig->getGenericParams()) {
    auto CanTy = GP->getCanonicalType();
    if (!ForwardingInterfaceSubsMap.getMap().lookup(GP))
      continue;
    auto Replacement = CallerInterfaceSubs.getMap().lookup(GP);
    // If not in the caller mapping yet, remap to the contextual type
    // corresponding to this generic parameter type.
    if (!Replacement)
      Replacement = CanTy;
    auto ClonerReplacement = SpecializedGenericEnv->mapTypeIntoContext(SM, Replacement);
    copySubstitutionMapEntry(CanTy, ForwardingInterfaceSubsMap, CanTy,
                             ClonerInterfaceSubs, true, ClonerReplacement);
  }

  // Get parameter substitutions for the original function.
  SmallVector<Substitution, 4> ClonerSubsVector;
  OrigGenericSig->getSubstitutions(ClonerInterfaceSubs.getMap(),
                                   QueryConformance(SM), ClonerSubsVector);
  ClonerParamSubs = Ctx.AllocateCopy(ClonerSubsVector);

  SmallVector<Substitution, 4> CallerSubsVector;
  SpecializedGenericSig->getSubstitutions(ForwardingInterfaceSubsMap.getMap(),
                                          QueryConformance(SM),
                                          CallerSubsVector);
  CallerParamSubs = Ctx.AllocateCopy(CallerSubsVector);
  OriginalParamSubs = CallerParamSubs;

  HasUnboundGenericParams = !SpecializedGenericSig->areAllParamsConcrete();
  createSubstitutedAndSpecializedTypes();
}

// =============================================================================
// GenericFuncSpecializer
// =============================================================================

GenericFuncSpecializer::GenericFuncSpecializer(SILFunction *GenericFunc,
                                               ArrayRef<Substitution> ParamSubs,
                                               IsFragile_t Fragile,
                                               const ReabstractionInfo &ReInfo)
    : M(GenericFunc->getModule()),
      GenericFunc(GenericFunc),
      ParamSubs(ParamSubs),
      Fragile(Fragile),
      ReInfo(ReInfo) {

  assert(GenericFunc->isDefinition() && "Expected definition to specialize!");

  auto FnTy = ReInfo.getSpecializedType();
  Mangle::Mangler Mangler;

  // TODO: Use the SILFunctionType of the substituted function type for the mangling.
  // Encode this whole type. What would be the name length increase? Do we need to
  // go for a shorter way of encoding the type/substitutionss?
  // If we go for encoding the SILFunctionType, we don't need the
  // AdjustedParamSubstitutions anymore, because the SILFunctionType type would contain
  // all we need in form of equal type constraints.

  std::string Old;
  if (ReInfo.isPartialSpecialization()) {
    PartialSpecializationMangler OldGenericMangler(Mangler, GenericFunc, FnTy,
                                                   Fragile);
    OldGenericMangler.mangle();
    Old = Mangler.finalize();
  } else {
    GenericSpecializationMangler OldGenericMangler(Mangler, GenericFunc,
                                                   ParamSubs, Fragile);
    OldGenericMangler.mangle();
    Old = Mangler.finalize();
  }

  // TODO: Clean-up once the new mangler is merged.
#if 1
  std::string New;
  if (ReInfo.isPartialSpecialization()) {
    NewMangling::PartialSpecializationMangler NewGenericMangler(
        GenericFunc, FnTy, Fragile, /*isReAbstracted*/ true);
    New = NewGenericMangler.mangle();
  } else {
    NewMangling::GenericSpecializationMangler NewGenericMangler(
        GenericFunc, ParamSubs, Fragile, /*isReAbstracted*/ true);
    New = NewGenericMangler.mangle();
  }

  ClonedName = NewMangling::selectMangling(Old, New);
#else
  ClonedName = Old;
#endif

  DEBUG(llvm::dbgs() << "    Specialized function " << ClonedName << '\n');
}

// Return an existing specialization if one exists.
SILFunction *GenericFuncSpecializer::lookupSpecialization() {
  if (SILFunction *SpecializedF = M.lookUpFunction(ClonedName)) {
    assert(ReInfo.getSpecializedType()
           == SpecializedF->getLoweredFunctionType() &&
           "Previously specialized function does not match expected type.");
    DEBUG(llvm::dbgs() << "Found an existing specialization for: " << ClonedName
                       << "\n");
    return SpecializedF;
  }
  DEBUG(llvm::dbgs() << "Could not find an existing specialization for: "
                     << ClonedName << "\n");
  return nullptr;
}

// Forward decl for prespecialization support.
static bool linkSpecialization(SILModule &M, SILFunction *F);

// Create a new specialized function if possible, and cache it.
SILFunction *GenericFuncSpecializer::tryCreateSpecialization() {
  // Do not create any new specializations at Onone.
  if (M.getOptions().Optimization <= SILOptions::SILOptMode::None)
    return nullptr;

  DEBUG(
    if (M.getOptions().Optimization <= SILOptions::SILOptMode::Debug) {
      llvm::dbgs() << "Creating a specialization: " << ClonedName << "\n"; });

  // Create a new function.
  SILFunction *SpecializedF = GenericCloner::cloneFunction(
      GenericFunc, Fragile, ReInfo,
      // Use these substitutions inside the new specialized function being
      // created.
      ReInfo.getClonerParamSubstitutions(),
      ClonedName);
  assert(SpecializedF->hasUnqualifiedOwnership());
  // Check if this specialization should be linked for prespecialization.
  linkSpecialization(M, SpecializedF);
  return SpecializedF;
}

// =============================================================================
// Apply substitution
// =============================================================================

/// Fix the case where a void function returns the result of an apply, which is
/// also a call of a void-returning function.
/// We always want a void function returning a tuple _instruction_.
static void fixUsedVoidType(SILValue VoidVal, SILLocation Loc,
                            SILBuilder &Builder) {
  assert(VoidVal->getType().isVoid());
  if (VoidVal->use_empty())
    return;
  auto *NewVoidVal = Builder.createTuple(Loc, VoidVal->getType(), { });
  VoidVal->replaceAllUsesWith(NewVoidVal);
}

// Prepare call arguments. Perform re-abstraction if required.
static void prepareCallArguments(ApplySite AI, SILBuilder &Builder,
                                 const ReabstractionInfo &ReInfo,
                                 SmallVectorImpl<SILValue> &Arguments,
                                 SILValue &StoreResultTo) {
  SILLocation Loc = AI.getLoc();
  unsigned Idx = ReInfo.getIndexOfFirstArg(AI);
  for (auto &Op : AI.getArgumentOperands()) {
    if (ReInfo.isArgConverted(Idx)) {
      if (ReInfo.isResultIndex(Idx)) {
        // The result is converted from indirect to direct. We need to insert
        // a store later.
        assert(!StoreResultTo);
        StoreResultTo = Op.get();
      } else {
        // An argument is converted from indirect to direct. Instead of the
        // address we pass the loaded value.
        SILValue Val = Builder.createLoad(Loc, Op.get(),
                                          LoadOwnershipQualifier::Unqualified);
        Arguments.push_back(Val);
      }
    } else {
      Arguments.push_back(Op.get());
    }
    ++Idx;
  }
}

/// Return a substituted callee function type.
static CanSILFunctionType
getCalleeSubstFunctionType(SILValue Callee, const ReabstractionInfo &ReInfo) {
  // Create a substituted callee type.
  auto CanFnTy =
      dyn_cast<SILFunctionType>(Callee->getType().getSwiftRValueType());
  auto CalleeSubstFnTy = CanFnTy;

  if (ReInfo.getSpecializedType()->isPolymorphic() &&
      !ReInfo.getCallerParamSubstitutions().empty()) {
    CalleeSubstFnTy = CanFnTy->substGenericArgs(
        ReInfo.getNonSpecializedFunction()->getModule(),
        ReInfo.getCallerParamSubstitutions());
    assert(!CalleeSubstFnTy->isPolymorphic() &&
           "Substituted callee type should not be polymorphic");
    assert(!CalleeSubstFnTy->hasTypeParameter() &&
           "Substituted callee type should not have type parameters");
  }

  return CalleeSubstFnTy;
}

// Create a new apply based on an old one, but with a different
// function being applied.
static ApplySite replaceWithSpecializedCallee(ApplySite AI,
                                              SILValue Callee,
                                              SILBuilder &Builder,
                                              const ReabstractionInfo &ReInfo) {
  SILLocation Loc = AI.getLoc();
  SmallVector<SILValue, 4> Arguments;
  SILValue StoreResultTo;

  prepareCallArguments(AI, Builder, ReInfo, Arguments, StoreResultTo);

  // Create a substituted callee type.
  ArrayRef<Substitution> Subs;
  if (ReInfo.getSpecializedType()->isPolymorphic()) {
    Subs = ReInfo.getCallerParamSubstitutions();
  }

  auto CalleeSubstFnTy = getCalleeSubstFunctionType(Callee, ReInfo);
  auto CalleeSILSubstFnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);

  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    SILBasicBlock *ResultBB = TAI->getNormalBB();
    assert(ResultBB->getSinglePredecessorBlock() == TAI->getParent());
    auto *NewTAI =
        Builder.createTryApply(Loc, Callee, CalleeSILSubstFnTy, Subs, Arguments,
                               ResultBB, TAI->getErrorBB());
    if (StoreResultTo) {
      // The original normal result of the try_apply is an empty tuple.
      assert(ResultBB->getNumArguments() == 1);
      Builder.setInsertionPoint(ResultBB->begin());
      fixUsedVoidType(ResultBB->getArgument(0), Loc, Builder);

      SILArgument *Arg = ResultBB->replacePHIArgument(
          0, StoreResultTo->getType().getObjectType(),
          ValueOwnershipKind::Owned);
      // Store the direct result to the original result address.
      Builder.createStore(Loc, Arg, StoreResultTo,
                          StoreOwnershipQualifier::Unqualified);
    }
    return NewTAI;
  }
  if (auto *A = dyn_cast<ApplyInst>(AI)) {
    auto *NewAI = Builder.createApply(Loc, Callee, CalleeSILSubstFnTy,
                                      CalleeSubstFnTy->getSILResult(), Subs,
                                      Arguments, A->isNonThrowing());
    if (StoreResultTo) {
      // Store the direct result to the original result address.
      fixUsedVoidType(A, Loc, Builder);
      Builder.createStore(Loc, NewAI, StoreResultTo,
                          StoreOwnershipQualifier::Unqualified);
    }
    A->replaceAllUsesWith(NewAI);
    return NewAI;
  }
  if (auto *PAI = dyn_cast<PartialApplyInst>(AI)) {
    CanSILFunctionType NewPAType = ReInfo.createSpecializedType(
        PAI->getFunctionType(), Builder.getModule());
    // SILType PTy =
    // SILType::getPrimitiveObjectType(ReInfo.getSpecializedType());
    SILType PTy = CalleeSILSubstFnTy;
    auto *NewPAI =
        Builder.createPartialApply(Loc, Callee, PTy, Subs, Arguments,
                                   SILType::getPrimitiveObjectType(NewPAType));
    PAI->replaceAllUsesWith(NewPAI);
    return NewPAI;
  }
  llvm_unreachable("unhandled kind of apply");
}

// Create a new apply based on an old one, but with a different
// function being applied.
ApplySite swift::
replaceWithSpecializedFunction(ApplySite AI, SILFunction *NewF,
                               const ReabstractionInfo &ReInfo) {
  SILBuilderWithScope Builder(AI.getInstruction());
  FunctionRefInst *FRI = Builder.createFunctionRef(AI.getLoc(), NewF);
  return replaceWithSpecializedCallee(AI, FRI, Builder, ReInfo);
}

/// Create a re-abstraction thunk for a partial_apply.
/// This is needed in case we converted some parameters/results of the
/// specialized function from indirect to direct but the result function of the
/// partial_apply still needs them as indirect.
/// We create a thunk which converts the direct parameters/results back to
/// indirect ones.
static SILFunction *createReabstractionThunk(const ReabstractionInfo &ReInfo,
                                     PartialApplyInst *OrigPAI,
                                     SILFunction *SpecializedFunc) {
  SILFunction *OrigF = OrigPAI->getCalleeFunction();
  SILModule &M = OrigF->getModule();

  IsFragile_t Fragile = IsNotFragile;
  if (OrigF->isFragile() && OrigPAI->getFunction()->isFragile())
    Fragile = IsFragile;

  std::string ThunkName;
  {
    Mangle::Mangler Mangler;
    std::string Old;
    if (ReInfo.isPartialSpecialization()) {
      PartialSpecializationMangler OldGenericMangler(
          Mangler, OrigF, ReInfo.getSubstitutedType(), Fragile,
          PartialSpecializationMangler::NotReabstracted);
      OldGenericMangler.mangle();
      Old = Mangler.finalize();
    } else {
      GenericSpecializationMangler OldGenericMangler(
          Mangler, OrigF, ReInfo.getOriginalParamSubstitutions(), Fragile,
          GenericSpecializationMangler::NotReabstracted);
      OldGenericMangler.mangle();
      Old = Mangler.finalize();
    }

    // TODO: Clean-up once the new mangler is merged.
#if 0
    NewMangling::GenericSpecializationMangler NewMangler(
        OrigF, ReInfo.getSubstitutedType(), Fragile,
        /*isReAbstracted*/ false);
    std::string New = NewMangler.mangle();
    ThunkName = NewMangling::selectMangling(Old, New);
#else
    ThunkName = Old;
#endif
  }

  auto Loc = RegularLocation::getAutoGeneratedLocation();

  SILFunction *Thunk = M.lookUpFunction(ThunkName);
  if (Thunk) {
    // Re-use an existing thunk.
    assert(Thunk->getLoweredFunctionType() == ReInfo.getSubstitutedType());
    assert(Thunk->getLinkage() == SILLinkage::Shared ||
           stripExternalFromLinkage(Thunk->getLinkage()) == SILLinkage::Shared);
    if (!Thunk->empty())
      return Thunk;
  }

  if (!Thunk) {
    Thunk = M.createFunction(
        SILLinkage::Shared, ThunkName, ReInfo.getSubstitutedType(),
        ReInfo.getSpecializedGenericEnvironment(), Loc, IsBare, IsTransparent,
        Fragile, IsThunk, SILFunction::NotRelevant);
    Thunk->setDebugScope(new (M) SILDebugScope(Loc, Thunk));
  }

  SILBasicBlock *EntryBB = Thunk->createBasicBlock();
  SILBuilder Builder(EntryBB);
  SILBasicBlock *SpecEntryBB = &*SpecializedFunc->begin();
  CanSILFunctionType SpecType = SpecializedFunc->getLoweredFunctionType();
  SILArgument *ReturnValueAddr = nullptr;

  // If the original specialized function had unqualified ownership, set the
  // thunk to have unqualified ownership as well.
  //
  // This is a stop gap measure to allow for easy inlining. We could always make
  // the Thunk qualified, but then we would need to either fix the inliner to
  // inline qualified into unqualified functions /or/ have the
  // OwnershipModelEliminator run as part of the normal compilation pipeline
  // (which we are not doing yet).
  if (SpecializedFunc->hasUnqualifiedOwnership()) {
    Thunk->setUnqualifiedOwnership();
  }

  // Convert indirect to direct parameters/results.
  SmallVector<SILValue, 4> Arguments;
  auto SpecArgIter = SpecEntryBB->args_begin();
  for (unsigned Idx = 0; Idx < ReInfo.getNumArguments(); Idx++) {
    if (ReInfo.isArgConverted(Idx)) {
      if (ReInfo.isResultIndex(Idx)) {
        // Store the result later.
        SILType Ty = SpecType->getSILResult().getAddressType();
        ReturnValueAddr = EntryBB->createFunctionArgument(Ty);
      } else {
        // Instead of passing the address, pass the loaded value.
        SILArgument *SpecArg = *SpecArgIter++;
        SILType Ty = SpecArg->getType().getAddressType();
        SILArgument *NewArg =
            EntryBB->createFunctionArgument(Ty, SpecArg->getDecl());
        auto *ArgVal = Builder.createLoad(Loc, NewArg,
                                          LoadOwnershipQualifier::Unqualified);
        Arguments.push_back(ArgVal);
      }
    } else {
      // No change to the argument.
      SILArgument *SpecArg = *SpecArgIter++;
      SILArgument *NewArg = EntryBB->createFunctionArgument(SpecArg->getType(),
                                                            SpecArg->getDecl());
      Arguments.push_back(NewArg);
    }
  }

  auto *FRI = Builder.createFunctionRef(Loc, SpecializedFunc);

  ArrayRef<Substitution> Subs;
  if (ReInfo.getSpecializedType()->isPolymorphic()) {
    Subs = ReInfo.getCallerParamSubstitutions();
  }
  // Create a substituted callee type.
  auto CalleeSubstFnTy = getCalleeSubstFunctionType(FRI, ReInfo);

  auto FnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);

  SILValue ReturnValue;
  if (SpecType->hasErrorResult()) {
    // Create the logic for calling a throwing function.
    SILBasicBlock *NormalBB = Thunk->createBasicBlock();
    SILBasicBlock *ErrorBB = Thunk->createBasicBlock();
    Builder.createTryApply(Loc, FRI, FnTy,
                           Subs, Arguments, NormalBB, ErrorBB);
    auto *ErrorVal = ErrorBB->createPHIArgument(
        SpecType->getErrorResult().getSILType(), VallueOwnershipKInd::Owned);
    Builder.setInsertionPoint(ErrorBB);
    Builder.createThrow(Loc, ErrorVal);
    ReturnValue = NormalBB->createPHIArgument(SpecType->getSILResult(),
                                              ValueOwnershipKind::Owned);
    Builder.setInsertionPoint(NormalBB);
  } else {
    ReturnValue = Builder.createApply(Loc, FRI, FnTy,
                                      SpecType->getSILResult(), Subs,
                                      Arguments, false);
  }
  if (ReturnValueAddr) {
    // Need to store the direct results to the original indirect address.
    Builder.createStore(Loc, ReturnValue, ReturnValueAddr,
                        StoreOwnershipQualifier::Unqualified);
    SILType VoidTy = OrigPAI->getSubstCalleeType()->getSILResult();
    assert(VoidTy.isVoid());
    ReturnValue = Builder.createTuple(Loc, VoidTy, { });
  }
  Builder.createReturn(Loc, ReturnValue);

  return Thunk;
}

void swift::trySpecializeApplyOfGeneric(
    ApplySite Apply, DeadInstructionSet &DeadApplies,
    llvm::SmallVectorImpl<SILFunction *> &NewFunctions) {
  assert(Apply.hasSubstitutions() && "Expected an apply with substitutions!");

  auto *F = Apply.getInstruction()->getFunction();
  auto *RefF = cast<FunctionRefInst>(Apply.getCallee())->getReferencedFunction();

  DEBUG(llvm::dbgs() << "  ApplyInst:\n";
        Apply.getInstruction()->dumpInContext());

  // If the caller is fragile but the callee is not, bail out.
  // Specializations have shared linkage, which means they do
  // not have an external entry point, Since the callee is not
  // fragile we cannot serialize the body of the specialized
  // callee either.
  if (F->isFragile() && !RefF->hasValidLinkageForFragileInline())
      return;

  // Do not specialize if the callee is explicitly excluded from
  // generic specializations.
  if (RefF &&
      RefF->hasSemanticsAttr("optimize.sil.generic.specialization.never"))
    return;

  // If the caller and callee are both fragile, preserve the fragility when
  // cloning the callee. Otherwise, strip it off so that we can optimize
  // the body more.
  IsFragile_t Fragile = IsNotFragile;
  if (F->isFragile() && RefF->isFragile())
    Fragile = IsFragile;

  ReabstractionInfo ReInfo(Apply, RefF, Apply.getSubstitutions());
  if (!ReInfo.canBeSpecialized())
    return;

  SILModule &M = F->getModule();

  bool needAdaptUsers = false;
  bool replacePartialApplyWithoutReabstraction = false;
  auto *PAI = dyn_cast<PartialApplyInst>(Apply);
  // TODO: Partial specializations of partial applies are
  // not supported yet.
  if (PAI && ReInfo.getSpecializedType()->isPolymorphic())
    return;
  if (PAI && ReInfo.hasConversions()) {
    // If we have a partial_apply and we converted some results/parameters from
    // indirect to direct there are 3 cases:
    // 1) All uses of the partial_apply are apply sites again. In this case
    //    we can just adapt all the apply sites which use the partial_apply.
    // 2) The result of the partial_apply is re-abstracted anyway (and the
    //    re-abstracted function type matches with our specialized type). In
    //    this case we can just skip the existing re-abstraction.
    // 3) For all other cases we need to create a new re-abstraction thunk.
    needAdaptUsers = true;
    for (Operand *Use : PAI->getUses()) {
      SILInstruction *User = Use->getUser();
      if (isa<RefCountingInst>(User))
        continue;
      if (isDebugInst(User))
        continue;

      auto FAS = FullApplySite::isa(User);
      if (FAS && FAS.getCallee() == Apply.getInstruction())
        continue;

      auto *PAIUser = dyn_cast<PartialApplyInst>(User);
      if (PAIUser && isPartialApplyOfReabstractionThunk(PAIUser)) {
        CanSILFunctionType NewPAType =
          ReInfo.createSpecializedType(PAI->getFunctionType(), M);
        if (PAIUser->getFunctionType() == NewPAType)
          continue;
      }
      replacePartialApplyWithoutReabstraction = true;
      break;
    }
  }

  GenericFuncSpecializer FuncSpecializer(RefF, Apply.getSubstitutions(),
                                         Fragile, ReInfo);
  SILFunction *SpecializedF = FuncSpecializer.lookupSpecialization();
  if (SpecializedF) {
    // Even if the pre-specialization exists already, try to preserve it
    // if it is whitelisted.
    linkSpecialization(M, SpecializedF);
  } else {
    SpecializedF = FuncSpecializer.tryCreateSpecialization();
    if (!SpecializedF)
      return;

    assert(SpecializedF->hasUnqualifiedOwnership());
    NewFunctions.push_back(SpecializedF);
  }

  assert(ReInfo.getSpecializedType()
         == SpecializedF->getLoweredFunctionType() &&
         "Previously specialized function does not match expected type.");

  // FIXME: Replace pre-specialization's "keep as public" hack with something
  // more principled
  assert((Fragile == SpecializedF->isFragile() ||
          SpecializedF->isKeepAsPublic()) &&
         "Previously specialized function does not match expected "
         "resilience level.");

  DeadApplies.insert(Apply.getInstruction());

  if (replacePartialApplyWithoutReabstraction) {
    // There are some unknown users of the partial_apply. Therefore we need a
    // thunk which converts from the re-abstracted function back to the
    // original function with indirect parameters/results.
    auto *PAI = cast<PartialApplyInst>(Apply.getInstruction());
    SILBuilderWithScope Builder(PAI);
    SILFunction *Thunk = createReabstractionThunk(ReInfo, PAI, SpecializedF);
    NewFunctions.push_back(Thunk);
    auto *FRI = Builder.createFunctionRef(PAI->getLoc(), Thunk);
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : PAI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    auto *NewPAI = Builder.createPartialApply(PAI->getLoc(), FRI,
                                      PAI->getSubstCalleeSILType(),
                                      {},
                                      Arguments,
                                      PAI->getType());
    PAI->replaceAllUsesWith(NewPAI);
    DeadApplies.insert(PAI);
    return;
  }
  // Make the required changes to the call site.
  ApplySite newApply = replaceWithSpecializedFunction(Apply, SpecializedF,
                                                      ReInfo);
  if (needAdaptUsers) {
    // Adapt all known users of the partial_apply. This is needed in case we
    // converted some indirect parameters/results to direct ones.
    auto *NewPAI = cast<PartialApplyInst>(newApply);
    ReInfo.prunePartialApplyArgs(NewPAI->getNumArguments());
    for (Operand *Use : NewPAI->getUses()) {
      SILInstruction *User = Use->getUser();
      if (auto FAS = FullApplySite::isa(User)) {
        SILBuilder Builder(User);
        replaceWithSpecializedCallee(FAS, NewPAI, Builder, ReInfo);
        DeadApplies.insert(FAS.getInstruction());
        continue;
      }
      if (auto *PAI = dyn_cast<PartialApplyInst>(User)) {
        // This is a partial_apply of a re-abstraction thunk. Just skip this.
        assert(PAI->getType() == NewPAI->getType());
        PAI->replaceAllUsesWith(NewPAI);
        DeadApplies.insert(PAI);
      }
    }
  }
}

// =============================================================================
// Prespecialized symbol lookup.
//
// This uses the SIL linker to checks for the does not load the body of the pres
// =============================================================================

static void keepSpecializationAsPublic(SILFunction *F) {
  DEBUG(auto DemangledNameString =
            swift::Demangle::demangleSymbolAsString(F->getName());
        StringRef DemangledName = DemangledNameString;
        llvm::dbgs() << "Keep specialization public: " << DemangledName << " : "
                     << F->getName() << "\n");
  // Make it public, so that others can refer to it.
  //
  // NOTE: This function may refer to non-public symbols, which may lead to
  // problems, if you ever try to inline this function. Therefore, these
  // specializations should only be used to refer to them, but should never
  // be inlined!  The general rule could be: Never inline specializations
  // from stdlib!
  //
  // NOTE: Making these specializations public at this point breaks
  // some optimizations. Therefore, just mark the function.
  // DeadFunctionElimination pass will check if the function is marked
  // and preserve it if required.
  F->setKeepAsPublic(true);
}

/// Link a specialization for generating prespecialized code.
///
/// For now, it is performed only for specializations in the
/// standard library. But in the future, one could think of
/// maintaining a cache of optimized specializations.
///
/// Mark specializations as public, so that they can be used by user
/// applications. These specializations are generated during -O compilation of
/// the library, but only used only by client code compiled at -Onone. They
/// should be never inlined.
static bool linkSpecialization(SILModule &M, SILFunction *F) {
  if (F->isKeepAsPublic())
    return true;
  // Do not remove functions from the white-list. Keep them around.
  // Change their linkage to public, so that other applications can refer to it.
  if (M.getOptions().Optimization >= SILOptions::SILOptMode::Optimize &&
      F->getModule().getSwiftModule()->getName().str() == SWIFT_ONONE_SUPPORT) {
    if (isWhitelistedSpecialization(F->getName())) {
      keepSpecializationAsPublic(F);
      return true;
    }
  }
  return false;
}

// The whitelist of classes and functions from the stdlib,
// whose specializations we want to preserve.
static const char *const WhitelistedSpecializations[] = {
    "Array",
    "_ArrayBuffer",
    "_ContiguousArrayBuffer",
    "Range",
    "RangeIterator",
    "CountableRange",
    "CountableRangeIterator",
    "ClosedRange",
    "ClosedRangeIterator",
    "CountableClosedRange",
    "CountableClosedRangeIterator",
    "IndexingIterator",
    "Collection",
    "ReversedCollection",
    "MutableCollection",
    "BidirectionalCollection",
    "RandomAccessCollection",
    "ReversedRandomAccessCollection",
    "RangeReplaceableCollection",
    "_allocateUninitializedArray",
    "UTF8",
    "UTF16",
    "String",
    "_StringBuffer",
    "_toStringReadOnlyPrintable",
};

/// Check of a given name could be a name of a white-listed
/// specialization.
bool swift::isWhitelistedSpecialization(StringRef SpecName) {
  // TODO: Once there is an efficient API to check if
  // a given symbol is a specialization of a specific type,
  // use it instead. Doing demangling just for this check
  // is just wasteful.
  auto DemangledNameString =
     swift::Demangle::demangleSymbolAsString(SpecName);

  StringRef DemangledName = DemangledNameString;

  DEBUG(llvm::dbgs() << "Check if whitelisted: " << DemangledName << "\n");

  auto pos = DemangledName.find("generic ", 0);
  auto oldpos = pos;
  if (pos == StringRef::npos)
    return false;

  // Create "of Swift"
  llvm::SmallString<64> OfString;
  llvm::raw_svector_ostream buffer(OfString);
  buffer << "of ";
  buffer << STDLIB_NAME <<'.';

  StringRef OfStr = buffer.str();
  DEBUG(llvm::dbgs() << "Check substring: " << OfStr << "\n");

  pos = DemangledName.find(OfStr, oldpos);

  if (pos == StringRef::npos) {
    // Create "of (extension in Swift).Swift"
    llvm::SmallString<64> OfString;
    llvm::raw_svector_ostream buffer(OfString);
    buffer << "of (extension in " << STDLIB_NAME << "):";
    buffer << STDLIB_NAME << '.';
    OfStr = buffer.str();
    pos = DemangledName.find(OfStr, oldpos);
    DEBUG(llvm::dbgs() << "Check substring: " << OfStr << "\n");
    if (pos == StringRef::npos)
      return false;
  }

  pos += OfStr.size();

  for (auto NameStr: WhitelistedSpecializations) {
    StringRef Name = NameStr;
    auto pos1 = DemangledName.find(Name, pos);
    if (pos1 == pos && !isalpha(DemangledName[pos1+Name.size()])) {
      return true;
    }
  }

  return false;
}

/// Try to look up an existing specialization in the specialization cache.
/// If it is found, it tries to link this specialization.
///
/// For now, it performs a lookup only in the standard library.
/// But in the future, one could think of maintaining a cache
/// of optimized specializations.
static SILFunction *lookupExistingSpecialization(SILModule &M,
                                                 StringRef FunctionName) {
  // Try to link existing specialization only in -Onone mode.
  // All other compilation modes perform specialization themselves.
  // TODO: Cache optimized specializations and perform lookup here?
  // Only check that this function exists, but don't read
  // its body. It can save some compile-time.
  if (isWhitelistedSpecialization(FunctionName))
    return M.hasFunction(FunctionName, SILLinkage::PublicExternal);

  return nullptr;
}

SILFunction *swift::lookupPrespecializedSymbol(SILModule &M,
                                               StringRef FunctionName) {
  // First check if the module contains a required specialization already.
  auto *Specialization = M.lookUpFunction(FunctionName);
  if (Specialization) {
    if (Specialization->getLinkage() == SILLinkage::PublicExternal)
      return Specialization;
  }

  // Then check if the required specialization can be found elsewhere.
  Specialization = lookupExistingSpecialization(M, FunctionName);
  if (!Specialization)
    return nullptr;

  assert(hasPublicVisibility(Specialization->getLinkage()) &&
         "Pre-specializations should have public visibility");

  Specialization->setLinkage(SILLinkage::PublicExternal);

  assert(Specialization->isExternalDeclaration()  &&
         "Specialization should be a public external declaration");

  DEBUG(llvm::dbgs() << "Found existing specialization for: " << FunctionName
                     << '\n';
        llvm::dbgs() << swift::Demangle::demangleSymbolAsString(
                            Specialization->getName())
                     << "\n\n");

  return Specialization;
}

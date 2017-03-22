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
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/GenericEnvironment.h"

using namespace swift;

/// Set to true to enable the support for partial specialization.
llvm::cl::opt<bool> EnablePartialSpecialization(
    "sil-partial-specialization", llvm::cl::init(false),
    llvm::cl::desc("Enable partial specialization of generics"));

/// If set, then generic specialization tries to specialize using
/// all substitutions, even if they the replacement types are generic.
llvm::cl::opt<bool> SpecializeGenericSubstitutions(
    "sil-partial-specialization-with-generic-substitutions",
    llvm::cl::init(false),
    llvm::cl::desc("Enable partial specialization with generic substitutions"));

static bool OptimizeGenericSubstitutions = false;
static bool SupportGenericSubstitutions = true;

// Max depth of a type which can be processed by the generic
// specializer.
// E.g. the depth of Array<Array<Array<T>>> is 3.
// No specializations will be produced, if any of generic parameters contains
// a bound generic type with the depth higher than this threshold
static const unsigned TypeDepthThreshold = 50;
// Set the width threshold rather high, because some projects uses very wide
// tuples to model fixed size arrays.
static const unsigned TypeWidthThreshold = 2000;

// Compute the width and the depth of a type.
// We compute both, because some pathological test-cases result in very
// wide types and some others result in very deep types. It is important
// to bail as soon as we hit the threshold on any of both dimentions to
// prevent compiler hangs and crashes.
static std::pair<unsigned, unsigned> getTypeDepthAndWidth(Type t) {
  unsigned Depth = 0;
  unsigned Width = 0;
  if (auto *BGT = t->getAs<BoundGenericType>()) {
    auto *NTD = BGT->getNominalOrBoundGenericNominal();
    if (NTD) {
      auto StoredProperties = NTD->getStoredProperties();
      Width += std::distance(StoredProperties.begin(), StoredProperties.end());
    }
    Depth++;
    unsigned MaxTypeDepth = 0;
    auto GenericArgs = BGT->getGenericArgs();
    for (auto Ty : GenericArgs) {
      unsigned TypeWidth;
      unsigned TypeDepth;
      std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(Ty);
      if (TypeDepth > MaxTypeDepth)
        MaxTypeDepth = TypeDepth;
      Width += TypeWidth;
    }
    Depth += MaxTypeDepth;
    return std::make_pair(Depth, Width);
  }

  if (auto *TupleTy = t->getAs<TupleType>()) {
    Width += TupleTy->getNumElements();
    Depth++;
    unsigned MaxTypeDepth = 0;
    auto ElementTypes = TupleTy->getElementTypes();
    for (auto Ty : ElementTypes) {
      unsigned TypeWidth;
      unsigned TypeDepth;
      std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(Ty);
      if (TypeDepth > MaxTypeDepth)
        MaxTypeDepth = TypeDepth;
      Width += TypeWidth;
    }
    Depth += MaxTypeDepth;
    return std::make_pair(Depth, Width);
  }

  if (auto *FnTy = t->getAs<SILFunctionType>()) {
    Depth++;
    unsigned MaxTypeDepth = 0;
    auto Params = FnTy->getParameters();
    Width += Params.size();
    for (auto Param : Params) {
      unsigned TypeWidth;
      unsigned TypeDepth;
      std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(Param.getType());
      if (TypeDepth > MaxTypeDepth)
        MaxTypeDepth = TypeDepth;
      Width += TypeWidth;
    }
    auto Results = FnTy->getResults();
    Width += Results.size();
    for (auto Result : Results) {
      unsigned TypeWidth;
      unsigned TypeDepth;
      std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(Result.getType());
      if (TypeDepth > MaxTypeDepth)
        MaxTypeDepth = TypeDepth;
      Width += TypeWidth;
    }
    if (FnTy->hasErrorResult()) {
      Width += 1;
      unsigned TypeWidth;
      unsigned TypeDepth;
      std::tie(TypeDepth, TypeWidth) =
          getTypeDepthAndWidth(FnTy->getErrorResult().getType());
      if (TypeDepth > MaxTypeDepth)
        MaxTypeDepth = TypeDepth;
      Width += TypeWidth;
    }
    Depth += MaxTypeDepth;
    return std::make_pair(Depth, Width);
  }

  if (auto *FnTy = t->getAs<FunctionType>()) {
    Depth++;
    unsigned MaxTypeDepth = 0;
    unsigned TypeWidth;
    unsigned TypeDepth;
    std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(FnTy->getInput());
    if (TypeDepth > MaxTypeDepth)
      MaxTypeDepth = TypeDepth;
    Width += TypeWidth;
    std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(FnTy->getResult());
    if (TypeDepth > MaxTypeDepth)
      MaxTypeDepth = TypeDepth;
    Width += TypeWidth;
    Depth += MaxTypeDepth;
    return std::make_pair(Depth, Width);
  }

  if (auto *MT = t->getAs<MetatypeType>()) {
    Depth += 1;
    unsigned TypeWidth;
    unsigned TypeDepth;
    std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(MT->getInstanceType());
    Width += TypeWidth;
    Depth += TypeDepth;
    return std::make_pair(Depth, Width);
  }

  return std::make_pair(Depth, Width);
}

static bool isTypeTooComplex(Type t) {
  unsigned TypeWidth;
  unsigned TypeDepth;
  std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(t);
  return TypeWidth >= TypeWidthThreshold || TypeDepth >= TypeDepthThreshold;
}

// =============================================================================
// ReabstractionInfo
// =============================================================================

static bool shouldNotSpecializeCallee(SILFunction *Callee) {
  if (!Callee->shouldOptimize()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize function " << Callee->getName()
          << " marked to be excluded from optimizations.\n");
    return true;
  }

  if (Callee->hasSemanticsAttr("optimize.sil.specialize.generic.never"))
    return true;

  return false;
}

/// Prepares the ReabstractionInfo object for further processing and checks
/// if the current function can be specialized at all.
/// Returns false, if the current function cannot be specialized.
/// Returns true otherwise.
bool ReabstractionInfo::prepareAndCheck(ApplySite Apply, SILFunction *Callee,
                                        SubstitutionList ParamSubs) {
  if (shouldNotSpecializeCallee(Callee))
    return false;

  SpecializedGenericEnv = nullptr;
  SpecializedGenericSig = nullptr;
  OriginalParamSubs = ParamSubs;
  auto CalleeGenericSig = Callee->getLoweredFunctionType()->getGenericSignature();
  auto CalleeGenericEnv = Callee->getGenericEnvironment();

  OriginalF = Callee;
  this->Apply = Apply;

  SubstitutionMap InterfaceSubs;

  // Get the original substitution map.
  if (CalleeGenericSig)
    InterfaceSubs = CalleeGenericSig->getSubstitutionMap(ParamSubs);

  // We do not support partial specialization.
  if (!EnablePartialSpecialization && InterfaceSubs.hasArchetypes()) {
    DEBUG(llvm::dbgs() << "    Partial specialization is not supported.\n");
    DEBUG(for (auto Sub : ParamSubs) { Sub.dump(); });
    return false;
  }

  // Perform some checks to see if we need to bail.
  if (InterfaceSubs.hasDynamicSelf()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize with dynamic self.\n");
    return false;
  }

  // Check if the substitution contains any generic types that are too deep.
  // If this is the case, bail to avoid the explosion in the number of 
  // generated specializations.
  for (auto Sub : ParamSubs) {
    auto Replacement = Sub.getReplacement();
    if (isTypeTooComplex(Replacement)) {
      DEBUG(llvm::dbgs()
            << "    Cannot specialize because the generic type is too deep.\n");
      return false;
    }
  }

  // Check if we have substitutions which replace generic type parameters with
  // concrete types or unbound generic types.
  bool HasConcreteGenericParams = false;
  bool HasNonArchetypeGenericParams = false;
  HasUnboundGenericParams = false;
  for (auto GP : CalleeGenericSig->getSubstitutableParams()) {
    // Check only the substitutions for the generic parameters.
    // Ignore any dependent types, etc.
    auto Replacement = Type(GP).subst(InterfaceSubs);
    if (!Replacement->is<ArchetypeType>())
      HasNonArchetypeGenericParams = true;

    if (Replacement->hasArchetype()) {
      HasUnboundGenericParams = true;
      // Check if the replacement is an archetype which is more specific
      // than the corresponding archetype in the original generic signature.
      // If this is the case, then specialization makes sense, because
      // it would produce something more specific.
      if (CalleeGenericEnv) {
        if (auto Archetype = Replacement->getAs<ArchetypeType>()) {
          auto OrigArchetype =
              CalleeGenericEnv->mapTypeIntoContext(GP)->castTo<ArchetypeType>();
          if (Archetype->requiresClass() && !OrigArchetype->requiresClass())
            HasNonArchetypeGenericParams = true;
          if (Archetype->getLayoutConstraint() &&
              !OrigArchetype->getLayoutConstraint())
            HasNonArchetypeGenericParams = true;
        }
      }
      continue;
    }

    HasConcreteGenericParams = true;
  }

  if (HasUnboundGenericParams) {
    if (!HasNonArchetypeGenericParams && !HasConcreteGenericParams) {
      DEBUG(llvm::dbgs() << "    Partial specialization is not supported if "
                            "all substitutions are archetypes.\n");
      DEBUG(for (auto Sub : ParamSubs) {
          Sub.dump();
        });
      return false;
    }

    // We need a generic environment for the partial specialization.
    if (!CalleeGenericEnv)
      return false;
  }

  return true;
}

bool ReabstractionInfo::canBeSpecialized(ApplySite Apply, SILFunction *Callee,
                                         SubstitutionList ParamSubs) {
  ReabstractionInfo ReInfo;
  return ReInfo.prepareAndCheck(Apply, Callee, ParamSubs);
}

ReabstractionInfo::ReabstractionInfo(ApplySite Apply, SILFunction *Callee,
                                     ArrayRef<Substitution> ParamSubs,
                                     bool ConvertIndirectToDirect) {
  if (!prepareAndCheck(Apply, Callee, ParamSubs))
    return;

  this->ConvertIndirectToDirect = ConvertIndirectToDirect;

  if (SpecializeGenericSubstitutions) {
    specializeConcreteAndGenericSubstitutions(Apply, Callee, ParamSubs);
  } else {
    specializeConcreteSubstitutions(Apply, Callee, ParamSubs);
  }
  verify();
  if (SpecializedGenericSig) {
    DEBUG(llvm::dbgs() << "\n\nPartially specialized types for function: "
                       << Callee->getName() << "\n\n";
          llvm::dbgs() << "Original generic function type:\n"
                       << Callee->getLoweredFunctionType() << "\n"
                       << "Partially specialized generic function type:\n"
                       << SpecializedType << "\n\n");
  }

  // Some sanity checks.
  auto SpecializedFnTy = getSpecializedType();
  auto SpecializedSubstFnTy = SpecializedFnTy;

  if (SpecializedFnTy->isPolymorphic() &&
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
  if (getSubstitutedType() == Callee->getLoweredFunctionType()) {
    DEBUG(llvm::dbgs() << "The new specialized type is the same as "
                          "the original "
                          "type. Don't specialize!\n";
          llvm::dbgs() << "The type is: " << getSubstitutedType() << "\n");
    SpecializedType = CanSILFunctionType();
    SubstitutedType = CanSILFunctionType();
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
    return;
  }

  if (SpecializedGenericSig) {
    // It is a partial specializaiton.
    DEBUG(llvm::dbgs() << "Specializing the call:\n";
          Apply.getInstruction()->dumpInContext();
          llvm::dbgs() << "\n\nPartially specialized types for function: "
                       << Callee->getName() << "\n\n";
          llvm::dbgs() << "Original generic function type:\n"
                       << Callee->getLoweredFunctionType() << "\n\n";
          llvm::dbgs() << "\nOriginal call substitution:\n";
          for (auto Sub : getOriginalParamSubstitutions()) {
            llvm::dbgs() << "Sub:\n";
            Sub.dump();
            llvm::dbgs() << "\n";
          }

          llvm::dbgs() << "Partially specialized generic function type:\n"
                       << getSpecializedType() << "\n\n";
          llvm::dbgs() << "\nSpecialization call substitution:\n";
          for (auto Sub : getCallerParamSubstitutions()) {
            llvm::dbgs() << "Sub:\n";
            Sub.dump();
            llvm::dbgs() << "\n";
          });
  }
}

bool ReabstractionInfo::canBeSpecialized() const {
  return getSpecializedType();
}

bool ReabstractionInfo::isFullSpecialization() const {
  return !hasArchetypes(getOriginalParamSubstitutions());
}

bool ReabstractionInfo::isPartialSpecialization() const {
  return hasArchetypes(getOriginalParamSubstitutions());
}

void ReabstractionInfo::createSubstitutedAndSpecializedTypes() {
  auto &M = OriginalF->getModule();

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
  NumFormalIndirectResults = SubstitutedType->getNumIndirectFormalResults();
  Conversions.resize(NumFormalIndirectResults +
                     SubstitutedType->getParameters().size());

  CanGenericSignature CanSig;
  if (SpecializedGenericSig)
    CanSig = SpecializedGenericSig->getCanonicalSignature();
  Lowering::GenericContextScope GenericScope(M.Types, CanSig);

  SILFunctionConventions substConv(SubstitutedType, M);

  if (SubstitutedType->getNumDirectFormalResults() == 0) {
    // The original function has no direct result yet. Try to convert the first
    // indirect result to a direct result.
    // TODO: We could also convert multiple indirect results by returning a
    // tuple type and created tuple_extract instructions at the call site.
    unsigned IdxForResult = 0;
    for (SILResultInfo RI : SubstitutedType->getIndirectFormalResults()) {
      assert(RI.isFormalIndirect());
      if (substConv.getSILType(RI).isLoadable(M) && !RI.getType()->isVoid()) {
        Conversions.set(IdxForResult);
        break;
      }
      ++IdxForResult;
    }
  }

  // Try to convert indirect incoming parameters to direct parameters.
  unsigned IdxForParam = NumFormalIndirectResults;
  for (SILParameterInfo PI : SubstitutedType->getParameters()) {
    if (substConv.getSILType(PI).isLoadable(M) &&
        PI.getConvention() == ParameterConvention::Indirect_In) {
      Conversions.set(IdxForParam);
    }
    ++IdxForParam;
  }

  // Produce a specialized type, which is the substituted type with
  // the parameters/results passing conventions adjusted according
  // to the conversions selected above.
  SpecializedType = createSpecializedType(SubstitutedType, M);
}

/// Create a new substituted type with the updated signature.
CanSILFunctionType
ReabstractionInfo::createSubstitutedType(SILFunction *OrigF,
                                         const SubstitutionMap &SubstMap,
                                         bool HasUnboundGenericParams) {
  auto &M = OrigF->getModule();
  if ((SpecializedGenericSig &&
       SpecializedGenericSig->areAllParamsConcrete()) ||
      !HasUnboundGenericParams) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
  }

  CanGenericSignature CanSpecializedGenericSig;
  if (SpecializedGenericSig)
    CanSpecializedGenericSig = SpecializedGenericSig->getCanonicalSignature();

  // First substitute concrete types into the existing function type.
  CanSILFunctionType FnTy;
  {
    Lowering::GenericContextScope GenericScope(M.Types,
                                               CanSpecializedGenericSig);
    FnTy = OrigF->getLoweredFunctionType()->substGenericArgs(M, SubstMap);
    // FIXME: Some of the added new requirements may not have been taken into
    // account by the substGenericArgs. So, canonicalize in the context of the
    // specialized signature.
    FnTy = cast<SILFunctionType>(
        CanSpecializedGenericSig->getCanonicalTypeInContext(
            FnTy, *M.getSwiftModule()));
  }
  assert(FnTy);

  // Use the new specialized generic signature.
  auto NewFnTy = SILFunctionType::get(
      CanSpecializedGenericSig, FnTy->getExtInfo(), FnTy->getCalleeConvention(),
      FnTy->getParameters(), FnTy->getResults(), FnTy->getOptionalErrorResult(),
      M.getASTContext());

  // This is an interface type. It should not have any archetypes.
  assert(!NewFnTy->hasArchetype());
  return NewFnTy;
}

// Convert the substituted function type into a specialized function type based
// on the ReabstractionInfo.
CanSILFunctionType ReabstractionInfo::
createSpecializedType(CanSILFunctionType SubstFTy, SILModule &M) const {
  llvm::SmallVector<SILResultInfo, 8> SpecializedResults;
  llvm::SmallVector<SILParameterInfo, 8> SpecializedParams;

  unsigned IndirectResultIdx = 0;
  for (SILResultInfo RI : SubstFTy->getResults()) {
    if (RI.isFormalIndirect()) {
      if (isFormalResultConverted(IndirectResultIdx++)) {
        // Convert the indirect result to a direct result.
        SILType SILResTy = SILType::getPrimitiveObjectType(RI.getType());
        // Indirect results are passed as owned, so we also need to pass the
        // direct result as owned (except it's a trivial type).
        auto C = (SILResTy.isTrivial(M) ? ResultConvention::Unowned :
                  ResultConvention::Owned);
        SpecializedResults.push_back(SILResultInfo(RI.getType(), C));
        continue;
      }
    }
    // No conversion: re-use the original, substituted result info.
    SpecializedResults.push_back(RI);
  }
  unsigned ParamIdx = 0;
  for (SILParameterInfo PI : SubstFTy->getParameters()) {
    if (isParamConverted(ParamIdx++)) {
      // Convert the indirect parameter to a direct parameter.
      SILType SILParamTy = SILType::getPrimitiveObjectType(PI.getType());
      // Indirect parameters are passed as owned, so we also need to pass the
      // direct parameter as owned (except it's a trivial type).
      auto C = (SILParamTy.isTrivial(M) ? ParameterConvention::Direct_Unowned :
                ParameterConvention::Direct_Owned);
      SpecializedParams.push_back(SILParameterInfo(PI.getType(), C));
    } else {
      // No conversion: re-use the original, substituted parameter info.
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

static std::pair<GenericEnvironment *, GenericSignature *>
getGenericEnvironmentAndSignature(GenericSignatureBuilder &Builder,
                                  SILModule &M) {
  auto *GenericSig =
      Builder.getGenericSignature()->getCanonicalSignature().getPointer();
  auto *GenericEnv = GenericSig->createGenericEnvironment(*M.getSwiftModule());

  // FIXME: This is a workaround for the signature minimization bug.
  GenericSignatureBuilder TmpBuilder(
      M.getASTContext(), LookUpConformanceInModule(M.getSwiftModule()));
  TmpBuilder.addGenericSignature(GenericSig);
  TmpBuilder.finalize(SourceLoc(), GenericSig->getGenericParams(),
                      /*allowConcreteGenericParams=*/true);
  GenericSig =
      TmpBuilder.getGenericSignature()->getCanonicalSignature().getPointer();
  GenericEnv = GenericSig->createGenericEnvironment(*M.getSwiftModule());

  assert(!GenericSig || GenericEnv);
  return std::make_pair(GenericEnv, GenericSig);
}

std::pair<GenericEnvironment *, GenericSignature *>
getSignatureWithRequirements(GenericSignature *OrigGenSig,
                             GenericEnvironment *OrigGenericEnv,
                             ArrayRef<Requirement> Requirements,
                             SILModule &M) {
  // Form a new generic signature based on the old one.
  GenericSignatureBuilder Builder(M.getASTContext(),
                           LookUpConformanceInModule(M.getSwiftModule()));

  // First, add the old generic signature.
  Builder.addGenericSignature(OrigGenSig);

  auto Source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  // For each substitution with a concrete type as a replacement,
  // add a new concrete type equality requirement.
  for (auto &Req : Requirements) {
    Builder.addRequirement(Req, Source);
  }

  Builder.finalize(SourceLoc(), OrigGenSig->getGenericParams(),
                   /*allowConcreteGenericParams=*/true);
  return getGenericEnvironmentAndSignature(Builder, M);
}

/// Perform some sanity checks for the requirements
static void
checkSpecializationRequirements(ArrayRef<Requirement> Requirements) {
  for (auto &Req : Requirements) {
    if (Req.getKind() == RequirementKind::SameType) {
      auto FirstType = Req.getFirstType();
      auto SecondType = Req.getSecondType();
      assert(FirstType && SecondType);
      assert(!FirstType->hasArchetype());
      assert(!SecondType->hasArchetype());

      // Only one of the types should be concrete.
      assert(FirstType->hasTypeParameter() != SecondType->hasTypeParameter() &&
             "Only concrete type same-type requirements are supported by "
             "generic specialization");

      (void) FirstType;
      (void) SecondType;

      continue;
    }

    if (Req.getKind() == RequirementKind::Layout) {
      continue;
    }

    llvm_unreachable("Unknown type of requirement in generic specialization");
  }
}

ReabstractionInfo::ReabstractionInfo(SILFunction *OrigF,
                                     ArrayRef<Requirement> Requirements) {
  if (shouldNotSpecializeCallee(OrigF))
    return;

  // Perform some sanity checks for the requirements
  checkSpecializationRequirements(Requirements);

  OriginalF = OrigF;
  ConvertIndirectToDirect = true;

  SILModule &M = OrigF->getModule();
  auto &Ctx = M.getASTContext();

  auto OrigGenericSig = OrigF->getLoweredFunctionType()->getGenericSignature();
  auto OrigGenericEnv = OrigF->getGenericEnvironment();

  std::tie(SpecializedGenericEnv, SpecializedGenericSig) =
      getSignatureWithRequirements(OrigGenericSig, OrigGenericEnv,
                                   Requirements, M);

  {
    SmallVector<Substitution, 4> List;

    OrigGenericSig->getSubstitutions(
      [&](SubstitutableType *type) -> Type {
        return SpecializedGenericEnv->mapTypeIntoContext(type);
      },
      LookUpConformanceInSignature(*SpecializedGenericSig),
      List);
    ClonerParamSubs = Ctx.AllocateCopy(List);
  }

  {
    SmallVector<Substitution, 4> List;

    SpecializedGenericSig->getSubstitutions(
      [&](SubstitutableType *type) -> Type {
        return OrigGenericEnv->mapTypeIntoContext(type);
      },
      LookUpConformanceInSignature(*SpecializedGenericSig),
      List);
    CallerParamSubs = Ctx.AllocateCopy(List);
  }

  {
    CallerInterfaceSubs = OrigGenericSig->getSubstitutionMap(
      [&](SubstitutableType *type) -> Type {
        return SpecializedGenericEnv->mapTypeOutOfContext(
          SpecializedGenericEnv->mapTypeIntoContext(type));
      },
      LookUpConformanceInSignature(*SpecializedGenericSig));
  }

  OriginalParamSubs = CallerParamSubs;

  HasUnboundGenericParams = !SpecializedGenericSig->areAllParamsConcrete();
  createSubstitutedAndSpecializedTypes();
}

static void verifySubstitutionList(SubstitutionList Subs, StringRef Name) {
  DEBUG(llvm::dbgs() << "\nSubstitutions for " << Name << "\n";
        for (auto Sub : Subs) {
          Sub.getReplacement()->dump();
        });
  for (auto Sub : Subs) {
    assert(!Sub.getReplacement()->hasError() &&
           "There should be no error types in substitutions");
  }
}

// Builds a new signature based on the old one and adds same type
// requirements for those generic type parameters that are concrete
// according to the partial substitution. This way, the signature
// has exactly the same generic parameter types, just with more
// requirements.
// Current issues:
// - If Sig2 = GenericSignatureBuilder(Sig2 + Req), then GenericSignatureBuilder(Sig2) != Sig2
// - The set of requirements is not really minimized.
// - Some requirements are lost, when you add a same type parameter to the builder.

// Initialize SpecializedType if and only if the specialization is allowed.
void ReabstractionInfo::specializeConcreteSubstitutions(
    ApplySite Apply, SILFunction *Callee, ArrayRef<Substitution> ParamSubs) {

  SILModule &M = Callee->getModule();
  auto &Ctx = M.getASTContext();

  OriginalF = Callee;

  auto OrigGenericSig = Callee->getLoweredFunctionType()->getGenericSignature();
  auto OrigGenericEnv = Callee->getGenericEnvironment();

  SubstitutionMap InterfaceSubs;
  // Get the original substitution map.
  if (Callee->getLoweredFunctionType()->getGenericSignature())
    InterfaceSubs = Callee->getLoweredFunctionType()->getGenericSignature()
      ->getSubstitutionMap(ParamSubs);

  // This is a workaround for the rdar://30610428
  if (!EnablePartialSpecialization) {
    SubstitutedType =
        Callee->getLoweredFunctionType()->substGenericArgs(M, InterfaceSubs);
    ClonerParamSubs = OriginalParamSubs;
    CallerParamSubs = {};
    createSubstitutedAndSpecializedTypes();
    return;
  }

  // Build a set of requirements.
  SmallVector<Requirement, 4> Requirements;

  for (auto DP : OrigGenericSig->getGenericParams()) {
    auto Replacement = Type(DP).subst(InterfaceSubs);
    if (Replacement->hasArchetype())
      continue;
    // Replacemengt is concrete. Add a same type requirement.
    Requirement Req(RequirementKind::SameType, DP, Replacement);
    Requirements.push_back(Req);
  }

  std::tie(SpecializedGenericEnv, SpecializedGenericSig) =
      getSignatureWithRequirements(OrigGenericSig, OrigGenericEnv,
                                   Requirements, M);
  HasUnboundGenericParams = !SpecializedGenericSig->areAllParamsConcrete();

  // No partial specializations!
  //if (HasUnboundGenericParams)
  //  return;

  {
    SmallVector<Substitution, 4> List;

    OrigGenericSig->getSubstitutions(
      [&](SubstitutableType *type) -> Type {
        return SpecializedGenericEnv->mapTypeIntoContext(type);
      },
      LookUpConformanceInSignature(*SpecializedGenericSig),
      List);
    ClonerParamSubs = Ctx.AllocateCopy(List);
    verifySubstitutionList(ClonerParamSubs, "ClonerParamSubs");
  }

  {
    SmallVector<Substitution, 4> List;
    SpecializedGenericSig->getSubstitutions(InterfaceSubs, List);
    CallerParamSubs = Ctx.AllocateCopy(List);
    verifySubstitutionList(CallerParamSubs, "CallerParamSubs");
  }

  {
    CallerInterfaceSubs = OrigGenericSig->getSubstitutionMap(
      [&](SubstitutableType *type) -> Type {
        return SpecializedGenericEnv->mapTypeOutOfContext(
          SpecializedGenericEnv->mapTypeIntoContext(type));
      },
        //LookUpConformanceInSignature(*SpecializedGenericSig));
      LookUpConformanceInSignature(*OrigGenericSig));
  }

  HasUnboundGenericParams = !SpecializedGenericSig->areAllParamsConcrete();
  createSubstitutedAndSpecializedTypes();
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
  // If replacement is a concrete type, this substitution
  // should participate.
  if (!Replacement->hasArchetype())
    return true;

  // We cannot handle opened existentials yet.
  if (Replacement->hasOpenedExistential())
    return false;

  if (!SupportGenericSubstitutions) {
    // Don't partially specialize if the replacement contains an archetype.
    if (Replacement->hasArchetype())
      return false;
  }

  if (OptimizeGenericSubstitutions) {
    // Is it an unconstrained generic parameter?
    if (Conformances.empty()) {
      if (Replacement->is<ArchetypeType>()) {
        // TODO: If Replacement add a new layout constraint, then
        // it may be still useful to perform the partial specialization.
        return false;
      }
    }
  }

  return true;
}

static void remapRequirements(GenericSignature *GenSig,
                              SubstitutionMap &SubsMap,
                              GenericSignatureBuilder &Builder,
                              ModuleDecl *SM) {
  auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();

  if (!GenSig)
    return;

  // Next, add each of the requirements (mapped from the requirement's
  // interface types into the specialized interface type parameters).
  // RequirementSource source(RequirementSource::Explicit, SourceLoc());

  // Add requirements derived from the caller signature for the
  // caller's archetypes mapped to the specialized signature.
  for (auto &reqReq : GenSig->getRequirements()) {
    DEBUG(llvm::dbgs() << "\n\nRe-mapping the requirement:\n"; reqReq.dump());

    auto first = reqReq.getFirstType();
    // Is this generic generic type equivalent to a concrete type?
    if (first->hasTypeParameter() &&
        !GenSig->getCanonicalTypeInContext(first, *SM)->hasTypeParameter())
      continue;

    first = reqReq.getFirstType().subst(SubsMap);
    assert(!first->hasError());

    auto Kind = reqReq.getKind();

    switch (Kind) {
    case RequirementKind::SameType:
    case RequirementKind::Superclass:
    case RequirementKind::Conformance: {
      auto second = reqReq.getSecondType().subst(SubsMap);
      assert(!second->hasError());
      // Substitute the constrained types.
      if (Kind != RequirementKind::SameType && !first->hasTypeParameter())
        break;
      if (Kind == RequirementKind::SameType && !first->hasTypeParameter() &&
          !second->hasTypeParameter())
        break;

      Requirement Req(Kind, first, second);
      auto Failure = Builder.addRequirement(Req, source);
      assert(!Failure);
      DEBUG(llvm::dbgs() << "\nRe-mapped requirement:\n"; Req.dump());
      break;
    }
    case RequirementKind::Layout: {
      if (!first->hasTypeParameter())
        break;

      Requirement Req(RequirementKind::Layout, first,
                      reqReq.getLayoutConstraint());
      auto Failure = Builder.addRequirement(Req, source);
      assert(!Failure);
      DEBUG(llvm::dbgs() << "\nRe-mapped requirement:\n"; Req.dump());
      break;
    }
    }
  }
}

namespace {

/// A helper class for creating partially specialized function signatures.
class FunctionSignaturePartialSpecializer {
  // Maps caller's generic parameters to generic parameters of the specialized
  // function.
  llvm::DenseMap<SubstitutableType *, Type>
      CallerInterfaceToSpecializedInterfaceMapping;

  // Maps callee's generic parameters to generic parameters of the specialized
  // function.
  llvm::DenseMap<SubstitutableType *, Type>
      CalleeInterfaceToSpecializedInterfaceMapping;

  // Maps the generic parameters of the specialized function to the caller's
  // contextual types.
  llvm::DenseMap<SubstitutableType *, Type>
      SpecializedInterfaceToCallerArchetypeMapping;

  // A SubstitutionMap for re-mapping caller's interface types
  // to interface types of the specialized function.
  SubstitutionMap CallerInterfaceToSpecializedInterfaceMap;

  // Maps callee's interface types to caller's contextual types.
  // It is computed from the original SubstitutionList.
  SubstitutionMap CalleeInterfaceToCallerArchetypeMap;

  // Maps callee's interface types to specialzed functions interface types.
  SubstitutionMap CalleeInterfaceToSpecializedInterfaceMap;

  SILModule &M;
  ModuleDecl *SM;
  ASTContext &Ctx;

  // This is a builder for a new partially specialized generic signature.
  GenericSignatureBuilder Builder;

  // Set of newly created generic type parameters.
  SmallVector<GenericTypeParamType*, 4> AllGenericParams;

  // Archetypes used in the substitutions of an apply instructions.
  // These are the contextual archetypes of the caller function, which
  // invokes a generic function that is being specialized.
  llvm::SmallSetVector<ArchetypeType *, 2> UsedCallerArchetypes;

  // Number of created generic parameters so far.
  unsigned GPIdx = 0;

  void createGenericParamsForUsedCallerArchetypes(
      GenericEnvironment *CallerGenericEnv);

  void createGenericParamsForCalleeGenericParams(
      GenericSignature *CalleeGenericSig, GenericEnvironment *CallerGenericEnv);

  void addRequirements(GenericSignature *CalleeGenericSig);

  std::pair<GenericEnvironment *, GenericSignature *>
  getSpecializedGenericEnvironmentAndSignature();

  void computeCallerInterfaceToSpecializedInterfaceMap(
      GenericSignature *CallerGenericSig);

  void computeCalleeInterfaceToSpecializedInterfaceMap(
      GenericSignature *CalleeGenericSig);

  /// Collect all used archetypes from all the substitutions.
  /// Take into account only those archetypes that occur in the
  /// substitutions of generic parameters which will be partially
  /// specialized. Ignore all others.
  void callectUsedCallerArchetypes(SubstitutionList ParamSubs);

  // Create a new generic parameter.
  GenericTypeParamType *createGenericParam();

public:
  FunctionSignaturePartialSpecializer(SILModule &M)
      : M(M), SM(M.getSwiftModule()), Ctx(M.getASTContext()),
        Builder(Ctx, LookUpConformanceInModule(SM)) {}

  std::pair<GenericEnvironment *, GenericSignature *>
  createSpecializedGenericSignature(GenericSignature *CallerGenericSig,
                                    GenericEnvironment *CallerGenericEnv,
                                    GenericSignature *CalleeGenericSig,
                                    SubstitutionList ParamSubs);

  void computeClonerParamSubs(GenericSignature *CalleeGenericSig,
                              ReabstractionInfo &ReInfo,
                              SubstitutionList &ClonerParamSubs);

  void computeCallerParamSubs(GenericSignature *SpecializedGenericSig,
                              SubstitutionList &CallerParamSubs);

  void computeCallerInterfaceSubs(GenericSignature *CalleeGenericSig,
                                  ReabstractionInfo &ReInfo,
                                  SubstitutionMap &CallerInterfaceSubs);

  CanSILFunctionType
  getSpecializedSubstFunctionType(CanSILFunctionType CalleeFnTy,
                                  ReabstractionInfo &ReInfo);
};

} // end of namespace

GenericTypeParamType *
FunctionSignaturePartialSpecializer::createGenericParam() {
  auto GP = GenericTypeParamType::get(0, GPIdx++, Ctx);
  AllGenericParams.push_back(GP);
  Builder.addGenericParameter(GP);
  return GP;
}

/// Collect all used caller's archetypes from all the substitutions.
void FunctionSignaturePartialSpecializer::callectUsedCallerArchetypes(
    SubstitutionList ParamSubs) {
  for (auto Sub : ParamSubs) {
    auto Replacement = Sub.getReplacement();
    if (!Replacement->hasArchetype())
      continue;

    // If the substitution will not be performed in the specialized
    // function, there is no need to check for any archetypes inside
    // the replacement.
    if (!shouldBePartiallySpecialized(Replacement, Sub.getConformances()))
      continue;

    // Add used generic parameters/archetypes.
    Replacement.visit([&](Type Ty) {
      if (auto Archetype = Ty->getAs<ArchetypeType>()) {
        UsedCallerArchetypes.insert(Archetype->getPrimary());
      }
    });
  }
}

void FunctionSignaturePartialSpecializer::
    computeCallerInterfaceToSpecializedInterfaceMap(
        GenericSignature *CallerGenericSig) {
  if (!CallerGenericSig)
    return;

  CallerInterfaceToSpecializedInterfaceMap =
      CallerGenericSig->getSubstitutionMap(
          [&](SubstitutableType *type) -> Type {
            return CallerInterfaceToSpecializedInterfaceMapping.lookup(type);
          },
          LookUpConformanceInSignature(*CallerGenericSig));

  DEBUG(llvm::dbgs() << "\n\nCallerInterfaceToSpecializedInterfaceMap map:\n";
        CallerInterfaceToSpecializedInterfaceMap.dump());
}

void FunctionSignaturePartialSpecializer::
    computeCalleeInterfaceToSpecializedInterfaceMap(
        GenericSignature *CalleeGenericSig) {
  CalleeInterfaceToSpecializedInterfaceMap =
      CalleeGenericSig->getSubstitutionMap(
          [&](SubstitutableType *type) -> Type {
            return CalleeInterfaceToSpecializedInterfaceMapping.lookup(type);
          },
          LookUpConformanceInSignature(*CalleeGenericSig));

  DEBUG(llvm::dbgs() << "\n\nCalleeInterfaceToSpecializedInterfaceMap:\n";
        CalleeInterfaceToSpecializedInterfaceMap.dump());
}

/// Generate a new generic type parameter for each used archetype from
/// the caller.
void FunctionSignaturePartialSpecializer::
    createGenericParamsForUsedCallerArchetypes(
        GenericEnvironment *CallerGenericEnv) {
  for (auto CallerArchetype : UsedCallerArchetypes) {
    auto CallerGenericParam =
        CallerGenericEnv->mapTypeOutOfContext(CallerArchetype);
    assert(CallerGenericParam->is<GenericTypeParamType>());

    DEBUG(llvm::dbgs() << "\n\nChecking used caller archetype:\n";
          CallerArchetype->dump();
          llvm::dbgs() << "It corresponds to the caller generic parameter:\n";
          CallerGenericParam->dump());

    // Create an equivalent generic parameter.
    auto SubstGenericParam = createGenericParam();
    auto SubstGenericParamCanTy = SubstGenericParam->getCanonicalType();

    CallerInterfaceToSpecializedInterfaceMapping
        [CallerGenericParam->getCanonicalType()
             ->castTo<GenericTypeParamType>()] = SubstGenericParam;

    SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
        CallerArchetype;

    DEBUG(llvm::dbgs() << "\nCreated a new specialized generic parameter:\n";
          SubstGenericParam->dump();
          llvm::dbgs() << "Created a mapping "
                          "(caller interface -> specialize interface):\n"
                       << CallerGenericParam << " -> " << SubstGenericParamCanTy
                       << "\n";
          llvm::dbgs() << "Created a mapping"
                          "(specialized interface -> caller archetype):\n"
                       << SubstGenericParamCanTy << " -> "
                       << CallerArchetype->getCanonicalType() << "\n");
  }
}

/// Create a new generic parameter for each of the callee's generic parameters
/// which require a substition.
void FunctionSignaturePartialSpecializer::
    createGenericParamsForCalleeGenericParams(
        GenericSignature *CalleeGenericSig,
        GenericEnvironment *CallerGenericEnv) {
  auto Source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  for (auto GP : CalleeGenericSig->getGenericParams()) {
    auto CanTy = GP->getCanonicalType();
    auto CanTyInContext =
        CalleeGenericSig->getCanonicalTypeInContext(CanTy, *SM);
    auto Replacement = CanTyInContext.subst(CalleeInterfaceToCallerArchetypeMap);
    DEBUG(llvm::dbgs() << "\n\nChecking callee generic parameter:\n";
          CanTy->dump());
    if (!Replacement) {
      DEBUG(llvm::dbgs() << "No replacement found. Skipping.\n");
      continue;
    }

    DEBUG(llvm::dbgs() << "Replacement found:\n"; Replacement->dump());

    bool ShouldSpecializeGP = shouldBePartiallySpecialized(
        Replacement,
        {});
    //CalleeInterfaceToCallerArchetypeMap.lookupConformances(CanTy));

    if (ShouldSpecializeGP) {
      DEBUG(llvm::dbgs() << "Should be partially specialized.\n");
    } else {
      DEBUG(llvm::dbgs() << "Should not be partially specialized.\n");
    }

    // Create an equivalent generic parameter in the specialized
    // generic environment.
    auto SubstGenericParam = createGenericParam();
    auto SubstGenericParamCanTy = SubstGenericParam->getCanonicalType();

    // Remember which specialized generic parameter correspond's to callee's
    // generic parameter.
    CalleeInterfaceToSpecializedInterfaceMapping[GP] = SubstGenericParam;

    DEBUG(llvm::dbgs() << "\nCreated a new specialized generic parameter:\n";
          SubstGenericParam->dump();
          llvm::dbgs() << "Created a mapping "
                          "(callee interface -> specialized interface):\n"
                       << CanTy << " -> " << SubstGenericParamCanTy << "\n");

    // if (!Replacement->hasArchetype()) {
    if (!ShouldSpecializeGP) {
      // Remember the original subsitution from the apply instruction.
      SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
          Replacement;
      DEBUG(llvm::dbgs() << "Created a mapping (specialized interface -> "
                            "caller archetype):\n"
                         << Type(SubstGenericParam) << " -> " << Replacement
                         << "\n");
      continue;
    }

    // Add a same type requirement based on the provided generic parameter
    // substitutions.
    auto ReplacementCallerInterfaceTy = Replacement;
    if (CallerGenericEnv)
      ReplacementCallerInterfaceTy =
          CallerGenericEnv->mapTypeOutOfContext(Replacement);

    auto SpecializedReplacementCallerInterfaceTy =
        ReplacementCallerInterfaceTy.subst(
            CallerInterfaceToSpecializedInterfaceMap);
    assert(!SpecializedReplacementCallerInterfaceTy->hasError());

    Requirement Req(RequirementKind::SameType, SubstGenericParamCanTy,
                    SpecializedReplacementCallerInterfaceTy);
    Builder.addRequirement(Req, Source);

    DEBUG(llvm::dbgs() << "Added a requirement:\n"; Req.dump());

    if (ReplacementCallerInterfaceTy->is<GenericTypeParamType>()) {
      // Remember that the new generic parameter corrsponds
      // to the same caller archetype, which correponds to
      // the ReplacementCallerInterfaceTy.
      SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
          SpecializedInterfaceToCallerArchetypeMapping.lookup(
              ReplacementCallerInterfaceTy
                  .subst(CallerInterfaceToSpecializedInterfaceMap)
                  ->castTo<SubstitutableType>());
      DEBUG(llvm::dbgs()
            << "Created a mapping (specialized interface -> "
               "caller archetype):\n"
            << Type(SubstGenericParam) << " -> "
            << SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam]
                   ->getCanonicalType()
            << "\n");
    }
  }
}

// Add each of the requirements (mapped from the requirement's
// interface types into the specialized interface type parameters).
// TODO: Do we need to add requirements of the caller's archetypes, which
// stem from the caller's generic signature? If so, which ones? All of them?
// Just some of them? Most likely we need to add only those which are not
// present in the callee's signature.
void FunctionSignaturePartialSpecializer::addRequirements(
    GenericSignature *CalleeGenericSig) {
  remapRequirements(CalleeGenericSig, CalleeInterfaceToSpecializedInterfaceMap,
                    Builder, SM);
}

std::pair<GenericEnvironment *, GenericSignature *>
FunctionSignaturePartialSpecializer::
    getSpecializedGenericEnvironmentAndSignature() {
  // Finalize the archetype builder.
  Builder.finalize(SourceLoc(), AllGenericParams,
                   /*allowConcreteGenericParams=*/true);
  // Get the specialized generic signature and generic environment.
  if (!AllGenericParams.empty()) {
    // Produce the generic signature and environment.
    return getGenericEnvironmentAndSignature(Builder, M);
  }
  return std::make_pair(nullptr, nullptr);
}

void FunctionSignaturePartialSpecializer::computeClonerParamSubs(
    GenericSignature *CalleeGenericSig, ReabstractionInfo &ReInfo,
    SubstitutionList &ClonerParamSubs) {
  SmallVector<Substitution, 4> List;

  CalleeGenericSig->getSubstitutions(
      [&](SubstitutableType *type) -> Type {
        DEBUG(llvm::dbgs() << "\ngetSubstitution for ClonerParamSubs:\n"
                           << Type(type) << "\n"
                           << "in generic signature:\n";
              CalleeGenericSig->dump());
        auto SpecializedInterfaceTy =
            Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);
        return ReInfo.getSpecializedGenericEnvironment()->mapTypeIntoContext(
            SpecializedInterfaceTy);
      },
      LookUpConformanceInSignature(*ReInfo.getSpecializedGenericSignature()),
      List);
  ClonerParamSubs = Ctx.AllocateCopy(List);
  verifySubstitutionList(ClonerParamSubs, "ClonerParamSubs");
}

void FunctionSignaturePartialSpecializer::computeCallerParamSubs(
    GenericSignature *SpecializedGenericSig,
    SubstitutionList &CallerParamSubs) {
  // TODO: Handle the case where calle's generic type parameter should
  // not be partially specialized. In this case,
  // CalleeInterfaceToCallerArchetypeMap would not contian a mapping for it.

  SmallVector<Substitution, 4> List;

  // Define a substitution map for re-mapping interface types of
  // the specialized function to contextual types of the caller.
  auto SpecializedInterfaceToCallerArchetypeMap =
      SpecializedGenericSig->getSubstitutionMap(
          [&](SubstitutableType *type) -> Type {
            return SpecializedInterfaceToCallerArchetypeMapping.lookup(type);
          },
          LookUpConformanceInSignature(*SpecializedGenericSig));

  DEBUG(llvm::dbgs() << "\n\nSpecializedInterfaceToCallerArchetypeMap map:\n";
        SpecializedInterfaceToCallerArchetypeMap.dump());

  SpecializedGenericSig->getSubstitutions(
      SpecializedInterfaceToCallerArchetypeMap, List);

  CallerParamSubs = Ctx.AllocateCopy(List);
  verifySubstitutionList(CallerParamSubs, "CallerParamSubs");
}

void FunctionSignaturePartialSpecializer::computeCallerInterfaceSubs(
    GenericSignature *CalleeGenericSig, ReabstractionInfo &ReInfo,
    SubstitutionMap &CallerInterfaceSubs) {
  CallerInterfaceSubs = CalleeGenericSig->getSubstitutionMap(
      [&](SubstitutableType *type) -> Type {
        // First, map callee's interface type to specialized interface type.
        auto Ty = Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);
        Type SpecializedInterfaceTy =
            ReInfo.getSpecializedGenericEnvironment()->mapTypeOutOfContext(
                ReInfo.getSpecializedGenericEnvironment()->mapTypeIntoContext(
                    Ty));
        assert(!SpecializedInterfaceTy->hasError());
        return SpecializedInterfaceTy;
      },
      LookUpConformanceInSignature(*CalleeGenericSig));

  DEBUG(llvm::dbgs() << "\n\nCallerInterfaceSubs map:\n";
        CallerInterfaceSubs.dump());
}

CanSILFunctionType
FunctionSignaturePartialSpecializer::getSpecializedSubstFunctionType(
    CanSILFunctionType CalleeFnTy, ReabstractionInfo &ReInfo) {
  // TODO: Should we try to minimize the number of generic parameters?
  // We can remove the generic parameter if it is not used
  // anywhere in the function signature.
  // This is a specialized interface type of the callee.

  auto *SpecializedGenericSig = ReInfo.getSpecializedGenericSignature();
  auto *SpecializedGenericEnv = ReInfo.getSpecializedGenericEnvironment();

  CanGenericSignature CanSpecializedGenericSig;
  if (SpecializedGenericSig && !SpecializedGenericSig->areAllParamsConcrete())
    CanSpecializedGenericSig = SpecializedGenericSig->getCanonicalSignature();

  CanSILFunctionType SpecializedSubstFnTy;
  {
    Lowering::GenericContextScope GenericScope(M.Types,
                                               CanSpecializedGenericSig);
    SpecializedSubstFnTy = CalleeFnTy->substGenericArgs(
        M,
        [&](SubstitutableType *type) -> Type {
          auto SpecializedInterfaceTy =
              Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);

          return SpecializedGenericEnv->mapTypeOutOfContext(
              SpecializedGenericEnv->mapTypeIntoContext(
                  SpecializedInterfaceTy));
        },
        LookUpConformanceInSignature(*SpecializedGenericSig));
  }

  return SpecializedSubstFnTy;
}

std::pair<GenericEnvironment *, GenericSignature *>
FunctionSignaturePartialSpecializer::createSpecializedGenericSignature(
    GenericSignature *CallerGenericSig, GenericEnvironment *CallerGenericEnv,
    GenericSignature *CalleeGenericSig, SubstitutionList ParamSubs) {
  CalleeInterfaceToCallerArchetypeMap =
      CalleeGenericSig->getSubstitutionMap(ParamSubs);

  // Collect all used caller's archetypes from all the substitutions.
  callectUsedCallerArchetypes(ParamSubs);

  // Generate a new generic type parameter for each used archetype from
  // the caller.
  createGenericParamsForUsedCallerArchetypes(CallerGenericEnv);

  // Create a SubstitutionMap for re-mapping caller's interface types
  // to interface types of the specialized function.
  computeCallerInterfaceToSpecializedInterfaceMap(CallerGenericSig);

  // Add generic parameters that will come from the callee.
  // Introduce a new generic parameter in the new new generic signature
  // for each generic parameter from the callee.
  createGenericParamsForCalleeGenericParams(CalleeGenericSig, CallerGenericEnv);

  computeCalleeInterfaceToSpecializedInterfaceMap(CalleeGenericSig);

  // Add requirements from the callee signature.
  addRequirements(CalleeGenericSig);

  return getSpecializedGenericEnvironmentAndSignature();
}

// Builds a new generic and function signatures for a partial specialization.
// Allows for partial specializations even if substitututions contain
// type parameters.
//
// The new generic signature has the following generic parameters:
// - For each substitution with a concrete type CT as a replacement for a
// generic type T, it introduces a generic parameter T' and a
// requirement T' == CT
// - For all other substitutions that are considered for partial specialization,
// it collects first the archetypes used in the replacements. Then for each such
// archetype A a new generic parameter T' introduced.
// - If there is a substitution for type T and this substitution is execluded
// from partial specialization (e.g. because it is impossible or would result
// in a less efficient code), then a new generic parameter T' is introduced,
// which does not get any additional, more specific requirements based on the
// substitutions.
//
// After all generic parameters are added accoriding to the rules above,
// the requirements of the callee's signature are re-mapped by re-formulating
// them in terms of the newly introduced generic parameters. In case a remapped
// requirement does not contain any generic types, it can be omitted, because
// it is fulfilled already.

// Current issues:
// - If Sig2 = GenericSignatureBuilder(Sig2 + Req), then
// GenericSignatureBuilder(Sig2) != Sig2
// - The set of requirements is not really minimized.
// - Some requirements are lost, when you add a same type parameter to the
// builder.
void ReabstractionInfo::specializeConcreteAndGenericSubstitutions(
    ApplySite Apply, SILFunction *Callee, ArrayRef<Substitution> ParamSubs) {
  SILModule &M = Callee->getModule();
  auto *SM = M.getSwiftModule();
  auto &Ctx = M.getASTContext();

  // Caller is the SILFunction containing the apply instruction.
  auto CallerGenericSig =
      Apply.getFunction()->getLoweredFunctionType()->getGenericSignature();
  auto CallerGenericEnv = Apply.getFunction()->getGenericEnvironment();

  // Callee is the generic function being called by the apply instruction.
  auto CalleeFnTy = Callee->getLoweredFunctionType();
  auto CalleeGenericSig = CalleeFnTy->getGenericSignature();

  // Used naming convention:
  // Caller - the function containg the provided apply instruction.
  // Callee - the callee of the provided apply instruction.
  // Specialized - the specialized callee which is being created.

  DEBUG(llvm::dbgs() << "\n\nTrying partial specialization for: "
                     << Callee->getName() << "\n";
        llvm::dbgs() << "Callee generic signature is:\n";
        CalleeGenericSig->dump());

  FunctionSignaturePartialSpecializer FSPS(M);

  // Get the parially specialized generic signature and generic environment.
  auto GenPair = FSPS.createSpecializedGenericSignature(
      CallerGenericSig, CallerGenericEnv, CalleeGenericSig, ParamSubs);

  if (GenPair.second) {
    SpecializedGenericSig = GenPair.second->getCanonicalSignature();
    SpecializedGenericEnv = GenPair.first;
    DEBUG(llvm::dbgs() << "\nCreated SpecializedGenericSig:\n";
          SpecializedGenericSig->dump());
  }

  // Create substitution lists for the caller and cloner.
  FSPS.computeClonerParamSubs(CalleeGenericSig, *this, ClonerParamSubs);
  FSPS.computeCallerParamSubs(SpecializedGenericSig, CallerParamSubs);
  // Create a substitution map for the caller interface substitutions.
  FSPS.computeCallerInterfaceSubs(CalleeGenericSig, *this, CallerInterfaceSubs);

  // Create a function type for the new specialized function signature using
  // the new generic signature.
  auto SpecializedSubstFnTy =
      FSPS.getSpecializedSubstFunctionType(CalleeFnTy, *this);

  if (!SpecializedGenericSig || SpecializedGenericSig->areAllParamsConcrete()) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
  }
  assert(SpecializedGenericSig || !SpecializedSubstFnTy->hasTypeParameter());

  SubstitutedType = SILFunctionType::get(
      SpecializedGenericSig, SpecializedSubstFnTy->getExtInfo(),
      SpecializedSubstFnTy->getCalleeConvention(),
      SpecializedSubstFnTy->getParameters(), SpecializedSubstFnTy->getResults(),
      SpecializedSubstFnTy->getOptionalErrorResult(), Ctx);

  assert(!SubstitutedType->hasArchetype() &&
         "Function type should not contain archetypes");

  HasUnboundGenericParams =
      SpecializedGenericSig && !SpecializedGenericSig->areAllParamsConcrete();
  createSubstitutedAndSpecializedTypes();

  if (getSubstitutedType() != Callee->getLoweredFunctionType()) {
    if (getSubstitutedType()->isPolymorphic())
      DEBUG(llvm::dbgs() << "Created new specialized type: " << SpecializedType
                         << "\n");
  }
}

// =============================================================================
// GenericFuncSpecializer
// =============================================================================

GenericFuncSpecializer::GenericFuncSpecializer(SILFunction *GenericFunc,
                                               SubstitutionList ParamSubs,
                                               IsFragile_t Fragile,
                                               const ReabstractionInfo &ReInfo)
    : M(GenericFunc->getModule()),
      GenericFunc(GenericFunc),
      ParamSubs(ParamSubs),
      Fragile(Fragile),
      ReInfo(ReInfo) {

  assert(GenericFunc->isDefinition() && "Expected definition to specialize!");
  auto FnTy = ReInfo.getSpecializedType();

  if (ReInfo.isPartialSpecialization()) {
    Mangle::PartialSpecializationMangler Mangler(
        GenericFunc, FnTy, Fragile, /*isReAbstracted*/ true);
    ClonedName = Mangler.mangle();
  } else {
    Mangle::GenericSpecializationMangler Mangler(
        GenericFunc, ParamSubs, Fragile, /*isReAbstracted*/ true);
    ClonedName = Mangler.mangle();
  }
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

void ReabstractionInfo::verify() const {
  assert((!SpecializedGenericSig && !SpecializedGenericEnv &&
          !getSpecializedType()->isPolymorphic()) ||
         (SpecializedGenericSig && SpecializedGenericEnv &&
          getSpecializedType()->isPolymorphic()));
}

// Create a new specialized function if possible, and cache it.
SILFunction *GenericFuncSpecializer::tryCreateSpecialization() {
  // Do not create any new specializations at Onone.
  if (M.getOptions().Optimization <= SILOptions::SILOptMode::None)
    return nullptr;

  DEBUG(
    if (M.getOptions().Optimization <= SILOptions::SILOptMode::Debug) {
      llvm::dbgs() << "Creating a specialization: " << ClonedName << "\n"; });

  ReInfo.verify();

  // Create a new function.
  SILFunction *SpecializedF = GenericCloner::cloneFunction(
      GenericFunc, Fragile, ReInfo,
      // Use these substitutions inside the new specialized function being
      // created.
      ReInfo.getClonerParamSubstitutions(),
      ClonedName);
  assert((SpecializedF->getLoweredFunctionType()->isPolymorphic() &&
          SpecializedF->getGenericEnvironment()) ||
         (!SpecializedF->getLoweredFunctionType()->isPolymorphic() &&
          !SpecializedF->getGenericEnvironment()));
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
  /// SIL function conventions for the original apply site with substitutions.
  SILLocation Loc = AI.getLoc();
  auto substConv = AI.getSubstCalleeConv();
  unsigned ArgIdx = AI.getCalleeArgIndexOfFirstAppliedArg();
  for (auto &Op : AI.getArgumentOperands()) {
    auto handleConversion = [&]() {
      // Rewriting SIL arguments is only for lowered addresses.
      if (!substConv.useLoweredAddresses())
        return false;

      if (ArgIdx < substConv.getSILArgIndexOfFirstParam()) {
        // Handle result arguments.
        unsigned formalIdx =
            substConv.getIndirectFormalResultIndexForSILArg(ArgIdx);
        if (ReInfo.isFormalResultConverted(formalIdx)) {
          // The result is converted from indirect to direct. We need to insert
          // a store later.
          assert(!StoreResultTo);
          StoreResultTo = Op.get();
          return true;
        }
      } else {
        // Handle arguments for formal parameters.
        unsigned paramIdx = ArgIdx - substConv.getSILArgIndexOfFirstParam();
        if (ReInfo.isParamConverted(paramIdx)) {
          // An argument is converted from indirect to direct. Instead of the
          // address we pass the loaded value.
          SILValue Val = Builder.createLoad(
              Loc, Op.get(), LoadOwnershipQualifier::Unqualified);
          Arguments.push_back(Val);
          return true;
        }
      }
      return false;
    };
    if (!handleConversion())
      Arguments.push_back(Op.get());

    ++ArgIdx;
  }
}

/// Return a substituted callee function type.
static CanSILFunctionType
getCalleeSubstFunctionType(SILValue Callee, SubstitutionList Subs) {
  // Create a substituted callee type.
  auto CanFnTy = Callee->getType().castTo<SILFunctionType>();
  auto CalleeSubstFnTy = CanFnTy;

  if (CanFnTy->isPolymorphic() && !Subs.empty()) {
    CalleeSubstFnTy = CanFnTy->substGenericArgs(*Callee->getModule(), Subs);
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
    if (auto FRI = dyn_cast<FunctionRefInst>(Callee)) {
      assert(Subs.size() ==
             FRI->getReferencedFunction()
                 ->getLoweredFunctionType()
                 ->getGenericSignature()
                 ->getSubstitutionListSize());
    }
  }

  auto CalleeSubstFnTy = getCalleeSubstFunctionType(Callee, Subs);
  auto CalleeSILSubstFnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);
  SILFunctionConventions substConv(CalleeSubstFnTy, Builder.getModule());

  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    SILBasicBlock *ResultBB = TAI->getNormalBB();
    assert(ResultBB->getSinglePredecessorBlock() == TAI->getParent());
    auto *NewTAI =
        Builder.createTryApply(Loc, Callee, CalleeSILSubstFnTy, Subs, Arguments,
                               ResultBB, TAI->getErrorBB());
    if (StoreResultTo) {
      assert(substConv.useLoweredAddresses());
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
                                      substConv.getSILResultType(), Subs,
                                      Arguments, A->isNonThrowing());
    if (StoreResultTo) {
      assert(substConv.useLoweredAddresses());
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

namespace {
class ReabstractionThunkGenerator {
  SILFunction *OrigF;
  SILModule &M;
  SILFunction *SpecializedFunc;
  const ReabstractionInfo &ReInfo;
  PartialApplyInst *OrigPAI;

  IsFragile_t Fragile = IsNotFragile;
  std::string ThunkName;
  RegularLocation Loc;
  SmallVector<SILValue, 4> Arguments;

public:
  ReabstractionThunkGenerator(const ReabstractionInfo &ReInfo,
                              PartialApplyInst *OrigPAI,
                              SILFunction *SpecializedFunc)
      : OrigF(OrigPAI->getCalleeFunction()), M(OrigF->getModule()),
        SpecializedFunc(SpecializedFunc), ReInfo(ReInfo), OrigPAI(OrigPAI),
        Loc(RegularLocation::getAutoGeneratedLocation()) {
    if (OrigF->isFragile() && OrigPAI->getFunction()->isFragile())
      Fragile = IsFragile;

    {
      if (!ReInfo.isPartialSpecialization()) {
        Mangle::GenericSpecializationMangler Mangler(
            OrigF, ReInfo.getOriginalParamSubstitutions(), Fragile,
            /*isReAbstracted*/ false);

        ThunkName = Mangler.mangle();
      } else {
        Mangle::PartialSpecializationMangler Mangler(
            OrigF, ReInfo.getSpecializedType(), Fragile,
            /*isReAbstracted*/ false);

        ThunkName = Mangler.mangle();
      }
    }
  }

  SILFunction *createThunk();

protected:
  SILValue createReabstractionThunkApply(SILBuilder &Builder);
  SILArgument *convertReabstractionThunkArguments(SILBuilder &Builder);
};
} // anonymous namespace

SILFunction *ReabstractionThunkGenerator::createThunk() {
  SILFunction *Thunk =
      M.getOrCreateSharedFunction(Loc, ThunkName, ReInfo.getSubstitutedType(),
                                  IsBare, IsTransparent, Fragile, IsThunk);
  // Re-use an existing thunk.
  if (!Thunk->empty())
    return Thunk;

  Thunk->setGenericEnvironment(ReInfo.getSpecializedGenericEnvironment());

  SILBasicBlock *EntryBB = Thunk->createBasicBlock();
  SILBuilder Builder(EntryBB);

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

  if (!SILModuleConventions(M).useLoweredAddresses()) {
    for (auto SpecArg : SpecializedFunc->getArguments()) {
      SILArgument *NewArg = EntryBB->createFunctionArgument(SpecArg->getType(),
                                                            SpecArg->getDecl());
      Arguments.push_back(NewArg);
    }
    SILValue ReturnValue = createReabstractionThunkApply(Builder);
    Builder.createReturn(Loc, ReturnValue);
    return Thunk;
  }
  // Handle lowered addresses.
  SILArgument *ReturnValueAddr = convertReabstractionThunkArguments(Builder);

  SILValue ReturnValue = createReabstractionThunkApply(Builder);

  if (ReturnValueAddr) {
    // Need to store the direct results to the original indirect address.
    Builder.createStore(Loc, ReturnValue, ReturnValueAddr,
                        StoreOwnershipQualifier::Unqualified);
    SILType VoidTy =
        OrigPAI->getSubstCalleeType()->getDirectFormalResultsType();
    assert(VoidTy.isVoid());
    ReturnValue = Builder.createTuple(Loc, VoidTy, {});
  }
  Builder.createReturn(Loc, ReturnValue);
  return Thunk;
}

// Create a call to a reabstraction thunk. Return the call's direct result.
SILValue ReabstractionThunkGenerator::createReabstractionThunkApply(
    SILBuilder &Builder) {
  SILFunction *Thunk = &Builder.getFunction();
  auto *FRI = Builder.createFunctionRef(Loc, SpecializedFunc);
  auto Subs = Thunk->getForwardingSubstitutions();
  auto CalleeSubstFnTy = getCalleeSubstFunctionType(FRI, Subs);
  auto CalleeSILSubstFnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);
  auto specConv = SpecializedFunc->getConventions();
  if (!SpecializedFunc->getLoweredFunctionType()->hasErrorResult()) {
    return Builder.createApply(Loc, FRI, CalleeSILSubstFnTy,
                               specConv.getSILResultType(), Subs, Arguments,
                               false);
  }
  // Create the logic for calling a throwing function.
  SILBasicBlock *NormalBB = Thunk->createBasicBlock();
  SILBasicBlock *ErrorBB = Thunk->createBasicBlock();
  Builder.createTryApply(Loc, FRI, CalleeSILSubstFnTy, Subs,
                         Arguments, NormalBB, ErrorBB);
  auto *ErrorVal = ErrorBB->createPHIArgument(specConv.getSILErrorType(),
                                              ValueOwnershipKind::Owned);
  Builder.setInsertionPoint(ErrorBB);
  Builder.createThrow(Loc, ErrorVal);
  SILValue ReturnValue = NormalBB->createPHIArgument(
      specConv.getSILResultType(), ValueOwnershipKind::Owned);
  Builder.setInsertionPoint(NormalBB);
  return ReturnValue;
}

// Create SIL arguments for a reabstraction thunk with lowered addresses. This
// may involve replacing indirect arguments with loads and stores. Return the
// SILArgument for the address of an indirect result, or nullptr.
//
// FIXME: Remove this if we don't need to create reabstraction thunks after
// address lowering.
SILArgument *ReabstractionThunkGenerator::convertReabstractionThunkArguments(
    SILBuilder &Builder) {
  SILFunction *Thunk = &Builder.getFunction();
  CanSILFunctionType SpecType = SpecializedFunc->getLoweredFunctionType();
  CanSILFunctionType SubstType = ReInfo.getSubstitutedType();
  auto specConv = SpecializedFunc->getConventions();
  SILFunctionConventions substConv(SubstType, M);

  assert(specConv.useLoweredAddresses());

  // ReInfo.NumIndirectResults corresponds to SubstTy's formal indirect
  // results. SpecTy may have fewer formal indirect results.
  assert(SubstType->getNumIndirectFormalResults()
         >= SpecType->getNumIndirectFormalResults());

  SILBasicBlock *EntryBB = Thunk->getEntryBlock();
  SILArgument *ReturnValueAddr = nullptr;
  auto SpecArgIter = SpecializedFunc->getArguments().begin();
  auto cloneSpecializedArgument = [&]() {
    // No change to the argument.
    SILArgument *SpecArg = *SpecArgIter++;
    SILArgument *NewArg =
        EntryBB->createFunctionArgument(SpecArg->getType(), SpecArg->getDecl());
    Arguments.push_back(NewArg);
  };
  // ReInfo.NumIndirectResults corresponds to SubstTy's formal indirect
  // results. SpecTy may have fewer formal indirect results.
  assert(SubstType->getNumIndirectFormalResults()
         >= SpecType->getNumIndirectFormalResults());
  unsigned resultIdx = 0;
  for (auto substRI : SubstType->getIndirectFormalResults()) {
    if (ReInfo.isFormalResultConverted(resultIdx++)) {
      // Convert an originally indirect to direct specialized result.
      // Store the result later.
      // FIXME: This only handles a single result! Partial specialization could
      // induce some combination of direct and indirect results.
      SILType ResultTy = substConv.getSILType(substRI);
      assert(ResultTy.isAddress());
      assert(!ReturnValueAddr);
      ReturnValueAddr = EntryBB->createFunctionArgument(ResultTy);
      continue;
    }
    // If the specialized result is already indirect, simply clone the indirect
    // result argument.
    assert((*SpecArgIter)->getType().isAddress());
    cloneSpecializedArgument();
  }
  assert(SpecArgIter
         == SpecializedFunc->getArgumentsWithoutIndirectResults().begin());
  unsigned numParams = SpecType->getNumParameters();
  assert(numParams == SubstType->getNumParameters());
  for (unsigned paramIdx = 0; paramIdx < numParams; ++paramIdx) {
    if (ReInfo.isParamConverted(paramIdx)) {
      // Convert an originally indirect to direct specialized parameter.
      assert(!specConv.isSILIndirect(SpecType->getParameters()[paramIdx]));
      // Instead of passing the address, pass the loaded value.
      SILType ParamTy =
          substConv.getSILType(SubstType->getParameters()[paramIdx]);
      assert(ParamTy.isAddress());
      SILArgument *SpecArg = *SpecArgIter++;
      SILArgument *NewArg =
          EntryBB->createFunctionArgument(ParamTy, SpecArg->getDecl());
      auto *ArgVal =
          Builder.createLoad(Loc, NewArg, LoadOwnershipQualifier::Unqualified);
      Arguments.push_back(ArgVal);
      continue;
    }
    // Simply clone unconverted direct or indirect parameters.
    cloneSpecializedArgument();
  }
  assert(SpecArgIter == SpecializedFunc->getArguments().end());
  return ReturnValueAddr;
}

void swift::trySpecializeApplyOfGeneric(
    ApplySite Apply, DeadInstructionSet &DeadApplies,
    llvm::SmallVectorImpl<SILFunction *> &NewFunctions) {
  assert(Apply.hasSubstitutions() && "Expected an apply with substitutions!");

  auto *F = Apply.getInstruction()->getFunction();
  auto *RefF = cast<FunctionRefInst>(Apply.getCallee())->getReferencedFunction();

  DEBUG(llvm::dbgs() << "\n\n*** ApplyInst:\n";
        Apply.getInstruction()->dumpInContext());

  // If the caller is fragile but the callee is not, bail out.
  // Specializations have shared linkage, which means they do
  // not have an external entry point, Since the callee is not
  // fragile we cannot serialize the body of the specialized
  // callee either.
  if (F->isFragile() && !RefF->hasValidLinkageForFragileInline())
      return;

  if (shouldNotSpecializeCallee(RefF))
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
    DEBUG(llvm::dbgs() << "Created specialized function: "
                       << SpecializedF->getName() << "\n"
                       << "Specialized function type: "
                       << SpecializedF->getLoweredFunctionType() << "\n");
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
    SILFunction *Thunk =
        ReabstractionThunkGenerator(ReInfo, PAI, SpecializedF).createThunk();
    NewFunctions.push_back(Thunk);
    auto *FRI = Builder.createFunctionRef(PAI->getLoc(), Thunk);
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : PAI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    auto Subs = ReInfo.getCallerParamSubstitutions();
    auto CalleeSubstFnTy = getCalleeSubstFunctionType(FRI, Subs);
    auto CalleeSILSubstFnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);
    auto *NewPAI = Builder.createPartialApply(PAI->getLoc(), FRI,
                                              CalleeSILSubstFnTy, Subs,
                                              Arguments, PAI->getType());
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
    return M.findFunction(FunctionName, SILLinkage::PublicExternal);

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


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

// Max depth of a bound generic which can be processed by the generic
// specializer.
// E.g. the depth of Array<Array<Array<T>>> is 3.
// No specializations will be produced, if any of generic parameters contains
// a bound generic type with the depth higher than this threshold 
static const unsigned BoundGenericDepthThreshold = 50;

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

// =============================================================================
// ReabstractionInfo
// =============================================================================

// Initialize SpecializedType iff the specialization is allowed.
ReabstractionInfo::ReabstractionInfo(ApplySite Apply, SILFunction *OrigF,
                                     SubstitutionList ParamSubs) {
  if (!OrigF->shouldOptimize()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize function " << OrigF->getName()
                       << " marked to be excluded from optimizations.\n");
    return;
  }

  OriginalF = OrigF;
  OriginalParamSubs = ParamSubs;
  ClonerParamSubs = ParamSubs;
  CallerParamSubs = ParamSubs;
  SpecializedGenericSig = nullptr;
  SpecializedGenericEnv = nullptr;

  SubstitutionMap InterfaceSubs;
  if (OrigF->getLoweredFunctionType()->getGenericSignature())
    InterfaceSubs = OrigF->getLoweredFunctionType()->getGenericSignature()
      ->getSubstitutionMap(ParamSubs);

  // We do not support partial specialization.
  if (InterfaceSubs.hasArchetypes()) {
    DEBUG(llvm::dbgs() <<
          "    Cannot specialize with unbound interface substitutions.\n");
    DEBUG(for (auto Sub : ParamSubs) {
            Sub.dump();
          });
    return;
  }
  if (InterfaceSubs.hasDynamicSelf()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize with dynamic self.\n");
    return;
  }

  // Check if the substitution contains any generic types that are too deep.
  // If this is the case, bail to avoid the explosion in the number of 
  // generated specializations.
  for (auto Sub : ParamSubs) {
    auto Replacement = Sub.getReplacement();
    if (Replacement.findIf([](Type ty) -> bool {
          return getBoundGenericDepth(ty) >= BoundGenericDepthThreshold;
        })) {
      return;
    }
  }

  SILModule &M = OrigF->getModule();
  SubstitutedType = SILType::substFuncType(M, InterfaceSubs,
                                           OrigF->getLoweredFunctionType(),
                                           /*dropGenerics = */ true);

  NumFormalIndirectResults = SubstitutedType->getNumIndirectFormalResults();
  Conversions.resize(NumFormalIndirectResults
                     + SubstitutedType->getParameters().size());
  if (SubstitutedType->getNumDirectFormalResults() == 0) {
    // The original function has no direct result yet. Try to convert the first
    // indirect result to a direct result.
    // TODO: We could also convert multiple indirect results by returning a
    // tuple type and created tuple_extract instructions at the call site.
    SILFunctionConventions substConv(SubstitutedType, M);
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
  // The Conversions index domain is
  // [0..<NumFormalIndirectResults + NumParameters]. This is *not* the same as
  // a SubstitutedType's SIL argument index.
  unsigned IdxForParam = NumFormalIndirectResults;
  for (SILParameterInfo PI : SubstitutedType->getParameters()) {
    if (PI.getSILStorageType().isLoadable(M)
        && PI.getConvention() == ParameterConvention::Indirect_In) {
      Conversions.set(IdxForParam);
    }
    ++IdxForParam;
  }
  SpecializedType = createSpecializedType(SubstitutedType, M);
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
  // to the converions selected above.
  SpecializedType = createSpecializedType(SubstitutedType, M);
}

/// Create a new substituted type with the updated signature.
CanSILFunctionType
ReabstractionInfo::createSubstitutedType(SILFunction *OrigF,
                                         const SubstitutionMap &SubstMap,
                                         bool HasUnboundGenericParams) {
  auto &M = OrigF->getModule();
  auto OrigFnTy = OrigF->getLoweredFunctionType();

  // First substitute concrete types into the existing function type.
  auto FnTy = OrigFnTy->substGenericArgs(
      M, QuerySubstitutionMap{SubstMap},
        LookUpConformanceInSubstitutionMap(SubstMap));

  if ((SpecializedGenericSig &&
       SpecializedGenericSig->areAllParamsConcrete()) ||
      !HasUnboundGenericParams) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
  }

  // Use the new specialized generic signature.
  auto NewFnTy = SILFunctionType::get(
      SpecializedGenericSig, FnTy->getExtInfo(), FnTy->getCalleeConvention(),
      FnTy->getParameters(), FnTy->getResults(),
      FnTy->getOptionalErrorResult(), M.getASTContext());

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

  RequirementSource Source(RequirementSource::Explicit, SourceLoc());
  // For each substitution with a concrete type as a replacement,
  // add a new concrete type equality requirement.
  for (auto &Req : Requirements) {
    Builder.addRequirement(Req, Source);
  }

  Builder.finalize(SourceLoc(), OrigGenSig->getGenericParams());
  auto *GenericSig = Builder.getGenericSignature();
  // Remember the new generic environment.
  auto *GenericEnv = GenericSig->createGenericEnvironment(*M.getSwiftModule());

  return std::make_pair(GenericEnv, GenericSig);
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
  if (!OrigF->shouldOptimize()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize function " << OrigF->getName()
                       << " marked to be excluded from optimizations.\n");
    return;
  }

  // Perform some sanity checks for the requirements
  checkSpecializationRequirements(Requirements);

  OriginalF = OrigF;
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

  std::string Old;

  if (ReInfo.isPartialSpecialization()) {
    Mangle::Mangler Mangler;
    PartialSpecializationMangler OldGenericMangler(Mangler, GenericFunc, FnTy,
                                                   Fragile);
    OldGenericMangler.mangle();
    Old = Mangler.finalize();
  } else {
    Mangle::Mangler Mangler;
    GenericSpecializationMangler OldGenericMangler(Mangler, GenericFunc,
                                                   ParamSubs, Fragile);
    OldGenericMangler.mangle();
    Old = Mangler.finalize();
  }

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

// Create a new apply based on an old one, but with a different
// function being applied.
static ApplySite replaceWithSpecializedCallee(ApplySite AI,
                                              SILValue Callee,
                                              SILBuilder &Builder,
                                              const ReabstractionInfo &ReInfo) {
  SILLocation Loc = AI.getLoc();
  SmallVector<SILValue, 4> Arguments;
  SILValue StoreResultTo;
  /// SIL function conventions for the original apply site with substitutions.
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

  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    SILBasicBlock *ResultBB = TAI->getNormalBB();
    assert(ResultBB->getSinglePredecessorBlock() == TAI->getParent());
    auto *NewTAI =
      Builder.createTryApply(Loc, Callee, Callee->getType(), {},
                             Arguments, ResultBB, TAI->getErrorBB());
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
    auto *NewAI = Builder.createApply(Loc, Callee, Arguments, A->isNonThrowing());
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
    CanSILFunctionType NewPAType =
      ReInfo.createSpecializedType(PAI->getFunctionType(), Builder.getModule());
    SILType PTy = SILType::getPrimitiveObjectType(ReInfo.getSpecializedType());
    auto *NewPAI =
      Builder.createPartialApply(Loc, Callee, PTy, {}, Arguments,
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
      Mangle::Mangler M;
      GenericSpecializationMangler OldMangler(
          M, OrigF, ReInfo.getOriginalParamSubstitutions(), Fragile,
          GenericSpecializationMangler::NotReabstracted);
      OldMangler.mangle();
      std::string Old = M.finalize();

      NewMangling::GenericSpecializationMangler NewMangler(
        OrigF, ReInfo.getOriginalParamSubstitutions(), Fragile,
          /*isReAbstracted*/ false);

      std::string New = NewMangler.mangle();
      ThunkName = NewMangling::selectMangling(Old, New);
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
  auto specConv = SpecializedFunc->getConventions();
  if (!SpecializedFunc->getLoweredFunctionType()->hasErrorResult()) {
    return Builder.createApply(Loc, FRI, SpecializedFunc->getLoweredType(),
                               specConv.getSILResultType(), {}, Arguments,
                               false);
  }
  // Create the logic for calling a throwing function.
  SILBasicBlock *NormalBB = Thunk->createBasicBlock();
  SILBasicBlock *ErrorBB = Thunk->createBasicBlock();
  Builder.createTryApply(Loc, FRI, SpecializedFunc->getLoweredType(), {},
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

  // ReInfo.NumIndirectResults correponds to SubstTy's formal indirect
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
  // ReInfo.NumIndirectResults correponds to SubstTy's formal indirect
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

  DEBUG(llvm::dbgs() << "  ApplyInst:\n";
        Apply.getInstruction()->dumpInContext());

  // If the caller is fragile but the callee is not, bail out.
  // Specializations have shared linkage, which means they do
  // not have an external entry point, Since the callee is not
  // fragile we cannot serialize the body of the specialized
  // callee either.
  if (F->isFragile() && !RefF->hasValidLinkageForFragileInline())
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


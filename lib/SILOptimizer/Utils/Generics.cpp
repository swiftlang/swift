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

#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/Basic/Statistic.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/OptimizationRemark.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/GenericCloner.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/Strings.h"

using namespace swift;

STATISTIC(NumPreventedGenericSpecializationLoops,
          "# of prevented infinite generic specializations loops");

STATISTIC(NumPreventedTooComplexGenericSpecializations,
          "# of prevented generic specializations with too complex "
          "generic type parameters");

/// Set to true to enable the support for partial specialization.
llvm::cl::opt<bool> EnablePartialSpecialization(
    "sil-partial-specialization", llvm::cl::init(false),
    llvm::cl::desc("Enable partial specialization of generics"));

/// If set, then generic specialization tries to specialize using
/// all substitutions, even if they the replacement types are generic.
llvm::cl::opt<bool> SupportGenericSubstitutions(
    "sil-partial-specialization-with-generic-substitutions",
    llvm::cl::init(false),
    llvm::cl::desc("Enable partial specialization with generic substitutions"));

/// Set to true to print detected infinite generic specialization loops that
/// were prevented.
llvm::cl::opt<bool> PrintGenericSpecializationLoops(
    "sil-print-generic-specialization-loops", llvm::cl::init(false),
    llvm::cl::desc("Print detected infinite generic specialization loops that "
                   "were prevented"));

static bool OptimizeGenericSubstitutions = false;

/// Max depth of a type which can be processed by the generic
/// specializer.
/// E.g. the depth of Array<Array<Array<T>>> is 3.
/// No specializations will be produced, if any of generic parameters contains
/// a bound generic type with the depth higher than this threshold
static const unsigned TypeDepthThreshold = 50;
/// Set the width threshold rather high, because some projects uses very wide
/// tuples to model fixed size arrays.
static const unsigned TypeWidthThreshold = 2000;

/// Compute the width and the depth of a type.
/// We compute both, because some pathological test-cases result in very
/// wide types and some others result in very deep types. It is important
/// to bail as soon as we hit the threshold on any of both dimensions to
/// prevent compiler hangs and crashes.
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
    auto Params = FnTy->getParams();
    Width += Params.size();
    for (auto &Param : Params) {
      unsigned TypeWidth;
      unsigned TypeDepth;
      std::tie(TypeDepth, TypeWidth) = getTypeDepthAndWidth(Param.getOldType());
      if (TypeDepth > MaxTypeDepth)
        MaxTypeDepth = TypeDepth;
      Width += TypeWidth;
    }
    unsigned TypeWidth;
    unsigned TypeDepth;
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

namespace {

/// A helper class used to check whether one type is structurally contained
/// the other type either completely or partially.
class TypeComparator : public TypeMatcher<TypeComparator> {
  bool IsContained = false;

public:
  bool isEqual(CanType T1, CanType T2) { return T1 == T2; }
  /// Check whether the type T1 is different from T2 and contained in the type
  /// T2.
  bool isStrictlyContainedIn(CanType T1, CanType T2) {
    if (isEqual(T1, T2))
      return false;
    return T2.findIf([&T1, this](Type T) -> bool {
      return isEqual(T->getCanonicalType(), T1);
    });
  }

  /// Check whether the type T1 is strictly or partially contained in the type
  /// T2.
  /// Partially contained means that if you drop the common structural "prefix"
  /// of T1 and T2 and get T1' and T2' then T1' is strictly contained in T2'.
  bool isPartiallyContainedIn(CanType T1, CanType T2) {
    if (isStrictlyContainedIn(T1, T2))
      return true;
    match(T1, T2);
    return IsContained;
  }

  /// This method is invoked aftre skipping a common prefix of two types,
  /// when a structural difference is found.
  bool mismatch(TypeBase *firstType, TypeBase *secondType,
                Type sugaredFirstType) {
    auto firstCanType = firstType->getCanonicalType();
    auto secondCanType = secondType->getCanonicalType();
    if (isEqual(firstCanType, secondCanType))
      return false;
    if (isStrictlyContainedIn(firstCanType, secondCanType)) {
      IsContained = true;
      return false;
    }
    return false;
  }
};

} // anonymous namespace

/// Checks if a second substitution map is an expanded version of
/// the first substitution map.
/// This is the case if at least one of the substitution type in Subs2 is
/// "bigger" than the corresponding substitution type in Subs1.
/// Type T2 is "smaller" than type T1 if T2 is structurally contained in T1.
static bool growingSubstitutions(SubstitutionMap Subs1,
                                 SubstitutionMap Subs2) {
  auto Replacements1 = Subs1.getReplacementTypes();
  auto Replacements2 = Subs2.getReplacementTypes();
  assert(Replacements1.size() == Replacements2.size());
  TypeComparator TypeCmp;
  // Perform component-wise comparisions for substitutions.
  for (unsigned idx : indices(Replacements1)) {
    auto Type1 = Replacements1[idx]->getCanonicalType();
    auto Type2 = Replacements2[idx]->getCanonicalType();
    // If types are the same, the substitution type does not grow.
    if (TypeCmp.isEqual(Type2, Type1))
      continue;
    // If the new substitution type is getting smaller, the
    // substitution type does not grow.
    if (TypeCmp.isPartiallyContainedIn(Type2, Type1))
      continue;
    if (TypeCmp.isPartiallyContainedIn(Type1, Type2)) {
      LLVM_DEBUG(llvm::dbgs() << "Type:\n"; Type1.dump();
                 llvm::dbgs() << "is (partially) contained in type:\n";
                 Type2.dump();
                 llvm::dbgs() << "Replacements[" << idx
                              << "] has got bigger since last time.\n");
      return true;
    }
    // None of the types is contained in the other type.
    // They are not comparable in this sense.
  }

  // The substitition list is not growing.
  return false;
}

/// Checks whether specializing a given generic apply would create an infinite
/// cycle in the generic specializations graph. This can be the case if there is
/// a loop in the specialization graph and generic parameters at each iteration
/// of such a loop are getting bigger and bigger.
/// The specialization graph is represented by means of SpecializationInformation.
/// We use this meta-information about specializations to detect cycles in this
/// graph.
static bool createsInfiniteSpecializationLoop(ApplySite Apply) {
  if (!Apply)
    return false;
  auto *Callee = Apply.getCalleeFunction();
  SILFunction *Caller = nullptr;
  Caller = Apply.getFunction();
  int numAcceptedCycles = 1;

  // Name of the function to be specialized.
  auto GenericFunc = Callee;

  LLVM_DEBUG(llvm::dbgs() << "\n\n\nChecking for a specialization cycle:\n"
                          << "Caller: " << Caller->getName() << "\n"
                          << "Callee: " << Callee->getName() << "\n";
             llvm::dbgs() << "Substitutions:\n";
             Apply.getSubstitutionMap().dump(llvm::dbgs()));

  auto *CurSpecializationInfo = Apply.getSpecializationInfo();
  if (CurSpecializationInfo) {
    LLVM_DEBUG(llvm::dbgs() << "Scan call-site's history\n");
  } else if (Caller->isSpecialization()) {
    CurSpecializationInfo = Caller->getSpecializationInfo();
    LLVM_DEBUG(llvm::dbgs() << "Scan caller's specialization history\n");
  }

  while (CurSpecializationInfo) {
    LLVM_DEBUG(llvm::dbgs() << "Current caller is a specialization:\n"
                            << "Caller: "
                            << CurSpecializationInfo->getCaller()->getName()
                            << "\n"
                            << "Parent: "
                            << CurSpecializationInfo->getParent()->getName()
                            << "\n";
               llvm::dbgs() << "Substitutions:\n";
               for (auto Replacement :
                     CurSpecializationInfo->getSubstitutions()
                       .getReplacementTypes()) {
                 Replacement->dump();
               });

    if (CurSpecializationInfo->getParent() == GenericFunc) {
      LLVM_DEBUG(llvm::dbgs() << "Found a call graph loop, checking "
                                 "substitutions\n");
      // Consider if components of the substitution list gets bigger compared to
      // the previously seen specialization of the same generic function.
      if (growingSubstitutions(CurSpecializationInfo->getSubstitutions(),
                               Apply.getSubstitutionMap())) {
        LLVM_DEBUG(llvm::dbgs() << "Found a generic specialization loop!\n");

        // Accept a cycles up to a limit. This is necessary to generate
        // efficient code for some library functions, like compactMap, which
        // contain small specialization cycles.
        if (numAcceptedCycles == 0)
          return true;
        numAcceptedCycles--;
      }
    }

    // Get the next element of the specialization history.
    auto *CurCaller = CurSpecializationInfo->getCaller();
    CurSpecializationInfo = nullptr;
    if (!CurCaller)
      break;
    LLVM_DEBUG(llvm::dbgs() << "\nCurrent caller is: " << CurCaller->getName()
                            << "\n");
    if (!CurCaller->isSpecialization())
      break;
    CurSpecializationInfo = CurCaller->getSpecializationInfo();
  }

  assert(!CurSpecializationInfo);
  LLVM_DEBUG(llvm::dbgs() << "Stop the scan: Current caller is not a "
                             "specialization\n");
  return false;
}

// =============================================================================
// ReabstractionInfo
// =============================================================================

static bool shouldNotSpecializeCallee(SILFunction *Callee,
                                      SubstitutionMap Subs = {}) {
  if (Callee->hasSemanticsAttr("optimize.sil.specialize.generic.never"))
    return true;

  if (Subs.hasAnySubstitutableParams() &&
      Callee->hasSemanticsAttr("optimize.sil.specialize.generic.partial.never"))
    return true;

  return false;
}

/// Prepares the ReabstractionInfo object for further processing and checks
/// if the current function can be specialized at all.
/// Returns false, if the current function cannot be specialized.
/// Returns true otherwise.
bool ReabstractionInfo::prepareAndCheck(ApplySite Apply, SILFunction *Callee,
                                        SubstitutionMap ParamSubs,
                                        OptRemark::Emitter *ORE) {
  if (shouldNotSpecializeCallee(Callee))
    return false;

  SpecializedGenericEnv = nullptr;
  SpecializedGenericSig = nullptr;
  auto CalleeGenericSig = Callee->getLoweredFunctionType()->getGenericSignature();
  auto CalleeGenericEnv = Callee->getGenericEnvironment();

  this->Callee = Callee;
  this->Apply = Apply;

  // Get the original substitution map.
  CalleeParamSubMap = ParamSubs;

  using namespace OptRemark;
  // We do not support partial specialization.
  if (!EnablePartialSpecialization && CalleeParamSubMap.hasArchetypes()) {
    LLVM_DEBUG(llvm::dbgs() <<"    Partial specialization is not supported.\n");
    LLVM_DEBUG(ParamSubs.dump(llvm::dbgs()));
    return false;
  }

  // Perform some checks to see if we need to bail.
  if (CalleeParamSubMap.hasDynamicSelf()) {
    REMARK_OR_DEBUG(ORE, [&]() {
      return RemarkMissed("DynamicSelf", *Apply.getInstruction())
             << IndentDebug(4) << "Cannot specialize with dynamic self";
    });
    return false;
  }

  // Check if the substitution contains any generic types that are too deep.
  // If this is the case, bail to avoid the explosion in the number of 
  // generated specializations.
  for (auto Replacement : ParamSubs.getReplacementTypes()) {
    if (isTypeTooComplex(Replacement)) {
      REMARK_OR_DEBUG(ORE, [&]() {
        return RemarkMissed("TypeTooDeep", *Apply.getInstruction())
               << IndentDebug(4)
               << "Cannot specialize because the generic type is too deep";
      });
      NumPreventedTooComplexGenericSpecializations++;
      return false;
    }
  }

  // Check if we have substitutions which replace generic type parameters with
  // concrete types or unbound generic types.
  bool HasConcreteGenericParams = false;
  bool HasNonArchetypeGenericParams = false;
  HasUnboundGenericParams = false;

  CalleeGenericSig->forEachParam([&](GenericTypeParamType *GP, bool Canonical) {
    if (!Canonical)
      return;

    // Check only the substitutions for the generic parameters.
    // Ignore any dependent types, etc.
    auto Replacement = Type(GP).subst(CalleeParamSubMap);
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
    } else {
      HasConcreteGenericParams = true;
    }
  });

  if (HasUnboundGenericParams) {
    // Bail if we cannot specialize generic substitutions, but all substitutions
    // were generic.
    if (!HasConcreteGenericParams && !SupportGenericSubstitutions) {
      LLVM_DEBUG(llvm::dbgs() << "    Partial specialization is not supported "
                            "if all substitutions are generic.\n");
      LLVM_DEBUG(ParamSubs.dump(llvm::dbgs()));
      return false;
    }

    if (!HasNonArchetypeGenericParams && !HasConcreteGenericParams) {
      LLVM_DEBUG(llvm::dbgs() << "    Partial specialization is not supported "
                            "if all substitutions are archetypes.\n");
      LLVM_DEBUG(ParamSubs.dump(llvm::dbgs()));
      return false;
    }

    // We need a generic environment for the partial specialization.
    if (!CalleeGenericEnv)
      return false;

    // Bail if the callee should not be partially specialized.
    if (shouldNotSpecializeCallee(Callee, ParamSubs))
      return false;
  }

  // Check if specializing this call site would create in an infinite generic
  // specialization loop.
  if (createsInfiniteSpecializationLoop(Apply)) {
    REMARK_OR_DEBUG(ORE, [&]() {
      return RemarkMissed("SpecializationLoop", *Apply.getInstruction())
             << IndentDebug(4)
             << "Generic specialization is not supported if it would result in "
                "a generic specialization of infinite depth. Callee "
             << NV("Callee", Callee)
             << " occurs multiple times on the call chain";
    });
    if (PrintGenericSpecializationLoops)
      llvm::errs() << "Detected and prevented an infinite "
                      "generic specialization loop for callee: "
                   << Callee->getName() << '\n';
    NumPreventedGenericSpecializationLoops++;
    return false;
  }

  return true;
}

bool ReabstractionInfo::canBeSpecialized(ApplySite Apply, SILFunction *Callee,
                                         SubstitutionMap ParamSubs) {
  ReabstractionInfo ReInfo;
  return ReInfo.prepareAndCheck(Apply, Callee, ParamSubs);
}

ReabstractionInfo::ReabstractionInfo(ApplySite Apply, SILFunction *Callee,
                                     SubstitutionMap ParamSubs,
                                     bool ConvertIndirectToDirect,
                                     OptRemark::Emitter *ORE) {
  if (!prepareAndCheck(Apply, Callee, ParamSubs, ORE))
    return;

  this->ConvertIndirectToDirect = ConvertIndirectToDirect;

  SILFunction *Caller = nullptr;
  if (Apply)
    Caller = Apply.getFunction();

  if (!EnablePartialSpecialization || !HasUnboundGenericParams) {
    // Fast path for full specializations.
    performFullSpecializationPreparation(Callee, ParamSubs);
  } else {
    performPartialSpecializationPreparation(Caller, Callee, ParamSubs);
  }
  verify();
  if (SpecializedGenericSig) {
    LLVM_DEBUG(llvm::dbgs() << "\n\nPartially specialized types for function: "
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
      !getCallerParamSubstitutionMap().empty()) {
    auto CalleeFnTy = Callee->getLoweredFunctionType();
    assert(CalleeFnTy->isPolymorphic());
    auto CalleeSubstFnTy = CalleeFnTy->substGenericArgs(
        Callee->getModule(), getCalleeParamSubstitutionMap());
    assert(!CalleeSubstFnTy->isPolymorphic() &&
           "Substituted callee type should not be polymorphic");
    assert(!CalleeSubstFnTy->hasTypeParameter() &&
           "Substituted callee type should not have type parameters");

    SpecializedSubstFnTy = SpecializedFnTy->substGenericArgs(
        Callee->getModule(), getCallerParamSubstitutionMap());

    assert(!SpecializedSubstFnTy->isPolymorphic() &&
           "Substituted callee type should not be polymorphic");
    assert(!SpecializedSubstFnTy->hasTypeParameter() &&
           "Substituted callee type should not have type parameters");

    auto SpecializedCalleeSubstFnTy =
        createSpecializedType(CalleeSubstFnTy, Callee->getModule());

    if (SpecializedSubstFnTy != SpecializedCalleeSubstFnTy) {
      llvm::dbgs() << "SpecializedFnTy:\n" << SpecializedFnTy << "\n";
      llvm::dbgs() << "SpecializedSubstFnTy:\n" << SpecializedSubstFnTy << "\n";
      getCallerParamSubstitutionMap().getCanonical().dump(llvm::dbgs());
      llvm::dbgs() << "\n\n";

      llvm::dbgs() << "CalleeFnTy:\n" << CalleeFnTy << "\n";
      llvm::dbgs() << "SpecializedCalleeSubstFnTy:\n" << SpecializedCalleeSubstFnTy << "\n";
      ParamSubs.getCanonical().dump(llvm::dbgs());
      llvm::dbgs() << "\n\n";
      assert(SpecializedSubstFnTy == SpecializedCalleeSubstFnTy &&
             "Substituted function types should be the same");
    }
  }

  // If the new type is the same, there is nothing to do and
  // no specialization should be performed.
  if (getSubstitutedType() == Callee->getLoweredFunctionType()) {
    LLVM_DEBUG(llvm::dbgs() << "The new specialized type is the same as "
                               "the original type. Don't specialize!\n";
               llvm::dbgs() << "The type is: " << getSubstitutedType() << "\n");
    SpecializedType = CanSILFunctionType();
    SubstitutedType = CanSILFunctionType();
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
    return;
  }

  if (SpecializedGenericSig) {
    // It is a partial specialization.
    LLVM_DEBUG(llvm::dbgs() << "Specializing the call:\n";
               Apply.getInstruction()->dumpInContext();
               llvm::dbgs() << "\n\nPartially specialized types for function: "
                            << Callee->getName() << "\n\n";
               llvm::dbgs() << "Callee generic function type:\n"
                            << Callee->getLoweredFunctionType() << "\n\n";
               llvm::dbgs() << "Callee's call substitution:\n";
               getCalleeParamSubstitutionMap().getCanonical().dump(llvm::dbgs());

               llvm::dbgs() << "Partially specialized generic function type:\n"
                            << getSpecializedType() << "\n\n";
               llvm::dbgs() << "\nSpecialization call substitution:\n";
               getCallerParamSubstitutionMap().getCanonical().dump(llvm::dbgs());
               );
  }
}

bool ReabstractionInfo::canBeSpecialized() const {
  return getSpecializedType();
}

bool ReabstractionInfo::isFullSpecialization() const {
  return !getCalleeParamSubstitutionMap().hasArchetypes();
}

bool ReabstractionInfo::isPartialSpecialization() const {
  return getCalleeParamSubstitutionMap().hasArchetypes();
}

void ReabstractionInfo::createSubstitutedAndSpecializedTypes() {
  auto &M = Callee->getModule();

  // Find out how the function type looks like after applying the provided
  // substitutions.
  if (!SubstitutedType) {
    SubstitutedType = createSubstitutedType(Callee, CallerInterfaceSubs,
                                            HasUnboundGenericParams);
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
      if (substConv.getSILType(RI).isLoadable(M) && !RI.getType()->isVoid() &&
          shouldExpand(M, substConv.getSILType(RI).getObjectType())) {
        Conversions.set(IdxForResult);
        break;
      }
      ++IdxForResult;
    }
  }

  // Try to convert indirect incoming parameters to direct parameters.
  unsigned IdxForParam = NumFormalIndirectResults;
  for (SILParameterInfo PI : SubstitutedType->getParameters()) {
    auto IdxToInsert = IdxForParam;
    ++IdxForParam;
    if (!substConv.getSILType(PI).isLoadable(M)) {
      continue;
    }

    switch (PI.getConvention()) {
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_In_Guaranteed:
      Conversions.set(IdxToInsert);
      break;
    case ParameterConvention::Indirect_In_Constant:
    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_InoutAliasable:
    case ParameterConvention::Direct_Owned:
    case ParameterConvention::Direct_Unowned:
    case ParameterConvention::Direct_Guaranteed:
      break;
    }
  }

  // Produce a specialized type, which is the substituted type with
  // the parameters/results passing conventions adjusted according
  // to the conversions selected above.
  SpecializedType = createSpecializedType(SubstitutedType, M);
}

/// Create a new substituted type with the updated signature.
CanSILFunctionType
ReabstractionInfo::createSubstitutedType(SILFunction *OrigF,
                                         SubstitutionMap SubstMap,
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
    if (CanSpecializedGenericSig)
      FnTy = cast<SILFunctionType>(
          CanSpecializedGenericSig->getCanonicalTypeInContext(FnTy));
    else {
      FnTy = cast<SILFunctionType>(FnTy->getCanonicalType());
      assert(!FnTy->hasTypeParameter() && "Type parameters outside generic context?");
    }
  }
  assert(FnTy);

  // Use the new specialized generic signature.
  auto NewFnTy = SILFunctionType::get(
      CanSpecializedGenericSig, FnTy->getExtInfo(),
      FnTy->getCoroutineKind(), FnTy->getCalleeConvention(),
      FnTy->getParameters(), FnTy->getYields(),
      FnTy->getResults(), FnTy->getOptionalErrorResult(),
      M.getASTContext(), FnTy->getWitnessMethodConformanceOrNone());

  // This is an interface type. It should not have any archetypes.
  assert(!NewFnTy->hasArchetype());
  return NewFnTy;
}

/// Convert the substituted function type into a specialized function type based
/// on the ReabstractionInfo.
CanSILFunctionType ReabstractionInfo::
createSpecializedType(CanSILFunctionType SubstFTy, SILModule &M) const {
  llvm::SmallVector<SILResultInfo, 8> SpecializedResults;
  llvm::SmallVector<SILYieldInfo, 8> SpecializedYields;
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
    if (!isParamConverted(ParamIdx++)) {
      // No conversion: re-use the original, substituted parameter info.
      SpecializedParams.push_back(PI);
      continue;
    }

    // Convert the indirect parameter to a direct parameter.
    SILType SILParamTy = SILType::getPrimitiveObjectType(PI.getType());
    // Indirect parameters are passed as owned/guaranteed, so we also
    // need to pass the direct/guaranteed parameter as
    // owned/guaranteed (except it's a trivial type).
    auto C = ParameterConvention::Direct_Unowned;
    if (!SILParamTy.isTrivial(M)) {
      if (PI.isGuaranteed()) {
        C = ParameterConvention::Direct_Guaranteed;
      } else {
        C = ParameterConvention::Direct_Owned;
      }
    }
    SpecializedParams.push_back(SILParameterInfo(PI.getType(), C));
  }
  for (SILYieldInfo YI : SubstFTy->getYields()) {
    // For now, always just use the original, substituted parameter info.
    SpecializedYields.push_back(YI);
  }
  return SILFunctionType::get(
      SubstFTy->getGenericSignature(),
      SubstFTy->getExtInfo(), SubstFTy->getCoroutineKind(),
      SubstFTy->getCalleeConvention(),
      SpecializedParams, SpecializedYields, SpecializedResults,
      SubstFTy->getOptionalErrorResult(), M.getASTContext(),
      SubstFTy->getWitnessMethodConformanceOrNone());
}

/// Create a new generic signature from an existing one by adding
/// additional requirements.
static std::pair<GenericEnvironment *, GenericSignature *>
getGenericEnvironmentAndSignatureWithRequirements(
    GenericSignature *OrigGenSig, GenericEnvironment *OrigGenericEnv,
    ArrayRef<Requirement> Requirements, SILModule &M) {
  // Form a new generic signature based on the old one.
  GenericSignatureBuilder Builder(M.getASTContext());

  // First, add the old generic signature.
  Builder.addGenericSignature(OrigGenSig);

  auto Source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  // For each substitution with a concrete type as a replacement,
  // add a new concrete type equality requirement.
  for (auto &Req : Requirements) {
    Builder.addRequirement(Req, Source, M.getSwiftModule());
  }

  auto NewGenSig =
    std::move(Builder).computeGenericSignature(
                                   SourceLoc(),
                                   /*allowConcreteGenericParams=*/true);
  auto NewGenEnv = NewGenSig->createGenericEnvironment();
  return { NewGenEnv, NewGenSig };
}

/// This is a fast path for full specializations.
/// There is no need to form a new generic signature in such cases,
/// because the specialized function will be non-generic.
void ReabstractionInfo::performFullSpecializationPreparation(
    SILFunction *Callee, SubstitutionMap ParamSubs) {
  assert((!EnablePartialSpecialization || !HasUnboundGenericParams) &&
         "Only full specializations are handled here");

  SILModule &M = Callee->getModule();

  this->Callee = Callee;

  // Get the original substitution map.
  ClonerParamSubMap = ParamSubs;

  SubstitutedType = Callee->getLoweredFunctionType()->substGenericArgs(
      M, ClonerParamSubMap);
  CallerParamSubMap = {};
  createSubstitutedAndSpecializedTypes();
}

/// If the archetype (or any of its dependent types) has requirements
/// depending on other archetypes, return true.
/// Otherwise return false.
static bool hasNonSelfContainedRequirements(ArchetypeType *Archetype,
                                            GenericSignature *Sig,
                                            GenericEnvironment *Env) {
  auto Reqs = Sig->getRequirements();
  auto CurrentGP = Archetype->getInterfaceType()
                       ->getCanonicalType()
                       ->getRootGenericParam();
  for (auto Req : Reqs) {
    switch(Req.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::Layout:
      // FIXME: Second type of a superclass requirement may contain
      // generic parameters.
      continue;
    case RequirementKind::SameType: {
      // Check if this requirement contains more than one generic param.
      // If this is the case, then these archetypes are interdependent and
      // we should return true.
      auto First = Req.getFirstType()->getCanonicalType();
      auto Second = Req.getSecondType()->getCanonicalType();
      llvm::SmallSetVector<TypeBase *, 2> UsedGenericParams;
      First.visit([&](Type Ty) {
        if (auto *GP = Ty->getAs<GenericTypeParamType>()) {
          UsedGenericParams.insert(GP);
        }
      });
      Second.visit([&](Type Ty) {
        if (auto *GP = Ty->getAs<GenericTypeParamType>()) {
          UsedGenericParams.insert(GP);
        }
      });

      if (UsedGenericParams.count(CurrentGP) && UsedGenericParams.size() > 1)
        return true;
    }
    }
  }
  return false;
}

/// Collect all requirements for a generic parameter corresponding to a given
/// archetype.
static void collectRequirements(ArchetypeType *Archetype, GenericSignature *Sig,
                                GenericEnvironment *Env,
                                SmallVectorImpl<Requirement> &CollectedReqs) {
  auto Reqs = Sig->getRequirements();
  auto CurrentGP = Archetype->getInterfaceType()
                       ->getCanonicalType()
                       ->getRootGenericParam();
  CollectedReqs.clear();
  for (auto Req : Reqs) {
    switch(Req.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::Layout:
      // If it is a generic param or something derived from it, add this
      // requirement.

      // FIXME: Second type of a superclass requirement may contain
      // generic parameters.

      if (Req.getFirstType()->getCanonicalType()->getRootGenericParam() ==
          CurrentGP)
        CollectedReqs.push_back(Req);
      continue;
    case RequirementKind::SameType: {
      // Check if this requirement contains more than one generic param.
      // If this is the case, then these archetypes are interdependent and
      // we should return true.
      auto First = Req.getFirstType()->getCanonicalType();
      auto Second = Req.getSecondType()->getCanonicalType();
      llvm::SmallSetVector<GenericTypeParamType *, 2> UsedGenericParams;
      First.visit([&](Type Ty) {
        if (auto *GP = Ty->getAs<GenericTypeParamType>()) {
          UsedGenericParams.insert(GP);
        }
      });
      Second.visit([&](Type Ty) {
        if (auto *GP = Ty->getAs<GenericTypeParamType>()) {
          UsedGenericParams.insert(GP);
        }
      });

      if (!UsedGenericParams.count(CurrentGP))
        continue;

      if (UsedGenericParams.size() != 1) {
        llvm::dbgs() << "Strange requirement for "
                     << CurrentGP->getCanonicalType() << "\n";
        Req.dump();
      }
      assert(UsedGenericParams.size() == 1);
      CollectedReqs.push_back(Req);
      continue;
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
                             GenericSignature *Sig, GenericEnvironment *Env) {
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

  // If the archetype used (or any of its dependent types) has requirements
  // depending on other caller's archetypes, then we don't want to specialize
  // on it as it may require introducing more generic parameters, which
  // is not beneficial.

  // Collect the archetypes used by the replacement type.
  llvm::SmallSetVector<ArchetypeType *, 2> UsedArchetypes;
  Replacement.visit([&](Type Ty) {
    if (auto Archetype = Ty->getAs<ArchetypeType>()) {
      UsedArchetypes.insert(Archetype->getPrimary());
    }
  });

  // Check if any of the used archetypes are non-self contained when
  // it comes to requirements.
  for (auto *UsedArchetype : UsedArchetypes) {
    if (hasNonSelfContainedRequirements(UsedArchetype, Sig, Env)) {
      LLVM_DEBUG(llvm::dbgs() << "Requirements of the archetype depend on "
                            "other caller's generic parameters! "
                            "It cannot be partially specialized:\n";
                 UsedArchetype->dump();
                 llvm::dbgs() << "This archetype is used in the substitution: "
                              << Replacement << "\n");
      return false;
    }
  }

  if (OptimizeGenericSubstitutions) {
    // Is it an unconstrained generic parameter?
    if (auto Archetype = Replacement->getAs<ArchetypeType>()) {
      if (Archetype->getConformsTo().empty()) {
        // TODO: If Replacement add a new layout constraint, then
        // it may be still useful to perform the partial specialization.
        return false;
      }
    }
  }

  return true;
}

namespace swift {

/// A helper class for creating partially specialized function signatures.
///
/// The following naming convention is used to describe the members and
/// functions:
/// Caller - the function which invokes the callee.
/// Callee - the callee to be specialized.
/// Specialized - the specialized callee which is being created.
class FunctionSignaturePartialSpecializer {
  /// Maps caller's generic parameters to generic parameters of the specialized
  /// function.
  llvm::DenseMap<SubstitutableType *, Type>
      CallerInterfaceToSpecializedInterfaceMapping;

  /// Maps callee's generic parameters to generic parameters of the specialized
  /// function.
  llvm::DenseMap<SubstitutableType *, Type>
      CalleeInterfaceToSpecializedInterfaceMapping;

  /// Maps the generic parameters of the specialized function to the caller's
  /// contextual types.
  llvm::DenseMap<SubstitutableType *, Type>
      SpecializedInterfaceToCallerArchetypeMapping;

  /// A SubstitutionMap for re-mapping caller's interface types
  /// to interface types of the specialized function.
  SubstitutionMap CallerInterfaceToSpecializedInterfaceMap;

  /// Maps callee's interface types to caller's contextual types.
  /// It is computed from the original substitutions.
  SubstitutionMap CalleeInterfaceToCallerArchetypeMap;

  /// Maps callee's interface types to specialized functions interface types.
  SubstitutionMap CalleeInterfaceToSpecializedInterfaceMap;

  /// Maps the generic parameters of the specialized function to the caller's
  /// contextual types.
  SubstitutionMap SpecializedInterfaceToCallerArchetypeMap;

  /// Generic signatures and environments for the caller, callee and
  /// the specialized function.
  GenericSignature *CallerGenericSig;
  GenericEnvironment *CallerGenericEnv;

  GenericSignature *CalleeGenericSig;
  GenericEnvironment *CalleeGenericEnv;

  GenericSignature *SpecializedGenericSig;
  GenericEnvironment *SpecializedGenericEnv;

  SILModule &M;
  ModuleDecl *SM;
  ASTContext &Ctx;

  /// This is a builder for a new partially specialized generic signature.
  GenericSignatureBuilder Builder;

  /// Set of newly created generic type parameters.
  SmallVector<GenericTypeParamType*, 4> AllGenericParams;

  /// Archetypes used in the substitutions of an apply instructions.
  /// These are the contextual archetypes of the caller function, which
  /// invokes a generic function that is being specialized.
  llvm::SmallSetVector<ArchetypeType *, 2> UsedCallerArchetypes;

  /// Number of created generic parameters so far.
  unsigned GPIdx = 0;

  void createGenericParamsForUsedCallerArchetypes();

  void createGenericParamsForCalleeGenericParams();

  void addRequirements(ArrayRef<Requirement> Reqs, SubstitutionMap &SubsMap);

  void addCallerRequirements();

  void addCalleeRequirements();

  std::pair<GenericEnvironment *, GenericSignature *>
  getSpecializedGenericEnvironmentAndSignature();

  void computeCallerInterfaceToSpecializedInterfaceMap();

  void computeCalleeInterfaceToSpecializedInterfaceMap();

  void computeSpecializedInterfaceToCallerArchetypeMap();

  /// Collect all used archetypes from all the substitutions.
  /// Take into account only those archetypes that occur in the
  /// substitutions of generic parameters which will be partially
  /// specialized. Ignore all others.
  void collectUsedCallerArchetypes(SubstitutionMap ParamSubs);

  /// Create a new generic parameter.
  GenericTypeParamType *createGenericParam();

public:
  FunctionSignaturePartialSpecializer(SILModule &M,
                                      GenericSignature *CallerGenericSig,
                                      GenericEnvironment *CallerGenericEnv,
                                      GenericSignature *CalleeGenericSig,
                                      GenericEnvironment *CalleeGenericEnv,
                                      SubstitutionMap ParamSubs)
      : CallerGenericSig(CallerGenericSig), CallerGenericEnv(CallerGenericEnv),
        CalleeGenericSig(CalleeGenericSig), CalleeGenericEnv(CalleeGenericEnv),
        M(M), SM(M.getSwiftModule()), Ctx(M.getASTContext()),
        Builder(Ctx) {
    SpecializedGenericSig = nullptr;
    SpecializedGenericEnv = nullptr;
    CalleeInterfaceToCallerArchetypeMap = ParamSubs;
  }

  /// This constructor is used by when processing @_specialize.
  /// In this case, the caller and the callee are the same function.
  FunctionSignaturePartialSpecializer(SILModule &M,
                                      GenericSignature *CalleeGenericSig,
                                      GenericEnvironment *CalleeGenericEnv,
                                      ArrayRef<Requirement> Requirements)
      : CallerGenericSig(CalleeGenericSig), CallerGenericEnv(CalleeGenericEnv),
        CalleeGenericSig(CalleeGenericSig), CalleeGenericEnv(CalleeGenericEnv),
        M(M), SM(M.getSwiftModule()), Ctx(M.getASTContext()),
        Builder(Ctx) {

    // Create the new generic signature using provided requirements.
    std::tie(SpecializedGenericEnv, SpecializedGenericSig) =
        getGenericEnvironmentAndSignatureWithRequirements(
            CalleeGenericSig, CalleeGenericEnv, Requirements, M);

    // Compute SubstitutionMaps required for re-mapping.

    // Callee's generic signature and specialized generic signature
    // use the same set of generic parameters, i.e. each generic
    // parameter should be mapped to itself.
    for (auto GP : CalleeGenericSig->getGenericParams()) {
      CalleeInterfaceToSpecializedInterfaceMapping[GP] = Type(GP);
    }
    computeCalleeInterfaceToSpecializedInterfaceMap();

    // Each generic parameter of the callee is mapped to its own
    // archetype.
    SpecializedInterfaceToCallerArchetypeMap =
      SubstitutionMap::get(
        SpecializedGenericSig,
        [&](SubstitutableType *type) -> Type {
          return CalleeGenericEnv->mapTypeIntoContext(type);
        },
        LookUpConformanceInSignature(*SpecializedGenericSig));
  }

  GenericSignature *getSpecializedGenericSignature() {
    return SpecializedGenericSig;
  }

  GenericEnvironment *getSpecializedGenericEnvironment() {
    return SpecializedGenericEnv;
  }

  void createSpecializedGenericSignature(SubstitutionMap ParamSubs);

  void createSpecializedGenericSignatureWithNonGenericSubs();

  SubstitutionMap computeClonerParamSubs();

  SubstitutionMap getCallerParamSubs();

  void computeCallerInterfaceSubs(SubstitutionMap &CallerInterfaceSubs);
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
void FunctionSignaturePartialSpecializer::collectUsedCallerArchetypes(
    SubstitutionMap ParamSubs) {
  for (auto Replacement : ParamSubs.getReplacementTypes()) {
    if (!Replacement->hasArchetype())
      continue;

    // If the substitution will not be performed in the specialized
    // function, there is no need to check for any archetypes inside
    // the replacement.
    if (!shouldBePartiallySpecialized(Replacement,
                                      CallerGenericSig, CallerGenericEnv))
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
    computeCallerInterfaceToSpecializedInterfaceMap() {
  if (!CallerGenericSig)
    return;

  CallerInterfaceToSpecializedInterfaceMap =
    SubstitutionMap::get(
      CallerGenericSig,
      [&](SubstitutableType *type) -> Type {
        return CallerInterfaceToSpecializedInterfaceMapping.lookup(type);
      },
      LookUpConformanceInSignature(*CallerGenericSig));

  LLVM_DEBUG(llvm::dbgs() << "\n\nCallerInterfaceToSpecializedInterfaceMap "
                             "map:\n";
             CallerInterfaceToSpecializedInterfaceMap.dump(llvm::dbgs()));
}

void FunctionSignaturePartialSpecializer::
    computeSpecializedInterfaceToCallerArchetypeMap() {
  // Define a substitution map for re-mapping interface types of
  // the specialized function to contextual types of the caller.
  SpecializedInterfaceToCallerArchetypeMap =
    SubstitutionMap::get(
      SpecializedGenericSig,
      [&](SubstitutableType *type) -> Type {
        LLVM_DEBUG(llvm::dbgs() << "Mapping specialized interface type to "
                                   "caller archetype:\n";
                   llvm::dbgs() << "Interface type: "; type->dump();
                   llvm::dbgs() << "Archetype: ";
                   auto Archetype =
                      SpecializedInterfaceToCallerArchetypeMapping.lookup(type);
                   if (Archetype) Archetype->dump();
                   else llvm::dbgs() << "Not found!\n";);
        return SpecializedInterfaceToCallerArchetypeMapping.lookup(type);
      },
      LookUpConformanceInSignature(*SpecializedGenericSig));
  LLVM_DEBUG(llvm::dbgs() << "\n\nSpecializedInterfaceToCallerArchetypeMap "
                             "map:\n";
             SpecializedInterfaceToCallerArchetypeMap.dump(llvm::dbgs()));
}

void FunctionSignaturePartialSpecializer::
    computeCalleeInterfaceToSpecializedInterfaceMap() {
  CalleeInterfaceToSpecializedInterfaceMap =
    SubstitutionMap::get(
      CalleeGenericSig,
      [&](SubstitutableType *type) -> Type {
        return CalleeInterfaceToSpecializedInterfaceMapping.lookup(type);
      },
      LookUpConformanceInSignature(*CalleeGenericSig));

  LLVM_DEBUG(llvm::dbgs() << "\n\nCalleeInterfaceToSpecializedInterfaceMap:\n";
             CalleeInterfaceToSpecializedInterfaceMap.dump(llvm::dbgs()));
}

/// Generate a new generic type parameter for each used archetype from
/// the caller.
void FunctionSignaturePartialSpecializer::
    createGenericParamsForUsedCallerArchetypes() {
  for (auto CallerArchetype : UsedCallerArchetypes) {
    auto CallerGenericParam = CallerArchetype->getInterfaceType();
    assert(CallerGenericParam->is<GenericTypeParamType>());

    LLVM_DEBUG(llvm::dbgs() << "\n\nChecking used caller archetype:\n";
               CallerArchetype->dump();
               llvm::dbgs() << "It corresponds to the caller generic "
                               "parameter:\n";
               CallerGenericParam->dump());

    // Create an equivalent generic parameter.
    auto SubstGenericParam = createGenericParam();
    auto SubstGenericParamCanTy = SubstGenericParam->getCanonicalType();
    (void)SubstGenericParamCanTy;

    CallerInterfaceToSpecializedInterfaceMapping
        [CallerGenericParam->getCanonicalType()
             ->castTo<GenericTypeParamType>()] = SubstGenericParam;

    SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
        CallerArchetype;

    LLVM_DEBUG(llvm::dbgs() << "\nCreated a new specialized generic "
                               "parameter:\n";
               SubstGenericParam->dump();
               llvm::dbgs() << "Created a mapping "
                               "(caller interface -> specialize interface):\n"
                            << CallerGenericParam << " -> "
                            << SubstGenericParamCanTy << "\n";
               llvm::dbgs() << "Created a mapping"
                               "(specialized interface -> caller archetype):\n"
                            << SubstGenericParamCanTy << " -> "
                            << CallerArchetype->getCanonicalType() << "\n");
  }
}

/// Create a new generic parameter for each of the callee's generic parameters
/// which requires a substitution.
void FunctionSignaturePartialSpecializer::
    createGenericParamsForCalleeGenericParams() {
  auto Source =
      GenericSignatureBuilder::FloatingRequirementSource::forAbstract();
  for (auto GP : CalleeGenericSig->getGenericParams()) {
    auto CanTy = GP->getCanonicalType();
    auto CanTyInContext =
        CalleeGenericSig->getCanonicalTypeInContext(CanTy);
    auto Replacement = CanTyInContext.subst(CalleeInterfaceToCallerArchetypeMap);
    LLVM_DEBUG(llvm::dbgs() << "\n\nChecking callee generic parameter:\n";
               CanTy->dump());
    if (!Replacement) {
      LLVM_DEBUG(llvm::dbgs() << "No replacement found. Skipping.\n");
      continue;
    }

    LLVM_DEBUG(llvm::dbgs() << "Replacement found:\n"; Replacement->dump());

    bool ShouldSpecializeGP = shouldBePartiallySpecialized(
        Replacement, CallerGenericSig, CallerGenericEnv);

    if (ShouldSpecializeGP) {
      LLVM_DEBUG(llvm::dbgs() << "Should be partially specialized.\n");
    } else {
      LLVM_DEBUG(llvm::dbgs() << "Should not be partially specialized.\n");
    }

    // Create an equivalent generic parameter in the specialized
    // generic environment.
    auto SubstGenericParam = createGenericParam();
    auto SubstGenericParamCanTy = SubstGenericParam->getCanonicalType();

    // Remember which specialized generic parameter correspond's to callee's
    // generic parameter.
    CalleeInterfaceToSpecializedInterfaceMapping[GP] = SubstGenericParam;

    LLVM_DEBUG(llvm::dbgs() << "\nCreated a new specialized generic "
                               "parameter:\n";
               SubstGenericParam->dump();
               llvm::dbgs() << "Created a mapping "
                               "(callee interface -> specialized interface):\n"
                            << CanTy << " -> "
                            << SubstGenericParamCanTy << "\n");

    if (!ShouldSpecializeGP) {
      // Remember the original substitution from the apply instruction.
      SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
          Replacement;
      LLVM_DEBUG(llvm::dbgs() << "Created a mapping (specialized interface -> "
                                 "caller archetype):\n"
                              << Type(SubstGenericParam) << " -> "
                              << Replacement << "\n");
      continue;
    }

    // Add a same type requirement based on the provided generic parameter
    // substitutions.
    auto ReplacementCallerInterfaceTy = Replacement->mapTypeOutOfContext();

    auto SpecializedReplacementCallerInterfaceTy =
        ReplacementCallerInterfaceTy.subst(
            CallerInterfaceToSpecializedInterfaceMap);
    assert(!SpecializedReplacementCallerInterfaceTy->hasError());

    Requirement Req(RequirementKind::SameType, SubstGenericParamCanTy,
                    SpecializedReplacementCallerInterfaceTy);
    Builder.addRequirement(Req, Source, SM);

    LLVM_DEBUG(llvm::dbgs() << "Added a requirement:\n"; Req.dump());

    if (ReplacementCallerInterfaceTy->is<GenericTypeParamType>()) {
      // Remember that the new generic parameter corresponds
      // to the same caller archetype, which corresponds to
      // the ReplacementCallerInterfaceTy.
      SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
          SpecializedInterfaceToCallerArchetypeMapping.lookup(
              ReplacementCallerInterfaceTy
                  .subst(CallerInterfaceToSpecializedInterfaceMap)
                  ->castTo<SubstitutableType>());
      LLVM_DEBUG(llvm::dbgs()
              << "Created a mapping (specialized interface -> "
                 "caller archetype):\n"
              << Type(SubstGenericParam) << " -> "
              << SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam]
                 ->getCanonicalType()
              << "\n");
      continue;
    }

    SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam] =
        Replacement;

    LLVM_DEBUG(llvm::dbgs()
              << "Created a mapping (specialized interface -> "
                 "caller archetype):\n"
              << Type(SubstGenericParam) << " -> "
              << SpecializedInterfaceToCallerArchetypeMapping[SubstGenericParam]
                 ->getCanonicalType()
              << "\n");
  }
}

/// Add requirements from a given list of requirements to the
/// GenericSignatureBuilder. Re-map them using the provided SubstitutionMap.
void FunctionSignaturePartialSpecializer::addRequirements(
    ArrayRef<Requirement> Reqs, SubstitutionMap &SubsMap) {
  auto source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();

  for (auto &reqReq : Reqs) {
    LLVM_DEBUG(llvm::dbgs() << "\n\nRe-mapping the requirement:\n";
               reqReq.dump());
    Builder.addRequirement(*reqReq.subst(SubsMap), source, SM);
  }
}

/// Add requirements from the caller's signature.
void FunctionSignaturePartialSpecializer::addCallerRequirements() {
  for (auto CallerArchetype : UsedCallerArchetypes) {
    // Add requirements for this caller generic parameter and its dependent
    // types.
    SmallVector<Requirement, 4> CollectedReqs;
    collectRequirements(CallerArchetype, CallerGenericSig, CallerGenericEnv,
                        CollectedReqs);
    if (!CollectedReqs.empty()) {
      LLVM_DEBUG(llvm::dbgs() << "Adding caller archetype requirements:\n";
                 for (auto Req : CollectedReqs) {
                   Req.dump();
                 }
                 CallerInterfaceToSpecializedInterfaceMap.dump(llvm::dbgs());
                );
      addRequirements(CollectedReqs, CallerInterfaceToSpecializedInterfaceMap);
    }
  }
}

/// Add requirements from the callee's signature.
void FunctionSignaturePartialSpecializer::addCalleeRequirements() {
  if (CalleeGenericSig)
    addRequirements(CalleeGenericSig->getRequirements(),
                    CalleeInterfaceToSpecializedInterfaceMap);
}

std::pair<GenericEnvironment *, GenericSignature *>
FunctionSignaturePartialSpecializer::
    getSpecializedGenericEnvironmentAndSignature() {
  if (AllGenericParams.empty())
    return { nullptr, nullptr };

  // Finalize the archetype builder.
  auto GenSig =
      std::move(Builder).computeGenericSignature(
                                      SourceLoc(),
                                      /*allowConcreteGenericParams=*/true);
  auto GenEnv = GenSig->createGenericEnvironment();
  return { GenEnv, GenSig };
}

SubstitutionMap FunctionSignaturePartialSpecializer::computeClonerParamSubs() {
  return SubstitutionMap::get(
    CalleeGenericSig,
    [&](SubstitutableType *type) -> Type {
      LLVM_DEBUG(llvm::dbgs() << "\ngetSubstitution for ClonerParamSubs:\n"
                              << Type(type) << "\n"
                              << "in generic signature:\n";
                 CalleeGenericSig->dump());
      auto SpecializedInterfaceTy =
          Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);
      return SpecializedGenericEnv->mapTypeIntoContext(
          SpecializedInterfaceTy);
    },
    LookUpConformanceInSignature(*SpecializedGenericSig));
}

SubstitutionMap FunctionSignaturePartialSpecializer::getCallerParamSubs() {
  return SpecializedInterfaceToCallerArchetypeMap;
}

void FunctionSignaturePartialSpecializer::computeCallerInterfaceSubs(
    SubstitutionMap &CallerInterfaceSubs) {
  CallerInterfaceSubs = SubstitutionMap::get(
    CalleeGenericSig,
    [&](SubstitutableType *type) -> Type {
      // First, map callee's interface type to specialized interface type.
      auto Ty = Type(type).subst(CalleeInterfaceToSpecializedInterfaceMap);
      Type SpecializedInterfaceTy =
        SpecializedGenericEnv->mapTypeIntoContext(Ty)
          ->mapTypeOutOfContext();
      assert(!SpecializedInterfaceTy->hasError());
      return SpecializedInterfaceTy;
    },
    LookUpConformanceInSignature(*CalleeGenericSig));

  LLVM_DEBUG(llvm::dbgs() << "\n\nCallerInterfaceSubs map:\n";
             CallerInterfaceSubs.dump(llvm::dbgs()));
}

/// Fast-path for the case when generic substitutions are not supported.
void FunctionSignaturePartialSpecializer::
    createSpecializedGenericSignatureWithNonGenericSubs() {
  // Simply create a set of same-type requirements based on concrete
  // substitutions.
  SmallVector<Requirement, 4> Requirements;
  CalleeGenericSig->forEachParam([&](GenericTypeParamType *GP, bool Canonical) {
    if (!Canonical)
      return;
    auto Replacement = Type(GP).subst(CalleeInterfaceToCallerArchetypeMap);
    if (Replacement->hasArchetype())
      return;
    // Replacement is concrete. Add a same type requirement.
    Requirement Req(RequirementKind::SameType, GP, Replacement);
    Requirements.push_back(Req);
  });

  // Create a new generic signature by taking the existing one
  // and adding new requirements to it. No need to introduce
  // any new generic parameters.
  auto GenPair = getGenericEnvironmentAndSignatureWithRequirements(
      CalleeGenericSig, CalleeGenericEnv, Requirements, M);

  if (GenPair.second) {
    SpecializedGenericSig = GenPair.second->getCanonicalSignature();
    SpecializedGenericEnv = GenPair.first;
  }

  for (auto GP : CalleeGenericSig->getGenericParams()) {
    CalleeInterfaceToSpecializedInterfaceMapping[GP] = Type(GP);
  }
  computeCalleeInterfaceToSpecializedInterfaceMap();

  SpecializedInterfaceToCallerArchetypeMap =
    CalleeInterfaceToCallerArchetypeMap;
}

void FunctionSignaturePartialSpecializer::createSpecializedGenericSignature(
    SubstitutionMap ParamSubs) {
  // Collect all used caller's archetypes from all the substitutions.
  collectUsedCallerArchetypes(ParamSubs);

  // Generate a new generic type parameter for each used archetype from
  // the caller.
  createGenericParamsForUsedCallerArchetypes();

  // Create a SubstitutionMap for re-mapping caller's interface types
  // to interface types of the specialized function.
  computeCallerInterfaceToSpecializedInterfaceMap();

  // Add generic parameters that will come from the callee.
  // Introduce a new generic parameter in the new generic signature
  // for each generic parameter from the callee.
  createGenericParamsForCalleeGenericParams();

  computeCalleeInterfaceToSpecializedInterfaceMap();

  // Add requirements from the callee's generic signature.
  addCalleeRequirements();

  // Add requirements from the caller's generic signature.
  addCallerRequirements();

  auto GenPair = getSpecializedGenericEnvironmentAndSignature();
  if (GenPair.second) {
    SpecializedGenericSig = GenPair.second->getCanonicalSignature();
    SpecializedGenericEnv = GenPair.first;
    computeSpecializedInterfaceToCallerArchetypeMap();
  }
}

/// Builds a new generic and function signatures for a partial specialization.
/// Allows for partial specializations even if substitutions contain
/// type parameters.
///
/// The new generic signature has the following generic parameters:
/// - For each substitution with a concrete type CT as a replacement for a
/// generic type T, it introduces a generic parameter T' and a
/// requirement T' == CT
/// - For all other substitutions that are considered for partial specialization,
/// it collects first the archetypes used in the replacements. Then for each such
/// archetype A a new generic parameter T' introduced.
/// - If there is a substitution for type T and this substitution is excluded
/// from partial specialization (e.g. because it is impossible or would result
/// in a less efficient code), then a new generic parameter T' is introduced,
/// which does not get any additional, more specific requirements based on the
/// substitutions.
///
/// After all generic parameters are added according to the rules above,
/// the requirements of the callee's signature are re-mapped by re-formulating
/// them in terms of the newly introduced generic parameters. In case a remapped
/// requirement does not contain any generic types, it can be omitted, because
/// it is fulfilled already.
///
/// If any of the generic parameters were introduced for caller's archetypes,
/// their requirements from the caller's signature are re-mapped by
/// re-formulating them in terms of the newly introduced generic parameters.
void ReabstractionInfo::performPartialSpecializationPreparation(
    SILFunction *Caller, SILFunction *Callee,
    SubstitutionMap ParamSubs) {
  SILModule &M = Callee->getModule();

  // Caller is the SILFunction containing the apply instruction.
  CanGenericSignature CallerGenericSig;
  GenericEnvironment *CallerGenericEnv = nullptr;
  if (Caller) {
    CallerGenericSig = Caller->getLoweredFunctionType()->getGenericSignature();
    CallerGenericEnv = Caller->getGenericEnvironment();
  }

  // Callee is the generic function being called by the apply instruction.
  auto CalleeFnTy = Callee->getLoweredFunctionType();
  auto CalleeGenericSig = CalleeFnTy->getGenericSignature();
  auto CalleeGenericEnv = Callee->getGenericEnvironment();

  LLVM_DEBUG(llvm::dbgs() << "\n\nTrying partial specialization for: "
                          << Callee->getName() << "\n";
             llvm::dbgs() << "Callee generic signature is:\n";
             CalleeGenericSig->dump());

  FunctionSignaturePartialSpecializer FSPS(M,
                                           CallerGenericSig, CallerGenericEnv,
                                           CalleeGenericSig, CalleeGenericEnv,
                                           ParamSubs);

  // Create the partially specialized generic signature and generic environment.
  if (SupportGenericSubstitutions)
    FSPS.createSpecializedGenericSignature(ParamSubs);
  else
    FSPS.createSpecializedGenericSignatureWithNonGenericSubs();

  // Once the specialized signature is known, compute different
  // maps and function types based on it. The specializer will need
  // them for cloning and specializing the function body, rewriting
  // the original apply instruction, etc.
  finishPartialSpecializationPreparation(FSPS);
}

void ReabstractionInfo::finishPartialSpecializationPreparation(
    FunctionSignaturePartialSpecializer &FSPS) {
  SpecializedGenericSig = FSPS.getSpecializedGenericSignature();
  SpecializedGenericEnv = FSPS.getSpecializedGenericEnvironment();

  if (SpecializedGenericSig) {
    LLVM_DEBUG(llvm::dbgs() << "\nCreated SpecializedGenericSig:\n";
               SpecializedGenericSig->dump(); SpecializedGenericEnv->dump());
  }

  // Create substitution lists for the caller and cloner.
  ClonerParamSubMap = FSPS.computeClonerParamSubs();
  CallerParamSubMap = FSPS.getCallerParamSubs();

  // Create a substitution map for the caller interface substitutions.
  FSPS.computeCallerInterfaceSubs(CallerInterfaceSubs);

  if (CalleeParamSubMap.empty()) {
    // It can happen if there is no caller or it is an eager specialization.
    CalleeParamSubMap = CallerParamSubMap;
  }

  HasUnboundGenericParams =
      SpecializedGenericSig && !SpecializedGenericSig->areAllParamsConcrete();

  createSubstitutedAndSpecializedTypes();

  if (getSubstitutedType() != Callee->getLoweredFunctionType()) {
    if (getSubstitutedType()->isPolymorphic())
      LLVM_DEBUG(llvm::dbgs() << "Created new specialized type: "
                              << SpecializedType << "\n");
  }
}

/// Perform some sanity checks for the requirements provided in @_specialize.
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

/// This constructor is used when processing @_specialize.
ReabstractionInfo::ReabstractionInfo(SILFunction *Callee,
                                     ArrayRef<Requirement> Requirements) {
  if (shouldNotSpecializeCallee(Callee))
    return;

  // Perform some sanity checks for the requirements.
  checkSpecializationRequirements(Requirements);

  this->Callee = Callee;
  ConvertIndirectToDirect = true;

  SILModule &M = Callee->getModule();

  auto CalleeGenericSig =
      Callee->getLoweredFunctionType()->getGenericSignature();
  auto *CalleeGenericEnv = Callee->getGenericEnvironment();

  FunctionSignaturePartialSpecializer FSPS(M,
                                           CalleeGenericSig, CalleeGenericEnv,
                                           Requirements);

  finishPartialSpecializationPreparation(FSPS);
}

// =============================================================================
// GenericFuncSpecializer
// =============================================================================

GenericFuncSpecializer::GenericFuncSpecializer(
    SILOptFunctionBuilder &FuncBuilder, SILFunction *GenericFunc,
    SubstitutionMap ParamSubs, IsSerialized_t Serialized,
    const ReabstractionInfo &ReInfo)
    : FuncBuilder(FuncBuilder), M(GenericFunc->getModule()),
      GenericFunc(GenericFunc),
      ParamSubs(ParamSubs),
      Serialized(Serialized),
      ReInfo(ReInfo) {

  assert(GenericFunc->isDefinition() && "Expected definition to specialize!");
  auto FnTy = ReInfo.getSpecializedType();

  if (ReInfo.isPartialSpecialization()) {
    Mangle::PartialSpecializationMangler Mangler(
        GenericFunc, FnTy, Serialized, /*isReAbstracted*/ true);
    ClonedName = Mangler.mangle();
  } else {
    Mangle::GenericSpecializationMangler Mangler(
        GenericFunc, ParamSubs, Serialized, /*isReAbstracted*/ true);
    ClonedName = Mangler.mangle();
  }
  LLVM_DEBUG(llvm::dbgs() << "    Specialized function " << ClonedName << '\n');
}

/// Return an existing specialization if one exists.
SILFunction *GenericFuncSpecializer::lookupSpecialization() {
  if (SILFunction *SpecializedF = M.lookUpFunction(ClonedName)) {
    if (ReInfo.getSpecializedType() != SpecializedF->getLoweredFunctionType()) {
      llvm::dbgs() << "Looking for a function: " << ClonedName << "\n"
                   << "Expected type: " << ReInfo.getSpecializedType() << "\n"
                   << "Found    type: "
                   << SpecializedF->getLoweredFunctionType() << "\n";
    }
    assert(ReInfo.getSpecializedType()
           == SpecializedF->getLoweredFunctionType() &&
           "Previously specialized function does not match expected type.");
    LLVM_DEBUG(llvm::dbgs() << "Found an existing specialization for: "
                            << ClonedName << "\n");
    return SpecializedF;
  }
  LLVM_DEBUG(llvm::dbgs() << "Could not find an existing specialization for: "
                          << ClonedName << "\n");
  return nullptr;
}

/// Forward decl for prespecialization support.
static bool linkSpecialization(SILModule &M, SILFunction *F);

void ReabstractionInfo::verify() const {
  assert((!SpecializedGenericSig && !SpecializedGenericEnv &&
          !getSpecializedType()->isPolymorphic()) ||
         (SpecializedGenericSig && SpecializedGenericEnv &&
          getSpecializedType()->isPolymorphic()));
}

/// Create a new specialized function if possible, and cache it.
SILFunction *GenericFuncSpecializer::tryCreateSpecialization() {
  // Do not create any new specializations at Onone.
  if (!GenericFunc->shouldOptimize())
    return nullptr;

  LLVM_DEBUG(llvm::dbgs() << "Creating a specialization: "
                          << ClonedName << "\n");

  ReInfo.verify();

  // Create a new function.
  SILFunction *SpecializedF = GenericCloner::cloneFunction(
      FuncBuilder, GenericFunc, Serialized, ReInfo,
      // Use these substitutions inside the new specialized function being
      // created.
      ReInfo.getClonerParamSubstitutionMap(),
      ClonedName);
  assert((SpecializedF->getLoweredFunctionType()->isPolymorphic() &&
          SpecializedF->getGenericEnvironment()) ||
         (!SpecializedF->getLoweredFunctionType()->isPolymorphic() &&
          !SpecializedF->getGenericEnvironment()));
  assert(!SpecializedF->hasQualifiedOwnership());
  // Check if this specialization should be linked for prespecialization.
  linkSpecialization(M, SpecializedF);
  // Store the meta-information about how this specialization was created.
  auto *Caller = ReInfo.getApply() ? ReInfo.getApply().getFunction() : nullptr;
  SubstitutionMap Subs = Caller ? ReInfo.getApply().getSubstitutionMap()
                                : ReInfo.getClonerParamSubstitutionMap();
  SpecializedF->setClassSubclassScope(SubclassScope::NotApplicable);
  SpecializedF->setSpecializationInfo(
      GenericSpecializationInformation::create(Caller, GenericFunc, Subs));
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

/// Prepare call arguments. Perform re-abstraction if required.
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
getCalleeSubstFunctionType(SILValue Callee, SubstitutionMap Subs) {
  // Create a substituted callee type.
  auto CanFnTy = Callee->getType().castTo<SILFunctionType>();
  return CanFnTy->substGenericArgs(*Callee->getModule(), Subs);
}

/// Create a new apply based on an old one, but with a different
/// function being applied.
static ApplySite replaceWithSpecializedCallee(ApplySite AI,
                                              SILValue Callee,
                                              SILBuilder &Builder,
                                              const ReabstractionInfo &ReInfo) {
  SILLocation Loc = AI.getLoc();
  SmallVector<SILValue, 4> Arguments;
  SILValue StoreResultTo;

  prepareCallArguments(AI, Builder, ReInfo, Arguments, StoreResultTo);

  // Create a substituted callee type.
  SubstitutionMap Subs;
  if (ReInfo.getSpecializedType()->isPolymorphic()) {
    Subs = ReInfo.getCallerParamSubstitutionMap();
  }

  auto CalleeSubstFnTy = getCalleeSubstFunctionType(Callee, Subs);
  auto CalleeSILSubstFnTy = SILType::getPrimitiveObjectType(CalleeSubstFnTy);
  SILFunctionConventions substConv(CalleeSubstFnTy, Builder.getModule());

  if (auto *TAI = dyn_cast<TryApplyInst>(AI)) {
    SILBasicBlock *ResultBB = TAI->getNormalBB();
    assert(ResultBB->getSinglePredecessorBlock() == TAI->getParent());
    auto *NewTAI = Builder.createTryApply(Loc, Callee, Subs, Arguments,
                                          ResultBB, TAI->getErrorBB());
    if (StoreResultTo) {
      assert(substConv.useLoweredAddresses());
      // The original normal result of the try_apply is an empty tuple.
      assert(ResultBB->getNumArguments() == 1);
      Builder.setInsertionPoint(ResultBB->begin());
      fixUsedVoidType(ResultBB->getArgument(0), Loc, Builder);

      SILArgument *Arg = ResultBB->replacePhiArgument(
          0, StoreResultTo->getType().getObjectType(),
          ValueOwnershipKind::Owned);
      // Store the direct result to the original result address.
      Builder.createStore(Loc, Arg, StoreResultTo,
                          StoreOwnershipQualifier::Unqualified);
    }
    return NewTAI;
  }
  if (auto *A = dyn_cast<ApplyInst>(AI)) {
    auto *NewAI = Builder.createApply(Loc, Callee, Subs, Arguments,
                                      A->isNonThrowing());
    if (StoreResultTo) {
      assert(substConv.useLoweredAddresses());
      if (!CalleeSILSubstFnTy.isNoReturnFunction()) {
        // Store the direct result to the original result address.
        fixUsedVoidType(A, Loc, Builder);
        Builder.createStore(Loc, NewAI, StoreResultTo,
                            StoreOwnershipQualifier::Unqualified);
      } else {
        Builder.createUnreachable(Loc);
        // unreachable should be the terminator instruction.
        // So, split the current basic block right after the
        // inserted unreachable instruction.
        Builder.getInsertionPoint()->getParent()->split(
            Builder.getInsertionPoint());
      }
    }
    A->replaceAllUsesWith(NewAI);
    return NewAI;
  }
  if (auto *A = dyn_cast<BeginApplyInst>(AI)) {
    assert(!StoreResultTo);
    auto *NewAI = Builder.createBeginApply(Loc, Callee, Subs, Arguments,
                                           A->isNonThrowing());
    A->replaceAllUsesPairwiseWith(NewAI);
    return NewAI;
  }
  if (auto *PAI = dyn_cast<PartialApplyInst>(AI)) {
    auto *NewPAI = Builder.createPartialApply(
        Loc, Callee, Subs, Arguments,
        PAI->getType().getAs<SILFunctionType>()->getCalleeConvention());
    PAI->replaceAllUsesWith(NewPAI);
    return NewPAI;
  }
  llvm_unreachable("unhandled kind of apply");
}

/// Create a new apply based on an old one, but with a different
/// function being applied.
ApplySite swift::
replaceWithSpecializedFunction(ApplySite AI, SILFunction *NewF,
                               const ReabstractionInfo &ReInfo) {
  SILBuilderWithScope Builder(AI.getInstruction());
  FunctionRefInst *FRI = Builder.createFunctionRef(AI.getLoc(), NewF);
  return replaceWithSpecializedCallee(AI, FRI, Builder, ReInfo);
}

namespace {

class ReabstractionThunkGenerator {
  SILOptFunctionBuilder &FunctionBuilder;
  SILFunction *OrigF;
  SILModule &M;
  SILFunction *SpecializedFunc;
  const ReabstractionInfo &ReInfo;
  PartialApplyInst *OrigPAI;

  IsSerialized_t Serialized = IsNotSerialized;
  std::string ThunkName;
  RegularLocation Loc;
  SmallVector<SILValue, 4> Arguments;

public:
  ReabstractionThunkGenerator(SILOptFunctionBuilder &FunctionBuilder,
                              const ReabstractionInfo &ReInfo,
                              PartialApplyInst *OrigPAI,
                              SILFunction *SpecializedFunc)
      : FunctionBuilder(FunctionBuilder), OrigF(OrigPAI->getCalleeFunction()), M(OrigF->getModule()),
        SpecializedFunc(SpecializedFunc), ReInfo(ReInfo), OrigPAI(OrigPAI),
        Loc(RegularLocation::getAutoGeneratedLocation()) {
    if (OrigF->isSerialized() && OrigPAI->getFunction()->isSerialized())
      Serialized = IsSerializable;

    {
      if (!ReInfo.isPartialSpecialization()) {
        Mangle::GenericSpecializationMangler Mangler(
            OrigF, ReInfo.getCalleeParamSubstitutionMap(), Serialized,
            /*isReAbstracted*/ false);

        ThunkName = Mangler.mangle();
      } else {
        Mangle::PartialSpecializationMangler Mangler(
            OrigF, ReInfo.getSpecializedType(), Serialized,
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
  SILFunction *Thunk = FunctionBuilder.getOrCreateSharedFunction(
      Loc, ThunkName, ReInfo.getSubstitutedType(), IsBare, IsTransparent,
      Serialized, ProfileCounter(), IsThunk);
  // Re-use an existing thunk.
  if (!Thunk->empty())
    return Thunk;

  Thunk->setGenericEnvironment(ReInfo.getSpecializedGenericEnvironment());

  // Set proper generic context scope for the type lowering.
  CanSILFunctionType SpecType = SpecializedFunc->getLoweredFunctionType();
  Lowering::GenericContextScope GenericScope(M.Types,
                                             SpecType->getGenericSignature());

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
  if (!SpecializedFunc->hasQualifiedOwnership()) {
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

/// Create a call to a reabstraction thunk. Return the call's direct result.
SILValue ReabstractionThunkGenerator::createReabstractionThunkApply(
    SILBuilder &Builder) {
  SILFunction *Thunk = &Builder.getFunction();
  auto *FRI = Builder.createFunctionRef(Loc, SpecializedFunc);
  auto Subs = Thunk->getForwardingSubstitutionMap();
  auto specConv = SpecializedFunc->getConventions();
  if (!SpecializedFunc->getLoweredFunctionType()->hasErrorResult()) {
    return Builder.createApply(Loc, FRI, Subs, Arguments, false);
  }
  // Create the logic for calling a throwing function.
  SILBasicBlock *NormalBB = Thunk->createBasicBlock();
  SILBasicBlock *ErrorBB = Thunk->createBasicBlock();
  Builder.createTryApply(Loc, FRI, Subs, Arguments, NormalBB, ErrorBB);
  auto *ErrorVal = ErrorBB->createPhiArgument(
      SpecializedFunc->mapTypeIntoContext(specConv.getSILErrorType()),
      ValueOwnershipKind::Owned);
  Builder.setInsertionPoint(ErrorBB);
  Builder.createThrow(Loc, ErrorVal);
  SILValue ReturnValue = NormalBB->createPhiArgument(
      SpecializedFunc->mapTypeIntoContext(specConv.getSILResultType()),
      ValueOwnershipKind::Owned);
  Builder.setInsertionPoint(NormalBB);
  return ReturnValue;
}

/// Create SIL arguments for a reabstraction thunk with lowered addresses. This
/// may involve replacing indirect arguments with loads and stores. Return the
/// SILArgument for the address of an indirect result, or nullptr.
///
/// FIXME: Remove this if we don't need to create reabstraction thunks after
/// address lowering.
SILArgument *ReabstractionThunkGenerator::convertReabstractionThunkArguments(
    SILBuilder &Builder) {
  SILFunction *Thunk = &Builder.getFunction();
  CanSILFunctionType SpecType = SpecializedFunc->getLoweredFunctionType();
  CanSILFunctionType SubstType = ReInfo.getSubstitutedType();
  auto specConv = SpecializedFunc->getConventions();
  (void)specConv;
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
      SILType ResultTy =
          SpecializedFunc->mapTypeIntoContext(substConv.getSILType(substRI));
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
      SILType ParamTy = SpecializedFunc->mapTypeIntoContext(
          substConv.getSILType(SubstType->getParameters()[paramIdx]));
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
    SILOptFunctionBuilder &FuncBuilder,
    ApplySite Apply, DeadInstructionSet &DeadApplies,
    llvm::SmallVectorImpl<SILFunction *> &NewFunctions,
    OptRemark::Emitter &ORE) {
  assert(Apply.hasSubstitutions() && "Expected an apply with substitutions!");
  auto *F = Apply.getFunction();
  auto *RefF = cast<FunctionRefInst>(Apply.getCallee())->getReferencedFunction();

  LLVM_DEBUG(llvm::dbgs() << "\n\n*** ApplyInst in function " << F->getName()
                          << ":\n";
             Apply.getInstruction()->dumpInContext());

  // If the caller is fragile but the callee is not, bail out.
  // Specializations have shared linkage, which means they do
  // not have an external entry point, Since the callee is not
  // fragile we cannot serialize the body of the specialized
  // callee either.
  if (F->isSerialized() && !RefF->hasValidLinkageForFragileInline())
      return;

  if (shouldNotSpecializeCallee(RefF))
    return;

  // If the caller and callee are both fragile, preserve the fragility when
  // cloning the callee. Otherwise, strip it off so that we can optimize
  // the body more.
  IsSerialized_t Serialized = IsNotSerialized;
  if (F->isSerialized() && RefF->isSerialized())
    Serialized = IsSerializable;

  // If it is OnoneSupport consider all specializations as non-serialized
  // as we do not SIL serialize their bodies.
  // It is important to set this flag here, because it affects the
  // mangling of the specialization's name.
  if (Apply.getModule().isOptimizedOnoneSupportModule())
    Serialized = IsNotSerialized;

  ReabstractionInfo ReInfo(Apply, RefF, Apply.getSubstitutionMap(),
                           /*ConvertIndirectToDirect=*/true, &ORE);
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
      if (User->isDebugInstruction())
        continue;

      auto FAS = FullApplySite::isa(User);
      if (FAS && FAS.getCallee() == PAI)
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

  GenericFuncSpecializer FuncSpecializer(FuncBuilder,
                                         RefF, Apply.getSubstitutionMap(),
                                         Serialized, ReInfo);
  SILFunction *SpecializedF = FuncSpecializer.lookupSpecialization();
  if (SpecializedF) {
    // Even if the pre-specialization exists already, try to preserve it
    // if it is one of our known pre-specializations for -Onone support.
    linkSpecialization(M, SpecializedF);
  } else {
    SpecializedF = FuncSpecializer.tryCreateSpecialization();
    if (!SpecializedF)
      return;
    LLVM_DEBUG(llvm::dbgs() << "Created specialized function: "
                            << SpecializedF->getName() << "\n"
                            << "Specialized function type: "
                            << SpecializedF->getLoweredFunctionType() << "\n");
    assert(!SpecializedF->hasQualifiedOwnership());
    NewFunctions.push_back(SpecializedF);
  }

  ORE.emit([&]() {
    std::string Str;
    llvm::raw_string_ostream OS(Str);
    SpecializedF->getLoweredFunctionType().print(
        OS, PrintOptions::printQuickHelpDeclaration());

    using namespace OptRemark;
    return RemarkPassed("Specialized", *Apply.getInstruction())
           << "Specialized function " << NV("Function", RefF) << " with type "
           << NV("FuncType", OS.str());
  });

  assert(ReInfo.getSpecializedType()
         == SpecializedF->getLoweredFunctionType() &&
         "Previously specialized function does not match expected type.");

  DeadApplies.insert(Apply.getInstruction());

  if (replacePartialApplyWithoutReabstraction) {
    // There are some unknown users of the partial_apply. Therefore we need a
    // thunk which converts from the re-abstracted function back to the
    // original function with indirect parameters/results.
    auto *PAI = cast<PartialApplyInst>(Apply.getInstruction());
    SILBuilderWithScope Builder(PAI);
    SILFunction *Thunk =
      ReabstractionThunkGenerator(FuncBuilder, ReInfo, PAI, SpecializedF).createThunk();
    NewFunctions.push_back(Thunk);
    auto *FRI = Builder.createFunctionRef(PAI->getLoc(), Thunk);
    SmallVector<SILValue, 4> Arguments;
    for (auto &Op : PAI->getArgumentOperands()) {
      Arguments.push_back(Op.get());
    }
    auto Subs = ReInfo.getCallerParamSubstitutionMap();
    auto *NewPAI = Builder.createPartialApply(
        PAI->getLoc(), FRI, Subs, Arguments,
        PAI->getType().getAs<SILFunctionType>()->getCalleeConvention());
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
  if (F->getLinkage() == SILLinkage::Public)
    return true;
  // Do not remove functions that are known prespecializations.
  // Keep them around. Change their linkage to public, so that other
  // applications can refer to them.
  if (M.isOptimizedOnoneSupportModule()) {
    if (isKnownPrespecialization(F->getName())) {
      F->setLinkage(SILLinkage::Public);
      F->setSerialized(IsNotSerialized);
      return true;
    }
  }
  return false;
}

/// The list of classes and functions from the stdlib
/// whose specializations we want to preserve.
static const char *const KnownPrespecializations[] = {
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
};

bool swift::isKnownPrespecialization(StringRef SpecName) {
  // TODO: Once there is an efficient API to check if
  // a given symbol is a specialization of a specific type,
  // use it instead. Doing demangling just for this check
  // is just wasteful.
  auto DemangledNameString =
     swift::Demangle::demangleSymbolAsString(SpecName);

  StringRef DemangledName = DemangledNameString;

  LLVM_DEBUG(llvm::dbgs() << "Check if known: " << DemangledName << "\n");

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
  LLVM_DEBUG(llvm::dbgs() << "Check substring: " << OfStr << "\n");

  pos = DemangledName.find(OfStr, oldpos);

  if (pos == StringRef::npos) {
    // Create "of (extension in Swift).Swift"
    llvm::SmallString<64> OfString;
    llvm::raw_svector_ostream buffer(OfString);
    buffer << "of (extension in " << STDLIB_NAME << "):";
    buffer << STDLIB_NAME << '.';
    OfStr = buffer.str();
    pos = DemangledName.find(OfStr, oldpos);
    LLVM_DEBUG(llvm::dbgs() << "Check substring: " << OfStr << "\n");
    if (pos == StringRef::npos)
      return false;
  }

  pos += OfStr.size();

  for (auto NameStr : KnownPrespecializations) {
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
  if (isKnownPrespecialization(FunctionName))
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

  LLVM_DEBUG(llvm::dbgs() << "Found existing specialization for: "
                          << FunctionName << '\n';
             llvm::dbgs() << swift::Demangle::demangleSymbolAsString(
                             Specialization->getName())
                          << "\n\n");

  return Specialization;
}


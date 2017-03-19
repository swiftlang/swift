//===--- Generics.h - Utilities for transforming generics -------*- C++ -*-===//
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
// This contains utilities for transforming generics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GENERICS_H
#define SWIFT_SIL_GENERICS_H

#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// Tries to specialize an \p Apply of a generic function. It can be a full
/// apply site or a partial apply.
/// Replaced and now dead instructions are returned in \p DeadApplies.
/// New created functions, like the specialized callee and thunks, are returned
/// in \p NewFunctions.
///
/// This is the top-level entry point for specializing an existing call site.
void trySpecializeApplyOfGeneric(
    ApplySite Apply, DeadInstructionSet &DeadApplies,
    llvm::SmallVectorImpl<SILFunction *> &NewFunctions);

/// Helper class to describe re-abstraction of function parameters done during
/// specialization.
///
/// Specifically, it contains information which formal parameters and returns
/// are changed from indirect values to direct values.
class ReabstractionInfo {
  /// A 1-bit means that this parameter/return value is converted from indirect
  /// to direct.
  llvm::SmallBitVector Conversions;

  /// If set, indirect to direct conversions should be performned by the generic
  /// specializer.
  bool ConvertIndirectToDirect;

  /// The first NumResults bits in Conversions refer to formal indirect
  /// out-parameters.
  unsigned NumFormalIndirectResults;

  /// The function type after applying the substitutions used to call the
  /// specialized function.
  CanSILFunctionType SubstitutedType;

  /// The function type after applying the re-abstractions on the
  /// SubstitutedType.
  CanSILFunctionType SpecializedType;

  /// The generic environment to be used by the specialization.
  GenericEnvironment *SpecializedGenericEnv;

  /// The generic signature of the specialization.
  /// It is nullptr if the specialization is not polymorphic.
  GenericSignature *SpecializedGenericSig;

  // Set of the substitutions used by the caller's apply instruction before
  // any transformations performed by the generic specializer.
  //
  // Maps caller's generic parameters to caller's archetypes.
  SubstitutionList OriginalParamSubs;

  // Set of substitutions to be used by the caller's apply when it calls a
  // specialized function.
  //
  // Maps caller's generic parameters to caller's archetypes.
  //
  // FIXME: How is this different from OriginalParamSubs? Right now both
  // are identical.
  SubstitutionList CallerParamSubs;

  // Replaces archetypes of the original callee with archetypes
  // or concrete types, if they were made concrete) of the specialized
  // callee.
  //
  // Maps original callee's generic parameters to specialized
  // callee archetypes.
  SubstitutionList ClonerParamSubs;

  // Reference to the original generic non-specialized function.
  SILFunction *OriginalF;

  // The apply site which invokes the generic function.
  ApplySite Apply;

  // Set if a specialized function has unbound generic parameters.
  bool HasUnboundGenericParams;

  // Substitutions to be used for creating a new function type
  // for the specialized function.
  //
  // Maps original callee's generic parameters to specialized callee's
  // generic parameters.
  // It uses interface types.
  SubstitutionMap CallerInterfaceSubs;

  // Create a new substituted type with the updated signature.
  CanSILFunctionType createSubstitutedType(SILFunction *OrigF,
                                           const SubstitutionMap &SubstMap,
                                           bool HasUnboundGenericParams);

  void createSubstitutedAndSpecializedTypes();
  bool prepareAndCheck(ApplySite Apply, SILFunction *Callee,
                       SubstitutionList ParamSubs);
  void specializeConcreteAndGenericSubstitutions(ApplySite Apply,
                                                 SILFunction *Callee,
                                                 SubstitutionList ParamSubs);
  void specializeConcreteSubstitutions(ApplySite Apply, SILFunction *Callee,
                                       SubstitutionList ParamSubs);

  ReabstractionInfo() {}
public:
  /// Constructs the ReabstractionInfo for generic function \p Orig with
  /// substitutions \p ParamSubs.
  /// If specialization is not possible getSpecializedType() will return an
  /// invalid type.
  ReabstractionInfo(ApplySite Apply, SILFunction *Callee,
                    SubstitutionList ParamSubs,
                    bool ConvertIndirectToDirect = true);

  /// Constructs the ReabstractionInfo for generic function \p Orig with
  /// additional requirements. Requirements may contain new layout,
  /// conformances or same concrete type requirements.
  ReabstractionInfo(SILFunction *Orig, ArrayRef<Requirement> Requirements);

  /// Returns true if the \p ParamIdx'th (non-result) formal parameter is
  /// converted from indirect to direct.
  bool isParamConverted(unsigned ParamIdx) const {
    return ConvertIndirectToDirect &&
           Conversions.test(ParamIdx + NumFormalIndirectResults);
  }

  /// Returns true if the \p ResultIdx'th formal result is converted from
  /// indirect to direct.
  bool isFormalResultConverted(unsigned ResultIdx) const {
    assert(ResultIdx < NumFormalIndirectResults);
    return ConvertIndirectToDirect && Conversions.test(ResultIdx);
  }

  /// Gets the total number of original function arguments.
  unsigned getNumArguments() const { return Conversions.size(); }

  /// Returns true if the \p ArgIdx'th argument is converted from an
  /// indirect
  /// result or parameter to a direct result or parameter.
  bool isArgConverted(unsigned ArgIdx) const {
    return Conversions.test(ArgIdx);
  }

  /// Returns true if there are any conversions from indirect to direct values.
  bool hasConversions() const { return Conversions.any(); }

  /// Remove the arguments of a partial apply, leaving the arguments for the
  /// partial apply result function.
  void prunePartialApplyArgs(unsigned numPartialApplyArgs) {
    assert(numPartialApplyArgs <= SubstitutedType->getNumParameters());
    assert(numPartialApplyArgs <= Conversions.size());
    Conversions.resize(Conversions.size() - numPartialApplyArgs);
  }

  /// Returns the index of the first argument of an apply site, which may be
  /// > 0 in case of a partial_apply.
  unsigned getIndexOfFirstArg(ApplySite Apply) const {
    unsigned numArgs = Apply.getNumArguments();
    assert(numArgs == Conversions.size() ||
           (numArgs < Conversions.size() && isa<PartialApplyInst>(Apply)));
    return Conversions.size() - numArgs;
  }

  /// Get the function type after applying the substitutions to the original
  /// generic function.
  CanSILFunctionType getSubstitutedType() const { return SubstitutedType; }

  /// Get the function type after applying the re-abstractions on the
  /// substituted type. Returns an invalid type if specialization is not
  /// possible.
  CanSILFunctionType getSpecializedType() const { return SpecializedType; }

  GenericEnvironment *getSpecializedGenericEnvironment() const {
    return SpecializedGenericEnv;
  }

  SubstitutionList getCallerParamSubstitutions() const {
    return CallerParamSubs;
  }

  SubstitutionList getClonerParamSubstitutions() const {
    return ClonerParamSubs;
  }

  SubstitutionList getOriginalParamSubstitutions() const {
    return OriginalParamSubs;
  }

  /// Create a specialized function type for a specific substituted type \p
  /// SubstFTy by applying the re-abstractions.
  CanSILFunctionType createSpecializedType(CanSILFunctionType SubstFTy,
                                           SILModule &M) const;

  SILFunction *getNonSpecializedFunction() const { return OriginalF; }

  /// Map type into a context of the specialized function.
  Type mapTypeIntoContext(Type type) const;

  /// Map SIL type into a context of the specialized function.
  SILType mapTypeIntoContext(SILType type) const;

  SILModule &getModule() const { return OriginalF->getModule(); }

  /// Returns true if generic specialization is possible.
  bool canBeSpecialized() const;

  /// Returns true if it is a full generic specialization.
  bool isFullSpecialization() const;

  /// Returns true if it is a partial generic specialization.
  bool isPartialSpecialization() const;

  /// Returns true if a given apply can be specialized.
  static bool canBeSpecialized(ApplySite Apply, SILFunction *Callee,
                               SubstitutionList ParamSubs);
};

/// Helper class for specializing a generic function given a list of
/// substitutions.
class GenericFuncSpecializer {
  SILModule &M;
  SILFunction *GenericFunc;
  SubstitutionList ParamSubs;
  IsFragile_t Fragile;
  const ReabstractionInfo &ReInfo;

  SubstitutionMap ContextSubs;
  std::string ClonedName;

public:
  GenericFuncSpecializer(SILFunction *GenericFunc,
                         SubstitutionList ParamSubs,
                         IsFragile_t Fragile,
                         const ReabstractionInfo &ReInfo);

  /// If we already have this specialization, reuse it.
  SILFunction *lookupSpecialization();

  /// Return a newly created specialized function.
  SILFunction *tryCreateSpecialization();

  /// Try to specialize GenericFunc given a list of ParamSubs.
  /// Returns either a new or existing specialized function, or nullptr.
  SILFunction *trySpecialization() {
    if (!ReInfo.getSpecializedType())
      return nullptr;

    SILFunction *SpecializedF = lookupSpecialization();
    if (!SpecializedF)
      SpecializedF = tryCreateSpecialization();

    return SpecializedF;
  }

  StringRef getClonedName() {
    return ClonedName;
  }
};

// =============================================================================
// Prespecialized symbol lookup.
// =============================================================================

/// Checks if a given mangled name could be a name of a whitelisted
/// specialization.
bool isWhitelistedSpecialization(StringRef SpecName);

/// Create a new apply based on an old one, but with a different
/// function being applied.
ApplySite replaceWithSpecializedFunction(ApplySite AI, SILFunction *NewF,
                                         const ReabstractionInfo &ReInfo);

/// Returns a SILFunction for the symbol specified by FunctioName if it is
/// visible to the current SILModule. This is used to link call sites to
/// externally defined specialization and should only be used when the function
/// body is not required for further optimization or inlining (-Onone).
SILFunction *lookupPrespecializedSymbol(SILModule &M, StringRef FunctionName);

} // end namespace swift

#endif

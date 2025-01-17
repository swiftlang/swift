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
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

namespace swift {

class FunctionSignaturePartialSpecializer;
class SILOptFunctionBuilder;

namespace OptRemark {
class Emitter;
} // namespace OptRemark

/// Tries to specialize an \p Apply of a generic function. It can be a full
/// apply site or a partial apply.
/// Replaced and now dead instructions are returned in \p DeadApplies.
/// New created functions, like the specialized callee and thunks, are returned
/// in \p NewFunctions.
///
/// This is the top-level entry point for specializing an existing call site.
void trySpecializeApplyOfGeneric(
    SILOptFunctionBuilder &FunctionBuilder,
    ApplySite Apply, DeadInstructionSet &DeadApplies,
    llvm::SmallVectorImpl<SILFunction *> &NewFunctions,
    OptRemark::Emitter &ORE,
    bool isMandatory);

/// Helper class to describe re-abstraction of function parameters done during
/// specialization.
///
/// Specifically, it contains information which formal parameters and returns
/// are changed from indirect values to direct values.
class ReabstractionInfo {
  /// A 1-bit means that this argument (= either indirect return value or
  /// parameter) is converted from indirect to direct.
  SmallBitVector Conversions;

  /// For each bit set in Conversions, there is a bit set in TrivialArgs if the
  /// argument has a trivial type.
  SmallBitVector TrivialArgs;

  /// A 1-bit means that the argument is a metatype argument. The argument is
  /// dropped and replaced by a `metatype` instruction in the entry block.
  /// Only used if `dropMetatypeArgs` is true.
  SmallBitVector droppedArguments;

  /// Set to true if the function has a re-abstracted (= converted from
  /// indirect to direct) resilient argument or return type. This can happen if
  /// the function is compiled within the type's resilience domain, i.e. in
  /// its module (where the type is loadable).
  /// In this case we need to generate a different mangled name for the
  /// function to distinguish it from functions in other modules, which cannot
  /// re-abstract this resilient type.
  /// Fortunately, a flag is sufficient to describe this: either a function has
  /// re-abstracted resilient types or not. It cannot happen that two
  /// functions have two different subsets of re-abstracted resilient parameter
  /// types.
  bool hasConvertedResilientParams = false;

  /// If set, indirect to direct conversions should be performed by the generic
  /// specializer.
  bool ConvertIndirectToDirect = true;

  /// If true, drop unused arguments.
  /// See `droppedArguments`.
  bool dropUnusedArguments = false;

  bool hasIndirectErrorResult = false;

  /// The first NumResults bits in Conversions refer to formal indirect
  /// out-parameters.
  unsigned NumFormalIndirectResults = 0;

  /// The function type after applying the substitutions used to call the
  /// specialized function.
  CanSILFunctionType SubstitutedType;

  /// The function type after applying the re-abstractions on the
  /// SubstitutedType.
  CanSILFunctionType SpecializedType;

  /// The generic environment to be used by the specialization.
  GenericEnvironment *SpecializedGenericEnv = nullptr;

  /// The generic signature of the specialization.
  /// It is nullptr if the specialization is not polymorphic.
  GenericSignature SpecializedGenericSig;

  // Set of substitutions from callee's invocation before
  // any transformations performed by the generic specializer.
  //
  // Maps callee's generic parameters to caller's archetypes.
  SubstitutionMap CalleeParamSubMap;

  // Set of substitutions to be used to invoke a specialized function.
  //
  // Maps generic parameters of the specialized callee function to caller's
  // archetypes.
  SubstitutionMap CallerParamSubMap;

  // Replaces archetypes of the original callee with archetypes
  // or concrete types, if they were made concrete) of the specialized
  // callee.
  SubstitutionMap ClonerParamSubMap;

  // Reference to the original generic non-specialized callee function, if available
  SILFunction *Callee = nullptr;

  // The method to specialize. This must be not null if Callee is null.
  SILDeclRef methodDecl;

  SILModule *M = nullptr;

  // The module the specialization is created in.
  ModuleDecl *TargetModule = nullptr;

  bool isWholeModule = false;

  // The apply site which invokes the generic function.
  ApplySite Apply;

  // Set if a specialized function has unbound generic parameters.
  bool HasUnboundGenericParams = false;

  // Substitutions to be used for creating a new function type
  // for the specialized function.
  //
  // Maps original callee's generic parameters to specialized callee's
  // generic parameters.
  // It uses interface types.
  SubstitutionMap CallerInterfaceSubs;

  bool isPrespecialization = false;

  // Is the generated specialization going to be serialized?
  SerializedKind_t Serialized = IsNotSerialized;

  enum TypeCategory {
    NotLoadable,
    Loadable,
    LoadableAndTrivial
  };
  
  // Create a new substituted type with the updated signature.
  CanSILFunctionType createSubstitutedType(SILFunction *OrigF,
                                           SubstitutionMap SubstMap,
                                           bool HasUnboundGenericParams);

public:
  void createSubstitutedAndSpecializedTypes();
private:
  
  TypeCategory getReturnTypeCategory(const SILResultInfo &RI,
                                     const SILFunctionConventions &substConv,
                                     TypeExpansionContext typeExpansion);

  TypeCategory getParamTypeCategory(const SILParameterInfo &PI,
                                    const SILFunctionConventions &substConv,
                                    TypeExpansionContext typeExpansion);

  bool prepareAndCheck(ApplySite Apply, SILFunction *Callee,
                       SubstitutionMap ParamSubs,
                       OptRemark::Emitter *ORE = nullptr);
  void performFullSpecializationPreparation(SILFunction *Callee,
                                            SubstitutionMap ParamSubs);
  void performPartialSpecializationPreparation(SILFunction *Caller,
                                               SILFunction *Callee,
                                               SubstitutionMap ParamSubs);
  void finishPartialSpecializationPreparation(
      FunctionSignaturePartialSpecializer &FSPS);

  TypeCategory handleReturnAndError(SILResultInfo RI, unsigned argIdx);

public:
  ReabstractionInfo(SILModule &M) : M(&M) {}

  /// Constructs the ReabstractionInfo for generic function \p Callee with
  /// substitutions \p ParamSubs.
  /// If specialization is not possible getSpecializedType() will return an
  /// invalid type.
  ReabstractionInfo(ModuleDecl *targetModule, bool isModuleWholeModule,
                    ApplySite Apply, SILFunction *Callee,
                    SubstitutionMap ParamSubs, SerializedKind_t Serialized,
                    bool ConvertIndirectToDirect, bool dropMetatypeArgs,
                    OptRemark::Emitter *ORE = nullptr);

  /// Constructs the ReabstractionInfo for generic function \p Callee with
  /// a specialization signature.
  ReabstractionInfo(ModuleDecl *targetModule, bool isModuleWholeModule,
                    SILFunction *Callee, GenericSignature SpecializedSig,
                    bool isPrespecialization = false);

  ReabstractionInfo(CanSILFunctionType substitutedType,
                    SILDeclRef methodDecl,
                    SILModule &M) :
    SubstitutedType(substitutedType),
    methodDecl(methodDecl),
    M(&M), isWholeModule(M.isWholeModule()) {}


  bool isPrespecialized() const { return isPrespecialization; }

  bool isSerialized() const { return Serialized == IsSerialized; }
  SerializedKind_t getSerializedKind() const { return Serialized; }

  unsigned param2ArgIndex(unsigned ParamIdx) const  {
    return ParamIdx + NumFormalIndirectResults + (hasIndirectErrorResult ? 1: 0);
  }

  unsigned indirectErrorIndex() const {
    assert(hasIndirectErrorResult);
    return NumFormalIndirectResults;
  }

  /// Returns true if the specialized function needs an alternative mangling.
  /// See hasConvertedResilientParams.
  bool needAlternativeMangling() const {
    return hasConvertedResilientParams;
  }

  TypeExpansionContext getResilienceExpansion() const {
    auto resilience = (Serialized ? ResilienceExpansion::Minimal
                                  : ResilienceExpansion::Maximal);
    return TypeExpansionContext(resilience, TargetModule, isWholeModule);
  }

  /// Returns true if the \p ParamIdx'th (non-result) formal parameter is
  /// converted from indirect to direct.
  bool isParamConverted(unsigned ParamIdx) const {
    return ConvertIndirectToDirect && isArgConverted(param2ArgIndex(ParamIdx));
  }

  /// Returns true if the \p ResultIdx'th formal result is converted from
  /// indirect to direct.
  bool isFormalResultConverted(unsigned ResultIdx) const {
    assert(ResultIdx < NumFormalIndirectResults);
    return ConvertIndirectToDirect && Conversions.test(ResultIdx);
  }

  bool isErrorResultConverted() const {
    return ConvertIndirectToDirect && Conversions.test(indirectErrorIndex());
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

  /// Returns true if the argument at `ArgIdx` is a dropped argument.
  /// See `droppedArguments`.
  bool isDroppedArgument(unsigned ArgIdx) const {
    return droppedArguments.test(ArgIdx);
  }

  const SmallBitVector &getDroppedArgs() const { return droppedArguments; }

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

  GenericSignature getSpecializedGenericSignature() const {
    return SpecializedGenericSig;
  }

  SubstitutionMap getCallerParamSubstitutionMap() const {
    return CallerParamSubMap;
  }

  SubstitutionMap getClonerParamSubstitutionMap() const {
    return ClonerParamSubMap;
  }

  SubstitutionMap getCalleeParamSubstitutionMap() const {
    return CalleeParamSubMap;
  }

  /// Create a specialized function type for a specific substituted type \p
  /// SubstFTy by applying the re-abstractions.
  CanSILFunctionType createSpecializedType(CanSILFunctionType SubstFTy,
                                           SILModule &M) const;

  CanSILFunctionType createThunkType(PartialApplyInst *forPAI) const;

  SILFunction *getNonSpecializedFunction() const { return Callee; }

  /// Map SIL type into a context of the specialized function.
  SILType mapTypeIntoContext(SILType type) const;

  SILModule &getModule() const { return *M; }

  /// Returns true if generic specialization is possible.
  bool canBeSpecialized() const;

  /// Returns true if it is a full generic specialization.
  bool isFullSpecialization() const;

  /// Returns true if it is a partial generic specialization.
  bool isPartialSpecialization() const;

  /// Returns true if a given apply can be specialized.
  static bool canBeSpecialized(ApplySite Apply, SILFunction *Callee,
                               SubstitutionMap ParamSubs);

  /// Returns the apply site for the current generic specialization.
  ApplySite getApply() const {
    return Apply;
  }

  void verify() const;
};

/// Helper class for specializing a generic function given a list of
/// substitutions.
class GenericFuncSpecializer {
  SILOptFunctionBuilder &FuncBuilder;
  SILModule &M;
  SILFunction *GenericFunc;
  SubstitutionMap ParamSubs;
  const ReabstractionInfo &ReInfo;

  SubstitutionMap ContextSubs;
  std::string ClonedName;

  bool isMandatory;

public:
  GenericFuncSpecializer(SILOptFunctionBuilder &FuncBuilder,
                         SILFunction *GenericFunc,
                         SubstitutionMap ParamSubs,
                         const ReabstractionInfo &ReInfo,
                         bool isMandatory = false);

  /// If we already have this specialization, reuse it.
  SILFunction *lookupSpecialization();

  /// Return a newly created specialized function.
  SILFunction *tryCreateSpecialization(bool forcePrespecialization = false);

  /// Try to specialize GenericFunc given a list of ParamSubs.
  /// Returns either a new or existing specialized function, or nullptr.
  SILFunction *trySpecialization(bool forcePrespecialization = false) {
    if (!ReInfo.getSpecializedType())
      return nullptr;

    SILFunction *SpecializedF = lookupSpecialization();
    if (!SpecializedF)
      SpecializedF = tryCreateSpecialization(forcePrespecialization);

    return SpecializedF;
  }

  StringRef getClonedName() {
    return ClonedName;
  }
};

// =============================================================================
// Prespecialized symbol lookup.
// =============================================================================

/// Checks if a given mangled name could be a name of a known
/// prespecialization for -Onone support.
bool isKnownPrespecialization(StringRef SpecName);

class TypeReplacements {
private:
  std::optional<SILType> resultType;
  llvm::MapVector<unsigned, CanType> indirectResultTypes;
  llvm::MapVector<unsigned, CanType> paramTypeReplacements;
  llvm::MapVector<unsigned, CanType> yieldTypeReplacements;

public:
  std::optional<SILType> getResultType() const { return resultType; }

  void setResultType(SILType type) { resultType = type; }

  bool hasResultType() const { return resultType.has_value(); }

  const llvm::MapVector<unsigned, CanType> &getIndirectResultTypes() const {
    return indirectResultTypes;
  }

  void addIndirectResultType(unsigned index, CanType type) {
    indirectResultTypes.insert(std::make_pair(index, type));
  }

  bool hasIndirectResultTypes() const { return !indirectResultTypes.empty(); }

  const llvm::MapVector<unsigned, CanType> &getParamTypeReplacements() const {
    return paramTypeReplacements;
  }

  void addParameterTypeReplacement(unsigned index, CanType type) {
    paramTypeReplacements.insert(std::make_pair(index, type));
  }

  bool hasParamTypeReplacements() const {
    return !paramTypeReplacements.empty();
  }

  const llvm::MapVector<unsigned, CanType> &getYieldTypeReplacements() const {
    return yieldTypeReplacements;
  }

  void addYieldTypeReplacement(unsigned index, CanType type) {
    yieldTypeReplacements.insert(std::make_pair(index, type));
  }

  bool hasYieldTypeReplacements() const {
    return !yieldTypeReplacements.empty();
  }

  bool hasTypeReplacements() const {
    return hasResultType() || hasParamTypeReplacements() ||
           hasIndirectResultTypes() || hasYieldTypeReplacements();
  }
};

ApplySite replaceWithSpecializedCallee(
    ApplySite applySite, SILValue callee, const ReabstractionInfo &reInfo,
    const TypeReplacements &typeReplacements = {});

/// Checks if all OnoneSupport pre-specializations are included in the module
/// as public functions.
///
/// Issues errors for all missing functions.
void checkCompletenessOfPrespecializations(SILModule &M);

/// Create a new apply based on an old one, but with a different
/// function being applied.
ApplySite replaceWithSpecializedFunction(ApplySite AI, SILFunction *NewF,
                                         const ReabstractionInfo &ReInfo);

/// Returns a SILFunction for the symbol specified by FunctionName if it is
/// visible to the current SILModule. This is used to link call sites to
/// externally defined specialization and should only be used when the function
/// body is not required for further optimization or inlining (-Onone).
SILFunction *lookupPrespecializedSymbol(SILModule &M, StringRef FunctionName);

} // end namespace swift

#endif

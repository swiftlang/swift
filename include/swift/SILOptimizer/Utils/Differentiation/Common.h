//===--- Common.h - Automatic differentiation common utils ----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Automatic differentiation common utilities.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_COMMON_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_COMMON_H

#include "swift/SIL/SILDifferentiabilityWitness.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/TypeSubstCloner.h"

namespace swift {

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

namespace autodiff {

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
raw_ostream &getADDebugStream();

/// Given a function call site, gathers all of its actual results (both direct
/// and indirect) in an order defined by its result type.
void collectAllActualResultsInTypeOrder(
    ApplyInst *ai, ArrayRef<SILValue> extractedDirectResults,
    SmallVectorImpl<SILValue> &results);

/// Returns the underlying instruction for the given SILValue, if it exists,
/// peering through function conversion instructions.
template <class Inst> Inst *peerThroughFunctionConversions(SILValue value) {
  if (auto *inst = dyn_cast<Inst>(value))
    return inst;
  if (auto *cvi = dyn_cast<CopyValueInst>(value))
    return peerThroughFunctionConversions<Inst>(cvi->getOperand());
  if (auto *bbi = dyn_cast<BeginBorrowInst>(value))
    return peerThroughFunctionConversions<Inst>(bbi->getOperand());
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(value))
    return peerThroughFunctionConversions<Inst>(tttfi->getOperand());
  if (auto *cfi = dyn_cast<ConvertFunctionInst>(value))
    return peerThroughFunctionConversions<Inst>(cfi->getOperand());
  if (auto *pai = dyn_cast<PartialApplyInst>(value))
    return peerThroughFunctionConversions<Inst>(pai->getCallee());
  return nullptr;
}

/// Given a range of elements, joins these into a single value. If there's
/// exactly one element, returns that element. Otherwise, creates a tuple using
/// a `tuple` instruction.
SILValue joinElements(ArrayRef<SILValue> elements, SILBuilder &builder,
                      SILLocation loc);

/// Given a value, extracts all elements to `results` from this value if it has
/// a tuple type. Otherwise, add this value directly to `results`.
void extractAllElements(SILValue value, SILBuilder &builder,
                        SmallVectorImpl<SILValue> &results);

/// Emit a zero value into the given buffer access by calling
/// `AdditiveArithmetic.zero`. The given type must conform to
/// `AdditiveArithmetic`.
void emitZeroIntoBuffer(SILBuilder &builder, CanType type,
                        SILValue bufferAccess, SILLocation loc);

//===----------------------------------------------------------------------===//
// Utilities for looking up derivatives of functions
//===----------------------------------------------------------------------===//

/// Returns a differentiability witness (definition or declaration) exactly
/// matching the specified indices. If none are found in the given `module`,
/// returns `nullptr`.
///
/// \param parameterIndices must be lowered to SIL.
/// \param resultIndices must be lowered to SIL.
SILDifferentiabilityWitness *
getExactDifferentiabilityWitness(SILModule &module, SILFunction *original,
                                 IndexSubset *parameterIndices,
                                 IndexSubset *resultIndices);

/// Finds the derivative configuration (from `@differentiable` and
/// `@derivative` attributes) for `original` whose parameter indices are a
/// minimal superset of the specified AST parameter indices. Returns `None` if
/// no such configuration is found.
///
/// \param parameterIndices must be lowered to SIL.
/// \param minimalASTParameterIndices is an output parameter that is set to the
/// AST indices of the minimal configuration, or to `nullptr` if no such
/// configuration exists.
Optional<AutoDiffConfig>
findMinimalDerivativeConfiguration(AbstractFunctionDecl *original,
                                   IndexSubset *parameterIndices,
                                   IndexSubset *&minimalASTParameterIndices);

/// Returns a differentiability witness for `original` whose parameter indices
/// are a minimal superset of the specified parameter indices and whose result
/// indices match the given result indices, out of all
/// differentiability witnesses that come from AST "@differentiable" or
/// "@differentiating" attributes.
///
/// This function never creates new differentiability witness definitions.
/// However, this function may create new differentiability witness declarations
/// referring to definitions in other modules when these witnesses have not yet
/// been declared in the current module.
///
/// \param module is the SILModule in which to get or create the witnesses.
/// \param parameterIndices must be lowered to SIL.
/// \param resultIndices must be lowered to SIL.
SILDifferentiabilityWitness *getOrCreateMinimalASTDifferentiabilityWitness(
    SILModule &module, SILFunction *original, IndexSubset *parameterIndices,
    IndexSubset *resultIndices);

} // end namespace autodiff

/// Creates arguments in the entry block based on the function type.
inline void createEntryArguments(SILFunction *f) {
  auto *entry = f->getEntryBlock();
  auto conv = f->getConventions();
  auto &ctx = f->getASTContext();
  auto moduleDecl = f->getModule().getSwiftModule();
  assert((entry->getNumArguments() == 0 || conv.getNumSILArguments() == 0) &&
         "Entry already has arguments?!");
  auto createFunctionArgument = [&](SILType type) {
    // Create a dummy parameter declaration.
    // Necessary to prevent crash during argument explosion optimization.
    auto loc = f->getLocation().getSourceLoc();
    auto *decl = new (ctx)
        ParamDecl(loc, loc, Identifier(), loc, Identifier(), moduleDecl);
    decl->setSpecifier(ParamDecl::Specifier::Default);
    entry->createFunctionArgument(type, decl);
  };
  // f->getLoweredFunctionType()->remap
  for (auto indResTy : conv.getIndirectSILResultTypes()) {
    if (indResTy.hasArchetype())
      indResTy = indResTy.mapTypeOutOfContext();
    createFunctionArgument(f->mapTypeIntoContext(indResTy).getAddressType());
    // createFunctionArgument(indResTy.getAddressType());
  }
  for (auto paramTy : conv.getParameterSILTypes()) {
    if (paramTy.hasArchetype())
      paramTy = paramTy.mapTypeOutOfContext();
    createFunctionArgument(f->mapTypeIntoContext(paramTy));
    // createFunctionArgument(paramTy);
  }
}

/// Cloner that remaps types using the target function's generic environment.
class BasicTypeSubstCloner final
    : public TypeSubstCloner<BasicTypeSubstCloner, SILOptFunctionBuilder> {

  static SubstitutionMap getSubstitutionMap(SILFunction *target) {
    if (auto *targetGenEnv = target->getGenericEnvironment())
      return targetGenEnv->getForwardingSubstitutionMap();
    return SubstitutionMap();
  }

public:
  explicit BasicTypeSubstCloner(SILFunction *original, SILFunction *target)
      : TypeSubstCloner(*target, *original, getSubstitutionMap(target)) {}

  void postProcess(SILInstruction *orig, SILInstruction *cloned) {
    SILClonerWithScopes::postProcess(orig, cloned);
  }

  void cloneFunction() {
    auto &newFunction = Builder.getFunction();
    auto *entry = newFunction.createBasicBlock();
    createEntryArguments(&newFunction);
    SmallVector<SILValue, 8> entryArguments(newFunction.getArguments().begin(),
                                            newFunction.getArguments().end());
    cloneFunctionBody(&Original, entry, entryArguments);
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_COMMON_H

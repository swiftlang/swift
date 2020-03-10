//===--- Common.h - Automatic differentiation common utils ----*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// Automatic differentiation common utilities.
//
// NOTE: Though automatic differentiation is developed as part of the Swift for
// TensorFlow project, it is completely independent from TensorFlow.
// Read the differentiable programming manifesto for more information:
// docs/DifferentiableProgramming.md.
//
// TODO: Move definitions from lib/SILOptimizer/Mandatory/Differentiation.cpp.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_COMMON_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_COMMON_H

#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"

namespace swift {

using llvm::DenseMap;
using llvm::SmallDenseMap;
using llvm::SmallDenseSet;
using llvm::SmallMapVector;
using llvm::SmallSet;

class ApplyInst;
class DifferentiableActivityInfo;

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

namespace autodiff {

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
raw_ostream &getADDebugStream();

/// Returns true if this is an full apply site whose callee has
/// `array.uninitialized_intrinsic` semantics.
bool isArrayLiteralIntrinsic(FullApplySite applySite);

/// If the given value `v` corresponds to an `ApplyInst` with
/// `array.uninitialized_intrinsic` semantics, returns the corresponding
/// `ApplyInst`. Otherwise, returns `nullptr`.
ApplyInst *getAllocateUninitializedArrayIntrinsic(SILValue v);

/// Given an element address from an `array.uninitialized_intrinsic` `apply`
/// instruction, returns the `apply` instruction. The element address is either
/// a `pointer_to_address` or `index_addr` instruction to the `RawPointer`
/// result of the instrinsic:
///
///     %result = apply %array.uninitialized_intrinsic : $(Array<T>, RawPointer)
///     (%array, %ptr) = destructure_tuple %result
///     %elt0 = pointer_to_address %ptr to $*T       // element address
///     %index_1 = integer_literal $Builtin.Word, 1
///     %elt1 = index_addr %elt0, %index_1           // element address
///     ...
ApplyInst *getAllocateUninitializedArrayIntrinsicElementAddress(SILValue v);

/// Given a value, finds its single `destructure_tuple` user if the value is
/// tuple-typed and such a user exists.
DestructureTupleInst *getSingleDestructureTupleUser(SILValue value);

/// Given a full apply site, apply the given callback to each of its
/// "direct results".
///
/// - `apply`
/// Special case because `apply` returns a single (possibly tuple-typed) result
/// instead of multiple results. If the `apply` has a single
/// `destructure_tuple` user, treat the `destructure_tuple` results as the
/// `apply` direct results.
///
/// - `begin_apply`
/// Apply callback to each `begin_apply` direct result.
///
/// - `try_apply`
/// Apply callback to each `try_apply` successor basic block argument.
void forEachApplyDirectResult(
    FullApplySite applySite, llvm::function_ref<void(SILValue)> resultCallback);

/// Given a function, gathers all of its formal results (both direct and
/// indirect) in an order defined by its result type. Note that "formal results"
/// refer to result values in the body of the function, not at call sites.
void collectAllFormalResultsInTypeOrder(SILFunction &function,
                                        SmallVectorImpl<SILValue> &results);

/// Given a function, gathers all of its direct results in an order defined by
/// its result type. Note that "formal results" refer to result values in the
/// body of the function, not at call sites.
void collectAllDirectResultsInTypeOrder(SILFunction &function,
                                        SmallVectorImpl<SILValue> &results);

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

/// For an `apply` instruction with active results, compute:
/// - The results of the `apply` instruction, in type order.
/// - The set of minimal parameter and result indices for differentiating the
///   `apply` instruction.
void collectMinimalIndicesForFunctionCall(
    ApplyInst *ai, SILAutoDiffIndices parentIndices,
    const DifferentiableActivityInfo &activityInfo,
    SmallVectorImpl<SILValue> &results, SmallVectorImpl<unsigned> &paramIndices,
    SmallVectorImpl<unsigned> &resultIndices);

/// Emit a zero value into the given buffer access by calling
/// `AdditiveArithmetic.zero`. The given type must conform to
/// `AdditiveArithmetic`.
void emitZeroIntoBuffer(SILBuilder &builder, CanType type,
                        SILValue bufferAccess, SILLocation loc);

/// Given a range of elements, joins these into a single value. If there's
/// exactly one element, returns that element. Otherwise, creates a tuple using
/// a `tuple` instruction.
SILValue joinElements(ArrayRef<SILValue> elements, SILBuilder &builder,
                      SILLocation loc);

/// Given a value, extracts all elements to `results` from this value if it has
/// a tuple type. Otherwise, add this value directly to `results`.
void extractAllElements(SILValue value, SILBuilder &builder,
                        SmallVectorImpl<SILValue> &results);

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

/// Helper class for visiting basic blocks in post-order post-dominance order,
/// based on a worklist algorithm.
class PostOrderPostDominanceOrder {
  SmallVector<DominanceInfoNode *, 16> buffer;
  PostOrderFunctionInfo *postOrderInfo;
  size_t srcIdx = 0;

public:
  /// Constructor.
  /// \p root The root of the post-dominator tree.
  /// \p postOrderInfo The post-order info of the function.
  /// \p capacity Should be the number of basic blocks in the dominator tree to
  ///             reduce memory allocation.
  PostOrderPostDominanceOrder(DominanceInfoNode *root,
                              PostOrderFunctionInfo *postOrderInfo,
                              int capacity = 0)
      : postOrderInfo(postOrderInfo) {
    buffer.reserve(capacity);
    buffer.push_back(root);
  }

  /// Get the next block from the worklist.
  DominanceInfoNode *getNext() {
    if (srcIdx == buffer.size())
      return nullptr;
    return buffer[srcIdx++];
  }

  /// Pushes the dominator children of a block onto the worklist in post-order.
  void pushChildren(DominanceInfoNode *node) {
    pushChildrenIf(node, [](SILBasicBlock *) { return true; });
  }

  /// Conditionally pushes the dominator children of a block onto the worklist
  /// in post-order.
  template <typename Pred>
  void pushChildrenIf(DominanceInfoNode *node, Pred pred) {
    SmallVector<DominanceInfoNode *, 4> children;
    for (auto *child : *node)
      children.push_back(child);
    llvm::sort(children.begin(), children.end(),
               [&](DominanceInfoNode *n1, DominanceInfoNode *n2) {
                 return postOrderInfo->getPONumber(n1->getBlock()) <
                        postOrderInfo->getPONumber(n2->getBlock());
               });
    for (auto *child : children) {
      SILBasicBlock *childBB = child->getBlock();
      if (pred(childBB))
        buffer.push_back(child);
    }
  }
};

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

  void run() {
    auto &target = Builder.getFunction();
    auto *entry = target.createBasicBlock();
    createEntryArguments(&target);
    SmallVector<SILValue, 8> entryArguments(target.getArguments().begin(),
                                            target.getArguments().end());
    cloneFunctionBody(&Original, entry, entryArguments);
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_COMMON_H

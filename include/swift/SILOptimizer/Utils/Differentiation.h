//===--- Differentiation.h - SIL Automatic Differentiation ----*- C++ -*---===//
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
// Reverse-mode automatic differentiation utilities.
//
// NOTE: Although the AD feature is developed as part of the Swift for
// TensorFlow project, it is completely independent from TensorFlow support.
//
// TODO: Move definitions from lib/SILOptimizer/Mandatory/Differentiation.cpp.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_H

#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"

namespace swift {

using llvm::DenseMap;
using llvm::SmallDenseMap;
using llvm::SmallDenseSet;
using llvm::SmallMapVector;
using llvm::SmallSet;

class ApplyInst;

//===----------------------------------------------------------------------===//
// Helpers
//===----------------------------------------------------------------------===//

namespace autodiff {

/// Prints an "[AD] " prefix to `llvm::dbgs()` and returns the debug stream.
/// This is being used to print short debug messages within the AD pass.
raw_ostream &getADDebugStream();

/// If the given value `v` corresponds to an `ApplyInst` with
/// `array.uninitialized_intrinsic` semantics, returns the corresponding
/// `ApplyInst`. Otherwise, returns `nullptr`.
ApplyInst *getAllocateUninitializedArrayIntrinsic(SILValue v);

/// Given an `apply` instruction, apply the given callback to each of its
/// direct results. If the `apply` instruction has a single `destructure_tuple`
/// user, apply the callback to the results of the `destructure_tuple` user.
void forEachApplyDirectResult(
    ApplyInst *ai, llvm::function_ref<void(SILValue)> resultCallback);

/// Given a function, gathers all of its formal results (both direct and
/// indirect) in an order defined by its result type. Note that "formal results"
/// refer to result values in the body of the function, not at call sites.
void collectAllFormalResultsInTypeOrder(SILFunction &function,
                                        SmallVectorImpl<SILValue> &results);

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
  for (auto indResTy : conv.getIndirectSILResultTypes())
    createFunctionArgument(f->mapTypeIntoContext(indResTy).getAddressType());
  for (auto paramTy : conv.getParameterSILTypes())
    createFunctionArgument(f->mapTypeIntoContext(paramTy));
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

#endif // SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_H

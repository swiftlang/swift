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
// TODO: Move definitions here from Differentiation.cpp.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_H
#define SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_H

#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/Utils/Local.h"

namespace swift {

using llvm::DenseMap;
using llvm::SmallDenseMap;
using llvm::SmallDenseSet;
using llvm::SmallMapVector;
using llvm::SmallSet;

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

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_MANDATORY_DIFFERENTIATION_H

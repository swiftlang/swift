//===--- Dominance.h - SIL dominance analysis ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides interfaces for computing and working with
// control-flow dominance in SIL.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_DOMINANCE_H
#define SWIFT_SIL_DOMINANCE_H

#include "llvm/Support/GenericDomTree.h"
#include "swift/SIL/CFG.h"

extern template class llvm::DominatorTreeBase<swift::SILBasicBlock>;
extern template class llvm::DominatorBase<swift::SILBasicBlock>;
extern template class llvm::DomTreeNodeBase<swift::SILBasicBlock>;

namespace swift {

typedef llvm::DomTreeNodeBase<SILBasicBlock> DominanceInfoNode;

/// A class for computing basic dominance information.
class DominanceInfo : public llvm::DominatorTreeBase<SILBasicBlock> {
public:
  DominanceInfo(SILFunction *F);

  /// Does instruction A properly dominate instruction B?
  bool properlyDominates(SILInstruction *a, SILInstruction *b);

  void verify() const;

  /// Return true if the other dominator tree does not match this dominator
  /// tree.
  inline bool errorOccuredOnComparison(const DominanceInfo &Other) const {
    const auto *R = getRootNode();
    const auto *OtherR = Other.getRootNode();

    if (!R || !OtherR || R->getBlock() != OtherR->getBlock())
      return true;

    // Returns *false* if they match.
    if (compare(Other))
      return true;

    return false;
  }

  using DominatorTreeBase::properlyDominates;

  bool isValid(SILFunction *F) const {
    return getNode(&F->front()) != nullptr;
  }
  void reset() {
    llvm::DominatorTreeBase<SILBasicBlock>::reset();
  }
};

/// Helper class for visiting basic blocks in dominance order, based on a
/// worklist algorithm. Example usage:
/// \code
///   DominanceOrder DomOrder(Function->front(), DominanceInfo);
///   while (SILBasicBlock *block = DomOrder.getNext()) {
///     doSomething(block);
///     domOrder.pushChildren(block);
///   }
/// \endcode
class DominanceOrder {
  
  SmallVector<SILBasicBlock *, 16> buffer;
  DominanceInfo *DT;
  size_t srcIdx = 0;
  
public:
  
  /// Constructor.
  /// \p entry The root of the dominator (sub-)tree.
  /// \p DT The dominance info of the function.
  /// \p capacity Should be the number of basic blocks in the dominator tree to
  ///             reduce memory allocation.
  DominanceOrder(SILBasicBlock *root, DominanceInfo *DT, int capacity = 0) :
            DT(DT) {
     buffer.reserve(capacity);
     buffer.push_back(root);
  }

  /// Gets the next block from the worklist.
  ///
  SILBasicBlock *getNext() {
    if (srcIdx == buffer.size())
      return nullptr;
    return buffer[srcIdx++];
  }

  /// Pushes the dominator children of a block onto the worklist.
  void pushChildren(SILBasicBlock *block) {
    pushChildrenIf(block, [] (SILBasicBlock *) { return true; });
  }

  /// Conditionally pushes the dominator children of a block onto the worklist.
  /// \p pred Takes a block (= a dominator child) as argument and returns true
  ///         if it should be added to the worklist.
  ///
  template <typename Pred> void pushChildrenIf(SILBasicBlock *block, Pred pred) {
    DominanceInfoNode *DINode = DT->getNode(block);
    for (auto *DIChild : *DINode) {
      SILBasicBlock *child = DIChild->getBlock();
      if (pred(child))
        buffer.push_back(DIChild->getBlock());
    }
  }
};

/// A class for computing basic post-dominance information.
class PostDominanceInfo : public llvm::DominatorTreeBase<SILBasicBlock> {
public:
  PostDominanceInfo(SILFunction *F);

  bool properlyDominates(SILInstruction *A, SILInstruction *B);

  using DominatorTreeBase::properlyDominates;
};


}  // end namespace swift

namespace llvm {

/// DominatorTree GraphTraits specialization so the DominatorTree can be
/// iterable by generic graph iterators.
template <> struct GraphTraits<swift::DominanceInfoNode *> {
  typedef swift::DominanceInfoNode NodeType;
  typedef NodeType::iterator ChildIteratorType;

  static NodeType *getEntryNode(NodeType *N) { return N; }
  static inline ChildIteratorType child_begin(NodeType *N) {
    return N->begin();
  }
  static inline ChildIteratorType child_end(NodeType *N) { return N->end(); }
};

} // end namespace llvm
#endif

//===--- StackPromotion.cpp - Promotes allocations to the stack -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "stack-promotion"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/CFG.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/GenericDomTreeConstruction.h"
#include "llvm/ADT/Statistic.h"

STATISTIC(NumStackPromoted, "Number of objects promoted to the stack");

using namespace swift;

/// Promotes heap allocated objects to the stack.
/// Following types of allocations are handled:
/// *) alloc_ref instructions of native swift classes: if promoted, the [stack]
///    attribute is set in the alloc_ref and a dealloc_ref [stack] is inserted
///    at the end of the object's lifetime.
/// *) Array buffers which are allocated by a call to swift_bufferAllocate: if
///    promoted the swift_bufferAllocate call is replaced by a call to
///    swift_bufferAllocateOnStack and a call to swift_bufferDeallocateFromStack
///    is inserted at the end of the buffer's lifetime.
///    Those calls are lowered by the LLVM SwiftStackPromotion pass.
///    TODO: This is a terrible hack, but necessary because we need constant
///    size and alignment for the final stack promotion decision. The arguments
///    to swift_bufferAllocate in SIL are not constant because they depend on
///    the not-yet-evaluatable sizeof and alignof builtins. Therefore we need
///    LLVM's constant propagation prior to deciding on stack promotion.
///    The solution to this problem is that we need native support for tail-
///    allocated arrays in SIL so that we can do the array buffer allocations
///    with alloc_ref instructions.
class StackPromoter {

  // Some analysis we need.

  SILFunction *F;
  EscapeAnalysis::ConnectionGraph *ConGraph;
  DominanceInfo *DT;
  EscapeAnalysis *EA;

  // We use our own post-dominator tree instead of PostDominatorAnalysis,
  // because we ignore unreachable blocks (actually all unreachable sub-graphs).
  // Example:
  //              |
  //             bb1
  //            /   \
  //   unreachable  bb2
  //                  \
  //
  // We want to get bb2 as immediate post-dominator of bb1. This is not the case
  // with the regular post-dominator tree.
  llvm::DominatorTreeBase<SILBasicBlock> PostDomTree;

  bool PostDomTreeValid;

  // Pseudo-functions for (de-)allocating array buffers on the stack.

  SILFunction *BufferAllocFunc = nullptr;
  SILFunction *BufferDeallocFunc = nullptr;

  bool ChangedInsts = false;
  bool ChangedCalls = false;

  /// Worklist for visiting all blocks.
  class WorkListType {
    /// The nesting depth of stack allocation instructions for each block.
    /// A value of -1 means: not known yet.
    /// A value of -2 means: not known and not visited yet.
    /// All blocks in this map with a value >= -1 are already visited.
    llvm::DenseMap<SILBasicBlock *, int> Block2StackDepth;

    /// The work list of not yet handled blocks.
    llvm::SmallVector<SILBasicBlock *, 8> ToHandle;

  public:
    bool empty() const { return ToHandle.empty(); }

    SILBasicBlock *pop_back_val() { return ToHandle.pop_back_val(); }

    /// Insert a block into the worklist and set its stack depth.
    void insert(SILBasicBlock *BB, int StackDepth) {
      auto Iter = Block2StackDepth.find(BB);
      if (Iter != Block2StackDepth.end() && Iter->second >= -1) {
        // We already handled the block.
        assert(StackDepth >= 0);
        if (Iter->second < 0) {
          // Update the stack depth if we didn't set it yet for the block.
          Iter->second = StackDepth;
        } else {
          assert(Iter->second == StackDepth &&
                   "inconsistent stack depth at a CFG merge point");
        }
      } else {
        Block2StackDepth[BB] = StackDepth;
        ToHandle.push_back(BB);
      }
    }

    bool insertAsUnhandled(SILBasicBlock *Pred) {
      return Block2StackDepth.insert({Pred, -2}).second;
    }

    int getStackDepth(SILBasicBlock *BB) {
      assert(Block2StackDepth.find(BB) != Block2StackDepth.end());
      int Depth = Block2StackDepth.lookup(BB);
      assert(Depth >= 0 && "EndBlock not reachable from StartBlock");
      return Depth;
    }
  };

  /// Tries to promote the allocation \p AI.
  void tryPromoteAlloc(SILInstruction *AI);

  /// Creates the external declaration for swift_bufferAllocateOnStack.
  SILFunction *getBufferAllocFunc(SILFunction *OrigFunc,
                                  SILLocation Loc);

  /// Creates the external declaration for swift_bufferDeallocateFromStack.
  SILFunction *getBufferDeallocFunc(SILFunction *OrigFunc,
                                    SILLocation Loc);

  /// Returns true if the allocation \p AI can be promoted.
  /// In this case it sets the \a DeallocInsertionPoint to the instruction
  /// where the deallocation must be inserted.
  /// It optionally also sets \a AllocInsertionPoint in case the allocation
  /// instruction must be moved to another place.
  bool canPromoteAlloc(SILInstruction *AI,
                       SILInstruction *&AllocInsertionPoint,
                       SILInstruction *&DeallocInsertionPoint);

  /// Returns the place where to insert the deallocation.
  /// Returns null if this doesn't succeed or, in case \p RestartPoint is set,
  /// a new iteration should be triggered.
  SILInstruction *findDeallocPoint(SILInstruction *StartInst,
                               SILInstruction *&RestartPoint,
                               EscapeAnalysis::CGNode *Node,
                               int NumUsePointsToFind);

  /// If \p CurrentBB is in a loop update the \p EndBlock so that it post-
  /// dominates the loop.
  /// Returns the new EndBlock or null if no one could be found.
  SILBasicBlock *updateEndBlock(SILBasicBlock *CurrentBB,
                                SILBasicBlock *EndBlock,
                                WorkListType &WorkList);

  bool strictlyDominates(SILBasicBlock *A, SILBasicBlock *B) {
    return A != B && DT->dominates(A, B);
  }

  bool strictlyPostDominates(SILBasicBlock *A, SILBasicBlock *B) {
    calculatePostDomTree();
    return A != B && PostDomTree.dominates(A, B);
  }

  bool postDominates(SILBasicBlock *A, SILBasicBlock *B) {
    calculatePostDomTree();
    return PostDomTree.dominates(A, B);
  }

  SILBasicBlock *getImmediatePostDom(SILBasicBlock *BB) {
    calculatePostDomTree();
    auto *Node = PostDomTree.getNode(BB);
    if (!Node)
      return nullptr;
    auto *IDomNode = Node->getIDom();
    if (!IDomNode)
      return nullptr;
    return IDomNode->getBlock();
  }
  
  void calculatePostDomTree() {
    if (!PostDomTreeValid) {
      // The StackPromoter acts as a "graph" for which the post-dominator-tree
      // is calculated.
      PostDomTree.recalculate(*this);
      PostDomTreeValid = true;
    }
  }

public:

  StackPromoter(SILFunction *F, EscapeAnalysis::ConnectionGraph *ConGraph,
                DominanceInfo *DT, EscapeAnalysis *EA) :
    F(F), ConGraph(ConGraph), DT(DT), EA(EA), PostDomTree(true),
    PostDomTreeValid(false) { }

  /// What did the optimization change?
  enum class ChangeState {
    None,
    Insts,
    Calls
  };

  SILFunction *getFunction() const { return F; }

  /// The main entry point for the optimization.
  ChangeState promote();
};

/// Returns true if instruction \p I is an allocation we can handle.
static bool isPromotableAllocInst(SILInstruction *I) {
  // Check for swift object allocation.
  if (auto *ARI = dyn_cast<AllocRefInst>(I)) {
    if (!ARI->isObjC())
      return true;
    return false;
  }
  // Check for array buffer allocation.
  auto *AI = dyn_cast<ApplyInst>(I);
  if (AI && AI->getNumArguments() == 3) {
    if (auto *Callee = AI->getReferencedFunction()) {
      if (Callee->getName() == "swift_bufferAllocate")
        return true;
    }
    return false;
  }
  return false;
}

StackPromoter::ChangeState StackPromoter::promote() {

  llvm::SetVector<SILBasicBlock *> ReachableBlocks;

  // First step: find blocks which end up in a no-return block (terminated by
  // an unreachable instruction).
  // Search for function-exiting blocks, i.e. return and throw.
  for (SILBasicBlock &BB : *F) {
    TermInst *TI = BB.getTerminator();
    if (TI->isFunctionExiting())
      ReachableBlocks.insert(&BB);
  }
  // Propagate the reachability up the control flow graph.
  unsigned Idx = 0;
  while (Idx < ReachableBlocks.size()) {
    SILBasicBlock *BB = ReachableBlocks[Idx++];
    for (SILBasicBlock *Pred : BB->getPreds())
      ReachableBlocks.insert(Pred);
  }

  // Search the whole function for stack promotable allocations.
  for (SILBasicBlock &BB : *F) {

    // Don't stack promote any allocation inside a code region which ends up in
    // a no-return block. Such allocations may missing their final release.
    // We would insert the deallocation too early, which may result in a
    // use-after-free problem.
    if (ReachableBlocks.count(&BB) == 0)
      continue;

    for (auto Iter = BB.begin(); Iter != BB.end();) {
      // The allocation instruction may be moved, so increment Iter prior to
      // doing the optimization.
      SILInstruction *I = &*Iter++;
      if (isPromotableAllocInst(I)) {
        tryPromoteAlloc(I);
      }
    }
  }
  if (ChangedCalls)
    return ChangeState::Calls;
  if (ChangedInsts)
    return ChangeState::Insts;
  return ChangeState::None;
}

void StackPromoter::tryPromoteAlloc(SILInstruction *I) {
  SILInstruction *AllocInsertionPoint = nullptr;
  SILInstruction *DeallocInsertionPoint = nullptr;
  if (!canPromoteAlloc(I, AllocInsertionPoint, DeallocInsertionPoint))
    return;

  DEBUG(llvm::dbgs() << "Promoted " << *I);
  DEBUG(llvm::dbgs() << "    in " << I->getFunction()->getName() << '\n');
  NumStackPromoted++;

  SILBuilder B(DeallocInsertionPoint);
  if (auto *ARI = dyn_cast<AllocRefInst>(I)) {
    // It's an object allocation. We set the [stack] attribute in the alloc_ref.
    ARI->setStackAllocatable();
    if (AllocInsertionPoint)
      ARI->moveBefore(AllocInsertionPoint);

    /// And create a dealloc_ref [stack] at the end of the object's lifetime.
    B.createDeallocRef(I->getLoc(), I, true);
    ChangedInsts = true;
    return;
  }
  if (auto *AI = dyn_cast<ApplyInst>(I)) {
    assert(!AllocInsertionPoint && "can't move call to swift_bufferAlloc");
    // It's an array buffer allocation.
    auto *OldFRI = cast<FunctionRefInst>(AI->getCallee());
    SILFunction *OldF = OldFRI->getReferencedFunction();
    SILLocation loc = (OldF->hasLocation() ? OldF->getLocation() : AI->getLoc());
    SILFunction *DeallocFun = getBufferDeallocFunc(OldF, loc);

    // We insert a swift_bufferDeallocateFromStack at the end of the buffer's
    // lifetime.
    auto *DeallocFRI = B.createFunctionRef(OldFRI->getLoc(), DeallocFun);
    B.createApply(loc, DeallocFRI, { AI }, false);

    // And replace the call to swift_bufferAllocate with a call to
    // swift_bufferAllocateOnStack.
    B.setInsertionPoint(AI);
    auto *AllocFRI = B.createFunctionRef(OldFRI->getLoc(),
                                    getBufferAllocFunc(OldF, loc));
    AI->setOperand(0, AllocFRI);

    ChangedCalls = true;
    return;
  }
  llvm_unreachable("unhandled allocation instruction");
}

SILFunction *StackPromoter::getBufferAllocFunc(SILFunction *OrigFunc,
                                               SILLocation Loc) {
  if (!BufferAllocFunc) {
    BufferAllocFunc = OrigFunc->getModule().getOrCreateFunction(
                          Loc,
                          "swift_bufferAllocateOnStack",
                          OrigFunc->getLinkage(),
                          OrigFunc->getLoweredFunctionType(),
                          OrigFunc->isBare(), IsNotTransparent,
                          OrigFunc->isFragile());
  }
  return BufferAllocFunc;
}

SILFunction *StackPromoter::getBufferDeallocFunc(SILFunction *OrigFunc,
                                                 SILLocation Loc) {
  if (!BufferDeallocFunc) {
    SILModule &M = OrigFunc->getModule();
    CanSILFunctionType OrigTy = OrigFunc->getLoweredFunctionType();
    CanType ObjectTy = OrigTy->getSILResult().getSwiftRValueType();

    // The function type for swift_bufferDeallocateFromStack.
    CanSILFunctionType FunTy = SILFunctionType::get(
      OrigTy->getGenericSignature(),
      OrigTy->getExtInfo(),
      OrigTy->getCalleeConvention(),
      { SILParameterInfo(ObjectTy, ParameterConvention::Direct_Guaranteed) },
      ArrayRef<SILResultInfo>(),
      OrigTy->getOptionalErrorResult(),
      M.getASTContext());

    BufferDeallocFunc = M.getOrCreateFunction(
      Loc,
      "swift_bufferDeallocateFromStack",
      OrigFunc->getLinkage(),
      FunTy,
      OrigFunc->isBare(), IsNotTransparent, OrigFunc->isFragile());
  }
  return BufferDeallocFunc;
}

namespace {

/// Iterator which iterates over all basic blocks of a function which are not
/// terminated by an unreachable inst.
class NonUnreachableBlockIter :
public std::iterator<std::forward_iterator_tag, SILBasicBlock, ptrdiff_t> {

  SILFunction::iterator BaseIterator;
  SILFunction::iterator End;

  void skipUnreachables() {
    while (true) {
      if (BaseIterator == End)
        return;
      if (!isa<UnreachableInst>(BaseIterator->getTerminator()))
        return;
      BaseIterator++;
    }
  }

public:
  NonUnreachableBlockIter(SILFunction::iterator BaseIterator,
                          SILFunction::iterator End) :
      BaseIterator(BaseIterator), End(End) {
    skipUnreachables();
  }

  NonUnreachableBlockIter() = default;
  
  SILBasicBlock &operator*() const { return *BaseIterator; }
  SILBasicBlock &operator->() const { return *BaseIterator; }
  
  NonUnreachableBlockIter &operator++() {
    BaseIterator++;
    skipUnreachables();
    return *this;
  }
  
  NonUnreachableBlockIter operator++(int unused) {
    NonUnreachableBlockIter Copy = *this;
    ++*this;
    return Copy;
  }

  friend bool operator==(NonUnreachableBlockIter lhs,
                         NonUnreachableBlockIter rhs) {
    return lhs.BaseIterator == rhs.BaseIterator;
  }
  friend bool operator!=(NonUnreachableBlockIter lhs,
                         NonUnreachableBlockIter rhs) {
    return !(lhs == rhs);
  }
};
}

namespace llvm {

/// Use the StackPromoter as a wrapper for the function. It holds the list of
/// basic blocks excluding all unreachable blocks.
template <> struct GraphTraits<StackPromoter *>
    : public GraphTraits<swift::SILBasicBlock*> {
  typedef StackPromoter *GraphType;

  static NodeType *getEntryNode(GraphType SP) {
    return &SP->getFunction()->front();
  }

  typedef NonUnreachableBlockIter nodes_iterator;
  static nodes_iterator nodes_begin(GraphType SP) {
    return nodes_iterator(SP->getFunction()->begin(), SP->getFunction()->end());
  }
  static nodes_iterator nodes_end(GraphType SP) {
    return nodes_iterator(SP->getFunction()->end(), SP->getFunction()->end());
  }
  static unsigned size(GraphType SP) {
    return std::distance(nodes_begin(SP), nodes_end(SP));
  }
};

}

bool StackPromoter::canPromoteAlloc(SILInstruction *AI,
                                    SILInstruction *&AllocInsertionPoint,
                                    SILInstruction *&DeallocInsertionPoint) {
  AllocInsertionPoint = nullptr;
  DeallocInsertionPoint = nullptr;
  auto *Node = ConGraph->getNodeOrNull(AI, EA);
  if (!Node)
    return false;

  // The most important check: does the object escape the current function?
  if (Node->escapes())
    return false;

  // Now we have to determine the lifetime of the allocated object in its
  // function.

  // Get all interesting uses of the object (e.g. release instructions). This
  // includes uses of objects where the allocation is stored to.
  int NumUsePointsToFind = ConGraph->getNumUsePoints(Node);
  if (NumUsePointsToFind == 0) {
    // There should always be at least one release for an allocated object.
    // But in case all paths from this block end in unreachable then the
    // final release of the object may be optimized away. We bail out in this
    // case.
    return false;
  }

  // Try to find the point where to insert the deallocation.
  // This might need more than one try in case we need to move the allocation
  // out of a stack-alloc-dealloc pair. See findDeallocPoint().
  SILInstruction *StartInst = AI;
  for (;;) {
    SILInstruction *RestartPoint = nullptr;
    DeallocInsertionPoint = findDeallocPoint(StartInst, RestartPoint, Node,
                                             NumUsePointsToFind);
    if (DeallocInsertionPoint)
      return true;

    if (!RestartPoint)
      return false;

    // Moving a buffer allocation call is not trivial because we would need to
    // move all the parameter calculations as well. So we just don't do it.
    if (!isa<AllocRefInst>(AI))
      return false;

    // Retry with moving the allocation up.
    AllocInsertionPoint = RestartPoint;
    StartInst = RestartPoint;
  }
}

SILInstruction *StackPromoter::findDeallocPoint(SILInstruction *StartInst,
                                                SILInstruction *&RestartPoint,
                                                EscapeAnalysis::CGNode *Node,
                                                int NumUsePointsToFind) {
  // In the following we check two requirements for stack promotion:
  // 1) Are all uses in the same control region as the alloc? E.g. if the
  //    allocation is in a loop then there may not be any uses of the object
  //    outside the loop.
  // 2) We need to find an insertion place for the deallocation so that it
  //    preserves a properly nested stack allocation-deallocation structure.
  SILBasicBlock *StartBlock = StartInst->getParent();

  // The block where we assume we can insert the deallocation.
  SILBasicBlock *EndBlock = StartBlock;

  // We visit all instructions starting at the allocation instruction.
  WorkListType WorkList;
  // It's important that the EndBlock is at the head of the WorkList so that
  // we handle it after all other blocks.
  WorkList.insert(EndBlock, -1);
  WorkList.insert(StartBlock, 0);

  for (;;) {
    SILBasicBlock *BB = WorkList.pop_back_val();
    int StackDepth = 0;
    SILBasicBlock::iterator Iter;
    if (BB == StartBlock) {
      // In the first block we start at the allocation instruction and not at
      // the begin of the block.
      Iter = StartInst->getIterator();
    } else {
      // Track all uses in the block arguments.
      for (SILArgument *BBArg : BB->getBBArgs()) {
        if (ConGraph->isUsePoint(BBArg, Node))
          NumUsePointsToFind--;
      }
      // Make sure that the EndBlock is not inside a loop (which does not
      // contain the StartBlock).
      // E.g.:
      //     %obj = alloc_ref // the allocation
      //     br loop
      //   loop:
      //     the_only_use_of_obj(%obj)
      //     cond_br ..., loop, exit
      //   exit:
      //     ... // this is the new EndBlock
      EndBlock = updateEndBlock(BB, EndBlock, WorkList);
      if (!EndBlock)
        return nullptr;
      Iter = BB->begin();
      StackDepth = WorkList.getStackDepth(BB);
    }
    // Visit all instructions of the current block.
    while (Iter != BB->end()) {
      SILInstruction &I = *Iter++;
      if (BB == EndBlock && StackDepth == 0 && NumUsePointsToFind == 0) {
        // We found a place to insert the stack deallocation.
        return &I;
      }
      if (I.isAllocatingStack()) {
        StackDepth++;
      } else if (I.isDeallocatingStack()) {
        if (StackDepth == 0) {
          // The allocation is inside a stack alloc-dealloc region and we are
          // now leaving this region without having found a place for the
          // deallocation. E.g.
          // E.g.:
          //     %1 = alloc_stack
          //     %obj = alloc_ref // the allocation
          //     dealloc_stack %1
          //     use_of_obj(%obj)
          //
          // In this case we can move the alloc_ref before the alloc_stack
          // to fix the nesting.
          auto *Alloc = dyn_cast<SILInstruction>(I.getOperand(0));
          if (!Alloc)
            return nullptr;

          // This should always be the case, but let's be on the safe side.
          if (!postDominates(StartBlock, Alloc->getParent()))
            return nullptr;

          // Trigger another iteration with a new start point;
          RestartPoint = Alloc;
          return nullptr;
        }
        StackDepth--;
      }
      // Track a use.
      if (ConGraph->isUsePoint(&I, Node) != 0)
        NumUsePointsToFind--;
    }
    if (WorkList.empty()) {
      if (EndBlock == BB) {
        // We reached the EndBlock but didn't find a place for the deallocation
        // so far (because we didn't find all uses yet or we entered another
        // stack alloc-dealloc region). Let's extend our lifetime region.
        // E.g.:
        //     %obj = alloc_ref // the allocation
        //     %1 = alloc_stack
        //     use_of_obj(%obj) // can't insert the deallocation in this block
        //     cond_br ..., bb1, bb2
        //   bb1:
        //     ...
        //     br bb2
        //   bb2:
        //     dealloc_stack %1 // this is the new EndBlock
        EndBlock = getImmediatePostDom(EndBlock);
        if (!EndBlock)
          return nullptr;
      }
      // Again, it's important that the EndBlock is the first in the WorkList.
      WorkList.insert(EndBlock, -1);
    }
    // Push the successor blocks to the WorkList.
    for (SILBasicBlock *Succ : BB->getSuccessors()) {
      if (!strictlyDominates(StartBlock, Succ)) {
        // The StartBlock is inside a loop but we couldn't find a deallocation
        // place in this loop, e.g. because there are uses outside the loop.
        // E.g.:
        //     %container = alloc_ref
        //     br loop
        //   loop:
        //     %obj = alloc_ref // the allocation
        //     store %obj to %some_field_in_container
        //     cond_br ..., loop, exit
        //   exit:
        //     use(%container)
        return nullptr;
      }
      WorkList.insert(Succ, StackDepth);
    }
  }
}

SILBasicBlock *StackPromoter::updateEndBlock(SILBasicBlock *CurrentBB,
                                             SILBasicBlock *EndBlock,
                                             WorkListType &WorkList) {
  llvm::SmallVector<SILBasicBlock *, 8> PredsToHandle;
  PredsToHandle.push_back(CurrentBB);

  // Starting from BB, go back the control flow graph until we reach already
  // handled blocks.
  while (!PredsToHandle.empty()) {
    SILBasicBlock *BB = PredsToHandle.pop_back_val();
    for (SILBasicBlock *Pred : BB->getPreds()) {
      // Make sure that the EndBlock post-dominates all blocks we are visiting.
      while (!strictlyPostDominates(EndBlock, Pred)) {
        EndBlock = getImmediatePostDom(EndBlock);
        if (!EndBlock)
          return nullptr;
      }
      if (WorkList.insertAsUnhandled(Pred))
        PredsToHandle.push_back(Pred);
    }
  }
  return EndBlock;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

namespace {

class StackPromotion : public SILFunctionTransform {

public:
  StackPromotion() {}

private:
  /// The entry point to the transformation.
  void run() override {
    DEBUG(llvm::dbgs() << "** StackPromotion **\n");

    auto *EA = PM->getAnalysis<EscapeAnalysis>();
    auto *DA = PM->getAnalysis<DominanceAnalysis>();

    SILFunction *F = getFunction();
    if (auto *ConGraph = EA->getConnectionGraph(F)) {
      StackPromoter promoter(F, ConGraph, DA->get(F), EA);
      switch (promoter.promote()) {
        case StackPromoter::ChangeState::None:
          break;
        case StackPromoter::ChangeState::Insts:
          invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
          break;
        case StackPromoter::ChangeState::Calls:
          invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
          break;
      }
    }
  }

  StringRef getName() override { return "StackPromotion"; }
};

} // end anonymous namespace

SILTransform *swift::createStackPromotion() {
  return new StackPromotion();
}

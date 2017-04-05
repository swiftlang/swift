//===--- ARCCodeMotion.cpp - SIL ARC Code Motion --------------------------===//
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
///
/// \file
///
/// This pass moves retains down and releases up. This, hopefully, will help
/// ARC sequence opt to remove retain and release pairs without worrying too
/// much about control flows.
///
/// It uses an optimistic iterative data flow to compute where to insert the
/// retains and releases for every reference-counted root. It then removes all
/// the old retain and release instructions and create the new ones.
///
/// This pass is more sophisticated than SILCodeMotion, as arc optimizations
/// can be very beneficial, use an optimistic global data flow to achieve
/// optimality.
///
/// Proof of Correctness:
/// -------------------
///
/// 1. Retains are blocked by MayDecrements. Its straightforward to prove that
/// retain sinking is correct. 
/// 
/// If a retain is sunk from Region A to Region B, that means there is no
/// blocking operation between where the retain was in Region A to where it is
/// sunk to in Region B. Since we only sink retains (we do not move any other
/// instructions) which themselves are NOT MayDecrement operations, and moving
/// retains can't turn non-decrement instruction MayDecrement.
///
/// 2. Releases are blocked by MayInterfere. If a release is hoisted from
/// Region B to Region A, that means there is no blocking operation from where
/// the release was in Region B and where the release is hoisted to in Region A.
///
/// The question is whether we can introduce such operation while we hoist
/// other releases. The answer is NO. because if such releases exist, they
/// would be blocked by the old release (we remove old release and recreate new
/// ones at the end of the pass) and will not be able to be hoisted beyond the
/// old release.
///
/// This proof also hinges on the fact that if release A interferes with
/// releases B then release B must interfere with release A. i.e. the 2
/// releases must have the symmetric property. Consider the 2 releases as 2
/// function calls, i.e. CallA (release A) and CallB (release B), if CallA
/// interferes with CallB, that means CallA must share some program states
/// (through read or write) with CallB. Then it is not possible for CallB
/// to not share any states with CallA. And if they do share states, then
/// its not possible for CallB to block CallA and CallA not to block CallB.
///
/// TODO: Sinking retains can block releases to be hoisted, and hoisting
/// releases can block retains to be sunk. Investigate when to sink retains and
/// when to hoist releases and their ordering in the pass pipeline.
///
/// TODO: Consider doing retain hoisting and release sinking. This can help
/// to discover disjoint lifetimes and we can try to stitch them together.
///
/// TODO: There are a lot of code duplications between retain and release code
/// motion in the data flow part. Consider whether we can share them.
/// Essentially, we can implement the release code motion by inverting the 
/// retain code motion, but this can also make the code less readable.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-rr-code-motion"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/RCIdentityAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumRetainsSunk, "Number of retains sunk");
STATISTIC(NumReleasesHoisted, "Number of releases hoisted");

llvm::cl::opt<bool> DisableARCCodeMotion("disable-arc-cm", llvm::cl::init(false));

/// Disable optimization if we have to break critical edges in the function.
llvm::cl::opt<bool>
DisableIfWithCriticalEdge("disable-with-critical-edge", llvm::cl::init(false));

//===----------------------------------------------------------------------===//
//                             Block State 
//===----------------------------------------------------------------------===//

struct BlockState {
  /// A bit vector for which the ith bit represents the ith refcounted root in
  /// RCRootVault.
  ///
  /// NOTE: we could do the data flow with BBSetIn or BBSetOut, but that would
  /// require us to create a temporary copy to check whether the BBSet has
  /// changed after the genset and killset has been applied.
  llvm::SmallBitVector BBSetIn;

  /// A bit vector for which the ith bit represents the ith refcounted root in
  /// RCRootVault.
  llvm::SmallBitVector BBSetOut;

  /// A bit vector for which the ith bit represents the ith refcounted root in
  /// RCRootVault. If the bit is set, that means this basic block creates a
  /// retain which can be sunk or a release which can be hoisted.
  llvm::SmallBitVector BBGenSet;

  /// A bit vector for which the ith bit represents the ith refcounted root in
  /// RCRootVault. If this bit is set, that means this basic block stops retain
  /// or release of the refcounted root to be moved across.
  llvm::SmallBitVector BBKillSet;

  /// A bit vector for which the ith bit represents the ith refcounted root in
  /// RCRootVault. If this bit is set, that means this is potentially a retain
  /// or release that can be sunk or hoisted to this point. This is used to
  /// optimize the time for computing genset and killset.
  ///
  /// NOTE: this vector contains an approximation of whether there will be a
  /// retain or release to a certain point of a basic block.
  llvm::SmallBitVector BBMaxSet;
};

/// CodeMotionContext - This is the base class which retain code motion and
/// release code motion inherits from. It defines an interface as to how the
/// code motion procedure should be.
class CodeMotionContext {
protected:
  /// Dataflow needs multiple iteration to converge. If this is false, then we
  /// do not need to generate the genset or killset, i.e. we can simply do 1
  /// pessimistic data flow iteration.
  bool MultiIteration;

  /// The allocator we are currently using.
  llvm::SpecificBumpPtrAllocator<BlockState> &BPA;

  /// Current function we are analyzing.
  SILFunction *F;

  /// Current post-order we are using.
  PostOrderFunctionInfo *PO;

  /// Current alias analysis we are using.
  AliasAnalysis *AA;

  /// Current rc-identity we are using.
  RCIdentityFunctionInfo *RCFI;

  /// All the unique refcount roots retained or released in the function.
  llvm::SetVector<SILValue> RCRootVault;

  /// Contains a map between RC roots to their index in the RCRootVault.
  /// used to facilitate fast RC roots to index lookup.
  llvm::DenseMap<SILValue, unsigned> RCRootIndex;

  /// All the retains or releases originally in the function. Eventually
  /// they will all be removed after all the new ones are generated.
  llvm::SmallPtrSet<SILInstruction *, 8> RCInstructions;

  /// All the places to place the new retains or releases after code motion.
  using InsertPointList = llvm::SmallVector<SILInstruction *, 2>;
  llvm::SmallDenseMap<SILValue, InsertPointList> InsertPoints;

  /// These are the blocks that have an RC instruction to process or it blocks
  /// some RC instructions. If the basic block has neither, we do not need to
  /// process the block again in the last iteration. We populate this set when
  /// we compute the genset and killset.
  llvm::SmallPtrSet<SILBasicBlock *, 8> InterestBlocks;

  /// Return the rc-identity root of the SILValue.
  SILValue getRCRoot(SILValue R) {
     return RCFI->getRCIdentityRoot(R);
  }

  /// Return the rc-identity root of the RC instruction, i.e.
  /// retain or release.
  SILValue getRCRoot(SILInstruction *I) {
    assert(isRetainInstruction(I) || isReleaseInstruction(I) &&
           "Extracting RC root from invalid instruction");
    return getRCRoot(I->getOperand(0));
  }

public:
  /// Constructor.
  CodeMotionContext(llvm::SpecificBumpPtrAllocator<BlockState> &BPA,
                    SILFunction *F,
                    PostOrderFunctionInfo *PO, AliasAnalysis *AA,
                    RCIdentityFunctionInfo *RCFI)
    : MultiIteration(true), BPA(BPA), F(F), PO(PO), AA(AA), RCFI(RCFI) {}

  /// virtual destructor.
  virtual ~CodeMotionContext() {}

  /// Run the data flow to move retains and releases.
  bool run();

  /// Check whether we need to run an optimistic iteration data flow.
  /// or a pessimistic would suffice.
  virtual bool requireIteration() = 0;

  /// Initialize necessary things to run the iterative data flow.
  virtual void initializeCodeMotionDataFlow() = 0;

  /// Initialize the basic block maximum refcounted set.
  virtual void initializeCodeMotionBBMaxSet() = 0;

  /// Compute the genset and killset for every root in every basic block.
  virtual void computeCodeMotionGenKillSet() = 0;

  /// Run the iterative data flow to converge.
  virtual void convergeCodeMotionDataFlow() = 0; 

  /// Use the data flow results, come up with places to insert the new inst.
  virtual void computeCodeMotionInsertPoints() = 0;

  /// Remove the old retains and create the new *moved* refcounted instructions
  virtual bool performCodeMotion() = 0;

  /// Merge the data flow states.
  virtual void mergeBBDataFlowStates(SILBasicBlock *BB) = 0;

  /// Compute the BBSetIn and BBSetOut for the current basic
  /// block with the generated gen and kill set.
  virtual bool processBBWithGenKillSet(SILBasicBlock *BB) = 0;

  /// Return true if the instruction blocks the Ptr to be moved further.
  virtual bool mayBlockCodeMotion(SILInstruction *II, SILValue Ptr) = 0;
};

bool CodeMotionContext::run() {
  // Initialize the data flow.
  initializeCodeMotionDataFlow();

  // Converge the BBSetOut with iterative data flow.
  if (MultiIteration) {
    initializeCodeMotionBBMaxSet();
    computeCodeMotionGenKillSet();
    convergeCodeMotionDataFlow();
  }

  // Compute the insertion point where each RC root can be moved to.
  computeCodeMotionInsertPoints();

  // Finally, generate new retains and remove the old retains.
  return performCodeMotion();
}

//===----------------------------------------------------------------------===//
//                          Retain Code Motion 
//===----------------------------------------------------------------------===//

class RetainBlockState : public BlockState {
public:
  /// Check whether the BBSetOut has changed. If it does, we need to rerun
  /// the data flow on this block's successors to reach fixed point.
  bool updateBBSetOut(llvm::SmallBitVector &X) {
    if (BBSetOut == X)
      return false;
    BBSetOut = X;
    return true;
  }

  /// constructor.
  RetainBlockState(bool IsEntry, unsigned size, bool MultiIteration) {
    // Iterative forward data flow.
    BBSetIn.resize(size, false);
    // Initialize to true if we are running optimistic data flow, i.e.
    // MultiIteration is true.
    BBSetOut.resize(size, MultiIteration);
    BBMaxSet.resize(size, !IsEntry && MultiIteration);
  
    // Genset and Killset are initially empty.
    BBGenSet.resize(size, false);
    BBKillSet.resize(size, false);
  }
};

/// RetainCodeMotionContext - Context to perform retain code motion.
class RetainCodeMotionContext : public CodeMotionContext {
  /// All the retain block state for all the basic blocks in the function. 
  llvm::SmallDenseMap<SILBasicBlock *, RetainBlockState *> BlockStates;

  /// Return true if the instruction blocks the Ptr to be moved further.
  bool mayBlockCodeMotion(SILInstruction *II, SILValue Ptr) override {
    // NOTE: If more checks are to be added, place the most expensive in the
    // end, this function is called many times.
    //
    // These terminator instructions block.
    if (isa<ReturnInst>(II) || isa<ThrowInst>(II) || isa<UnreachableInst>(II))
      return true;
    // Identical RC root blocks code motion, we will be able to move this retain
    // further once we move the blocking retain.
    if (isRetainInstruction(II) && getRCRoot(II) == Ptr)
      return true;
    // Ref count checks do not have side effects, but are barriers for retains.
    if (mayCheckRefCount(II))
      return true;
    // mayDecrement reference count stops code motion.
    if (mayDecrementRefCount(II, Ptr, AA)) 
      return true;
    // This instruction does not block the retain code motion.
    return false;
  }

  /// Return the previous instruction if it happens to be a retain with the
  /// given RC root, nullptr otherwise.
  SILInstruction *getPrevReusableInst(SILInstruction *I, SILValue Root) {
    if (&*I->getParent()->begin() == I)
      return nullptr;
    auto Prev = &*std::prev(SILBasicBlock::iterator(I));
    if (isRetainInstruction(Prev) && getRCRoot(Prev) == Root)
      return Prev;
    return nullptr;
  }

public:
  /// Constructor.
  RetainCodeMotionContext(llvm::SpecificBumpPtrAllocator<BlockState> &BPA,
                          SILFunction *F, PostOrderFunctionInfo *PO,
                          AliasAnalysis *AA, RCIdentityFunctionInfo *RCFI)
    : CodeMotionContext(BPA, F, PO, AA, RCFI) {
    MultiIteration = requireIteration();
  }

  /// virtual destructor.
  ~RetainCodeMotionContext() override {}

  /// Return true if we do not need optimistic data flow.
  bool requireIteration() override;

  /// Initialize necessary things to run the iterative data flow.
  void initializeCodeMotionDataFlow() override;

  /// Initialize the basic block maximum refcounted set.
  void initializeCodeMotionBBMaxSet() override;

  /// Compute the genset and killset for every root in every basic block.
  void computeCodeMotionGenKillSet() override;

  /// Run the iterative data flow to converge.
  void convergeCodeMotionDataFlow() override;

  /// Use the data flow results, come up with places to insert the new inst.
  void computeCodeMotionInsertPoints() override;

  /// Remove the old retains and create the new *moved* refcounted instructions
  bool performCodeMotion() override;

  /// Compute the BBSetIn and BBSetOut for the current basic block with the
  /// generated gen and kill set.
  bool processBBWithGenKillSet(SILBasicBlock *BB) override;

  /// Merge the data flow states.
  void mergeBBDataFlowStates(SILBasicBlock *BB) override;
};

bool RetainCodeMotionContext::requireIteration() {
  // If all basic blocks will have their predecessors processed if the basic
  // blocks in the functions are iterated in reverse post order. Then this
  // function can be processed in one iteration, i.e. no need to generate the
  // genset and killset.
  llvm::SmallPtrSet<SILBasicBlock *, 4> PBBs;
  for (SILBasicBlock *B : PO->getReversePostOrder()) {
    for (auto X : B->getPredecessorBlocks()) {
      if (!PBBs.count(X))
        return true;
    }
    PBBs.insert(B);
  }
  return false;
}

void RetainCodeMotionContext::initializeCodeMotionDataFlow() {
  // Find all the RC roots in the function.
  for (auto &BB : *F) {
    for (auto &II : BB) {
      if (!isRetainInstruction(&II))
        continue;
      RCInstructions.insert(&II);
      SILValue Root = getRCRoot(&II);
      if (RCRootIndex.find(Root) != RCRootIndex.end())
        continue;
      RCRootIndex[Root] = RCRootVault.size();
      RCRootVault.insert(Root);
    }
  }

  // Initialize all the data flow bit vector for all basic blocks.
  for (auto &BB : *F) {
    BlockStates[&BB] = new (BPA.Allocate())
                            RetainBlockState(&BB == &*F->begin(),
                            RCRootVault.size(), MultiIteration);
  }
}

void RetainCodeMotionContext::initializeCodeMotionBBMaxSet() {
  for (SILBasicBlock *BB : PO->getReversePostOrder()) {
    // If basic block has no predecessor, do nothing.
    BlockState *State = BlockStates[BB];
    if (BB->pred_empty()) {
      State->BBMaxSet.reset();
    } else {
      // Intersect in all predecessors' BBSetOut.
      State->BBMaxSet.set();
      for (auto E = BB->pred_end(), I = BB->pred_begin(); I != E; ++I) {
        State->BBMaxSet &= BlockStates[*I]->BBMaxSet;
     }
   }

   // Process the instructions in the basic block to find what refcounted
   // roots are retained. If we know that an RC root can't be retained at a
   // basic block, then we know we do not need to consider it for the killset.
   // NOTE: this is a conservative approximation, because some retains may be
   // blocked before it reaches this block.
   for (auto &II : *BB) {
      if (!isRetainInstruction(&II))
        continue;
      State->BBMaxSet.set(RCRootIndex[getRCRoot(&II)]);
    }
  }
}

void RetainCodeMotionContext::computeCodeMotionGenKillSet() {
  for (SILBasicBlock *BB : PO->getReversePostOrder()) {
    auto *State = BlockStates[BB];
    bool InterestBlock = false; 
    for (auto &I : *BB) {
      // Check whether this instruction blocks any RC root code motion.
      for (unsigned i = 0; i < RCRootVault.size(); ++i) {
        if (!State->BBMaxSet.test(i) || !mayBlockCodeMotion(&I, RCRootVault[i]))
          continue;
        // This is a blocking instruction for the rcroot.
        InterestBlock = true;
        State->BBKillSet.set(i);
        State->BBGenSet.reset(i);
      }
      // If this is a retain instruction, it also generates.
      if (isRetainInstruction(&I)) {
        unsigned idx = RCRootIndex[getRCRoot(&I)];
        State->BBGenSet.set(idx);
        assert(State->BBKillSet.test(idx) && "Killset computed incorrectly");
        State->BBKillSet.reset(idx);
        InterestBlock = true;
      }
    }

    // Is this a block that is interesting to the last iteration of the data
    // flow.
    if (!InterestBlock)
      continue;
    InterestBlocks.insert(BB);
  }
}

bool RetainCodeMotionContext::performCodeMotion() {
  bool Changed = false;
  // Create the new retain instructions.
  for (auto RC : RCRootVault) {
    auto Iter = InsertPoints.find(RC);
    if (Iter == InsertPoints.end())
      continue;
    for (auto IP : Iter->second) {
      // we are about to insert a new retain instruction before the insertion
      // point. Check if the previous instruction is reusable, reuse it, do
      // not insert new instruction and delete old one.
      if (auto I = getPrevReusableInst(IP, Iter->first)) {
        RCInstructions.erase(I);
        continue;
      }
      createIncrementBefore(Iter->first, IP);
      Changed = true;
    }
  }
  // Remove the old retain instructions.
  for (auto R : RCInstructions) {
    ++NumRetainsSunk;
    recursivelyDeleteTriviallyDeadInstructions(R, true);
  }
  return Changed;
}

void RetainCodeMotionContext::mergeBBDataFlowStates(SILBasicBlock *BB) {
  BlockState *State = BlockStates[BB];
  State->BBSetIn.reset();
  // If basic block has no predecessor, simply reset and return.
  if (BB->pred_empty())
    return;

  // Intersect in all predecessors' BBSetOuts.
  auto Iter = BB->pred_begin();
  State->BBSetIn = BlockStates[*Iter]->BBSetOut;
  Iter = std::next(Iter);
  for (auto E = BB->pred_end(); Iter != E; ++Iter) {
    State->BBSetIn &= BlockStates[*Iter]->BBSetOut;
  }
}

bool RetainCodeMotionContext::processBBWithGenKillSet(SILBasicBlock *BB) {
  RetainBlockState *State = BlockStates[BB];
  // Compute the BBSetOut at the end of the basic block.
  mergeBBDataFlowStates(BB);

  // Compute the BBSetIn at the beginning of the basic block.
  State->BBSetIn.reset(State->BBKillSet);
  State->BBSetIn |= State->BBGenSet;
 
  // If BBSetIn changes, then keep iterating until reached a fixed point.
  return State->updateBBSetOut(State->BBSetIn);
}

void RetainCodeMotionContext::convergeCodeMotionDataFlow() {
  // Process each basic block with the genset and killset. Every time the
  // BBSetOut of a basic block changes, the optimization is rerun on its
  // successors. 
  llvm::SmallVector<SILBasicBlock *, 16> WorkList;
  llvm::SmallPtrSet<SILBasicBlock *, 4> HandledBBs;
  // Push into reverse post order so that we can pop from the back and get
  // post order.
  for (SILBasicBlock *B : PO->getReversePostOrder()) {
    WorkList.push_back(B);
    HandledBBs.insert(B);
  }
  while (!WorkList.empty()) {
    SILBasicBlock *BB = WorkList.pop_back_val();
    HandledBBs.erase(BB);
    if (processBBWithGenKillSet(BB)) {
      for (auto &X : BB->getSuccessors()) {
        // We do not push basic block into the worklist if its already 
        // in the worklist.
        if (HandledBBs.count(X))
          continue;
        WorkList.push_back(X);
      }
    }
  }
}

void RetainCodeMotionContext::computeCodeMotionInsertPoints() {
  // The BBSetOuts have converged, run last iteration and figure out
  // insertion point for each refcounted root.
  for (SILBasicBlock *BB : PO->getReversePostOrder()) {
    mergeBBDataFlowStates(BB);
    RetainBlockState *S = BlockStates[BB];

    // Compute insertion point generated by the edge value transition.
    // If there is a transition from 1 to 0, that means we have a partial
    // merge, which means the retain can NOT be sunk to the current block,
    // so place it at the end of the predecessors.
    for (unsigned i = 0; i < RCRootVault.size(); ++i) {
      if (S->BBSetIn[i])
        continue;
      for (auto Pred : BB->getPredecessorBlocks()) {
        BlockState *PBB = BlockStates[Pred];
        if (!PBB->BBSetOut[i])
          continue;
        InsertPoints[RCRootVault[i]].push_back(Pred->getTerminator());
      }
    }

    // Is this block interesting. If we are sure this block does not generate
    // retains nor does it block any retains (i.e. no insertion point will be
    // created), we can skip it, as the BBSetOut has been converged if this is
    // a multi-iteration function.
    if (MultiIteration && !InterestBlocks.count(BB))
      continue;

    // Compute insertion point within the basic block. Process instructions in
    // the basic block in reverse post-order fashion.
    for (auto I = BB->begin(), E = BB->end(); I != E; ++I) {
      for (unsigned i = 0; i < RCRootVault.size(); ++i) {
        if (!S->BBSetIn[i] || !mayBlockCodeMotion(&*I, RCRootVault[i]))
          continue;
        S->BBSetIn.reset(i);
        InsertPoints[RCRootVault[i]].push_back(&*I);
      }

      // If this is a retain instruction, it also generates.
      if (isRetainInstruction(&*I)) {
        S->BBSetIn.set(RCRootIndex[getRCRoot(&*I)]);
      }
    }

    // Lastly update the BBSetOut, only necessary when we are running a single
    // iteration dataflow.
    if (!MultiIteration) {
      S->updateBBSetOut(S->BBSetIn);
    }
  }
}

//===----------------------------------------------------------------------===//
//                          Release Code Motion 
//===----------------------------------------------------------------------===//

class ReleaseBlockState : public BlockState {
public:
  /// Check whether the BBSetIn has changed. If it does, we need to rerun
  /// the data flow on this block's predecessors to reach fixed point.
  bool updateBBSetIn(llvm::SmallBitVector &X) {
    if (BBSetIn == X)
      return false;
    BBSetIn = X;
    return true;
  }

  /// constructor.
  ReleaseBlockState(bool IsExit, unsigned size, bool MultiIteration) {
    // backward data flow.
    // Initialize to true if we are running optimistic data flow, i.e.
    // MultiIteration is true.
    BBSetIn.resize(size, MultiIteration);
    BBSetOut.resize(size, false);
    BBMaxSet.resize(size, !IsExit && MultiIteration);
  
    // Genset and Killset are initially empty.
    BBGenSet.resize(size, false);
    BBKillSet.resize(size, false);
  }
};

/// ReleaseCodeMotionContext - Context to perform release code motion.
class ReleaseCodeMotionContext : public CodeMotionContext {
  /// All the release block state for all the basic blocks in the function. 
  llvm::SmallDenseMap<SILBasicBlock *, ReleaseBlockState *> BlockStates;

  /// We are not moving epilogue releases.
  bool FreezeEpilogueReleases;

  /// The epilogue release matcher we are currently using.
  ConsumedArgToEpilogueReleaseMatcher &ERM;

  /// Return true if the instruction blocks the Ptr to be moved further.
  bool mayBlockCodeMotion(SILInstruction *II, SILValue Ptr) override {
    // NOTE: If more checks are to be added, place the most expensive in the end.
    // This function is called many times.
    //
    // We can not move a release above the instruction that defines the
    // released value.
    if (II == Ptr)
      return true;
    // Identical RC root blocks code motion, we will be able to move this release
    // further once we move the blocking release.
    if (isReleaseInstruction(II) && getRCRoot(II) == Ptr)
      return true;
    // Stop at may interfere.
    if (mayHaveSymmetricInterference(II, Ptr, AA))
      return true;
    // This instruction does not block the release.
    return false;
  }

  /// Return the successor instruction if it happens to be a release with the
  /// given RC root, nullptr otherwise.
  SILInstruction *getPrevReusableInst(SILInstruction *I, SILValue Root) {
    if (&*I->getParent()->begin() == I)
      return nullptr;
    auto Prev = &*std::prev(SILBasicBlock::iterator(I));
    if (isReleaseInstruction(Prev) && getRCRoot(Prev) == Root)
      return Prev;
    return nullptr;
  }

public:
  /// Constructor.
  ReleaseCodeMotionContext(llvm::SpecificBumpPtrAllocator<BlockState> &BPA,
                           SILFunction *F, PostOrderFunctionInfo *PO,
                           AliasAnalysis *AA, RCIdentityFunctionInfo *RCFI,
                           bool FreezeEpilogueReleases,
                           ConsumedArgToEpilogueReleaseMatcher &ERM)
    : CodeMotionContext(BPA, F, PO, AA, RCFI),
      FreezeEpilogueReleases(FreezeEpilogueReleases), ERM(ERM) {
    MultiIteration = requireIteration();
  } 

  /// virtual destructor.
  ~ReleaseCodeMotionContext() override {}

  /// Return true if the data flow can converge in 1 iteration.
  bool requireIteration() override;

  /// Initialize necessary things to run the iterative data flow.
  void initializeCodeMotionDataFlow() override;

  /// Initialize the basic block maximum refcounted set.
  void initializeCodeMotionBBMaxSet() override;

  /// Compute the genset and killset for every root in every basic block.
  void computeCodeMotionGenKillSet() override;

  /// Run the iterative data flow to converge.
  void convergeCodeMotionDataFlow() override;

  /// Use the data flow results, come up with places to insert the new inst.
  void computeCodeMotionInsertPoints() override;

  /// Remove the old retains and create the new *moved* refcounted instructions
  bool performCodeMotion() override;

  /// Compute the BBSetIn and BBSetOut for the current basic
  /// block with the generated gen and kill set.
  bool processBBWithGenKillSet(SILBasicBlock *BB) override;

  /// Merge the data flow states.
  void mergeBBDataFlowStates(SILBasicBlock *BB) override;
};

bool ReleaseCodeMotionContext::requireIteration() {
  // If all basic blocks will have their successors processed if the basic
  // blocks in the functions are iterated in post order. Then this function
  // can be processed in one iteration, i.e. no need to generate the genset
  // and killset.
  llvm::SmallPtrSet<SILBasicBlock *, 4> PBBs;
  for (SILBasicBlock *B : PO->getPostOrder()) {
    for (auto &X : B->getSuccessors()) {
      if (!PBBs.count(X))
        return true;
    }
    PBBs.insert(B);
  }
  return false;
}

void ReleaseCodeMotionContext::initializeCodeMotionDataFlow() {
  // Find all the RC roots in the function.
  for (auto &BB : *F) {
    for (auto &II : BB) {
      if (!isReleaseInstruction(&II))
        continue;
      // Do not try to enumerate if we are not hoisting epilogue releases.
      if (FreezeEpilogueReleases && ERM.isEpilogueRelease(&II))
        continue;
      SILValue Root = getRCRoot(&II);
      RCInstructions.insert(&II);
      if (RCRootIndex.find(Root) != RCRootIndex.end())
        continue;
      RCRootIndex[Root] = RCRootVault.size();
      RCRootVault.insert(Root);
    }
  }

  // Initialize all the data flow bit vector for all basic blocks.
  for (auto &BB : *F) {
    BlockStates[&BB] = new (BPA.Allocate())
            ReleaseBlockState(BB.getTerminator()->isFunctionExiting(),
                              RCRootVault.size(), MultiIteration);
  }
}

void ReleaseCodeMotionContext::initializeCodeMotionBBMaxSet() {
  for (SILBasicBlock *BB : PO->getPostOrder()) {
    // If basic block has no successor, do nothing.
    BlockState *State = BlockStates[BB];
    if (BB->succ_empty()) {
      State->BBMaxSet.reset();
    } else {
      // Intersect in all successors' BBMaxOuts.
      State->BBMaxSet.set();
      for (auto E = BB->succ_end(), I = BB->succ_begin(); I != E; ++I) {
        State->BBMaxSet &= BlockStates[*I]->BBMaxSet;
     }
   }

   // Process the instructions in the basic block to find what refcounted
   // roots are released. If we know that an RC root can't be released at a
   // basic block, then we know we do not need to consider it for the killset.
   // NOTE: this is a conservative approximation, because some releases may be
   // blocked before it reaches this block.
   for (auto II = BB->rbegin(), IE = BB->rend(); II != IE; ++II) {
      if (!isReleaseInstruction(&*II))
        continue;
      State->BBMaxSet.set(RCRootIndex[getRCRoot(&*II)]);
    }
  }
}

void ReleaseCodeMotionContext::computeCodeMotionGenKillSet() {
  for (SILBasicBlock *BB : PO->getPostOrder()) {
    auto *State = BlockStates[BB];
    bool InterestBlock = false;
    for (auto I = BB->rbegin(), E = BB->rend(); I != E; ++I) {
      // Check whether this instruction blocks any RC root code motion.
      for (unsigned i = 0; i < RCRootVault.size(); ++i) {
        if (!State->BBMaxSet.test(i) || !mayBlockCodeMotion(&*I, RCRootVault[i]))
          continue;
        // This instruction blocks this RC root.
        InterestBlock = true;
        State->BBKillSet.set(i);
        State->BBGenSet.reset(i);
      }

      // If this is an epilogue release and we are freezing epilogue release
      // simply continue.
      if (FreezeEpilogueReleases && ERM.isEpilogueRelease(&*I))
        continue;

      // If this is a release instruction, it also generates.
      if (isReleaseInstruction(&*I)) {
        unsigned idx = RCRootIndex[getRCRoot(&*I)];
        State->BBGenSet.set(idx);
        assert(State->BBKillSet.test(idx) && "Killset computed incorrectly");
        State->BBKillSet.reset(idx);
        InterestBlock = true;
      }
    }

    // Handle SILArgument, SILArgument can invalidate.
    for (unsigned i = 0; i < RCRootVault.size(); ++i) {
      SILArgument *A = dyn_cast<SILArgument>(RCRootVault[i]);
      if (!A || A->getParent() != BB)
        continue;
      InterestBlock = true;
      State->BBKillSet.set(i);
      State->BBGenSet.reset(i);
    }

    // Is this interesting to the last iteration of the data flow.
    if (!InterestBlock)
      continue;
    InterestBlocks.insert(BB);
  }
}

void ReleaseCodeMotionContext::mergeBBDataFlowStates(SILBasicBlock *BB) {
  BlockState *State = BlockStates[BB];
  State->BBSetOut.reset();
  // If basic block has no successor, simply reset and return.
  if (BB->succ_empty())
    return;

  // Intersect in all successors' BBSetIn.
  auto Iter = BB->succ_begin();
  State->BBSetOut = BlockStates[*Iter]->BBSetIn;
  Iter = std::next(Iter);
  for (auto E = BB->succ_end(); Iter != E; ++Iter) {
    State->BBSetOut &= BlockStates[*Iter]->BBSetIn;
  }
}

bool ReleaseCodeMotionContext::performCodeMotion() {
  bool Changed = false;
  // Create the new releases at each anchor point.
  for (auto RC : RCRootVault) {
    auto Iter = InsertPoints.find(RC);
    if (Iter == InsertPoints.end())
      continue;
    for (auto IP : Iter->second) {
      // we are about to insert a new release instruction before the insertion
      // point. Check if the successor instruction is reusable, reuse it, do
      // not insert new instruction and delete old one.
      if (auto I = getPrevReusableInst(IP, Iter->first)) {
        RCInstructions.erase(I);
        continue;
      }
      createDecrementBefore(Iter->first, IP);
      Changed = true;
    }
  }
  // Remove the old release instructions.
  for (auto R : RCInstructions) {
    ++NumReleasesHoisted;
    recursivelyDeleteTriviallyDeadInstructions(R, true);
  }
  return Changed;
}

bool ReleaseCodeMotionContext::processBBWithGenKillSet(SILBasicBlock *BB) {
  ReleaseBlockState *State = BlockStates[BB];
  // Compute the BBSetOut at the end of the basic block.
  mergeBBDataFlowStates(BB);

  // Compute the BBSetIn at the beginning of the basic block.
  State->BBSetOut.reset(State->BBKillSet);
  State->BBSetOut |= State->BBGenSet;
 
  // If BBSetIn changes, then keep iterating until reached a fixed point.
  return State->updateBBSetIn(State->BBSetOut);
}

void ReleaseCodeMotionContext::convergeCodeMotionDataFlow() {
  // Process each basic block with the gen and kill set. Every time the
  // BBSetIn of a basic block changes, the optimization is rerun on its
  // predecessors.
  llvm::SmallVector<SILBasicBlock *, 16> WorkList;
  llvm::SmallPtrSet<SILBasicBlock *, 8> HandledBBs;
  // Push into reverse post order so that we can pop from the back and get
  // post order.
  for (SILBasicBlock *B : PO->getPostOrder()) {
    WorkList.push_back(B);
    HandledBBs.insert(B);
  }
  while (!WorkList.empty()) {
    SILBasicBlock *BB = WorkList.pop_back_val();
    HandledBBs.erase(BB);
    if (processBBWithGenKillSet(BB)) {
      for (auto X : BB->getPredecessorBlocks()) {
        // We do not push basic block into the worklist if its already 
        // in the worklist.
        if (HandledBBs.count(X))
          continue;
        WorkList.push_back(X);
      }
    }
  }
}

void ReleaseCodeMotionContext::computeCodeMotionInsertPoints() {
  // The BBSetIns have converged, run last iteration and figure out insertion
  // point for each RC root.
  for (SILBasicBlock *BB : PO->getPostOrder()) {
    // Intersect in the successor BBSetIns.
    mergeBBDataFlowStates(BB);
    ReleaseBlockState *S = BlockStates[BB];

    // Compute insertion point generated by the edge value transition.
    // If there is a transition from 1 to 0, that means we have a partial
    // merge, which means the release can NOT be hoisted to the current block.
    // place it at the successors.
    for (unsigned i = 0; i < RCRootVault.size(); ++i) {  
      if (S->BBSetOut[i])
        continue;
      for (auto &Succ : BB->getSuccessors()) {
        BlockState *SBB = BlockStates[Succ];
        if (!SBB->BBSetIn[i])
          continue;
        InsertPoints[RCRootVault[i]].push_back(&*(*Succ).begin());
      }
    }

    // Is this block interesting ?
    if (MultiIteration && !InterestBlocks.count(BB))
      continue;

    // Compute insertion point generated by MayUse terminator inst.
    // If terminator instruction can block the RC root. We will have no
    // choice but to anchor the release instructions in the successor blocks.
    for (unsigned i = 0; i < RCRootVault.size(); ++i) {
      SILInstruction *Term = BB->getTerminator();
      if (!S->BBSetOut[i] || !mayBlockCodeMotion(Term, RCRootVault[i]))
        continue;
      for (auto &Succ : BB->getSuccessors()) {
        BlockState *SBB = BlockStates[Succ];
        if (!SBB->BBSetIn[i])
          continue;
        InsertPoints[RCRootVault[i]].push_back(&*(*Succ).begin());
      }
      S->BBSetOut.reset(i);
    }

    // Compute insertion point generated within the basic block. Process
    // instructions in post-order fashion.
    for (auto I = std::next(BB->rbegin()), E = BB->rend(); I != E; ++I) {
      for (unsigned i = 0; i < RCRootVault.size(); ++i) {
        if (!S->BBSetOut[i] || !mayBlockCodeMotion(&*I, RCRootVault[i]))
          continue;
        auto *InsertPt = &*std::next(SILBasicBlock::iterator(&*I));
        InsertPoints[RCRootVault[i]].push_back(InsertPt);
        S->BBSetOut.reset(i);
      }

      // If we are freezing this epilogue release. Simply continue.
      if (FreezeEpilogueReleases && ERM.isEpilogueRelease(&*I))
        continue;

      // This release generates.
      if (isReleaseInstruction(&*I)) {
        S->BBSetOut.set(RCRootIndex[getRCRoot(&*I)]);
      }
    }

    // Compute insertion point generated by SILArgument. SILArgument blocks if
    // it defines the released value.
    for (unsigned i = 0; i < RCRootVault.size(); ++i) {
      if (!S->BBSetOut[i]) 
        continue;
      SILArgument *A = dyn_cast<SILArgument>(RCRootVault[i]);
      if (!A || A->getParent() != BB)
        continue;
      InsertPoints[RCRootVault[i]].push_back(&*BB->begin());
      S->BBSetOut.reset(i);
    }
   
    // Lastly update the BBSetIn, only necessary when we are running a single
    // iteration dataflow.
    if (!MultiIteration) {
      S->updateBBSetIn(S->BBSetOut);
    }
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

/// Code motion kind.
enum CodeMotionKind : unsigned { Retain = 0, Release = 1};

class ARCCodeMotion : public SILFunctionTransform {
  /// Whether to hoist releases or sink retains.
  CodeMotionKind Kind;

  /// Freeze epilogue release or not.
  bool FreezeEpilogueReleases;

public:
  StringRef getName() override { return "SIL ARC Code Motion"; }

  /// Constructor.
  ARCCodeMotion(CodeMotionKind H, bool F) : Kind(H), FreezeEpilogueReleases(F) {}

  /// The entry point to the transformation.
  void run() override {
    // Code motion disabled.
    if (DisableARCCodeMotion)
      return;

    // Respect function no.optimize.
    SILFunction *F = getFunction();
    if (!F->shouldOptimize())
      return;

    // Return if there is critical edge and we are disabling critical edge
    // splitting.
    if (DisableIfWithCriticalEdge && hasCriticalEdges(*F, false))
      return;

    DEBUG(llvm::dbgs() << "*** ARCCM on function: " << F->getName() << " ***\n");
    // Split all critical edges.
    //
    // TODO: maybe we can do this lazily or maybe we should disallow SIL passes
    // to create critical edges.
    bool EdgeChanged = splitAllCriticalEdges(*F, false, nullptr, nullptr);

    llvm::SpecificBumpPtrAllocator<BlockState> BPA;
    auto *PO = PM->getAnalysis<PostOrderAnalysis>()->get(F);
    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *RCFI = PM->getAnalysis<RCIdentityAnalysis>()->get(F);

    bool InstChanged = false;
    if (Kind == Release) {
      // TODO: we should consider Throw block as well, or better we should
      // abstract the Return block or Throw block away in the matcher.
      SILArgumentConvention Conv[] = {SILArgumentConvention::Direct_Owned};
      ConsumedArgToEpilogueReleaseMatcher ERM(RCFI, F,
            Conv,
            ConsumedArgToEpilogueReleaseMatcher::ExitKind::Return);

      ReleaseCodeMotionContext RelCM(BPA, F, PO, AA, RCFI, 
                                     FreezeEpilogueReleases, ERM); 
      // Run release hoisting.
      InstChanged |= RelCM.run();
    } else {
      RetainCodeMotionContext RetCM(BPA, F, PO, AA, RCFI);
      // Run retain sinking.
      InstChanged |= RetCM.run();
    }

    if (EdgeChanged) {
      // We splitted critical edges.
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      return;
    }
    if (InstChanged) {
      // We moved instructions.
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

/// Sink Retains.
SILTransform *swift::createRetainSinking() {
  return new ARCCodeMotion(CodeMotionKind::Retain, false);
}

/// Hoist releases, but not epilogue release. ASO relies on epilogue releases
/// to prove knownsafety on enclosed releases.
SILTransform *swift::createReleaseHoisting() {
  return new ARCCodeMotion(CodeMotionKind::Release, true);
}

/// Hoist all releases.
SILTransform *swift::createLateReleaseHoisting() {
  return new ARCCodeMotion(CodeMotionKind::Release, false);
}

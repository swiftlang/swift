//===--- DeadCodeElimination.cpp - Delete dead code  ----------------------===//
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

#define DEBUG_TYPE "sil-dce"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

STATISTIC(NumBranchesPromoted, "Number of dead branches promoted to jumps");
STATISTIC(NumDeletedInsts, "Number of instructions deleted");

namespace {

// Without any complex analysis, does this instruction seem like
// something that we need to keep?
// FIXME: Reconcile the similarities between this and
//        isInstructionTriviallyDead.
static bool seemsUseful(SILInstruction *I) {
  // begin_access is defined to have side effects, but this is not relevant for
  // DCE.
  if (isa<BeginAccessInst>(I))
    return false;

  // Even though begin_borrow/destroy_value/copy_value have side-effects, they
  // can be DCE'ed if they do not have useful dependencies/reverse dependencies
  if (isa<BeginBorrowInst>(I))
    return false;

  if (isa<DestroyValueInst>(I))
    return false;

  if (isa<CopyValueInst>(I))
    return false;

  if (I->mayHaveSideEffects())
    return true;

  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    // Although the onFastPath builtin has no side-effects we don't want to
    // remove it.
    return BI->getBuiltinInfo().ID == BuiltinValueKind::OnFastPath;
  }

  if (isa<UnreachableInst>(I))
    return true;
  if (auto TI = dyn_cast<TermInst>(I)) {
    if (TI->isFunctionExiting())
      return true;
  }

  return false;
}

// We map from post-dominator tree node to a ControllingInfo struct
// which contains the post-dominator tree level of this node, along
// with the direct predecessors that this node is control-dependent
// on, and the minimum level number of any predecessor in the subtree
// below this node in the post-dominator tree.
struct ControllingInfo {
  typedef std::pair<SILBasicBlock *, unsigned> PredInfo;

  SILBasicBlock *Block;
  // The post-dominator tree level for this node.
  unsigned Level;
  llvm::SmallVector<PredInfo, 2> ControllingPreds;
  unsigned MinTreePredLevel;
};

class DCE : public SILFunctionTransform {
  typedef llvm::DomTreeNodeBase<SILBasicBlock> PostDomTreeNode;

  llvm::SmallPtrSet<SILNode *, 16> LiveValues;
  llvm::SmallPtrSet<SILBasicBlock *, 16> LiveBlocks;
  llvm::SmallVector<SILInstruction *, 64> Worklist;
  PostDominanceInfo *PDT;
  llvm::DenseMap<SILBasicBlock *, ControllingInfo> ControllingInfoMap;

  // Maps instructions which produce a failing condition (like overflow
  // builtins) to the actual cond_fail instructions which handle the failure.

  // Dependencies which go in the reverse direction. Usually for a pair
  //   %1 = inst_a
  //   inst_b(%1)
  // the dependency goes from inst_b to inst_a: if inst_b is alive then
  // inst_a is also alive.
  // For some instructions the dependency is exactly the other way round, e.g.
  //   %1 = inst_which_can_fail
  //   cond_fail(%1)
  // In this case cond_fail is alive only if inst_which_can_fail is alive.
  // The key of this map is the source of the dependency (inst_a), the
  // value is the set of instructions dependent on it (inst_b).
  llvm::DenseMap<SILValue, SmallPtrSet<SILInstruction *, 4>>
      ReverseDependencies;

  /// Tracks if the pass changed branches.
  bool BranchesChanged;
  /// Tracks if the pass changed ApplyInsts.
  bool CallsChanged;

  /// The entry point to the transformation.
  void run() override {
    BranchesChanged = false;
    CallsChanged = false;

    SILFunction *F = getFunction();
    auto *DA = PM->getAnalysis<PostDominanceAnalysis>();
    PDT = DA->get(F);

    // If we have a function that consists of nothing but a
    // structurally infinite loop like:
    //   while true {}
    // we'll have an empty post dominator tree.
    if (!PDT->getRootNode())
      return;

    LLVM_DEBUG(F->dump());
    LLVM_DEBUG(PDT->print(llvm::dbgs()));

    assert(Worklist.empty() && LiveValues.empty() && LiveBlocks.empty() &&
           ControllingInfoMap.empty() && ReverseDependencies.empty() &&
           "Expected to start with empty data structures!");

    if (!precomputeControlInfo(*F)) {
      LiveValues.clear();
      LiveBlocks.clear();
      ControllingInfoMap.clear();
      ReverseDependencies.clear();
      return;
    }

    markLive(*F);
    if (removeDead(*F)) {
      using InvalidationKind = SILAnalysis::InvalidationKind;

      unsigned Inv = InvalidationKind::Instructions;
      if (CallsChanged)
        Inv |= (unsigned)InvalidationKind::Calls;
      if (BranchesChanged) {
        removeUnreachableBlocks(*F);
        Inv |= (unsigned)InvalidationKind::Branches;
      }

      invalidateAnalysis(SILAnalysis::InvalidationKind(Inv));
    }

    LiveValues.clear();
    LiveBlocks.clear();
    ControllingInfoMap.clear();
    ReverseDependencies.clear();
  }

  bool precomputeControlInfo(SILFunction &F);
  void markLive(SILFunction &F);
  /// Record a reverse dependency from \p from to \p to meaning \p to is live
  /// if \p from is also live.
  void addReverseDependency(SILValue from, SILInstruction *to);
  bool removeDead(SILFunction &F);

  void computeLevelNumbers(PostDomTreeNode *root);
  bool hasInfiniteLoops(SILFunction &F);
  void computePredecessorDependence(SILFunction &F);
  void computeMinPredecessorLevels(PostDomTreeNode *root);
  void insertControllingInfo(SILBasicBlock *Block, unsigned Level);

  void markValueLive(SILValue V);
  void markInstructionLive(SILInstruction *Inst);
  void markTerminatorArgsLive(SILBasicBlock *Pred, SILBasicBlock *Succ,
                              size_t ArgIndex);
  void markControllingTerminatorsLive(SILBasicBlock *Block);
  void propagateLiveBlockArgument(SILArgument *Arg);
  void propagateLiveness(SILInstruction *I);
  void collectControllingBlocksInTree(ControllingInfo &QueryInfo,
                                      PostDomTreeNode *root,
                           llvm::SmallPtrSetImpl<SILBasicBlock *> &Controlling);
  void collectControllingBlocks(SILBasicBlock *Block,
                                llvm::SmallPtrSetImpl<SILBasicBlock *> &);
  SILBasicBlock *nearestUsefulPostDominator(SILBasicBlock *Block);
  void replaceBranchWithJump(SILInstruction *Inst, SILBasicBlock *Block);
  /// If \p value is live, insert a lifetime ending operation in ossa.
  /// destroy_value for @owned value and end_borrow for a @guaranteed value.
  void endLifetimeOfLiveValue(SILValue value, SILInstruction *insertPt);
};

// Keep track of the fact that V is live and add it to our worklist
// so that we can process the values it depends on.
void DCE::markValueLive(SILValue V) {
  if (SILInstruction *inst = V->getDefiningInstruction())
    return markInstructionLive(inst);

  if (!LiveValues.insert(V).second || isa<SILUndef>(V))
    return;

  LLVM_DEBUG(llvm::dbgs() << "Marking as live: " << *V);

  auto *Arg = cast<SILArgument>(V);
  markControllingTerminatorsLive(Arg->getParent());
  propagateLiveBlockArgument(Arg);
}

void DCE::markInstructionLive(SILInstruction *Inst) {
  if (!LiveValues.insert(Inst->asSILNode()).second)
    return;

  LLVM_DEBUG(llvm::dbgs() << "Marking as live: " << *Inst);

  markControllingTerminatorsLive(Inst->getParent());
  Worklist.push_back(Inst);
}

/// Gets the producing instruction of a cond_fail condition. Currently these
/// are overflow builtins but may be extended to other instructions in the
/// future.
static BuiltinInst *getProducer(CondFailInst *CFI) {
  // Check for the pattern:
  //   %1 = builtin "some_operation_with_overflow"
  //   %2 = tuple_extract %1
  //   %3 = cond_fail %2
  SILValue FailCond = CFI->getOperand();
  if (auto *TEI = dyn_cast<TupleExtractInst>(FailCond)) {
    if (auto *BI = dyn_cast<BuiltinInst>(TEI->getOperand())) {
      return BI;
    }
  }
  return nullptr;
}

// Determine which instructions from this function we need to keep.
void DCE::markLive(SILFunction &F) {
  // Find the initial set of instructions in this function that appear
  // to be live in the sense that they are not trivially something we
  // can delete by examining only that instruction.
  for (auto &BB : F) {
    for (auto &I : BB) {
      switch (I.getKind()) {
      case SILInstructionKind::CondFailInst: {
        if (auto *Prod = getProducer(cast<CondFailInst>(&I))) {
          addReverseDependency(Prod, &I);
        } else {
          markInstructionLive(&I);
        }
        break;
      }
      case SILInstructionKind::FixLifetimeInst: {
        SILValue Op = I.getOperand(0);
        if (!Op->getType().isAddress()) {
          addReverseDependency(Op, &I);
        } else {
          markInstructionLive(&I);
        }
        break;
      }
      case SILInstructionKind::EndAccessInst: {
        // An end_access is live only if it's begin_access is also live.
        auto *beginAccess = cast<EndAccessInst>(&I)->getBeginAccess();
        addReverseDependency(beginAccess, &I);
        break;
      }
      case SILInstructionKind::DestroyValueInst:
      case SILInstructionKind::EndBorrowInst: {
        // The instruction is live only if it's operand value is also live
        addReverseDependency(I.getOperand(0), &I);
        break;
      }
      default:
        if (seemsUseful(&I))
          markInstructionLive(&I);
      }
    }
  }
  // Now propagate liveness backwards from each instruction in our
  // worklist, adding new instructions to the worklist as we discover
  // more that we need to keep.
  while (!Worklist.empty()) {
    auto *I = Worklist.pop_back_val();
    propagateLiveness(I);
  }
}

// Records a reverse dependency if needed. See DCE::ReverseDependencies.
void DCE::addReverseDependency(SILValue from, SILInstruction *to) {
  LLVM_DEBUG(llvm::dbgs() << "Adding reverse dependency from " << from << " to "
                          << to);
  ReverseDependencies[from].insert(to);
}

// Mark as live the terminator argument at index ArgIndex in Pred that
// targets Succ.
void DCE::markTerminatorArgsLive(SILBasicBlock *Pred,
                                 SILBasicBlock *Succ,
                                 size_t ArgIndex) {
  auto *Term = Pred->getTerminator();

  // If the arguments are live, we need to keep the terminator that
  // delivers those arguments.
  markInstructionLive(Term);

  switch (Term->getTermKind()) {
  case TermKind::ReturnInst:
  case TermKind::ThrowInst:
  case TermKind::UnwindInst:
  case TermKind::YieldInst:

  case TermKind::UnreachableInst:
  case TermKind::SwitchValueInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::CheckedCastAddrBranchInst:
    llvm_unreachable("Unexpected argument for terminator kind!");
    break;

  case TermKind::DynamicMethodBranchInst:
  case TermKind::SwitchEnumInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastValueBranchInst:
    assert(ArgIndex == 0 && "Expected a single argument!");

    // We do not need to do anything with these. If the resulting
    // argument is used at the destination these terminators will end
    // up live, and then our normal liveness propagation will mark the
    // single operand of these instructions as live.
    break;

  case TermKind::BranchInst:
    markValueLive(cast<BranchInst>(Term)->getArg(ArgIndex));
    break;

  case TermKind::CondBranchInst: {
    auto *CondBr = cast<CondBranchInst>(Term);

    if (CondBr->getTrueBB() == Succ) {
      auto TrueArgs = CondBr->getTrueArgs();
      markValueLive(TrueArgs[ArgIndex]);
    }

    if (CondBr->getFalseBB() == Succ) {
      auto FalseArgs = CondBr->getFalseArgs();
      markValueLive(FalseArgs[ArgIndex]);
    }

    break;
  }
  case TermKind::AwaitAsyncContinuationInst:
  case TermKind::TryApplyInst: {
    assert(ArgIndex == 0 && "Expect a single argument!");
    break;
  }
  }
}

// Propagate liveness back from Arg to the terminator arguments that
// supply its value.
void DCE::propagateLiveBlockArgument(SILArgument *Arg) {
  // Conceptually, the dependency from a debug instruction to its definition
  // is in reverse direction: Only if its definition (the Arg) is alive, also
  // the debug_value instruction is alive.
  for (Operand *DU : getDebugUses(Arg))
    markInstructionLive(DU->getUser());

  // Mark all reverse dependencies on the Arg live
  for (auto *depInst : ReverseDependencies.lookup(Arg)) {
    markInstructionLive(depInst);
  }

  auto *Block = Arg->getParent();
  auto ArgIndex = Arg->getIndex();

  for (auto Pred : Block->getPredecessorBlocks())
    markTerminatorArgsLive(Pred, Block, ArgIndex);
}

// Given an instruction which is considered live, propagate that liveness
// back to the instructions that produce values it consumes.
void DCE::propagateLiveness(SILInstruction *I) {
  if (!isa<TermInst>(I)) {
    for (auto &O : I->getAllOperands())
      markValueLive(O.get());

    // Conceptually, the dependency from a debug instruction to its definition
    // is in reverse direction: Only if its definition is alive, also the
    // debug_value instruction is alive.
    for (auto result : I->getResults())
      for (Operand *DU : getDebugUses(result))
        markInstructionLive(DU->getUser());

    // Handle all other reverse-dependency instructions, like cond_fail,
    // fix_lifetime, destroy_value, etc. Only if the definition is alive, the
    // user itself is alive.
    for (auto res : I->getResults()) {
      for (auto *depInst : ReverseDependencies.lookup(res)) {
        markInstructionLive(depInst);
      }
    }
    return;
  }

  switch (cast<TermInst>(I)->getTermKind()) {
  case TermKind::BranchInst:
  case TermKind::UnreachableInst:
  case TermKind::UnwindInst:
    return;

  case TermKind::ReturnInst:
  case TermKind::ThrowInst:
  case TermKind::CondBranchInst:
  case TermKind::SwitchEnumInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::DynamicMethodBranchInst:
    markValueLive(I->getOperand(0));
    return;

  case TermKind::AwaitAsyncContinuationInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastValueBranchInst:
  case TermKind::CheckedCastAddrBranchInst:
  case TermKind::TryApplyInst:
  case TermKind::SwitchValueInst:
  case TermKind::YieldInst:
    for (auto &O : I->getAllOperands())
      markValueLive(O.get());
    return;
  }
  llvm_unreachable("corrupt instruction!");
}

SILBasicBlock *DCE::nearestUsefulPostDominator(SILBasicBlock *Block) {
  // Find the nearest post-dominator that has useful instructions.
  auto *PostDomNode = PDT->getNode(Block)->getIDom();
  while (PostDomNode && !LiveBlocks.count(PostDomNode->getBlock()))
    PostDomNode = PostDomNode->getIDom();

  if (PostDomNode)
    return PostDomNode->getBlock();
  return nullptr;
}

// Replace the given conditional branching instruction with a plain
// jump (aka unconditional branch) to the destination block.
void DCE::replaceBranchWithJump(SILInstruction *Inst, SILBasicBlock *Block) {
  ++NumBranchesPromoted;

  assert(Block && "Expected a destination block!");

  assert((isa<CondBranchInst>(Inst) ||
          isa<SwitchValueInst>(Inst) ||
          isa<SwitchEnumInst>(Inst) ||
          isa<SwitchEnumAddrInst>(Inst) ||
          isa<DynamicMethodBranchInst>(Inst) ||
          isa<CheckedCastBranchInst>(Inst)) &&
         "Unexpected dead terminator kind!");

  SILInstruction *Branch;
  if (!Block->args_empty()) {
    std::vector<SILValue> Args;
    auto E = Block->args_end();
    for (auto A = Block->args_begin(); A != E; ++A) {
      assert(!LiveValues.count(*A) && "Unexpected live block argument!");
      Args.push_back(SILUndef::get((*A)->getType(), *(*A)->getFunction()));
    }
    Branch =
        SILBuilderWithScope(Inst).createBranch(Inst->getLoc(), Block, Args);
  } else {
    Branch = SILBuilderWithScope(Inst).createBranch(Inst->getLoc(), Block);
  }
  LLVM_DEBUG(llvm::dbgs() << "Inserted unconditional branch:\n");
  LLVM_DEBUG(Branch->dump());
  (void)Branch;
}

void DCE::endLifetimeOfLiveValue(SILValue value, SILInstruction *insertPt) {
  if (!LiveValues.count(value)) {
    return;
  }
  SILBuilderWithScope builder(insertPt);
  if (value.getOwnershipKind() == OwnershipKind::Owned) {
    builder.emitDestroyOperation(RegularLocation::getAutoGeneratedLocation(),
                                 value);
  }
  if (value.getOwnershipKind() == OwnershipKind::Guaranteed) {
    builder.emitEndBorrowOperation(RegularLocation::getAutoGeneratedLocation(),
                                   value);
  }
}

// Remove the instructions that are not potentially useful.
bool DCE::removeDead(SILFunction &F) {
  bool Changed = false;

  for (auto &BB : F) {
    for (unsigned i = 0; i < BB.getArguments().size();) {
      auto *arg = BB.getArgument(i);
      if (LiveValues.count(arg)) {
        i++;
        continue;
      }

      LLVM_DEBUG(llvm::dbgs() << "Removing dead argument:\n");
      LLVM_DEBUG(arg->dump());

      arg->replaceAllUsesWithUndef();

      if (!F.hasOwnership() || arg->getOwnershipKind() == OwnershipKind::None) {
        i++;
        Changed = true;
        BranchesChanged = true;
        continue;
      }

      if (!arg->isPhiArgument()) {
        // We cannot delete a non phi arg. If it was @owned, insert a
        // destroy_value, because its consuming user has already been marked
        // dead and will be deleted.
        // We do not have to end lifetime of a @guaranteed non phi arg.
        if (arg->getOwnershipKind() == OwnershipKind::Owned) {
          auto insertPt = getInsertAfterPoint(arg).getValue();
          SILBuilderWithScope builder(insertPt);
          auto *destroy = builder.createDestroyValue(insertPt->getLoc(), arg);
          LiveValues.insert(destroy->asSILNode());
        }
        i++;
        Changed = true;
        BranchesChanged = true;
        continue;
      }
      // In OSSA, we have to delete a dead phi argument and insert destroy or
      // end_borrow at its predecessors if the incoming values are live.
      // This is not necessary in non-OSSA, and will infact be incorrect.
      // Because, passing a value as a phi argument does not imply end of
      // lifetime in non-OSSA.
      for (auto *pred : BB.getPredecessorBlocks()) {
        auto *predTerm = pred->getTerminator();
        auto predArg = predTerm->getAllOperands()[i].get();
        endLifetimeOfLiveValue(predArg, predTerm);
      }
      erasePhiArgument(&BB, i);
      Changed = true;
      BranchesChanged = true;
    }

    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      auto *Inst = &*I;
      ++I;
      if (LiveValues.count(Inst->asSILNode()) || isa<BranchInst>(Inst))
        continue;

      // We want to replace dead terminators with unconditional branches to
      // the nearest post-dominator that has useful instructions.
      if (isa<TermInst>(Inst)) {
        SILBasicBlock *postDom = nearestUsefulPostDominator(Inst->getParent());
        if (!postDom)
          continue;

        LLVM_DEBUG(llvm::dbgs() << "Replacing branch: ");
        LLVM_DEBUG(Inst->dump());
        LLVM_DEBUG(llvm::dbgs() << "with jump to: BB" << postDom->getDebugID());

        replaceBranchWithJump(Inst, postDom);
        Inst->eraseFromParent();
        BranchesChanged = true;
        Changed = true;
        continue;
      }

      ++NumDeletedInsts;

      LLVM_DEBUG(llvm::dbgs() << "Removing dead instruction:\n");
      LLVM_DEBUG(Inst->dump());

      if (F.hasOwnership()) {
        for (auto &Op : Inst->getAllOperands()) {
          if (Op.isLifetimeEnding()) {
            endLifetimeOfLiveValue(Op.get(), Inst);
          }
        }
      }
      Inst->replaceAllUsesOfAllResultsWithUndef();

      if (isa<ApplyInst>(Inst))
        CallsChanged = true;

      Inst->eraseFromParent();
      Changed = true;
    }
  }

  return Changed;
}

// Precompute some information from the post-dominator tree to aid us
// in determining control dependence without generating a complete
// control dependence graph. Inspired by:
//   Optimal control dependence and the Roman chariots problem
//   TOPLAS, v19, issue 3, 1997
//   http://dx.doi.org/10.1145/256167.256217
//
// For each node in the post-dominator tree we will compute:
// -- A level number.
//
// -- The list of immediate predecessors that this block is
//    control-dependent on along with the level number in the
//    post-dominator tree of each of those predecessors.
//
// -- The lowest level number of any predecessor below the given node
//    in the post-dominator tree. This will be used to exit early in
//    later control-dependence queries.
//
// Returns true upon success, false if nodes that are not present in the
// post-dominator tree are detected.
bool DCE::precomputeControlInfo(SILFunction &F) {
  computeLevelNumbers(PDT->getRootNode());
  if (hasInfiniteLoops(F))
    return false;
  computePredecessorDependence(F);
  computeMinPredecessorLevels(PDT->getRootNode());
  return true;
}

void DCE::insertControllingInfo(SILBasicBlock *Block, unsigned Level) {
  assert(ControllingInfoMap.find(Block) == ControllingInfoMap.end() &&
         "Unexpected map entry for node!");

  ControllingInfo Info;
  Info.Block = Block;
  Info.Level = Level;
  Info.MinTreePredLevel = -1;

  ControllingInfoMap[Block] = Info;
}

// Assign a level number to each node in the post-dominator tree.
void DCE::computeLevelNumbers(PostDomTreeNode *root) {
  llvm::SmallVector<std::pair<PostDomTreeNode *, unsigned>, 32> workList;
  workList.push_back({root, 0});

  while (!workList.empty()) {
    auto entry = workList.pop_back_val();
    PostDomTreeNode *node = entry.first;
    unsigned level = entry.second;
    
    insertControllingInfo(node->getBlock(), level);
    
    for (PostDomTreeNode *child : *node) {
      workList.push_back({child, level + 1});
    }
  }
}

// Structurally infinite loops like:
//   bb1:
//     br bb1
// are not present in the post-dominator tree. Their presence
// requires significant modifications to the way the rest of the
// algorithm works. They should be rare, so for now we'll do the most
// conservative thing and completely bail out, doing no dead code
// elimination. Note this will also hit for unreachable code, but
// presumably we'll run DCE at some point after removing unreachable
// code.
bool DCE::hasInfiniteLoops(SILFunction &F) {
  for (auto &BB : F)
    if (ControllingInfoMap.find(&BB) == ControllingInfoMap.end())
      return true;

  return false;
}

// For each block, create a list of the direct predecessors that the
// block is control-dependent on. With each predecessor, also keep the
// level number of the predecessor in the post-dominator tree.
void DCE::computePredecessorDependence(SILFunction &F) {
  for (auto &BB : F) {
    assert(ControllingInfoMap.find(&BB) != ControllingInfoMap.end()
           && "Expected to already have a map entry for block!");

    for (auto Pred : BB.getPredecessorBlocks())
      if (!PDT->properlyDominates(&BB, Pred)) {
        assert(ControllingInfoMap.find(Pred) != ControllingInfoMap.end() &&
               "Expected to already have a map entry for block!");

        auto PredLevel = ControllingInfoMap[Pred].Level;
        auto PredInfo = std::make_pair(Pred, PredLevel);

        auto &MapElement = ControllingInfoMap[&BB];
        MapElement.ControllingPreds.push_back(PredInfo);
      }
  }
}

// Assign the minimum post-dominator tree level to each node in the tree.
void DCE::computeMinPredecessorLevels(PostDomTreeNode *root) {
  llvm::SmallVector<PostDomTreeNode *, 32> postDomOrder;
  postDomOrder.reserve(ControllingInfoMap.size());
  postDomOrder.push_back(root);

  for (unsigned idx = 0; idx < postDomOrder.size(); ++idx) {
    PostDomTreeNode *node = postDomOrder[idx];
    for (PostDomTreeNode *child : *node) {
      postDomOrder.push_back(child);
    }
  }

  for (PostDomTreeNode *node : llvm::reverse(postDomOrder)) {
    SILBasicBlock *block = node->getBlock();
    assert(ControllingInfoMap.find(block) != ControllingInfoMap.end() &&
           "Expected to have map entry for node!");

    ControllingInfo &nodeInfo = ControllingInfoMap[block];
    for (auto &pred : nodeInfo.ControllingPreds) {
      nodeInfo.MinTreePredLevel = std::min(nodeInfo.MinTreePredLevel, pred.second);
    }
    if (PostDomTreeNode *parentNode = node->getIDom()) {
      ControllingInfo &parentInfo = ControllingInfoMap[parentNode->getBlock()];
      parentInfo.MinTreePredLevel = std::min(parentInfo.MinTreePredLevel, nodeInfo.MinTreePredLevel);
    }
  }
}

void DCE::collectControllingBlocksInTree(ControllingInfo &QueryInfo,
                                         PostDomTreeNode *root,
                          llvm::SmallPtrSetImpl<SILBasicBlock *> &Controlling) {
  llvm::SmallVector<PostDomTreeNode *, 32> workList;
  workList.push_back(root);

  while (!workList.empty()) {
    PostDomTreeNode *node = workList.pop_back_val();
    SILBasicBlock *block = node->getBlock();

    assert(ControllingInfoMap.find(block) != ControllingInfoMap.end() &&
          "Expected to have map entry for node!");

    auto &nodeInfo = ControllingInfoMap[block];
    if (nodeInfo.MinTreePredLevel > QueryInfo.Level)
      continue;

    for (auto &PredInfo : nodeInfo.ControllingPreds) {
      if (PredInfo.second <= QueryInfo.Level) {
        assert(PDT->properlyDominates(
                              PDT->getNode(PredInfo.first)->getIDom()->getBlock(),
                                           QueryInfo.Block) &&
               "Expected predecessor's post-dominator to post-dominate node.");
        Controlling.insert(PredInfo.first);
      }
    }
    for (PostDomTreeNode *child : *node) {
      workList.push_back(child);
    }
  }
}

// Walk the post-dominator tree from the query block down, building
// the set of blocks that the given block is control-dependent on. To
// determine control dependence we use some precomputed information
// about the direct predecessors that control each block, along with
// the level numbers in the post-dominator tree of those controlling
// predecessors. We can use the latter to terminate the walk down the
// dominator tree early.
void DCE::collectControllingBlocks(SILBasicBlock *Block,
                          llvm::SmallPtrSetImpl<SILBasicBlock *> &Controlling) {
  // First add the blocks that QueryNode is directly control-dependent on.
  assert(ControllingInfoMap.find(Block) != ControllingInfoMap.end() &&
         "Expected map entry for node!");
  auto &MapEntry = ControllingInfoMap[Block];

  // Now walk the children looking for nodes that have controlling
  // predecessors that have the same or lower level number in the
  // post-dominator tree.
  collectControllingBlocksInTree(MapEntry, PDT->getNode(Block), Controlling);
}

void DCE::markControllingTerminatorsLive(SILBasicBlock *Block) {
  if (LiveBlocks.count(Block))
    return;

  LiveBlocks.insert(Block);

  llvm::SmallPtrSet<SILBasicBlock *, 4> ControllingBlocks;
  collectControllingBlocks(Block, ControllingBlocks);

  for (auto BB : ControllingBlocks)
    markInstructionLive(BB->getTerminator());
}

} // end anonymous namespace

SILTransform *swift::createDCE() {
  return new DCE();
}

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
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"
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
  if (I->mayHaveSideEffects())
    return true;

  if (auto *BI = dyn_cast<BuiltinInst>(I)) {
    // Although the onFastPath builtin has no side-effects we don't want to
    // remove it.
    if (BI->getBuiltinInfo().ID == BuiltinValueKind::OnFastPath)
      return true;
    return false;
  }

  if (isa<ReturnInst>(I) || isa<UnreachableInst>(I) || isa<ThrowInst>(I))
    return true;

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

  llvm::SmallPtrSet<ValueBase *, 16> LiveValues;
  llvm::SmallPtrSet<SILBasicBlock *, 16> LiveBlocks;
  llvm::SmallVector<SILInstruction *, 64> Worklist;
  PostDominanceInfo *PDT;
  llvm::DenseMap<SILBasicBlock *, ControllingInfo> ControllingInfoMap;

  // Maps instructions which produce a failing condition (like overflow
  // builtins) to the actual cond_fail instructions which handle the failure.

  // Dependencies which go in the reverse direction. Usually for a pair
  //   %1 = inst_a
  //   inst_b(%1)
  // the dependency goes from inst_b to inst_a: if inst_b is alive then also
  // inst_a is alive.
  // For some instructions the dependency is exactly the other way round, e.g.
  //   %1 = inst_which_can_fail
  //   cond_fail(%1)
  // In this case cond_fail is alive only if inst_which_can_fail is alive.
  // The key of this map is the source of the dependency (inst_a), the
  // value is the destination (inst_b).
  llvm::DenseMap<SILInstruction *, SILInstruction *> ReverseDependencies;

  /// Tracks if the pass changed branches.
  bool BranchesChanged;
  /// Tracks if the pass changed ApplyInsts.
  bool CallsChanged;

  /// The entry point to the transformation.
  void run() override {
    BranchesChanged = false;
    CallsChanged = false;

    SILFunction *F = getFunction();

    auto* DA = PM->getAnalysis<PostDominanceAnalysis>();
    PDT = DA->get(F);

    // If we have a function that consists of nothing but a
    // structurally infinite loop like:
    //   while true {}
    // we'll have an empty post dominator tree.
    if (!PDT->getRootNode())
      return;

    DEBUG(F->dump());
    DEBUG(PDT->print(llvm::dbgs()));

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
      if (CallsChanged)        Inv |= (unsigned) InvalidationKind::Calls;
      if (BranchesChanged)     Inv |= (unsigned) InvalidationKind::Branches;

      invalidateAnalysis(SILAnalysis::InvalidationKind(Inv));
    }

    LiveValues.clear();
    LiveBlocks.clear();
    ControllingInfoMap.clear();
    ReverseDependencies.clear();
  }

  bool precomputeControlInfo(SILFunction &F);
  void markLive(SILFunction &F);
  void addReverseDependency(SILInstruction *From, SILInstruction *To);
  bool removeDead(SILFunction &F);

  void computeLevelNumbers(PostDomTreeNode *Node, unsigned Level);
  bool hasInfiniteLoops(SILFunction &F);
  void computePredecessorDependence(SILFunction &F);
  unsigned computeMinPredecessorLevels(PostDomTreeNode *Node);
  void insertControllingInfo(SILBasicBlock *Block, unsigned Level);

  void markValueLive(ValueBase *V);
  void markTerminatorArgsLive(SILBasicBlock *Pred, SILBasicBlock *Succ,
                              size_t ArgIndex);
  void markControllingTerminatorsLive(SILBasicBlock *Block);
  void propagateLiveBlockArgument(SILArgument *Arg);
  void propagateLiveness(SILInstruction *I);
  void collectControllingBlocksInTree(ControllingInfo &QueryInfo,
                                      PostDomTreeNode *Node,
                           llvm::SmallPtrSetImpl<SILBasicBlock *> &Controlling);
  void collectControllingBlocks(SILBasicBlock *Block,
                                llvm::SmallPtrSetImpl<SILBasicBlock *> &);
  SILBasicBlock *nearestUsefulPostDominator(SILBasicBlock *Block);
  void replaceBranchWithJump(SILInstruction *Inst, SILBasicBlock *Block);

  StringRef getName() override { return "Dead Code Elimination"; }
};

// Keep track of the fact that V is live and add it to our worklist
// so that we can process the values it depends on.
void DCE::markValueLive(ValueBase *V) {
  if (LiveValues.count(V) || isa<SILUndef>(V))
    return;

  DEBUG(llvm::dbgs() << "Marking as live:\n");
  DEBUG(V->dump());

  LiveValues.insert(V);

  if (auto *Def = dyn_cast<SILInstruction>(V)) {
    markControllingTerminatorsLive(Def->getParent());
    Worklist.push_back(Def);
    return;
  }

  assert(isa<SILArgument>(V) &&
         "Only expected instructions and arguments!");

  auto *Arg = cast<SILArgument>(V);
  markControllingTerminatorsLive(Arg->getParent());
  propagateLiveBlockArgument(Arg);
}

/// Gets the producing instruction of a cond_fail condition. Currently these
/// are overflow builtins but may be extended to other instructions in the
/// future.
static SILInstruction *getProducer(CondFailInst *CFI) {
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
      if (auto *CFI = dyn_cast<CondFailInst>(&I)) {
        // A cond_fail is only alive if its (identifiable) producer is alive.
        if (SILInstruction *Prod = getProducer(CFI)) {
          addReverseDependency(Prod, CFI);
        } else {
          markValueLive(CFI);
        }
        continue;
      }
      if (auto *FLI = dyn_cast<FixLifetimeInst>(&I)) {
        // A fix_lifetime (with a non-address type) is only alive if it's
        // definition is alive.
        SILValue Op = FLI->getOperand();
        auto *OpInst = dyn_cast<SILInstruction>(Op);
        if (OpInst && !Op->getType().isAddress()) {
          addReverseDependency(OpInst, FLI);
        } else {
          markValueLive(FLI);
        }
        continue;
      }
      if (seemsUseful(&I))
        markValueLive(&I);
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

// Records a reverse dependency. See DCE::ReverseDependencies.
void DCE::addReverseDependency(SILInstruction *From, SILInstruction *To) {
  assert(!ReverseDependencies.lookup(To) &&
         "Target of reverse dependency already in map");
  SILInstruction *&Target = ReverseDependencies[From];
  SILInstruction *ExistingTarget = Target;
  Target = To;
  if (ExistingTarget) {
    // Create a single linked chain if From has multiple targets.
    ReverseDependencies[To] = ExistingTarget;
  }
}

// Mark as live the terminator argument at index ArgIndex in Pred that
// targets Succ.
void DCE::markTerminatorArgsLive(SILBasicBlock *Pred,
                                 SILBasicBlock *Succ,
                                 size_t ArgIndex) {
  auto *Term = Pred->getTerminator();

  // If the arguments are live, we need to keep the terminator that
  // delivers those arguments.
  markValueLive(Term);

  switch (Term->getTermKind()) {
  case TermKind::ReturnInst:
  case TermKind::ThrowInst:

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
    markValueLive(DU->getUser());

  if (isa<SILFunctionArgument>(Arg))
    return;

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
    for (Operand *DU : getDebugUses(I))
      markValueLive(DU->getUser());

    // Handle all other reverse-dependency instructions, like cond_fail and
    // fix_lifetime. Only if the definition is alive, the user itself is alive.
    if (SILInstruction *DepInst = ReverseDependencies.lookup(I)) {
      markValueLive(DepInst);
    }
    return;
  }

  switch (ValueKindAsTermKind(I->getKind())) {
  case TermKind::BranchInst:
  case TermKind::UnreachableInst:
    return;

  case TermKind::ReturnInst:
  case TermKind::ThrowInst:
  case TermKind::CondBranchInst:
  case TermKind::SwitchEnumInst:
  case TermKind::SwitchEnumAddrInst:
  case TermKind::DynamicMethodBranchInst:
  case TermKind::CheckedCastBranchInst:
  case TermKind::CheckedCastValueBranchInst:
    markValueLive(I->getOperand(0));
    return;

  case TermKind::TryApplyInst:
  case TermKind::SwitchValueInst:
    for (auto &O : I->getAllOperands())
      markValueLive(O.get());
    return;

  case TermKind::CheckedCastAddrBranchInst:
    markValueLive(I->getOperand(0));
    markValueLive(I->getOperand(1));
    return;
  }
  llvm_unreachable("corrupt instruction!");
}

SILBasicBlock *DCE::nearestUsefulPostDominator(SILBasicBlock *Block) {
  // Find the nearest post-dominator that has useful instructions.
  auto *PostDomNode = PDT->getNode(Block)->getIDom();
  while (!LiveBlocks.count(PostDomNode->getBlock()))
    PostDomNode = PostDomNode->getIDom();

  return PostDomNode->getBlock();
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
      Args.push_back(SILUndef::get((*A)->getType(), (*A)->getModule()));
    }
    Branch =
        SILBuilderWithScope(Inst).createBranch(Inst->getLoc(), Block, Args);
  } else {
    Branch = SILBuilderWithScope(Inst).createBranch(Inst->getLoc(), Block);
  }
  DEBUG(llvm::dbgs() << "Inserted unconditional branch:\n");
  DEBUG(Branch->dump());
  (void)Branch;
}

// Remove the instructions that are not potentially useful.
bool DCE::removeDead(SILFunction &F) {
  bool Changed = false;

  for (auto &BB : F) {
    for (auto I = BB.args_begin(), E = BB.args_end(); I != E;) {
      auto Inst = *I++;
      if (LiveValues.count(Inst))
        continue;

      DEBUG(llvm::dbgs() << "Removing dead argument:\n");
      DEBUG(Inst->dump());

      if (Inst->hasValue()) {
        auto *Undef = SILUndef::get(Inst->getType(), Inst->getModule());
        Inst->replaceAllUsesWith(Undef);
      }

      Changed = true;
    }

    for (auto I = BB.begin(), E = BB.end(); I != E; ) {
      auto *Inst = &*I;
      I++;
      if (LiveValues.count(Inst) || isa<BranchInst>(Inst))
        continue;

      // We want to replace dead terminators with unconditional branches to
      // the nearest post-dominator that has useful instructions.
      if (isa<TermInst>(Inst)) {
        replaceBranchWithJump(Inst,
                              nearestUsefulPostDominator(Inst->getParent()));
        Inst->eraseFromParent();
        BranchesChanged = true;
        Changed = true;
        continue;
      }

      ++NumDeletedInsts;

      DEBUG(llvm::dbgs() << "Removing dead instruction:\n");
      DEBUG(Inst->dump());

      if (Inst->hasValue()) {
        auto *Undef = SILUndef::get(Inst->getType(), Inst->getModule());
        Inst->replaceAllUsesWith(Undef);
      }


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
  computeLevelNumbers(PDT->getRootNode(), 0);
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

// Assign a level number to each node in the post-dominator tree, and
void DCE::computeLevelNumbers(PostDomTreeNode *Node, unsigned Level) {
  insertControllingInfo(Node->getBlock(), Level);

  for (auto Child = Node->begin(), End = Node->end(); Child != End; ++Child)
    computeLevelNumbers(*Child, Level + 1);
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

// Return the minimum post-dominator tree level of any of the
// direct controlling predecessors of this node or any child.
unsigned DCE::computeMinPredecessorLevels(PostDomTreeNode *Node) {
  unsigned Min = -1;

  auto *Block = Node->getBlock();

  assert(ControllingInfoMap.find(Block) != ControllingInfoMap.end() &&
         "Expected to have map entry for node!");

  auto &MapElement = ControllingInfoMap[Block];
  for (auto &PredInfo : MapElement.ControllingPreds)
    Min = std::min(Min, PredInfo.second);

  for (auto Child = Node->begin(), End = Node->end(); Child != End; ++Child)
    Min = std::min(Min, computeMinPredecessorLevels(*Child));

  MapElement.MinTreePredLevel = Min;

  return Min;
}

void DCE::collectControllingBlocksInTree(ControllingInfo &QueryInfo,
                                         PostDomTreeNode *Node,
                          llvm::SmallPtrSetImpl<SILBasicBlock *> &Controlling) {
  auto *Block = Node->getBlock();

  assert(ControllingInfoMap.find(Block) != ControllingInfoMap.end() &&
         "Expected to have map entry for node!");

  auto &MapEntry = ControllingInfoMap[Block];
  if (MapEntry.MinTreePredLevel > QueryInfo.Level)
    return;

  for (auto &PredInfo : MapEntry.ControllingPreds)
    if (PredInfo.second <= QueryInfo.Level) {
      assert(PDT->properlyDominates(
                            PDT->getNode(PredInfo.first)->getIDom()->getBlock(),
                                         QueryInfo.Block) &&
             "Expected predecessor's post-dominator to post-dominate node.");
      Controlling.insert(PredInfo.first);
    }

  for (auto Child = Node->begin(), End = Node->end(); Child != End; ++Child)
    collectControllingBlocksInTree(QueryInfo, (*Child), Controlling);
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
    markValueLive(BB->getTerminator());
}

} // end anonymous namespace

SILTransform *swift::createDCE() {
  return new DCE();
}

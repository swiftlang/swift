//===--- SILMem2Reg.cpp - Promotes AllocStacks to registers ---------------===//
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
// This pass promotes AllocStack instructions into virtual register
// references. It only handles load, store and deallocation
// instructions. The algorithm is based on:
//
//  Sreedhar and Gao. A linear time algorithm for placing phi-nodes. POPL '95.
//
//===----------------------------------------------------------------------===//


#define DEBUG_TYPE "sil-mem2reg"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/Projection.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include <algorithm>
#include <queue>
using namespace swift;

STATISTIC(NumAllocStackFound,    "Number of AllocStack found");
STATISTIC(NumAllocStackCaptured, "Number of AllocStack captured");
STATISTIC(NumInstRemoved,        "Number of Instructions removed");
STATISTIC(NumPhiPlaced,          "Number of Phi blocks placed");

namespace {

typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
typedef llvm::DenseMap<DomTreeNode *, unsigned> DomTreeLevelMap;

/// Promotes a single AllocStackInst into registers..
class StackAllocationPromoter {
  typedef llvm::DenseSet<SILBasicBlock *> BlockSet;
  typedef llvm::DenseMap<SILBasicBlock *, SILInstruction *> BlockToInstMap;

  // Use a priority queue keyed on dominator tree level so that inserted nodes
  // are handled from the bottom of the dom tree upwards.
  typedef std::pair<DomTreeNode *, unsigned> DomTreeNodePair;
  typedef std::priority_queue<DomTreeNodePair, SmallVector<DomTreeNodePair, 32>,
                                 llvm::less_second> NodePriorityQueue;

  /// The AllocStackInst that we are handling.
  AllocStackInst *ASI;

  /// The deallocation Instruction. This value could be NULL if there are
  /// multiple deallocations.
  DeallocStackInst *DSI;

  /// Dominator info.
  DominanceInfo *DT;

  /// Map from dominator tree node to tree level.
  DomTreeLevelMap &DomTreeLevels;

  /// The builder used to create new instructions during register promotion.
  SILBuilder &B;

  /// Records the last store instruction in each block for a specific
  /// AllocStackInst.
  BlockToInstMap LastStoreInBlock;
public:
  /// C'tor.
  StackAllocationPromoter(AllocStackInst *Asi, DominanceInfo *Di,
                          DomTreeLevelMap &DomTreeLevels, SILBuilder &B)
      : ASI(Asi), DSI(0), DT(Di), DomTreeLevels(DomTreeLevels), B(B) {
        // Scan the users in search of a deallocation instruction.
        for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI)
          if (DeallocStackInst *D = dyn_cast<DeallocStackInst>(UI->getUser())) {
            // Don't record multiple dealloc instructions.
            if (DSI) {
              DSI = 0;
              break;
            }
            // Record the deallocation instruction.
            DSI = D;
          }
      }

  /// Promote the Allocation.
  void run();

private:
  /// \brief Promote AllocStacks into SSA.
  void promoteAllocationToPhi();

  /// \brief Replace the dummy nodes with new block arguments.
  void addBlockArguments(BlockSet &PhiBlocks);

  /// \brief Fix all of the branch instructions and the uses to use
  /// the AllocStack definitions (which include stores and Phis).
  void fixBranchesAndUses(BlockSet &Blocks);

  /// \brief update the branch instructions with the new Phi argument.
  /// The blocks in \p PhiBlocks are blocks that define a value, \p Dest is
  /// the branch destination, and \p Pred is the predecessors who's branch we
  /// modify.
  void fixPhiPredBlock(BlockSet &PhiBlocks, SILBasicBlock *Dest,
                       SILBasicBlock *Pred);

  /// \brief Get the value for this AllocStack variable that is
  /// flowing out of StartBB.
  SILValue getLiveOutValue(BlockSet &PhiBlocks, SILBasicBlock *StartBB);

  /// \brief Get the value for this AllocStack variable that is
  /// flowing into BB.
  SILValue getLiveInValue(BlockSet &PhiBlocks, SILBasicBlock *BB);

  /// \brief Prune AllocStacks usage in the function. Scan the function
  /// and remove in-block usage of the AllocStack. Leave only the first
  /// load and the last store.
  void pruneAllocStackUsage();

  /// \brief Promote all of the AllocStacks in a single basic block in one
  /// linear scan. This function deletes all of the loads and stores except
  /// for the first load and the last store.
  /// \returns the last StoreInst found or zero if none found.
  StoreInst *promoteAllocationInBlock(SILBasicBlock *BB);
};

} // end of namespace

namespace {
/// Promote memory to registers
class MemoryToRegisters {
  /// The function that we are optimizing.
  SILFunction &F;

  /// Dominators.
  DominanceInfo *DT;

  /// The builder used to create new instructions during register promotion.
  SILBuilder B;

  /// \brief Check if the AllocStackInst \p ASI is only written into.
  bool isWriteOnlyAllocation(AllocStackInst *ASI, bool Promoted = false);

  /// \brief Promote all of the AllocStacks in a single basic block in one
  /// linear scan. Note: This function deletes all of the users of the
  /// AllocStackInst, including the DeallocStackInst but it does not remove the
  /// AllocStackInst itself!
  void removeSingleBlockAllocation(AllocStackInst *ASI);

public:
  /// C'tor
  MemoryToRegisters(SILFunction &Func, DominanceInfo *Dt) : F(Func), DT(Dt),
                                                            B(Func) {}

  /// \brief Promote memory to registers. Return True on change.
  bool run();
};

} // end anonymous namespace.

/// Returns true if \p I is an address of a LoadInst, skipping struct and
/// tuple address projections. Sets \p singleBlock to null if the load (or
/// it's address is not in \p singleBlock.
static bool isAddressForLoad(SILInstruction *I, SILBasicBlock *&singleBlock) {
  
  if (isa<LoadInst>(I))
    return true;

  if (!isa<StructElementAddrInst>(I) && !isa<TupleElementAddrInst>(I))
    return false;
  
  // Recursively search for other (non-)loads in the instruction's uses.
  for (auto UI : I->getUses()) {
    SILInstruction *II = UI->getUser();
    if (II->getParent() != singleBlock)
      singleBlock = nullptr;
    
    if (!isAddressForLoad(II, singleBlock))
        return false;
  }
  return true;
}

/// Returns true if this AllocStacks is captured.
/// Sets \p inSingleBlock to true if all uses of \p ASI are in a single block.
static bool isCaptured(AllocStackInst *ASI, bool &inSingleBlock) {
  
  SILBasicBlock *singleBlock = ASI->getParent();
  
  // For all users of the AllocStack instruction.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();

    if (II->getParent() != singleBlock)
      singleBlock = nullptr;
    
    // Loads are okay.
    if (isAddressForLoad(II, singleBlock))
      continue;

    // We can store into an AllocStack (but not the pointer).
    if (StoreInst *SI = dyn_cast<StoreInst>(II))
      if (SI->getDest().getDef() == ASI)
        continue;

    // Deallocation is also okay, as are DebugValueAddr. We will turn
    // the latter into DebugValue.
    if (isa<DeallocStackInst>(II) || isa<DebugValueAddrInst>(II))
      continue;

    // Other instructions are assumed to capture the AllocStack.
    DEBUG(llvm::dbgs() << "*** AllocStack is captured by: " << *II);
    return true;
  }

  // None of the users capture the AllocStack.
  inSingleBlock = (singleBlock != nullptr);
  return false;
}

/// Returns true if the AllocStack is only stored into.
bool MemoryToRegisters::isWriteOnlyAllocation(AllocStackInst *ASI,
                                              bool Promoted) {
  // For all users of the AllocStack:
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();

    // It is okay to store into this AllocStack.
    if (StoreInst *SI = dyn_cast<StoreInst>(II))
      if (!isa<AllocStackInst>(SI->getSrc()))
        continue;

    // Deallocation is also okay.
    if (isa<DeallocStackInst>(II))
      continue;

    // If we haven't already promoted the AllocStack, we may see
    // DebugValueAddr uses.
    if (!Promoted && isa<DebugValueAddrInst>(II))
      continue;

    // Can't do anything else with it.
    DEBUG(llvm::dbgs() << "*** AllocStack has non-write use: " << *II);
    return false;
  }

  return true;
}

/// Promote a DebugValueAddr to a DebugValue of the given value.
static void
promoteDebugValueAddr(DebugValueAddrInst *DVAI, SILValue Value, SILBuilder &B) {
  assert(Value.isValid() && "Expected valid value");
  B.setInsertionPoint(DVAI);
  B.createDebugValue(DVAI->getLoc(), Value);
  DVAI->eraseFromParent();
}

/// Returns true if \p I is a load which loads from \p ASI.
static bool isLoadFromStack(SILInstruction *I, AllocStackInst *ASI) {
  if (!isa<LoadInst>(I))
    return false;
  
  // Skip struct and tuple address projections.
  ValueBase *op = I->getOperand(0).getDef();
  while (op != ASI) {
    if (!isa<StructElementAddrInst>(op) && !isa<TupleElementAddrInst>(op))
      return false;
    
    op = cast<SILInstruction>(op)->getOperand(0).getDef();
  }
  return true;
}

/// Collects all load instructions which (transitively) use \p I as address.
static void collectLoads(SILInstruction *I, SmallVectorImpl<LoadInst *> &Loads) {
  if (LoadInst *load = dyn_cast<LoadInst>(I)) {
    Loads.push_back(load);
    return;
  }
  if (!isa<StructElementAddrInst>(I) && !isa<TupleElementAddrInst>(I))
    return;
  
  // Recursively search for other loads in the instruction's uses.
  for (auto UI : I->getUses()) {
    collectLoads(UI->getUser(), Loads);
  }
}


static void replaceLoad(LoadInst *LI, SILValue val, AllocStackInst *ASI) {
  ProjectionPath projections;
  SILValue op = LI->getOperand();
  while (op.getDef() != ASI) {
    assert(isa<StructElementAddrInst>(op) || isa<TupleElementAddrInst>(op));
    SILInstruction *Inst = cast<SILInstruction>(op);
    auto projection = Projection::addressProjectionForInstruction(Inst);
    projections.push_back(projection.getValue());
    op = Inst->getOperand(0);
  }
  SILBuilder builder(LI);
  for (auto iter = projections.rbegin(); iter != projections.rend(); ++iter) {
    const Projection &projection = *iter;
    val = projection.createValueProjection(builder, LI->getLoc(), val).get();
  }
  op = LI->getOperand();
  SILValue(LI, 0).replaceAllUsesWith(val);
  LI->eraseFromParent();
  while (op.getDef() != ASI && op.use_empty()) {
    assert(isa<StructElementAddrInst>(op) || isa<TupleElementAddrInst>(op));
    SILInstruction *Inst = cast<SILInstruction>(op);
    SILValue next = Inst->getOperand(0);
    Inst->eraseFromParent();
    op = next;
  }
}

StoreInst *
StackAllocationPromoter::promoteAllocationInBlock(SILBasicBlock *BB) {
  DEBUG(llvm::dbgs() << "*** Promoting ASI in block: " << *ASI);

  // We don't know the value of the alloca until we find the first store.
  SILValue RunningVal = SILValue();
  // Keep track of the last StoreInst that we found.
  StoreInst *LastStore = nullptr;

  // For all instructions in the block.
  for (auto BBI = BB->begin(), E = BB->end(); BBI != E;) {
    SILInstruction *Inst = BBI++;

    if (isLoadFromStack(Inst, ASI)) {
      if (RunningVal.isValid()) {
        // If we are loading from the AllocStackInst and we already know the
        // content of the Alloca then use it.
        DEBUG(llvm::dbgs() << "*** Promoting load: " << *Inst);
        
        replaceLoad(cast<LoadInst>(Inst), RunningVal, ASI);
        NumInstRemoved++;
      } else if (Inst->getOperand(0).getDef() == ASI) {
        // If we don't know the content of the AllocStack then the loaded
        // value *is* the new value;
        DEBUG(llvm::dbgs() << "*** First load: " << *Inst);
        RunningVal = Inst;
      }
      continue;
    }

    // Remove stores and record the value that we are saving as the running
    // value.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      if (SI->getDest().getDef() != ASI)
        continue;

      // The stored value is the new running value.
      RunningVal = SI->getSrc();

      // If we met a store before this one, delete it.
      if (LastStore) {
        NumInstRemoved++;
        DEBUG(llvm::dbgs() << "*** Removing redundant store: " << *LastStore);
        LastStore->eraseFromParent();
      }
      LastStore = SI;
      continue;
    }

    // Replace debug_value_addr with debug_value of the promoted value
    // if we have a valid value to use at this point. Otherwise we'll
    // promote this when we deal with hooking up phis.
    if (auto *DVAI = dyn_cast<DebugValueAddrInst>(Inst)) {
      if (DVAI->getOperand() == ASI->getAddressResult() &&
          RunningVal.isValid())
        promoteDebugValueAddr(DVAI, RunningVal, B);
      continue;
    }

    // Stop on deallocation.
    if (DeallocStackInst *DSI = dyn_cast<DeallocStackInst>(Inst)) {
      if (DSI->getOperand() == ASI)
        break;
    }
  }
  if (LastStore) {
    DEBUG(llvm::dbgs() << "*** Finished promotion. Last store: " << *LastStore);
  } else {
    DEBUG(llvm::dbgs() << "*** Finished promotion with no stores.\n");
  }
  return LastStore;
}

void MemoryToRegisters::removeSingleBlockAllocation(AllocStackInst *ASI) {
  DEBUG(llvm::dbgs() << "*** Promoting in-block: " << *ASI);

  SILBasicBlock *BB = ASI->getParent();

  // The default value of the AllocStack is NULL because we don't have
  // unilitialized variables in Swift.
  SILValue RunningVal = SILValue();

  // For all instructions in the block.
  for (auto BBI = BB->begin(), E = BB->end(); BBI != E;) {
    SILInstruction *Inst = BBI++;

    // Remove instructions that we are loading from. Replace the loaded value
    // with our running value.
    if (isLoadFromStack(Inst, ASI)) {
      if (!RunningVal.isValid()) {
        assert(ASI->getElementType().isVoid() &&
               "Expected initialization of non-void type!");
        RunningVal = SILUndef::get(ASI->getElementType(), ASI->getModule());
      }
      replaceLoad(cast<LoadInst>(Inst), RunningVal, ASI);
      NumInstRemoved++;
      continue;
    }

    // Remove stores and record the value that we are saving as the running
    // value.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      if (SI->getDest().getDef() == ASI) {
        RunningVal = SI->getSrc();
        Inst->eraseFromParent();
        NumInstRemoved++;
        continue;
      }
    }

    // Replace debug_value_addr with debug_value of the promoted value.
    if (auto *DVAI = dyn_cast<DebugValueAddrInst>(Inst)) {
      if (DVAI->getOperand() == ASI->getAddressResult()) {
        if (RunningVal.isValid()) {
          promoteDebugValueAddr(DVAI, RunningVal, B);
        } else {
          // Drop debug_value_addr of uninitialized void values.
          assert(ASI->getElementType().isVoid() &&
                 "Expected initialization of non-void type!");
          DVAI->eraseFromParent();
        }
      }
      continue;
    }

    // Remove deallocation.
    if (DeallocStackInst *DSI = dyn_cast<DeallocStackInst>(Inst)) {
      if (DSI->getOperand() == ASI) {
        Inst->eraseFromParent();
        NumInstRemoved++;
        // No need to continue scanning after deallocation.
        break;
      }
    }
  }
}

void StackAllocationPromoter::addBlockArguments(BlockSet &PhiBlocks) {
  DEBUG(llvm::dbgs() << "*** Adding new block arguments.\n");

  SILModule &M = ASI->getModule();

  for (auto Block : PhiBlocks)
    new (M) SILArgument(Block, ASI->getElementType());
}

SILValue
StackAllocationPromoter::getLiveOutValue(BlockSet &PhiBlocks,
                                         SILBasicBlock *StartBB) {
  DEBUG(llvm::dbgs() << "*** Searching for a value definition.\n");
  // Walk the Dom tree in search of a defining value:
  for (DomTreeNode *Node = DT->getNode(StartBB); Node; Node = Node->getIDom()) {
    SILBasicBlock *BB = Node->getBlock();

    // If there is a store (that must come after the phi), use its value.
    BlockToInstMap::iterator it = LastStoreInBlock.find(BB);
    if (it != LastStoreInBlock.end())
      if (StoreInst *St = dyn_cast_or_null<StoreInst>(it->second)) {
        DEBUG(llvm::dbgs() << "*** Found Store def " << *St->getSrc());
        return St->getSrc();
      }

    // If there is a Phi definition in this block:
    if (PhiBlocks.count(BB)) {
      // Return the dummy instruction that represents the new value that we will
      // add to the basic block.
      SILValue Phi = BB->getBBArg(BB->getNumBBArg()-1);
      DEBUG(llvm::dbgs() << "*** Found a dummy Phi def " << *Phi);
      return Phi;
    }

    // Move to the next dominating block.
    DEBUG(llvm::dbgs() << "*** Walking up the iDOM.\n");
  }
  DEBUG(llvm::dbgs() << "*** Could not find a Def. Using Undef.\n");
  return SILUndef::get(ASI->getElementType(), ASI->getModule());
}

SILValue
StackAllocationPromoter::getLiveInValue(BlockSet &PhiBlocks,
                                        SILBasicBlock *BB) {
  // First, check if there is a Phi value in the current block. We know that
  // our loads happen before stores, so we need to first check for Phi nodes
  // in the first block, but stores first in all other stores in the idom
  // chain.
  if (PhiBlocks.count(BB)) {
    DEBUG(llvm::dbgs() << "*** Found a local Phi definition.\n");
    return BB->getBBArg(BB->getNumBBArg()-1);
  }

  assert(DT->getNode(BB) && "Block is not in dominator tree!");

  // No phi for this value in this block means that the value flowing
  // out of the immediate dominator reaches here.
  DomTreeNode *IDom = DT->getNode(BB)->getIDom();
  assert(IDom &&
         "Attempt to get live-in value for alloc_stack in entry block!");

  return getLiveOutValue(PhiBlocks, IDom->getBlock());
}

void StackAllocationPromoter::fixPhiPredBlock(BlockSet &PhiBlocks,
                                              SILBasicBlock *Dest,
                                              SILBasicBlock *Pred) {
  TermInst *TI = Pred->getTerminator();
  DEBUG(llvm::dbgs() << "*** Fixing the terminator " << TI << ".\n");

  SILValue Def = getLiveOutValue(PhiBlocks, Pred);

  DEBUG(llvm::dbgs() << "*** Found the definition: " << *Def);

  addArgumentToBranch(Def, Dest, TI);
  TI->eraseFromParent();
}

void StackAllocationPromoter::fixBranchesAndUses(BlockSet &PhiBlocks) {
  // First update uses of the value.
  SmallVector<LoadInst *, 4> collectedLoads;
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E;) {
    auto *Inst = UI->getUser();
    UI++;

    collectedLoads.clear();
    collectLoads(Inst, collectedLoads);
    for (LoadInst *LI : collectedLoads) {
      SILValue Def;
      // If this block has no predecessors then nothing dominates it and
      // the instruction is unreachable. If the instruction we're
      // examining is a value, replace it with undef. Either way, delete
      // the instruction and move on.
      SILBasicBlock *BB = LI->getParent();
      if (BB->pred_empty() || !DT->getNode(BB)) {
        Def = SILUndef::get(ASI->getElementType(), ASI->getModule());
      } else {
        Def = getLiveInValue(PhiBlocks, BB);
      }
      
      DEBUG(llvm::dbgs() << "*** Replacing " << *LI << " with Def " << *Def);
        
      // Replace the load with the definition that we found.
      replaceLoad(LI, Def, ASI);
      NumInstRemoved++;
    }
    if (isa<DebugValueAddrInst>(Inst)) {
      // If this block has no predecessors then nothing dominates it and
      // the instruction is unreachable. If the instruction we're
      // examining is a value, replace it with undef. Either way, delete
      // the instruction and move on.
      SILBasicBlock *BB = Inst->getParent();
      if (BB->pred_empty() || !DT->getNode(BB)) {
        Inst->eraseFromParent();
      } else {
        // Otherwise we expect a DebugValueAddr, which we will replace
        // with a DebugValue.
        SILValue Def = getLiveInValue(PhiBlocks, BB);
        promoteDebugValueAddr(cast<DebugValueAddrInst>(Inst), Def, B);
      }
      NumInstRemoved++;
    }
  }

  // Now that all of the uses are fixed we can fix the branches that point
  // to the blocks with the added arguments.

  // For each Block with a new Phi argument:
  for (auto Block : PhiBlocks) {
    // Fix all predecessors.
    for (auto PBB : Block->getPreds()) {
      assert(PBB && "Invalid block!");
      fixPhiPredBlock(PhiBlocks, Block, PBB);
    }
  }
}

void StackAllocationPromoter::pruneAllocStackUsage() {
  DEBUG(llvm::dbgs() << "*** Pruning : " << *ASI);
  BlockSet Blocks;

  // Insert all of the blocks that ASI is live in.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI)
    Blocks.insert(UI->getUser()->getParent());

  // Clear AllocStack state.
  LastStoreInBlock.clear();

  for (auto Block : Blocks) {
    StoreInst *SI = promoteAllocationInBlock(Block);
    LastStoreInBlock[Block] = SI;
  }

  DEBUG(llvm::dbgs() << "*** Finished pruning : " << *ASI);
}

/// Compute the dominator tree levels for DT.
static void computeDomTreeLevels(DominanceInfo *DT,
                                 DomTreeLevelMap &DomTreeLevels) {
  // TODO: This should happen once per function.
  SmallVector<DomTreeNode *, 32> Worklist;
  DomTreeNode *Root = DT->getRootNode();
  DomTreeLevels[Root] = 0;
  Worklist.push_back(Root);
  while (!Worklist.empty()) {
    DomTreeNode *Node = Worklist.pop_back_val();
    unsigned ChildLevel = DomTreeLevels[Node] + 1;
    for (auto CI = Node->begin(), CE = Node->end(); CI != CE; ++CI) {
      DomTreeLevels[*CI] = ChildLevel;
      Worklist.push_back(*CI);
    }
  }
}

void StackAllocationPromoter::promoteAllocationToPhi() {
  DEBUG(llvm::dbgs() << "*** Placing Phis for : " << *ASI);

  // A list of blocks that will require new Phi values.
  BlockSet PhiBlocks;

  // The "piggy-bank" data-structure that we use for processing the dom-tree
  // bottom-up.
  NodePriorityQueue PQ;

  // Collect all of the stores into the AllocStack. We know that at this point
  // we have at most one store per block.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();
    // We need to place Phis for this block.
    if (isa<StoreInst>(II)) {
      // If the block is in the dom tree (dominated by the entry block).
      if (DomTreeNode *Node = DT->getNode(II->getParent()))
        PQ.push(std::make_pair(Node, DomTreeLevels[Node]));
    }
  }

  DEBUG(llvm::dbgs() << "*** Found: " << PQ.size() << " Defs\n");

  // A list of nodes for which we already calculated the dominator frontier.
  llvm::SmallPtrSet<DomTreeNode *, 32> Visited;

  SmallVector<DomTreeNode *, 32> Worklist;

  // Scan all of the definitions in the function bottom-up using the priority
  // queue.
  while (!PQ.empty()) {
    DomTreeNodePair RootPair = PQ.top();
    PQ.pop();
    DomTreeNode *Root = RootPair.first;
    unsigned RootLevel = RootPair.second;

    // Walk all dom tree children of Root, inspecting their successors. Only
    // J-edges, whose target level is at most Root's level are added to the
    // dominance frontier.
    Worklist.clear();
    Worklist.push_back(Root);

    while (!Worklist.empty()) {
      DomTreeNode *Node = Worklist.pop_back_val();
      SILBasicBlock *BB = Node->getBlock();

      // For all successors of the node:
      for (auto &Succ : BB->getSuccessors()) {
        DomTreeNode *SuccNode = DT->getNode(Succ);

        // Skip D-edges (edges that are dom-tree edges).
        if (SuccNode->getIDom() == Node)
          continue;

        // Ignore J-edges that point to nodes that are not smaller or equal
        // to the root level.
        unsigned SuccLevel = DomTreeLevels[SuccNode];
        if (SuccLevel > RootLevel)
          continue;

        // Ignore visited nodes.
        if (!Visited.insert(SuccNode).second)
          continue;

        // If the new PHInode is not dominated by the allocation then it's dead.
        if (!DT->dominates(ASI->getParent(), SuccNode->getBlock()))
            continue;

        // If the new PHInode is properly dominated by the deallocation then it
        // is obviously a dead PHInode, so we don't need to insert it.
        if (DSI && DT->properlyDominates(DSI->getParent(),
                                         SuccNode->getBlock()))
          continue;

        // The successor node is a new PHINode. If this is a new PHI node
        // then it may require additional definitions, so add it to the PQ.
        if (PhiBlocks.insert(Succ).second)
          PQ.push(std::make_pair(SuccNode, SuccLevel));
      }

      // Add the children in the dom-tree to the worklist.
      for (auto CI = Node->begin(), CE = Node->end(); CI != CE; ++CI)
        if (!Visited.count(*CI))
          Worklist.push_back(*CI);
    }
  }

  DEBUG(llvm::dbgs() << "*** Found: " << PhiBlocks.size() << " new PHIs\n");
  NumPhiPlaced += PhiBlocks.size();

  // At this point we calculated the locations of all of the new Phi values.
  // Next, add the Phi values and promote all of the loads and stores into the
  // new locations.

  // Replace the dummy values with new block arguments.
  addBlockArguments(PhiBlocks);

  // Hook up the Phi nodes, loads, and debug_value_addr with incoming values.
  fixBranchesAndUses(PhiBlocks);

  DEBUG(llvm::dbgs() << "*** Finished placing Phis ***\n");
}

void StackAllocationPromoter::run() {
  // Reduce the number of load/stores in the function to minimum.
  // After this phase we are left with up to one load and store
  // per block and the last store is recorded.
  pruneAllocStackUsage();

  // Replace AllocStacks with Phi-nodes.
  promoteAllocationToPhi();
}

bool MemoryToRegisters::run() {
  bool Changed = false;

  Changed = splitAllCriticalEdges(F, true, DT, nullptr);

  // Compute dominator tree node levels for the function.
  DomTreeLevelMap DomTreeLevels;
  computeDomTreeLevels(DT, DomTreeLevels);

  for (auto &BB : F) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I;
      AllocStackInst *ASI = dyn_cast<AllocStackInst>(Inst);
      if (!ASI) {
        ++I;
        continue;
      }

      DEBUG(llvm::dbgs() << "*** Memory to register looking at: " << *I);
      NumAllocStackFound++;

      // Don't handle captured AllocStacks.
      bool inSingleBlock = false;
      if (isCaptured(ASI, inSingleBlock)) {
        NumAllocStackCaptured++;
        ++I;
        continue;
      }

      // For AllocStacks that are only used within a single basic blocks, use
      // the linear sweep to remove the AllocStack.
      if (inSingleBlock) {
        removeSingleBlockAllocation(ASI);

        DEBUG(llvm::dbgs() << "*** Deleting single block AllocStackInst: "
                           << *ASI);
        I++;
        ASI->eraseFromParent();
        NumInstRemoved++;
        Changed = true;
        continue;
      }

      // Remove write-only AllocStacks.
      if (isWriteOnlyAllocation(ASI)) {
        eraseUsesOfInstruction(ASI);

        DEBUG(llvm::dbgs() << "*** Deleting store-only AllocStack: " << *ASI);
        I++;
        ASI->eraseFromParent();
        Changed = true;
        NumInstRemoved++;
        continue;
      }

      DEBUG(llvm::dbgs() << "*** Need to insert Phis for " << *ASI);

      // Promote this allocation.
      StackAllocationPromoter(ASI, DT, DomTreeLevels, B).run();

      // Make sure that all of the allocations were promoted into registers.
      assert(isWriteOnlyAllocation(ASI, /* Promoted =*/ true) &&
             "Non-write uses left behind");
      // ... and erase the allocation.
      eraseUsesOfInstruction(ASI);

      I++;
      ASI->eraseFromParent();
      NumInstRemoved++;
      Changed = true;
    }
  }
  return Changed;
}

namespace {
class SILMem2Reg : public SILFunctionTransform {

  void run() override {
    SILFunction *F = getFunction();
    DEBUG(llvm::dbgs() << "** Mem2Reg on function: " << F->getName() <<" **\n");

    DominanceAnalysis* DA = PM->getAnalysis<DominanceAnalysis>();

    bool Changed = MemoryToRegisters(*F, DA->getDomInfo(F)).run();

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  StringRef getName() override { return "SIL Mem2Reg"; }
};
} // end anonymous namespace

SILTransform *swift::createMem2Reg() {
  return new SILMem2Reg();
}

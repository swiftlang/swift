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

#define DEBUG_TYPE "sil-mem2reg"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/Support/Debug.h"
using namespace swift;

STATISTIC(NumAllocStackRemoved,  "Number of AllocStack promoted");
STATISTIC(NumAllocStackFound,    "Number of AllocStack found");
STATISTIC(NumAllocStackCaptured, "Number of AllocStack captured");
STATISTIC(NumInstRemoved,        "Number of Instructions removed");
STATISTIC(NumPhiPlaces,          "Number of Phi blocks placed");

namespace {

/// Promotes a single AllocStackInst into registers..
class StackAllocationPromoter {
  typedef SmallVector<SILBasicBlock *, 16> BlockList;
  typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
  typedef llvm::DenseSet<SILBasicBlock *> BlockSet;
  typedef llvm::DenseMap<SILBasicBlock *, SILInstruction *> BlockToInstMap;

  /// The AllocStackInst that we are handling.
  AllocStackInst *ASI;

  /// Dominator info.
  DominanceInfo *DT;

  /// Records the last store instruction in each block for a specific
  /// AllocStackInst.
  BlockToInstMap LastStoreInBlock;

  /// We place DummyInstructions during the Phi placement phase because we can't
  /// mutate the block phi values without changing the predecessors at the
  /// same time. This maps basic blocks to the dummy instructions that represent
  /// a new value.
  BlockToInstMap DummyPhiVal;

public:
  /// C'tor.
  StackAllocationPromoter(AllocStackInst *Asi, DominanceInfo *Di)
      : ASI(Asi), DT(Di) {}

  /// Promote the Allocation.
  void run();

private:
  /// \brief Promote AllocStacks into SSA.
  void promoteAllocationToPhi();

  /// \brief Calculate the dominator frontier for block \p SBB and store the
  /// frontier blocks in \p Blocks.
  void calculateDomFrontier(SILBasicBlock *SBB, BlockList &Blocks);

  /// \brief Extend the basic block argument list for all of the blocks in
  /// \p Blocks.
  void placeDummyPhiValues(BlockSet &PhiBlocks);

  /// \brief Replace the dummy nodes with new block arguments.
  void addBlockArguments(BlockSet &PhiBlocks);

  /// \brief Fix all of the Br instructions and the loads to use the AllocStack
  /// definitions (which include stores and Phis).
  void fixBranchesAndLoads(BlockSet &Blocks);

  /// \brief update the branch instructions with the new Phi argument.
  /// The blocks in \p PhiBlocks are blocks that define a value, \p Dest is
  /// the branch destination, and \p Pred is the predecessors who's branch we
  /// modify.
  void fixPhiPredBlock(BlockSet &PhiBlocks, SILBasicBlock *Dest,
                       SILBasicBlock *Pred);

  /// \brief Get the definition for block.
  SILValue getDefinitionForValue(BlockSet &PhiBlocks, SILBasicBlock *StartBB);

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
  DominanceInfo DT;

  /// \brief Check if the AllocStackInst \p ASI is captured by any of its users.
  bool isCaptured(AllocStackInst *ASI);

  /// \brief Check if the AllocStackInst \p ASI is only used within a single
  /// basic block.
  bool isSingleBlockUsage(AllocStackInst *ASI);

  /// \brief Check if the AllocStackInst \p ASI is only written into.
  bool isWriteOnlyAllocation(AllocStackInst *ASI);

  /// \brief Promote all of the AllocStacks in a single basic block in one
  /// linear scan. Note: This function deletes all of the users of the
  /// AllocStackInst, including the DeallocStackInst but it does not remove the
  /// AllocStackInst itself!
  void removeSingleBlockAllocation(AllocStackInst *ASI);

public:
  /// C'tor
  MemoryToRegisters(SILFunction &Func) : F(Func), DT(&F) {}

  /// Promote memory to registers.
  void run();
};

} // end anonymous namespace.

void StackAllocationPromoter::calculateDomFrontier(SILBasicBlock *SBB,
                                                   BlockList &Blocks) {
  // Dominated blocks to visit.
  SmallVector<DomTreeNode *, 16> Worklist;

  // Visited successors.
  llvm::DenseSet<SILBasicBlock *> VisitedSucc;

  // Start with the first node.
  Worklist.push_back(DT->getNode(SBB));

  // For all blocks that are dominated by SBB:
  while (Worklist.size()) {
    DomTreeNode *Node = Worklist.back();
    Worklist.pop_back();

    // For all of the children of the node in the dom tree:
    for (DomTreeNode *Child : Node->getChildren())
      Worklist.push_back(Child);

    // Find successors that are not dominated by SBB.
    SILBasicBlock *BB = Node->getBlock();
    for (SILBasicBlock *SuccBB : BB->getSuccs()) {
      // If this block is dominated by our node then it is not in the dom
      // frontier.
      if (SuccBB->getSinglePredecessor() || DT->properlyDominates(SBB, SuccBB))
        continue;

      // If this is the first time we visited this node insert it to the
      // Dominance Frontier list.
      if (VisitedSucc.insert(SuccBB).second)
        Blocks.push_back(SuccBB);
    }
  }
}

/// Returns true if this AllocStacks is captured.
bool MemoryToRegisters::isCaptured(AllocStackInst *ASI) {
  // For all users of the AllocStack instruction.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();

    // Loads are okay.
    if (isa<LoadInst>(II))
      continue;

    // We can store into an AllocStack (but not the pointer).
    if (StoreInst *SI = dyn_cast<StoreInst>(II))
      if (SI->getDest().getDef() == ASI)
        continue;

    // Deallocation is also okay.
    if (isa<DeallocStackInst>(II))
      continue;

    // Other instructions are assumed to capture the AllocStack.
    DEBUG(llvm::errs() << "*** AllocStack is captured by: " << *II);
    return true;
  }

  // None of the users capture the AllocStack.
  return false;
}

/// Returns true if the AllocStack is only stored into.
bool MemoryToRegisters::isWriteOnlyAllocation(AllocStackInst *ASI) {
  // For all users of the AllocStack:
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();

    // It is okay to store into this AllocStack.
    if (StoreInst *SI = dyn_cast<StoreInst>(II))
      if (!isa<AllocStackInst>(SI->getSrc()))
        continue;

    // It is also okay to deallocate.
    if (isa<DeallocStackInst>(II))
      continue;

    // Can't do anything else with it.
    DEBUG(llvm::errs() << "*** AllocStack is loaded by: " << *II);
    return false;
  }

  return true;
}

/// Returns true if this AllocStack is only used within a single basic block.
bool MemoryToRegisters::isSingleBlockUsage(AllocStackInst *ASI) {
  assert(!isCaptured(ASI) && "This AllocStack must not be captured");
  SILBasicBlock *BB = ASI->getParent();

  // All of the users of the AllocStack must be in the same block.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI)
    if (UI->getUser()->getParent() != BB)
      return false;

  return true;
}

StoreInst *
StackAllocationPromoter::promoteAllocationInBlock(SILBasicBlock *BB) {
  DEBUG(llvm::errs() << "*** Promoting ASI in block: " << *ASI);

  // We don't know the value of the alloca until we find the first store.
  SILValue RunningVal = SILValue();
  // Keep track of the last StoreInst that we found.
  StoreInst *LastStore = 0;

  // For all instructions in the block.
  for (auto BBI = BB->begin(), E = BB->end(); BBI != E;) {
    SILInstruction *Inst = BBI++;
    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      // Make sure we are loading from this ASI.
      if (LI->getOperand().getDef() != ASI)
        continue;

      if (RunningVal.isValid()) {
        // If we are loading from the AllocStackInst and we already know the
        // conent of the Alloca then use it.
        DEBUG(llvm::errs() << "*** Promoting load: " << *LI);
        SILValue(Inst, 0).replaceAllUsesWith(RunningVal);
        Inst->eraseFromParent();
        NumInstRemoved++;
      } else {
        // If we don't know the content of the AllocStack then the loaded
        // value *is* the new value;
        DEBUG(llvm::errs() << "*** First load: " << *LI);
        RunningVal = LI;
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
        DEBUG(llvm::errs() << "*** Removing redundant store: " << *LastStore);
        LastStore->eraseFromParent();
      }
      LastStore = SI;
      continue;
    }

    // Stop on deallocation.
    if (DeallocStackInst *DSI = dyn_cast<DeallocStackInst>(Inst)) {
      if (DSI->getOperand() == ASI)
        break;
    }
  }
  if (LastStore) {
    DEBUG(llvm::errs() << "*** Finished promotion. Last store: " << *LastStore);
  } else {
    DEBUG(llvm::errs() << "*** Finished promotion with no stores.\n");
  }
  return LastStore;
}

void MemoryToRegisters::removeSingleBlockAllocation(AllocStackInst *ASI) {
  DEBUG(llvm::errs() << "*** Promoting in-block: " << *ASI);

  SILBasicBlock *BB = ASI->getParent();

  // The default value of the AllocStack is NULL because we don't have
  // unilitialized variables in Swift.
  SILValue RunningVal = SILValue();

  // For all instructions in the block.
  for (auto BBI = BB->begin(), E = BB->end(); BBI != E;) {
    SILInstruction *Inst = BBI++;
    // Remove instructions that we are loading from. Replace the loaded value
    // with our running value.
    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      if (LI->getOperand().getDef() == ASI) {
        assert(RunningVal.isValid() &&
               "The AllocStack must be initialized before usage.");
        SILValue(Inst, 0).replaceAllUsesWith(RunningVal);
        Inst->eraseFromParent();
        NumInstRemoved++;
        continue;
      }
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
  DEBUG(llvm::errs() << "*** Replacing dummy vals with new block arguments.\n");

  SILModule &M = ASI->getModule();

  for (auto Block : PhiBlocks) {
    SILInstruction *Dummy = DummyPhiVal[Block];
    SILType Ty = Dummy->getType(0);
    SILArgument *Arg = new (M) SILArgument(Ty, Block);
    SILValue(Dummy, 0).replaceAllUsesWith(Arg);
    Dummy->eraseFromParent();
  }
}

void StackAllocationPromoter::placeDummyPhiValues(BlockSet &PhiBlocks) {
  DEBUG(llvm::errs() << "*** Placing dummy values for " << PhiBlocks.size()
                     << " Blocks.\n");

  SILModule &M = ASI->getModule();
  SILType Ty = ASI->getElementType();
  SILFunction *F = ASI->getParent()->getParent();

  for (auto Block : PhiBlocks) {
    SILBuilder Builder(F->begin()->begin());
    // Add a dummy value that will emulate the new argument that we will add
    // to the Phi later on.
    DummyPhiVal[Block] =
        Builder.createLoad(ASI->getLoc(), SILUndef::get(Ty, M));
  }
}

SILValue
StackAllocationPromoter::getDefinitionForValue(BlockSet &PhiBlocks,
                                                  SILBasicBlock *StartBB) {
  DEBUG(llvm::errs() << "*** Searching for a value definition.\n");
  // Walk the Dom tree in search of a defining value:
  DomTreeNode *Node = DT->getNode(StartBB);
  while (true) {
    SILBasicBlock *BB = Node->getBlock();

    // If there is a store (that must comes after the Phi) use its value.
    BlockToInstMap::iterator it = LastStoreInBlock.find(BB);
    if (it != LastStoreInBlock.end())
      if (StoreInst *St = dyn_cast_or_null<StoreInst>(it->second)) {
        DEBUG(llvm::errs() << "*** Found Store def " << *St->getSrc());
        return St->getSrc();
      }

    // If there is a Phi definition in this block:
    if (PhiBlocks.count(BB)) {
      // Return the dummy instruction that represents the new value that we will
      // add to the basic block.
      assert(DummyPhiVal.count(BB) && "Can't find ");
      SILValue Arg = DummyPhiVal[BB];
      DEBUG(llvm::errs() << "*** Found a dummy Phi def " << *Arg);
      return Arg;
    }

    // Move to the next dominating block.
    Node = Node->getIDom();
    if (!Node) {
      DEBUG(llvm::errs() << "*** Could not find a Def. Using Undef.\n");
      return SILValue();
    }

    DEBUG(llvm::errs() << "*** Walking up the iDOM.\n");
  }

  llvm_unreachable("Could not find a definition");
}


/// \brief Add an argument, \p val, to the branch-edge that is pointing into
/// block \p Dest. Return a new instruction and do not erase the old
/// instruction.
static TermInst *addArgumentToBranch(SILValue Val, SILBasicBlock *Dest,
                                     TermInst *Branch) {
  SILBuilder Builder(Branch);

  if (CondBranchInst *CBI = dyn_cast<CondBranchInst>(Branch)) {
    DEBUG(llvm::errs() << "*** Fixing CondBranchInst.\n");

    SmallVector<SILValue, 8> TrueArgs;
    SmallVector<SILValue, 8> FalseArgs;

    for (auto A : CBI->getTrueArgs())
      TrueArgs.push_back(A);

    for (auto A : CBI->getFalseArgs())
      FalseArgs.push_back(A);

    if (Dest == CBI->getTrueBB())
      TrueArgs.push_back(Val);
    else
      FalseArgs.push_back(Val);

    return Builder.createCondBranch(CBI->getLoc(), CBI->getCondition(),
                                    CBI->getTrueBB(), TrueArgs,
                                    CBI->getFalseBB(), FalseArgs);
  }

  if (BranchInst *BI = dyn_cast<BranchInst>(Branch)) {
    DEBUG(llvm::errs() << "*** Fixing BranchInst.\n");

    SmallVector<SILValue, 8> Args;

    for (auto A : BI->getArgs())
      Args.push_back(A);

    Args.push_back(Val);
    return Builder.createBranch(BI->getLoc(), BI->getDestBB(), Args);
  }

  llvm_unreachable("unsupported terminator");
}

void StackAllocationPromoter::fixPhiPredBlock(BlockSet &PhiBlocks,
                                              SILBasicBlock *Dest,
                                              SILBasicBlock *Pred) {
  TermInst *TI = Pred->getTerminator();
  DEBUG(llvm::errs() << "*** Fixing the terminator " << TI << ".\n");

  SILValue Def = getDefinitionForValue(PhiBlocks, Pred);
  if (!Def.isValid())
    Def =  SILUndef::get(ASI->getElementType(), ASI->getModule());

  DEBUG(llvm::errs() << "*** Found the definition: " << *Def);

  addArgumentToBranch(Def, Dest, TI);
  TI->eraseFromParent();
}

void StackAllocationPromoter::fixBranchesAndLoads(BlockSet &PhiBlocks) {
  // Start by fixing loads:
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E;) {
    LoadInst *LI = dyn_cast<LoadInst>(UI->getUser());
    UI++;
    if (!LI)
      continue;

    // First, check if there is a Phi value in the current block. We know that
    // our loads happen before stores, so we need to first check for Phi nodes
    // in the first block, but stores first in all other stores in the idom
    // chain.
    SILBasicBlock *BB = LI->getParent();
    if (PhiBlocks.count(BB)) {
      DEBUG(llvm::errs() << "*** Found a local Phi definiton.\n");
      // Replace the load with the last argument of the BB, which is our Phi.
      assert(DummyPhiVal.count(BB) && "Can't find dummy val for current block");
      SILValue(LI, 0).replaceAllUsesWith(DummyPhiVal[BB]);
      LI->eraseFromParent();
      NumInstRemoved++;
      // We are done with this Load. Move on to the next Load.
      continue;
    }

    // We know that the load definition is not in our block, so start the search
    // one level up the idom tree.
    DomTreeNode *Node = DT->getNode(BB);
    Node = Node->getIDom();
    assert(Node && "Promoting a load in the entry block ?");
    BB = Node->getBlock();

    SILValue Def = getDefinitionForValue(PhiBlocks, BB);
    DEBUG(llvm::errs() << "*** Replacing " << *LI << " with Def " << *Def);

    // Replace the load with the definition that we found.
    SILValue(LI, 0).replaceAllUsesWith(Def);
    LI->eraseFromParent();
    NumInstRemoved++;
  } // End of LoadInst loop.

  // Now that all of the loads are fixed we can fix the Branches that point
  // to the blocks with the added arguments. We keep a list of the fixed
  // predecessors.
  BlockSet FixedPreds;

  // For each Block with a new Phi argument:
  for (auto Block : PhiBlocks) {

    // For each predecessor block.
    SmallVector<SILBasicBlock *, 2> Preds(Block->pred_begin(),
                                          Block->pred_end());

    for (auto PBB : Preds) {
      assert(PBB && "Invalid block!");
      // Handle every branch only once.
      if (!FixedPreds.insert(PBB).second)
        continue;

      fixPhiPredBlock(PhiBlocks, Block, PBB);
    }
  }
}

void StackAllocationPromoter::pruneAllocStackUsage() {
  DEBUG(llvm::errs() << "*** Pruning : " << *ASI);
  BlockSet Blocks;

  // Insert all of the blocks that ASI is live in.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI)
    Blocks.insert(UI->getUser()->getParent());

  // Clear AllocStack state.
  LastStoreInBlock.clear();
  DummyPhiVal.clear();

  for (auto Block : Blocks) {
    StoreInst *SI = promoteAllocationInBlock(Block);
    LastStoreInBlock[Block] = SI;
  }

  DEBUG(llvm::errs() << "*** Finished pruning : " << *ASI);
}

void StackAllocationPromoter::promoteAllocationToPhi() {
  DEBUG(llvm::errs() << "*** Placing Phis for : " << *ASI);

  // A Worklist of defining values (StoreInst).
  llvm::SmallVector<SILBasicBlock *, 32> WorkList;

  // A list of blocks that will require new Phi values.
  BlockSet PhiBlocks;

  // Collect all of the stores into the AllocStack. We know that at this point
  // we have at most one store per block.
  for (auto UI = ASI->use_begin(), E = ASI->use_end(); UI != E; ++UI) {
    SILInstruction *II = UI->getUser();
    // We need to place Phis for this block.
    if (isa<StoreInst>(II))
      WorkList.push_back(II->getParent());
  }

  // Find all of the blocks that require Phi values using a dominator frontier.
  // The blocks with Phis are also definitions that may require generating new
  // Phi values.
  while (WorkList.size()) {
    SILBasicBlock *BB = WorkList.back();
    WorkList.pop_back();
    DEBUG(llvm::errs() << "*** Looking at &BB: " << BB << "\n");

    BlockList Frontier;
    calculateDomFrontier(BB, Frontier);

    DEBUG(llvm::errs() << "*** Found frontier (" << Frontier.size() << ")\n");

    unsigned NewStores = 0;
    for (auto &NewPhiBlock : Frontier) {
      assert(NewPhiBlock && "Invalid block");
      // If this is a new Phi node then it is also a new definition of a value
      // so we need to push this block to the work list.
      if (PhiBlocks.insert(NewPhiBlock).second) {
        NewStores++;
        NumPhiPlaces++;
        WorkList.push_back(NewPhiBlock);
      }
    }

    DEBUG(llvm::errs() << "*** New Phis required (" << NewStores << ")\n");
  }

  // At this point we calculated the locations of all of the new Phi values.
  // Next, add the Phi values and promote all of the loads and stores into the
  // new locations.

  // Place dummy Phi values that will imitate the new argument that we'll add
  // to each basic block.
  placeDummyPhiValues(PhiBlocks);

  // Hook up the Phi nodes and the loads with storing values.
  fixBranchesAndLoads(PhiBlocks);

  // Replace the dummy values with new block arguments.
  addBlockArguments(PhiBlocks);

  DEBUG(llvm::errs() << "*** Finished placing Phis ***\n");
}

void StackAllocationPromoter::run() {
  // Reduce the number of load/stores in the function to minimum.
  // After this phase we are left with up to one load and store
  // per block and the last store is recorded.
  pruneAllocStackUsage();

  // Replace AllocStacks with Phi-nodes.
  promoteAllocationToPhi();
}

void MemoryToRegisters::run() {
  for (auto &BB : F) {
    auto I = BB.begin(), E = BB.end();
    while (I != E) {
      SILInstruction *Inst = I;
      AllocStackInst *ASI = dyn_cast<AllocStackInst>(Inst);
      if (!ASI) {
        ++I;
        continue;
      }

      DEBUG(llvm::errs() << "*** Memory to register looking at: " << *I);
      NumAllocStackFound++;

      // Don't handle captured AllocStacks.
      if (isCaptured(ASI)) {
        NumAllocStackCaptured++;
        ++I;
        continue;
      }

      // For AllocStacks that are only used within a single basic blocks, use
      // the linear sweep to remove the AllocStack.
      if (isSingleBlockUsage(ASI)) {
        removeSingleBlockAllocation(ASI);

        DEBUG(llvm::errs() << "*** Deleting single block AllocStackInst: "
                           << *ASI);
        I++;
        ASI->eraseFromParent();
        NumInstRemoved++;
        NumAllocStackRemoved++;
        continue;
      }

      // Remove write-only AllocStacks.
      if (isWriteOnlyAllocation(ASI)) {
        eraseUsesOfInstruction(ASI);

        DEBUG(llvm::errs() << "*** Deleting store-only AllocStack: " << *ASI);
        I++;
        ASI->eraseFromParent();
        NumInstRemoved++;
        NumAllocStackRemoved++;
        continue;
      }

      DEBUG(llvm::errs() << "*** Need to insert Phis for " << *ASI);

      // Promote this allocation.
      StackAllocationPromoter(ASI, &DT).run();

      // Make sure that all of the allocations were promoted into registers.
      assert(isWriteOnlyAllocation(ASI) && "Loads left behind");
      // ... and erase the allocation.
      eraseUsesOfInstruction(ASI);

      I++;
      ASI->eraseFromParent();
      NumInstRemoved++;
      NumAllocStackRemoved++;
    }
  }
}

void promoteAllocasInFunction(SILFunction &F) { MemoryToRegisters(F).run(); }

void swift::performSILMem2Reg(SILModule *M) {
  for (auto &F : *M)
    if (!F.isExternalDeclaration())
      promoteAllocasInFunction(F);
}

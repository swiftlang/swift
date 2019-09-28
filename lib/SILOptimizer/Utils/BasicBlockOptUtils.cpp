//===--- BasicBlockOptUtils.cpp - SILOptimizer basic block utilities ------===//
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

#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

using namespace swift;

/// Remove all instructions in the body of \p BB in safe manner by using
/// undef.
void swift::clearBlockBody(SILBasicBlock *BB) {
  // Instructions in the dead block may be used by other dead blocks.  Replace
  // any uses of them with undef values.
  while (!BB->empty()) {
    // Grab the last instruction in the BB.
    auto *Inst = &BB->back();

    // Replace any still-remaining uses with undef values and erase.
    Inst->replaceAllUsesOfAllResultsWithUndef();
    Inst->eraseFromParent();
  }
}

// Handle the mechanical aspects of removing an unreachable block.
void swift::removeDeadBlock(SILBasicBlock *BB) {
  // Clear the body of BB.
  clearBlockBody(BB);

  // Now that the BB is empty, eliminate it.
  BB->eraseFromParent();
}


bool swift::removeUnreachableBlocks(SILFunction &Fn) {
  // All reachable blocks, but does not include the entry block.
  llvm::SmallPtrSet<SILBasicBlock *, 8> Visited;

  // Walk over the CFG, starting at the entry block, until all reachable blocks are visited.
  llvm::SmallVector<SILBasicBlock *, 8> Worklist(1, Fn.getEntryBlock());
  while (!Worklist.empty()) {
    SILBasicBlock *BB = Worklist.pop_back_val();
    for (auto &Succ : BB->getSuccessors()) {
      if (Visited.insert(Succ).second)
        Worklist.push_back(Succ);
    }
  }

  // Remove the blocks we never reached. Exclude the entry block from the iteration because it's
  // not included in the Visited set.
  bool Changed = false;
  for (auto It = std::next(Fn.begin()), End = Fn.end(); It != End; ) {
    auto *BB = &*It++;
    if (!Visited.count(BB)) {
      removeDeadBlock(BB);
      Changed = true;
    }
  }
  return Changed;
}

/// Helper function to perform SSA updates in case of jump threading.
void swift::updateSSAAfterCloning(BasicBlockCloner &Cloner,
                                  SILBasicBlock *SrcBB, SILBasicBlock *DestBB) {
  SILSSAUpdater SSAUp;
  for (auto AvailValPair : Cloner.AvailVals) {
    ValueBase *Inst = AvailValPair.first;
    if (Inst->use_empty())
      continue;

    SILValue NewRes(AvailValPair.second);

    SmallVector<UseWrapper, 16> UseList;
    // Collect the uses of the value.
    for (auto Use : Inst->getUses())
      UseList.push_back(UseWrapper(Use));

    SSAUp.Initialize(Inst->getType());
    SSAUp.AddAvailableValue(DestBB, Inst);
    SSAUp.AddAvailableValue(SrcBB, NewRes);

    if (UseList.empty())
      continue;

    // Update all the uses.
    for (auto U : UseList) {
      Operand *Use = U;
      SILInstruction *User = Use->getUser();
      assert(User && "Missing user");

      // Ignore uses in the same basic block.
      if (User->getParent() == DestBB)
        continue;

      SSAUp.RewriteUse(*Use);
    }
  }
}

// FIXME: Remove this. SILCloner should not create critical edges.
bool BasicBlockCloner::splitCriticalEdges(DominanceInfo *DT, SILLoopInfo *LI) {
  bool changed = false;
  // Remove any critical edges that the EdgeThreadingCloner may have
  // accidentally created.
  for (unsigned succIdx = 0, succEnd = origBB->getSuccessors().size();
       succIdx != succEnd; ++succIdx) {
    if (nullptr != splitCriticalEdge(origBB->getTerminator(), succIdx, DT, LI))
      changed |= true;
  }
  for (unsigned succIdx = 0, succEnd = getNewBB()->getSuccessors().size();
       succIdx != succEnd; ++succIdx) {
    auto *newBB =
        splitCriticalEdge(getNewBB()->getTerminator(), succIdx, DT, LI);
    changed |= (newBB != nullptr);
  }
  return changed;
}

// Populate 'projections' with the chain of address projections leading
// to and including 'inst'.
//
// Populate 'inBlockDefs' with all the non-address value definitions in
// the block that will be used outside this block after projection sinking.
//
// Return true on success, even if projections is empty.
bool SinkAddressProjections::analyzeAddressProjections(SILInstruction *inst) {
  projections.clear();
  inBlockDefs.clear();

  SILBasicBlock *bb = inst->getParent();
  auto pushOperandVal = [&](SILValue def) {
    if (def->getParentBlock() != bb)
      return true;

    if (!def->getType().isAddress()) {
      inBlockDefs.insert(def);
      return true;
    }
    if (auto *addressProj = dyn_cast<SingleValueInstruction>(def)) {
      if (addressProj->isTriviallyDuplicatable()) {
        projections.push_back(addressProj);
        return true;
      }
    }
    // Can't handle a multi-value or unclonable address producer.
    return false;
  };
  // Check the given instruction for any address-type results.
  for (auto result : inst->getResults()) {
    if (!isUsedOutsideOfBlock(result))
      continue;
    if (!pushOperandVal(result))
      return false;
  }
  // Recurse upward through address projections.
  for (unsigned idx = 0; idx < projections.size(); ++idx) {
    // Only one address result/operand can be handled per instruction.
    if (projections.size() != idx + 1)
      return false;

    for (SILValue operandVal : projections[idx]->getOperandValues())
      pushOperandVal(operandVal);
  }
  return true;
}

// Clone the projections gathered by 'analyzeAddressProjections' at
// their use site outside this block.
bool SinkAddressProjections::cloneProjections() {
  if (projections.empty())
    return false;

  SILBasicBlock *bb = projections.front()->getParent();
  SmallVector<Operand *, 4> usesToReplace;
  // Clone projections in last-to-first order.
  for (unsigned idx = 0; idx < projections.size(); ++idx) {
    auto *oldProj = projections[idx];
    assert(oldProj->getParent() == bb);
    usesToReplace.clear();
    for (Operand *use : oldProj->getUses()) {
      if (use->getUser()->getParent() != bb)
        usesToReplace.push_back(use);
    }
    for (Operand *use : usesToReplace) {
      auto *newProj = oldProj->clone(use->getUser());
      use->set(cast<SingleValueInstruction>(newProj));
    }
  }
  return true;
}

void StaticInitCloner::add(SILInstruction *InitVal) {
  // Don't schedule an instruction twice for cloning.
  if (NumOpsToClone.count(InitVal) != 0)
    return;

  ArrayRef<Operand> Ops = InitVal->getAllOperands();
  NumOpsToClone[InitVal] = Ops.size();
  if (Ops.empty()) {
    // It's an instruction without operands, e.g. a literal. It's ready to be
    // cloned first.
    ReadyToClone.push_back(InitVal);
  } else {
    // Recursively add all operands.
    for (const Operand &Op : Ops) {
      add(cast<SingleValueInstruction>(Op.get()));
    }
  }
}

SingleValueInstruction *
StaticInitCloner::clone(SingleValueInstruction *InitVal) {
  assert(NumOpsToClone.count(InitVal) != 0 && "InitVal was not added");
  // Find the right order to clone: all operands of an instruction must be
  // cloned before the instruction itself.
  while (!ReadyToClone.empty()) {
    SILInstruction *I = ReadyToClone.pop_back_val();

    // Clone the instruction into the SILGlobalVariable
    visit(I);

    // Check if users of I can now be cloned.
    for (SILValue result : I->getResults()) {
      for (Operand *Use : result->getUses()) {
        SILInstruction *User = Use->getUser();
        if (NumOpsToClone.count(User) != 0 && --NumOpsToClone[User] == 0)
          ReadyToClone.push_back(User);
      }
    }
  }
  return cast<SingleValueInstruction>(getMappedValue(InitVal));
}


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

/// Remove all instructions in the body of \p bb in safe manner by using
/// undef.
void swift::clearBlockBody(SILBasicBlock *bb) {
  // Instructions in the dead block may be used by other dead blocks.  Replace
  // any uses of them with undef values.
  while (!bb->empty()) {
    // Grab the last instruction in the bb.
    auto *inst = &bb->back();

    // Replace any still-remaining uses with undef values and erase.
    inst->replaceAllUsesOfAllResultsWithUndef();
    inst->eraseFromParent();
  }
}

// Handle the mechanical aspects of removing an unreachable block.
void swift::removeDeadBlock(SILBasicBlock *bb) {
  // Clear the body of bb.
  clearBlockBody(bb);

  // Now that the bb is empty, eliminate it.
  bb->eraseFromParent();
}

bool swift::removeUnreachableBlocks(SILFunction &f) {
  // All reachable blocks, but does not include the entry block.
  llvm::SmallPtrSet<SILBasicBlock *, 8> visited;

  // Walk over the CFG, starting at the entry block, until all reachable blocks are visited.
  llvm::SmallVector<SILBasicBlock *, 8> worklist(1, f.getEntryBlock());
  while (!worklist.empty()) {
    SILBasicBlock *bb = worklist.pop_back_val();
    for (auto &Succ : bb->getSuccessors()) {
      if (visited.insert(Succ).second)
        worklist.push_back(Succ);
    }
  }

  // Remove the blocks we never reached. Exclude the entry block from the iteration because it's
  // not included in the Visited set.
  bool changed = false;
  for (auto ii = std::next(f.begin()), end = f.end(); ii != end;) {
    auto *bb = &*ii++;
    if (!visited.count(bb)) {
      removeDeadBlock(bb);
      changed = true;
    }
  }
  return changed;
}

void BasicBlockCloner::updateSSAAfterCloning() {
  // All instructions should have been checked by canCloneInstruction. But we
  // still need to check the arguments.
  for (auto arg : origBB->getArguments()) {
    if ((needsSSAUpdate |= isUsedOutsideOfBlock(arg))) {
      break;
    }
  }
  if (!needsSSAUpdate)
    return;

  SILSSAUpdater ssaUpdater;
  for (auto availValPair : availVals) {
    ValueBase *inst = availValPair.first;
    if (inst->use_empty())
      continue;

    SILValue newResult(availValPair.second);

    SmallVector<UseWrapper, 16> useList;
    // Collect the uses of the value.
    for (auto *use : inst->getUses())
      useList.push_back(UseWrapper(use));

    ssaUpdater.Initialize(inst->getType());
    ssaUpdater.AddAvailableValue(origBB, inst);
    ssaUpdater.AddAvailableValue(getNewBB(), newResult);

    if (useList.empty())
      continue;

    // Update all the uses.
    for (auto useWrapper : useList) {
      Operand *use = useWrapper; // unwrap
      SILInstruction *user = use->getUser();
      assert(user && "Missing user");

      // Ignore uses in the same basic block.
      if (user->getParent() == origBB)
        continue;

      ssaUpdater.RewriteUse(*use);
    }
  }
}

// FIXME: Remove this. SILCloner should not create critical edges.
bool BasicBlockCloner::splitCriticalEdges(DominanceInfo *domInfo,
                                          SILLoopInfo *loopInfo) {
  bool changed = false;
  // Remove any critical edges that the EdgeThreadingCloner may have
  // accidentally created.
  for (unsigned succIdx = 0, succEnd = origBB->getSuccessors().size();
       succIdx != succEnd; ++succIdx) {
    if (nullptr
        != splitCriticalEdge(origBB->getTerminator(), succIdx, domInfo,
                             loopInfo))
      changed |= true;
  }
  for (unsigned succIdx = 0, succEnd = getNewBB()->getSuccessors().size();
       succIdx != succEnd; ++succIdx) {
    auto *newBB = splitCriticalEdge(getNewBB()->getTerminator(), succIdx,
                                    domInfo, loopInfo);
    changed |= (newBB != nullptr);
  }
  return changed;
}

void BasicBlockCloner::sinkAddressProjections() {
  // Because the address projections chains will be disjoint (an instruction
  // in one chain cannot use the result of an instruction in another chain),
  // the order they are sunk does not matter.
  InstructionDeleter deleter;
  for (auto ii = origBB->begin(), ie = origBB->end(); ii != ie;) {
    bool canSink = sinkProj.analyzeAddressProjections(&*ii);
    (void)canSink;
    assert(canSink && "canCloneInstruction should catch this.");

    sinkProj.cloneProjections();
    assert((sinkProj.getInBlockDefs().empty() || needsSSAUpdate)
           && "canCloneInstruction should catch this.");

    auto nextII = std::next(ii);
    deleter.trackIfDead(&*ii);
    ii = nextII;
  }
  deleter.cleanUpDeadInstructions();
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
      if (addressProj->isPure()) {
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
  // Clone projections in last-to-first order.
  for (unsigned idx = 0; idx < projections.size(); ++idx) {
    auto *oldProj = projections[idx];
    assert(oldProj->getParent() == bb);
    // Reset transient per-projection sets.
    usesToReplace.clear();
    firstBlockUse.clear();
    // Gather uses.
    for (Operand *use : oldProj->getUses()) {
      auto *useBB = use->getUser()->getParent();
      if (useBB != bb) {
        firstBlockUse.try_emplace(useBB, use);
        usesToReplace.push_back(use);
      }
    }
    // Replace uses. Uses must be handled in the same order they were discovered
    // above.
    //
    // Avoid cloning a projection multiple times per block. This avoids extra
    // projections, but also prevents the removal of DebugValue. If a
    // projection's only remaining is DebugValue, then it is deleted along with
    // the DebugValue.
    for (Operand *use : usesToReplace) {
      auto *useBB = use->getUser()->getParent();
      auto *firstUse = firstBlockUse.lookup(useBB);
      SingleValueInstruction *newProj;
      if (use == firstUse)
        newProj = cast<SingleValueInstruction>(oldProj->clone(use->getUser()));
      else {
        newProj = cast<SingleValueInstruction>(firstUse->get());
        assert(newProj->getParent() == useBB);
        newProj->moveFront(useBB);
      }
      use->set(newProj);
    }
  }
  return true;
}

void StaticInitCloner::add(SILInstruction *initVal) {
  // Don't schedule an instruction twice for cloning.
  if (numOpsToClone.count(initVal) != 0)
    return;

  ArrayRef<Operand> operands = initVal->getAllOperands();
  numOpsToClone[initVal] = operands.size();
  if (operands.empty()) {
    // It's an instruction without operands, e.g. a literal. It's ready to be
    // cloned first.
    readyToClone.push_back(initVal);
  } else {
    // Recursively add all operands.
    for (const Operand &operand : operands) {
      add(cast<SingleValueInstruction>(operand.get()));
    }
  }
}

SingleValueInstruction *
StaticInitCloner::clone(SingleValueInstruction *initVal) {
  assert(numOpsToClone.count(initVal) != 0 && "initVal was not added");
  // Find the right order to clone: all operands of an instruction must be
  // cloned before the instruction itself.
  while (!readyToClone.empty()) {
    SILInstruction *inst = readyToClone.pop_back_val();

    // Clone the instruction into the SILGlobalVariable
    visit(inst);

    // Check if users of I can now be cloned.
    for (SILValue result : inst->getResults()) {
      for (Operand *use : result->getUses()) {
        SILInstruction *user = use->getUser();
        if (numOpsToClone.count(user) != 0 && --numOpsToClone[user] == 0)
          readyToClone.push_back(user);
      }
    }
  }
  return cast<SingleValueInstruction>(getMappedValue(initVal));
}

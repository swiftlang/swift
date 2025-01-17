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
#include "swift/Basic/Assertions.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SIL/StackList.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"

using namespace swift;

/// Invoke \p visitor for each reachable block in \p f in worklist order (at
/// least one predecessor has been visited).
bool ReachableBlocks::visit(function_ref<bool(SILBasicBlock *)> visitor) {
  // Walk over the CFG, starting at the entry block, until all reachable blocks
  // are visited.
  auto *function = visited.getFunction();
  auto *entry = function->getEntryBlock();
  StackList<SILBasicBlock *> worklist(function);
  worklist.push_back(entry);
  visited.insert(entry);
  while (!worklist.empty()) {
    SILBasicBlock *bb = worklist.pop_back_val();
    if (!visitor(bb))
      return false;

    for (auto &succ : bb->getSuccessors()) {
      if (visited.insert(succ))
        worklist.push_back(succ);
    }
  }
  return true;
}

void ReachableBlocks::compute() {
  // Visit all the blocks without doing any extra work.
  visit([](SILBasicBlock *) { return true; });
  isComputed = true;
}

ReachingReturnBlocks::ReachingReturnBlocks(SILFunction *function)
    : worklist(function) {
  for (SILBasicBlock &block : *function) {
    if (isa<ReturnInst>(block.getTerminator()))
      worklist.push(&block);
  }
  
  while (SILBasicBlock *block = worklist.pop()) {
    for (SILBasicBlock *pred : block->getPredecessorBlocks()) {
      worklist.pushIfNotVisited(pred);
    }
  }
}

bool swift::removeUnreachableBlocks(SILFunction &f) {
  ReachableBlocks reachable(&f);
  reachable.compute();

  // Remove the blocks we never reached. Assume the entry block is visited.
  // Reachable's visited set contains dangling pointers during this loop.
  bool changed = false;
  for (auto ii = std::next(f.begin()), end = f.end(); ii != end;) {
    auto *bb = &*ii++;
    if (!reachable.isReachable(bb)) {
      bb->removeDeadBlock();
      changed = true;
    }
  }
  return changed;
}

//===----------------------------------------------------------------------===//
//                             BasicBlock Cloning
//===----------------------------------------------------------------------===//

// Return true if a guaranteed terminator result can be borrowed such that the
// nested borrow scope covers all its uses.
static bool canBorrowGuaranteedResult(SILValue guaranteedResult) {
  if (guaranteedResult->getOwnershipKind() != OwnershipKind::Guaranteed) {
    // Either this terminator forwards an owned value, or it is some legal
    // conversion to a non-guaranteed value. Either way, not interesting.
    return true;
  }
  return findInnerTransitiveGuaranteedUses(guaranteedResult);
}

bool swift::canCloneTerminator(TermInst *termInst) {
  // TODO: this is an awkward way to check for guaranteed terminator results.
  for (Operand &oper : termInst->getAllOperands()) {
    if (oper.getOperandOwnership() != OperandOwnership::GuaranteedForwarding)
      continue;

    if (!ForwardingOperand(&oper).visitForwardedValues(
          [&](SILValue termResult) {
            return canBorrowGuaranteedResult(termResult);
          })) {
        return false;
    }
  }
  return true;
}

void BasicBlockCloner::updateSSAAfterCloning() {
  SmallVector<SILPhiArgument *, 4> updateSSAPhis;
  // All instructions should have been checked by canCloneInstruction. But we
  // still need to check the arguments.
  for (auto arg : origBB->getArguments()) {
    if ((needsSSAUpdate |= isUsedOutsideOfBlock(arg))) {
      break;
    }
  }
  if (needsSSAUpdate) {
    SILSSAUpdater ssaUpdater(&updateSSAPhis);
    for (auto availValPair : availVals) {
      auto inst = availValPair.first;
      if (inst->use_empty())
        continue;

      SILValue newResult(availValPair.second);

      SmallVector<UseWrapper, 16> useList;
      // Collect the uses of the value.
      for (auto *use : inst->getUses())
        useList.push_back(UseWrapper(use));

      ssaUpdater.initialize(inst->getFunction(), inst->getType(),
                            inst->getOwnershipKind());
      ssaUpdater.addAvailableValue(origBB, inst);
      ssaUpdater.addAvailableValue(getNewBB(), newResult);

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

        ssaUpdater.rewriteUse(*use);
      }
    }
  }
  for (SILBasicBlock *b : blocksWithNewPhiArgs) {
    for (SILArgument *arg : b->getArguments()) {
      if (arg->getOwnershipKind() == OwnershipKind::Guaranteed) {
        updateSSAPhis.push_back(cast<SILPhiArgument>(arg));
      }
    }
  }
  updateGuaranteedPhis(pm, updateSSAPhis);
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
  deleter.cleanupDeadInstructions();
}

// Populate 'projections' with the chain of address projections leading
// to and including 'inst'.
//
// Populate 'inBlockDefs' with all the non-address value definitions in
// the block that will be used outside this block after projection sinking.
//
// Return true on success, even if projections is empty.
bool SinkAddressProjections::analyzeAddressProjections(SILInstruction *inst) {
  oldProjections.clear();
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
        oldProjections.push_back(addressProj);
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
  for (unsigned idx = 0; idx < oldProjections.size(); ++idx) {
    // Only one address result/operand can be handled per instruction.
    if (oldProjections.size() != idx + 1)
      return false;

    for (SILValue operandVal : oldProjections[idx]->getOperandValues())
      if (!pushOperandVal(operandVal))
        return false;
  }
  return true;
}

// Clone the projections gathered by 'analyzeAddressProjections' at
// their use site outside this block.
bool SinkAddressProjections::cloneProjections() {
  if (oldProjections.empty())
    return false;

  SILBasicBlock *bb = oldProjections.front()->getParent();
  // Clone projections in last-to-first order.
  for (unsigned idx = 0; idx < oldProjections.size(); ++idx) {
    auto *oldProj = oldProjections[idx];
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
      if (use == firstUse) {
        newProj = cast<SingleValueInstruction>(oldProj->clone(use->getUser()));
        if (newProjections) {
          newProjections->push_back(newProj);
        }
      } else {
        newProj = cast<SingleValueInstruction>(firstUse->get());
        assert(newProj->getParent() == useBB);
        newProj->moveFront(useBB);
      }
      use->set(newProj);
    }
  }
  return true;
}

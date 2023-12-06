//===--- COWOpts.cpp - Optimize COW operations ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This pass optimizes begin_cow_mutation and end_cow_mutation patterns.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "cow-opts"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SIL/NodeBits.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/StackList.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Constant folds the uniqueness result of begin_cow_mutation instructions.
///
/// If it can be proved that the buffer argument is uniquely referenced, the
/// uniqueness result is replaced with a constant boolean "true".
/// For example:
///
/// \code
///     %buffer = end_cow_mutation %mutable_buffer
///     // ...
///     // %buffer does not escape here
///     // ...
///     (%is_unique, %mutable_buffer2) = begin_cow_mutation %buffer
///     cond_br %is_unique, ...
/// \endcode
///
/// is replaced with
///
/// \code
///     %buffer = end_cow_mutation [keep_unique] %mutable_buffer
///     // ...
///     (%not_used, %mutable_buffer2) = begin_cow_mutation %buffer
///     %true = integer_literal 1
///     cond_br %true, ...
/// \endcode
///
/// Note that the keep_unique flag is set on the end_cow_mutation because the
/// code now relies on that the buffer is really uniquely referenced.
///
/// The optimization can also handle def-use chains between end_cow_mutation and
/// begin_cow_mutation which involve phi-arguments.
///
class COWOptsPass : public SILFunctionTransform {
public:
  COWOptsPass() {}

  void run() override;

private:
  AliasAnalysis *AA = nullptr;

  bool optimizeBeginCOW(BeginCOWMutationInst *BCM);

  static void collectEscapePoints(SILValue v,
                                  InstructionSetWithSize &escapePoints,
                                  ValueSet &handled);
};

void COWOptsPass::run() {
  SILFunction *F = getFunction();
  if (!F->shouldOptimize())
    return;

  LLVM_DEBUG(llvm::dbgs() << "*** COW optimization on function: "
                          << F->getName() << " ***\n");

  AA = PM->getAnalysis<AliasAnalysis>(F);

  bool changed = false;
  for (SILBasicBlock &block : *F) {
  
    for (SILInstruction &inst : block) {
      if (auto *beginCOW = dyn_cast<BeginCOWMutationInst>(&inst)) {
        if (optimizeBeginCOW(beginCOW))
          changed = true;
      }
    }
  }

  if (changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

static SILValue skipStructAndExtract(SILValue value) {
  while (true) {
    if (auto *si = dyn_cast<StructInst>(value)) {
      if (si->getNumOperands() != 1)
        return value;
      value = si->getOperand(0);
      continue;
    }
    if (auto *sei = dyn_cast<StructExtractInst>(value)) {
      value = sei->getOperand();
      continue;
    }
    return value;
  }
}

bool COWOptsPass::optimizeBeginCOW(BeginCOWMutationInst *BCM) {
  LLVM_DEBUG(llvm::dbgs() << "Looking at: ");
  LLVM_DEBUG(BCM->dump());

  SILFunction *function = BCM->getFunction();
  StackList<EndCOWMutationInst *> endCOWMutationInsts(function);
  InstructionSet endCOWMutationsFound(function);

  {
    // Collect all end_cow_mutation instructions, used by the begin_cow_mutation,
    // looking through block phi-arguments.
    StackList<SILValue> workList(function);
    ValueSet handled(function);
    workList.push_back(BCM->getOperand());
    while (!workList.empty()) {
      SILValue v = skipStructAndExtract(workList.pop_back_val());
      if (SILPhiArgument *arg = dyn_cast<SILPhiArgument>(v)) {
        if (handled.insert(arg)) {
          SmallVector<SILValue, 4> incomingVals;
          if (!arg->getIncomingPhiValues(incomingVals))
            return false;
          for (SILValue incomingVal : incomingVals) {
            workList.push_back(incomingVal);
          }
        }
      } else if (auto *ECM = dyn_cast<EndCOWMutationInst>(v)) {
        if (endCOWMutationsFound.insert(ECM))
          endCOWMutationInsts.push_back(ECM);
      } else {
        return false;
      }
    }
  }

  // Collect all uses of the end_cow_instructions, where the buffer can
  // potentially escape.
  InstructionSetWithSize potentialEscapePoints(function);
  {
    ValueSet handled(function);
    for (EndCOWMutationInst *ECM : endCOWMutationInsts) {
      collectEscapePoints(ECM, potentialEscapePoints, handled);
    }
  }

  if (!potentialEscapePoints.empty()) {
    // Now, this is the complicated part: check if there is an escape point
    // within the liverange between the end_cow_mutation(s) and
    // begin_cow_mutation.
    //
    // For store instructions we do a little bit more: only count a store as an
    // escape if there is a (potential) load from the same address within the
    // liverange.
    StackList<SILInstruction *> instWorkList(function);
    StackList<SILInstruction *> potentialLoadInsts(function);
    StackList<SILValue> storeAddrs(function);
    ValueSet storeAddrsFound(function);
    BasicBlockSet handled(function);
    int numStoresFound = 0;
    int numLoadsFound = 0;
  
    // This is a simple worklist-based backward dataflow analysis.
    // Start at the initial begin_cow_mutation and go backward.
    instWorkList.push_back(BCM);

    while (!instWorkList.empty()) {
      SILInstruction *inst = instWorkList.pop_back_val();
      for (;;) {
        if (potentialEscapePoints.contains(inst)) {
          if (auto *store = dyn_cast<StoreInst>(inst)) {
            // Don't immediately bail on a store instruction. Instead, remember
            // it and check if it interferes with any (potential) load.
            if (storeAddrsFound.insert(store->getDest())) {
              LLVM_DEBUG(llvm::dbgs() << "Found store escape, record: ");
              LLVM_DEBUG(inst->dump());
              storeAddrs.push_back(store->getDest());
              numStoresFound += 1;
            }
          } else {
            LLVM_DEBUG(llvm::dbgs() << "Found non-store escape, bailing out: ");
            LLVM_DEBUG(inst->dump());
            return false;
          }
        }
        if (inst->mayReadFromMemory()) {
          LLVM_DEBUG(llvm::dbgs() << "Found a may read inst, record: ");
          LLVM_DEBUG(inst->dump());
          potentialLoadInsts.push_back(inst);
          numLoadsFound += 1;
        }

        // An end_cow_mutation marks the begin of the liverange. It's the end
        // point of the dataflow analysis.
        auto *ECM = dyn_cast<EndCOWMutationInst>(inst);
        if (ECM && endCOWMutationsFound.contains(ECM))
          break;

        if (inst == &inst->getParent()->front()) {
          for (SILBasicBlock *pred : inst->getParent()->getPredecessorBlocks()) {
            if (handled.insert(pred))
              instWorkList.push_back(pred->getTerminator());
          }
          break;
        }

        inst = &*std::prev(inst->getIterator());
      }
    }
    
    // Check if there is any (potential) load from a memory location where the
    // buffer is stored to.
    if (numStoresFound != 0) {
      // Avoid quadratic behavior. Usually this limit is not exceeded.
      if (numStoresFound * numLoadsFound > 128)
        return false;
      for (SILInstruction *load : potentialLoadInsts) {
        for (SILValue storeAddr : storeAddrs) {
          if (!AA || AA->mayReadFromMemory(load, storeAddr)) {
            LLVM_DEBUG(llvm::dbgs() << "Found a store address aliasing with a load:");
            LLVM_DEBUG(load->dump());
            LLVM_DEBUG(storeAddr->dump());
            return false;
          }
        }
      }
    }
  }

  // Replace the uniqueness result of the begin_cow_mutation with an integer
  // literal of "true".
  SILBuilderWithScope B(BCM);
  auto *IL = B.createIntegerLiteral(BCM->getLoc(),
                                    BCM->getUniquenessResult()->getType(), 1);
  BCM->getUniquenessResult()->replaceAllUsesWith(IL);
  
  for (EndCOWMutationInst *ECM : endCOWMutationInsts) {
    // This is important for other optimizations: The code is now relying on
    // the buffer to be unique.
    ECM->setKeepUnique();
  }

  return true;
}

void COWOptsPass::collectEscapePoints(SILValue v,
                                      InstructionSetWithSize &escapePoints,
                                      ValueSet &handled) {
  if (!handled.insert(v))
    return;

  for (Operand *use : v->getUses()) {
    SILInstruction *user = use->getUser();
    switch (user->getKind()) {
      case SILInstructionKind::BeginCOWMutationInst:
      case SILInstructionKind::RefElementAddrInst:
      case SILInstructionKind::RefTailAddrInst:
      case SILInstructionKind::DebugValueInst:
        break;
      case SILInstructionKind::BranchInst:
        collectEscapePoints(cast<BranchInst>(user)->getArgForOperand(use),
                            escapePoints, handled);
        break;
      case SILInstructionKind::CondBranchInst:
        if (use->getOperandNumber() != CondBranchInst::ConditionIdx) {
          collectEscapePoints(cast<CondBranchInst>(user)->getArgForOperand(use),
                              escapePoints, handled);
        }
        break;
      case SILInstructionKind::StructInst:
      case SILInstructionKind::StructExtractInst:
      case SILInstructionKind::TupleInst:
      case SILInstructionKind::TupleExtractInst:
      case SILInstructionKind::UncheckedRefCastInst:
        collectEscapePoints(cast<SingleValueInstruction>(user),
                            escapePoints, handled);
        break;
      default:
        // Everything else is considered to be a potential escape of the buffer.
        escapePoints.insert(user);
    }
  }
}

} // end anonymous namespace

SILTransform *swift::createCOWOpts() {
  return new COWOptsPass();
}


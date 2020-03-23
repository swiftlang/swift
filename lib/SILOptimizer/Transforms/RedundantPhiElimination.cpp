//===--- RedundantPhiElimination.cpp - Remove redundant phi arguments -----===//
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
// This pass eliminates redundant basic block arguments.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-optimize-block-arguments"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Removes redundant basic block phi-arguments.
///
/// RedundantPhiEliminationPass eliminates block arguments which have
/// the same value as other arguments of the same block. This also works with
/// cycles, like two equivalent loop induction variables. Such patterns are
/// generated e.g. when using stdlib's enumerated() on Array.
///
/// \code
///   preheader:
///     br bb1(%initval, %initval)
///   header(%phi1, %phi2):
///     %next1 = builtin "add" (%phi1, %one)
///     %next2 = builtin "add" (%phi2, %one)
///     cond_br %loopcond, header(%next1, %next2), exit
///   exit:
/// \endcode
///
/// is replaced with
///
/// \code
///   preheader:
///     br bb1(%initval)
///   header(%phi1):
///     %next1 = builtin "add" (%phi1, %one)
///     %next2 = builtin "add" (%phi1, %one) // dead: will be cleaned-up later
///     cond_br %loopcond, header(%next1), exit
///   exit:
/// \endcode
///
/// Any remaining dead or "trivially" equivalent instructions will then be
/// cleaned-up by DCE and CSE, respectively.
///
/// RedundantPhiEliminationPass is not part of SimplifyCFG because
/// * no other SimplifyCFG optimization depends on it.
/// * compile time: it doesn't need to run every time SimplifyCFG runs.
///
class RedundantPhiEliminationPass : public SILFunctionTransform {
public:
  RedundantPhiEliminationPass() {}

  void run() override;

private:
  bool optimizeArgs(SILBasicBlock *block);

  bool valuesAreEqual(SILValue val1, SILValue val2);
};

void RedundantPhiEliminationPass::run() {
  SILFunction *F = getFunction();
  if (!F->shouldOptimize())
    return;

  LLVM_DEBUG(llvm::dbgs() << "*** RedundantPhiElimination on function: "
                          << F->getName() << " ***\n");

  bool changed = false;
  for (SILBasicBlock &block : *getFunction()) {
    changed |= optimizeArgs(&block);
  }

  if (changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

bool RedundantPhiEliminationPass::optimizeArgs(SILBasicBlock *block) {
  // Avoid running into quadratic behavior for blocks which have many arguments.
  // This is seldom, anyway.
  unsigned maxArgumentCombinations = 48;

  bool changed = false;
  unsigned numArgumentCombinations = 0;
  for (unsigned arg1Idx = 0; arg1Idx < block->getNumArguments(); ++arg1Idx) {
    for (unsigned arg2Idx = arg1Idx + 1; arg2Idx < block->getNumArguments();) {
      if (++numArgumentCombinations > maxArgumentCombinations)
        return changed;

      SILArgument *arg1 = block->getArgument(arg1Idx);
      SILArgument *arg2 = block->getArgument(arg2Idx);
      if (!arg1->isPhiArgument() || !arg2->isPhiArgument())
        continue;

      if (valuesAreEqual(arg1, arg2)) {
        arg2->replaceAllUsesWith(arg1);
        erasePhiArgument(block, arg2Idx);
        changed = true;
      } else {
        ++arg2Idx;
      }
    }
  }
  return changed;
}

bool RedundantPhiEliminationPass::valuesAreEqual(SILValue val1, SILValue val2) {

  // Again, avoid running into quadratic behavior in case of cycles or long
  // chains of instructions. This limit is practically never exceeded.
  unsigned maxNumberOfChecks = 16;

  SmallVector<std::pair<SILValue, SILValue>, 8> workList;
  llvm::SmallSet<std::pair<SILValue, SILValue>, 16> handled;
  
  workList.push_back({val1, val2});
  handled.insert({val1, val2});

  while (!workList.empty()) {
  
    if (handled.size() > maxNumberOfChecks)
      return false;
  
    auto valuePair = workList.pop_back_val();
    SILValue val1 = valuePair.first;
    SILValue val2 = valuePair.second;

    // The trivial case.
    if (val1 == val2)
      continue;
 
    if (val1->getKind() != val2->getKind())
      return false;
 
    if (auto *arg1 = dyn_cast<SILPhiArgument>(val1)) {
      auto *arg2 = cast<SILPhiArgument>(val2);
      SILBasicBlock *argBlock = arg1->getParent();
      if (argBlock != arg2->getParent())
        return false;
      if (arg1->getType() != arg2->getType())
        return false;
    
      // All incoming phi values must be equal.
      for (SILBasicBlock *pred : argBlock->getPredecessorBlocks()) {
        SILValue incoming1 = arg1->getIncomingPhiValue(pred);
        SILValue incoming2 = arg2->getIncomingPhiValue(pred);
        if (!incoming1 || !incoming2)
          return false;

        if (handled.insert({incoming1, incoming2}).second)
          workList.push_back({incoming1, incoming2});
      }
      continue;
    }
    
    if (auto *inst1 = dyn_cast<SingleValueInstruction>(val1)) {
      // Bail if the instructions have any side effects.
      if (inst1->getMemoryBehavior() != SILInstruction::MemoryBehavior::None)
        return false;

      auto *inst2 = cast<SingleValueInstruction>(val2);

      // Compare the operands by putting them on the worklist.
      if (!inst1->isIdenticalTo(inst2, [&](SILValue op1, SILValue op2) {
            if (handled.insert({op1, op2}).second)
              workList.push_back({op1, op2});
            return true;
          })) {
        return false;
      }
      continue;
    }
    
    return false;
  }

  return true;
}

} // end anonymous namespace

SILTransform *swift::createRedundantPhiElimination() {
  return new RedundantPhiEliminationPass();
}

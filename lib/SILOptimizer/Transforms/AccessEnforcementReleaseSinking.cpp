//===--- AccessEnforcementReleaseSinking.cpp - release sinking opt ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This function pass sinks releases out of access scopes.
///
/// General case:
/// begin_access A
/// ...
/// strong_release / release_value / destroy
/// end_access
///
/// The release instruction can be sunk below the end_access instruction,
/// This extends the lifetime of the released value, but, might allow us to
/// Mark the access scope as no nested conflict.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-release"

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

// Returns a bool: If this is a "sinkable" instruction type for this opt
static bool isSinkable(SILInstruction &inst) {
  switch (inst.getKind()) {
  default:
    return false;
  case SILInstructionKind::DestroyValueInst:
  case SILInstructionKind::ReleaseValueInst:
  case SILInstructionKind::ReleaseValueAddrInst:
  case SILInstructionKind::StrongReleaseInst:
  case SILInstructionKind::UnmanagedReleaseValueInst:
    return true;
  }
}

// Returns a bool: If this is a "barrier" instruction for this opt
static bool isBarrier(SILInstruction &inst) {
  switch (inst.getKind()) {
  default:
    return FullApplySite::isa(&inst) != FullApplySite();
  case SILInstructionKind::BeginAccessInst:
  case SILInstructionKind::BeginUnpairedAccessInst:
  case SILInstructionKind::IsUniqueInst:
    return true;
  }
}

// Processes a block bottom-up, keeping a lookout for end_access instructions
// If we encounter a "barrier" we clear out the current end_access
// If we encounter a "release", and we have a current end_access, we sink it
static void processBlock(SILBasicBlock &block) {
  EndAccessInst *bottomEndAccessInst = nullptr;
  for (auto reverseIt = block.rbegin(); reverseIt != block.rend();
       ++reverseIt) {
    SILInstruction &currIns = *reverseIt;
    if (auto *currEAI = dyn_cast<EndAccessInst>(&currIns)) {
      if (!bottomEndAccessInst) {
        bottomEndAccessInst = currEAI;
      }
    } else if (isBarrier(currIns)) {
      LLVM_DEBUG(llvm::dbgs() << "Found a barrier " << currIns
                              << ", clearing last seen end_access\n");
      bottomEndAccessInst = nullptr;
    } else if (isSinkable(currIns)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Found a sinkable instruction " << currIns << "\n");
      if (!bottomEndAccessInst) {
        LLVM_DEBUG(
            llvm::dbgs()
            << "Cannot be sunk: no open barrier-less end_access found\n");
        continue;
      }
      LLVM_DEBUG(llvm::dbgs() << "Moving sinkable instruction below "
                              << *bottomEndAccessInst << "\n");
      // We need to avoid iterator invalidation:
      // We know this is not the last instruction of the block:
      // 1) not a TermInst
      // 2) bottomEndAccessInst != nil
      assert(reverseIt != block.rbegin() &&
             "Did not expect a sinkable instruction at block's end");
      // Go back to previous iteration
      auto prevIt = reverseIt;
      --prevIt;
      // Move the instruction after the end_access
      currIns.moveAfter(bottomEndAccessInst);
      // make reverseIt into a valid iterator again
      reverseIt = prevIt;
    }
  }
}

namespace {
struct AccessEnforcementReleaseSinking : public SILFunctionTransform {
  void run() override {
    SILFunction *F = getFunction();
    if (F->empty())
      return;

    LLVM_DEBUG(llvm::dbgs() << "Running AccessEnforcementReleaseSinking on "
                            << F->getName() << "\n");

    for (SILBasicBlock &currBB : *F) {
      processBlock(currBB);
    }
  }
};
} // namespace

SILTransform *swift::createAccessEnforcementReleaseSinking() {
  return new AccessEnforcementReleaseSinking();
}

//===--- AccessEnforcementGlobalHoisting.cpp - global hoist opt ---===//
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
/// This function pass hoists global_addr used by access scopes to entry blocks
///
/// General case:
/// Loop:
/// %A = global_addr
/// begin_access %A
/// ...
/// end_access
///
/// The global_addr instruction can be hoisted to a function's entry block.
/// This might expose some potenital for access enforcement optimizations,
/// such as hoisting the begin_access out of the loop
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-global"

#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

static void processBlock(SILBasicBlock &currBlock, SILBasicBlock *entry) {
  TermInst *term = entry->getTerminator();
  assert(term && "Expected a terminator for entry block");
  for (SILInstruction &instr : currBlock) {
    BeginAccessInst *beginAccess = dyn_cast<BeginAccessInst>(&instr);
    if (!beginAccess)
      continue;
    GlobalAddrInst *globalAddr =
        dyn_cast<GlobalAddrInst>(beginAccess->getSource());
    if (!globalAddr)
      continue;
    if (globalAddr->getParent() == entry) {
      LLVM_DEBUG(llvm::dbgs()
                 << "Skipping " << *beginAccess
                 << ", global_addr instruction already in entry block\n");
      continue;
    }
    LLVM_DEBUG(llvm::dbgs()
               << "Moving " << *globalAddr << " Before " << *term << "\n");
    globalAddr->moveBefore(term);
  }
}

namespace {
struct AccessEnforcementGlobalHoisting : public SILFunctionTransform {
  void run() override {
    SILFunction *func = getFunction();
    if (func->empty())
      return;

    SILBasicBlock *entry = func->getEntryBlock();
    if (!entry)
      return;

    LLVM_DEBUG(llvm::dbgs() << "Running AccessEnforcementGlobalHoisting on "
                            << func->getName() << "\n");

    for (SILBasicBlock &currBB : *func) {
      if (&currBB == entry) {
        LLVM_DEBUG(llvm::dbgs() << "Skipping entry block - no need to hoist\n");
        continue;
      }
      processBlock(currBB, entry);
    }
  }
};
} // namespace

SILTransform *swift::createAccessEnforcementGlobalHoisting() {
  return new AccessEnforcementGlobalHoisting();
}

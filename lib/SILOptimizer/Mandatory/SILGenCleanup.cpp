//===--- SILGenCleanup.cpp ------------------------------------------------===//
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
///
/// Perform peephole-style "cleanup" to aid SIL diagnostic passes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "silgen-cleanup"

#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// SILGenCleanup: Top-Level Module Transform
//===----------------------------------------------------------------------===//

namespace {

// SILGenCleanup must run on all functions that will be seen by any analysis
// used by diagnostics before transforming the function that requires the
// analysis. e.g. Closures need to be cleaned up before the closure's parent can
// be diagnosed.
//
// TODO: This pass can be converted to a function transform if the mandatory
// pipeline runs in bottom-up closure order.
struct SILGenCleanup : SILModuleTransform {
  void run() override;
};

void SILGenCleanup::run() {
  auto &module = *getModule();
  for (auto &function : module) {
    LLVM_DEBUG(llvm::dbgs()
               << "\nRunning SILGenCleanup on " << function.getName() << "\n");

    DeadEndBlocks deadEndBlocks(&function);
    InstructionDeleter deleter;
    CanonicalizeInstruction sgCanonicalize(DEBUG_TYPE, deadEndBlocks, deleter);

    // Iterate over all blocks even if they aren't reachable. No phi-less
    // dataflow cycles should have been created yet, and these transformations
    // are simple enough they shouldn't be affected by cycles.
    for (auto &bb : function) {
      for (auto *instruction : deleter.updatingRange(&bb)) {
        sgCanonicalize.canonicalize(instruction);
        deleter.cleanupDeadInstructions();
      }
    }
    if (sgCanonicalize.deleter.hadCallbackInvocation()) {
      auto invalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&function, invalidKind);
    }
  }
}

} // end anonymous namespace

SILTransform *swift::createSILGenCleanup() { return new SILGenCleanup(); }

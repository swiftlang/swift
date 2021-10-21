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
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"

using namespace swift;

llvm::cl::opt<bool> EnableCompleteOSSALifetimes(
    "complete-ossa-lifetimes",
    llvm::cl::init(true),
    llvm::cl::desc("Automatically complete OSSA lifetimes after SILGen"),
    llvm::cl::Hidden);

// Define a CanonicalizeInstruction subclass for use in SILGenCleanup.
struct SILGenCanonicalize final : CanonicalizeInstruction {
  bool changed = false;
  InstructionDeleter &deleter;

  SILGenCanonicalize(SILFunction *f, InstructionDeleter &deleter)
    : CanonicalizeInstruction(f, DEBUG_TYPE), deleter(deleter) {}

  void notifyNewInstruction(SILInstruction * = nullptr) override {
    changed = true;
  }

  // Just delete the given 'inst' and record its operands. The callback isn't
  // allowed to mutate any other instructions.
  void killInstruction(SILInstruction *inst) override {
    deleter.forceDelete(inst);
    changed = true;
  }

  void notifyHasNewUsers(SILValue) override { changed = true; }
};

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

  bool completeOSSALifetimes(SILFunction *function);
};

bool SILGenCleanup::completeOSSALifetimes(SILFunction *function) {
  if (!EnableCompleteOSSALifetimes)
    return false;

  bool changed = false;

  for (auto &bb : *function) {
    for (SILArgument *arg : bb.getArguments()) {
      changed |= completeOSSALifetime(arg);
    }
    for (SILInstruction &inst : bb) {
      for (auto result : inst.getResults()) {
        changed |= completeOSSALifetime(result);
      }
    }
  }
  return changed;
}

void SILGenCleanup::run() {
  auto &module = *getModule();
  for (auto &function : module) {

    LLVM_DEBUG(llvm::dbgs()
               << "\nRunning SILGenCleanup on " << function.getName() << "\n");

    bool changed = completeOSSALifetimes(&function);

    InstructionDeleter deleter;
    SILGenCanonicalize sgCanonicalize(&function, deleter);

    // Iterate over all blocks even if they aren't reachable. No phi-less
    // dataflow cycles should have been created yet, and these transformations
    // are simple enough they shouldn't be affected by cycles.
    for (auto &bb : function) {
      for (SILInstruction *inst : deleter.updatingRange(&bb)) {
        sgCanonicalize.canonicalize(inst);
        deleter.cleanupDeadInstructions();
      }
    }
    changed |= sgCanonicalize.changed;
    if (changed) {
      auto invalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&function, invalidKind);
    }
    function.verifyOwnership();
  }
}

} // end anonymous namespace

SILTransform *swift::createSILGenCleanup() { return new SILGenCleanup(); }

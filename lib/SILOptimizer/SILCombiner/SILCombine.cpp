//===--- SILCombine.cpp ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A port of LLVM's InstCombine pass to SIL. Its main purpose is for performing
// small combining operations/peepholes at the SIL level. It additionally
// performs dead code elimination when it initially adds instructions to the
// work queue in order to reduce compile time by not visiting trivially dead
// instructions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-combine"
#include "SILCombiner.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumCombined, "Number of instructions combined");
STATISTIC(NumDeadInst, "Number of dead insts eliminated");

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

/// addReachableCodeToWorklist - Walk the function in depth-first order, adding
/// all reachable code to the worklist.
///
/// This has a couple of tricks to make the code faster and more powerful.  In
/// particular, we DCE instructions as we go, to avoid adding them to the
/// worklist (this significantly speeds up SILCombine on code where many
/// instructions are dead or constant).
void SILCombiner::addReachableCodeToWorklist(SILBasicBlock *BB) {
  llvm::SmallVector<SILBasicBlock *, 256> Worklist;
  llvm::SmallVector<SILInstruction *, 128> InstrsForSILCombineWorklist;
  llvm::SmallPtrSet<SILBasicBlock *, 32> Visited;

  Worklist.push_back(BB);
  do {
    BB = Worklist.pop_back_val();

    // We have now visited this block!  If we've already been here, ignore it.
    if (!Visited.insert(BB).second) continue;

    for (SILBasicBlock::iterator BBI = BB->begin(), E = BB->end(); BBI != E; ) {
      SILInstruction *Inst = &*BBI;
      ++BBI;

      // DCE instruction if trivially dead.
      if (isInstructionTriviallyDead(Inst)) {
        ++NumDeadInst;
        LLVM_DEBUG(llvm::dbgs() << "SC: DCE: " << *Inst << '\n');

        // We pass in false here since we need to signal to
        // eraseInstFromFunction to not add this instruction's operands to the
        // worklist since we have not initialized the worklist yet.
        //
        // The reason to just use a default argument here is that it allows us
        // to centralize all instruction removal in SILCombine into this one
        // function. This is important if we want to be able to update analyses
        // in a clean manner.
        eraseInstFromFunction(*Inst, BBI,
                              false /*Don't add operands to worklist*/);
        continue;
      }

      InstrsForSILCombineWorklist.push_back(Inst);
    }

    // Recursively visit successors.
    for (auto SI = BB->succ_begin(), SE = BB->succ_end(); SI != SE; ++SI)
      Worklist.push_back(*SI);
  } while (!Worklist.empty());

  // Once we've found all of the instructions to add to the worklist, add them
  // in reverse order. This way SILCombine will visit from the top of the
  // function down. This jives well with the way that it adds all uses of
  // instructions to the worklist after doing a transformation, thus avoiding
  // some N^2 behavior in pathological cases.
  addInitialGroup(InstrsForSILCombineWorklist);
}

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

// Define a CanonicalizeInstruction subclass for use in SILCombine.
class SILCombineCanonicalize final : CanonicalizeInstruction {
  SmallSILInstructionWorklist<256> &Worklist;
  bool changed = false;

public:
  SILCombineCanonicalize(
      SmallSILInstructionWorklist<256> &Worklist)
      : CanonicalizeInstruction(DEBUG_TYPE), Worklist(Worklist) {}

  void notifyNewInstruction(SILInstruction *inst) override {
    Worklist.add(inst);
    Worklist.addUsersOfAllResultsToWorklist(inst);
    changed = true;
  }

  // Just delete the given 'inst' and record its operands. The callback isn't
  // allowed to mutate any other instructions.
  void killInstruction(SILInstruction *inst) override {
    Worklist.eraseSingleInstFromFunction(*inst,
                                         /*AddOperandsToWorklist*/ true);
    changed = true;
  }

  void notifyHasNewUsers(SILValue value) override {
    Worklist.addUsersToWorklist(value);
    changed = true;
  }

  bool tryCanonicalize(SILInstruction *inst) {
    changed = false;
    canonicalize(inst);
    return changed;
  }
};

bool SILCombiner::doOneIteration(SILFunction &F, unsigned Iteration) {
  MadeChange = false;

  LLVM_DEBUG(llvm::dbgs() << "\n\nSILCOMBINE ITERATION #" << Iteration << " on "
                          << F.getName() << "\n");

  // Add reachable instructions to our worklist.
  addReachableCodeToWorklist(&*F.begin());

  SILCombineCanonicalize scCanonicalize(Worklist);

  // Process until we run out of items in our worklist.
  while (!Worklist.isEmpty()) {
    SILInstruction *I = Worklist.pop_back_val();

    // When we erase an instruction, we use the map in the worklist to check if
    // the instruction is in the worklist. If it is, we replace it with null
    // instead of shifting all members of the worklist towards the front. This
    // check makes sure that if we run into any such residual null pointers, we
    // skip them.
    if (I == nullptr)
      continue;

    // Check to see if we can DCE the instruction.
    if (isInstructionTriviallyDead(I)) {
      LLVM_DEBUG(llvm::dbgs() << "SC: DCE: " << *I << '\n');
      eraseInstFromFunction(*I);
      ++NumDeadInst;
      MadeChange = true;
      continue;
    }

    // Canonicalize the instruction.
    if (scCanonicalize.tryCanonicalize(I)) {
      MadeChange = true;
      continue;
    }

    // If we have reached this point, all attempts to do simple simplifications
    // have failed. Prepare to SILCombine.
    Builder.setInsertionPoint(I);

#ifndef NDEBUG
    std::string OrigI;
#endif
    LLVM_DEBUG(llvm::raw_string_ostream SS(OrigI); I->print(SS);
               OrigI = SS.str(););
    LLVM_DEBUG(llvm::dbgs() << "SC: Visiting: " << OrigI << '\n');

    if (SILInstruction *Result = visit(I)) {
      ++NumCombined;
      // Should we replace the old instruction with a new one?
      Worklist.replaceInstructionWithInstruction(I, Result
#ifndef NDEBUG
          ,
          OrigI
#endif
      );
      MadeChange = true;
    }

    // Our tracking list has been accumulating instructions created by the
    // SILBuilder during this iteration. Go through the tracking list and add
    // its contents to the worklist and then clear said list in preparation for
    // the next iteration.
    auto &TrackingList = *Builder.getTrackingList();
    for (SILInstruction *I : TrackingList) {
      LLVM_DEBUG(llvm::dbgs() << "SC: add " << *I
                              << " from tracking list to worklist\n");
      Worklist.add(I);
    }
    TrackingList.clear();
  }

  Worklist.resetChecked();
  return MadeChange;
}

bool SILCombiner::runOnFunction(SILFunction &F) {
  clear();

  bool Changed = false;
  // Perform iterations until we do not make any changes.
  while (doOneIteration(F, Iteration)) {
    Changed = true;
    Iteration++;
  }

  // Cleanup the builder and return whether or not we made any changes.
  return Changed;
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILCombine : public SILFunctionTransform {

  llvm::SmallVector<SILInstruction *, 64> TrackingList;
  
  /// The entry point to the transformation.
  void run() override {
    // FIXME: We should be able to handle ownership.
    if (getFunction()->hasOwnership())
      return;

    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    auto *PCA = PM->getAnalysis<ProtocolConformanceAnalysis>();
    auto *CHA = PM->getAnalysis<ClassHierarchyAnalysis>();

    SILOptFunctionBuilder FuncBuilder(*this);
    // Create a SILBuilder with a tracking list for newly added
    // instructions, which we will periodically move to our worklist.
    SILBuilder B(*getFunction(), &TrackingList);
    SILCombiner Combiner(FuncBuilder, B, AA, DA, PCA, CHA,
                         getOptions().RemoveRuntimeAsserts);
    bool Changed = Combiner.runOnFunction(*getFunction());
    assert(TrackingList.empty() &&
           "TrackingList should be fully processed by SILCombiner");

    if (Changed) {
      // Invalidate everything.
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
    }
  }
  
  void handleDeleteNotification(SILNode *node) override {
    auto I = dyn_cast<SILInstruction>(node);
    if (!I) return;

    // Linear searching the tracking list doesn't hurt because usually it only
    // contains a few elements.
    auto Iter = std::find(TrackingList.begin(), TrackingList.end(), I);
    if (Iter != TrackingList.end())
      TrackingList.erase(Iter);      
  }
  
  bool needsNotifications() override { return true; }

};

} // end anonymous namespace

SILTransform *swift::createSILCombine() {
  return new SILCombine();
}

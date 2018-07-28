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
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "SILCombiner.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumSimplified, "Number of instructions simplified");
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

void SILCombineWorklist::add(SILInstruction *I) {
  if (!WorklistMap.insert(std::make_pair(I, Worklist.size())).second)
    return;

  LLVM_DEBUG(llvm::dbgs() << "SC: ADD: " << *I << '\n');
  Worklist.push_back(I);
}

bool SILCombiner::doOneIteration(SILFunction &F, unsigned Iteration) {
  MadeChange = false;

  LLVM_DEBUG(llvm::dbgs() << "\n\nSILCOMBINE ITERATION #" << Iteration << " on "
                          << F.getName() << "\n");

  // Add reachable instructions to our worklist.
  addReachableCodeToWorklist(&*F.begin());

  // Process until we run out of items in our worklist.
  while (!Worklist.isEmpty()) {
    SILInstruction *I = Worklist.removeOne();

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

    // Check to see if we can instsimplify the instruction.
    if (SILValue Result = simplifyInstruction(I)) {
      ++NumSimplified;

      LLVM_DEBUG(llvm::dbgs() << "SC: Simplify Old = " << *I << '\n'
                              << "    New = " << *Result << '\n');

      // Erase the simplified instruction and any instructions that end its
      // scope. Nothing needs to be added to the worklist except for Result,
      // because the instruction and all non-replaced users will be deleted.
      replaceAllSimplifiedUsesAndErase(
          I, Result,
          [this](SILInstruction *Deleted) { Worklist.remove(Deleted); });

      // Push the new instruction and any users onto the worklist.
      Worklist.addUsersToWorklist(Result);

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
      if (Result != I) {
        assert(&*std::prev(SILBasicBlock::iterator(I)) == Result &&
              "Expected new instruction inserted before existing instruction!");

        LLVM_DEBUG(llvm::dbgs() << "SC: Old = " << *I << '\n'
                                << "    New = " << *Result << '\n');

        // Everything uses the new instruction now.
        replaceInstUsesPairwiseWith(I, Result);

        // Push the new instruction and any users onto the worklist.
        Worklist.add(Result);
        Worklist.addUsersOfAllResultsToWorklist(Result);

        eraseInstFromFunction(*I);
      } else {
        LLVM_DEBUG(llvm::dbgs() << "SC: Mod = " << OrigI << '\n'
                                << "    New = " << *I << '\n');

        // If the instruction was modified, it's possible that it is now dead.
        // if so, remove it.
        if (isInstructionTriviallyDead(I)) {
          eraseInstFromFunction(*I);
        } else {
          Worklist.add(I);
          Worklist.addUsersOfAllResultsToWorklist(I);
        }
      }
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

  Worklist.zap();
  return MadeChange;
}

void SILCombineWorklist::addInitialGroup(ArrayRef<SILInstruction *> List) {
  assert(Worklist.empty() && "Worklist must be empty to add initial group");
  Worklist.reserve(List.size()+16);
  WorklistMap.reserve(List.size());
  LLVM_DEBUG(llvm::dbgs() << "SC: ADDING: " << List.size()
                          << " instrs to worklist\n");
  while (!List.empty()) {
    SILInstruction *I = List.back();
    List = List.slice(0, List.size()-1);    
    WorklistMap.insert(std::make_pair(I, Worklist.size()));
    Worklist.push_back(I);
    }
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

// Insert the instruction New before instruction Old in Old's parent BB. Add
// New to the worklist.
SILInstruction *SILCombiner::insertNewInstBefore(SILInstruction *New,
                                                 SILInstruction &Old) {
  assert(New && New->getParent() == nullptr &&
         "New instruction already inserted into a basic block!");
  SILBasicBlock *BB = Old.getParent();
  BB->insert(&Old, New);  // Insert inst
  Worklist.add(New);
  return New;
}

// This method is to be used when an instruction is found to be dead,
// replaceable with another preexisting expression. Here we add all uses of I
// to the worklist, replace all uses of I with the new value, then return I,
// so that the combiner will know that I was modified.
void SILCombiner::replaceInstUsesWith(SingleValueInstruction &I, ValueBase *V) {
  Worklist.addUsersToWorklist(&I);   // Add all modified instrs to worklist.

  LLVM_DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
                          << "    with " << *V << '\n');

  I.replaceAllUsesWith(V);
}

/// Replace all of the results of the old instruction with the
/// corresponding results of the new instruction.
void SILCombiner::replaceInstUsesPairwiseWith(SILInstruction *oldI,
                                              SILInstruction *newI) {
  LLVM_DEBUG(llvm::dbgs() << "SC: Replacing " << *oldI << "\n"
                          << "    with " << *newI << '\n');

  auto oldResults = oldI->getResults();
  auto newResults = newI->getResults();
  assert(oldResults.size() == newResults.size());
  for (auto i : indices(oldResults)) {
    // Add all modified instrs to worklist.
    Worklist.addUsersToWorklist(oldResults[i]);

    oldResults[i]->replaceAllUsesWith(newResults[i]);
  }
}

// Some instructions can never be "trivially dead" due to side effects or
// producing a void value. In those cases, since we cannot rely on
// SILCombines trivially dead instruction DCE in order to delete the
// instruction, visit methods should use this method to delete the given
// instruction and upon completion of their peephole return the value returned
// by this method.
SILInstruction *SILCombiner::eraseInstFromFunction(SILInstruction &I,
                                            SILBasicBlock::iterator &InstIter,
                                            bool AddOperandsToWorklist) {
  LLVM_DEBUG(llvm::dbgs() << "SC: ERASE " << I << '\n');

  assert(onlyHaveDebugUsesOfAllResults(&I) &&
         "Cannot erase instruction that is used!");

  // Make sure that we reprocess all operands now that we reduced their
  // use counts.
  if (I.getNumOperands() < 8 && AddOperandsToWorklist) {
    for (auto &OpI : I.getAllOperands()) {
      if (auto *Op = OpI.get()->getDefiningInstruction()) {
        LLVM_DEBUG(llvm::dbgs() << "SC: add op " << *Op
                                << " from erased inst to worklist\n");
        Worklist.add(Op);
      }
    }
  }

  for (auto result : I.getResults())
    for (Operand *DU : getDebugUses(result))
      Worklist.remove(DU->getUser());

  Worklist.remove(&I);
  eraseFromParentWithDebugInsts(&I, InstIter);
  MadeChange = true;
  return nullptr;  // Don't do anything with I
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILCombine : public SILFunctionTransform {

  llvm::SmallVector<SILInstruction *, 64> TrackingList;
  
  /// The entry point to the transformation.
  void run() override {
    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *DA = PM->getAnalysis<DominanceAnalysis>();

    // Create a SILBuilder with a tracking list for newly added
    // instructions, which we will periodically move to our worklist.
    SILBuilder B(*getFunction(), &TrackingList);
    SILCombiner Combiner(B, AA, DA, getOptions().RemoveRuntimeAsserts);
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

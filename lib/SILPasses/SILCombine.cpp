//===-------------------------- SILCombine --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/SILPasses/Passes.h"
#include "SILCombiner.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/SimplifyInstruction.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
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
  llvm::SmallVector<SILBasicBlock*, 256> Worklist;
  llvm::SmallVector<SILInstruction*, 128> InstrsForSILCombineWorklist;
  llvm::SmallPtrSet<SILBasicBlock*, 64> Visited;

  Worklist.push_back(BB);
  do {
    BB = Worklist.pop_back_val();

    // We have now visited this block!  If we've already been here, ignore it.
    if (!Visited.insert(BB).second) continue;

    for (SILBasicBlock::iterator BBI = BB->begin(), E = BB->end(); BBI != E; ) {
      SILInstruction *Inst = BBI++;

      // DCE instruction if trivially dead.
      if (isInstructionTriviallyDead(Inst)) {
        ++NumDeadInst;
        DEBUG(llvm::dbgs() << "SC: DCE: " << *Inst << '\n');

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

  DEBUG(llvm::dbgs() << "SC: ADD: " << *I << '\n');
  Worklist.push_back(I);
}

bool SILCombiner::doOneIteration(SILFunction &F, unsigned Iteration) {
  MadeChange = false;

  DEBUG(llvm::dbgs() << "\n\nSILCOMBINE ITERATION #" << Iteration << " on "
                     << F.getName() << "\n");

  // Add reachable instructions to our worklist.
  addReachableCodeToWorklist(F.begin());

  // Process until we run out of items in our worklist.
  while (!Worklist.isEmpty()) {
    SILInstruction *I = Worklist.removeOne();

    // When we erase an instruction, we use the map in the worklist to check if
    // the instruction is in the worklist. If it is, we replace it with null
    // instead of shifting all members of the worklist towards the front. This
    // check makes sure that if we run into any such residual null pointers, we
    // skip them.
    if (I == 0)
      continue;

    // Check to see if we can DCE the instruction.
    if (isInstructionTriviallyDead(I)) {
      DEBUG(llvm::dbgs() << "SC: DCE: " << *I << '\n');
      eraseInstFromFunction(*I);
      ++NumDeadInst;
      MadeChange = true;
      continue;
    }

    // Check to see if we can instsimplify the instruction.
    if (SILValue Result = simplifyInstruction(I)) {
      ++NumSimplified;

      DEBUG(llvm::dbgs() << "SC: Simplify Old = " << *I << '\n'
                         << "    New = " << *Result.getDef() << '\n');

      // Everything uses the new instruction now.
      replaceInstUsesWith(*I, Result.getDef(), 0, Result.getResultNumber());

      // Push the new instruction and any users onto the worklist.
      Worklist.addUsersToWorklist(Result.getDef());

      eraseInstFromFunction(*I);
      MadeChange = true;
      continue;
    }

    // If we have reached this point, all attempts to do simple simplifications
    // have failed. Prepare to SILCombine.
    Builder->setInsertionPoint(I->getParent(), I);

#ifndef NDEBUG
    std::string OrigI;
#endif
    DEBUG(llvm::raw_string_ostream SS(OrigI); I->print(SS); OrigI = SS.str(););
    DEBUG(llvm::dbgs() << "SC: Visiting: " << OrigI << '\n');

    if (SILInstruction *Result = visit(I)) {
      ++NumCombined;
      // Should we replace the old instruction with a new one?
      if (Result != I) {
        // Insert the new instruction into the basic block.
        I->getParent()->getInstList().insert(I, Result);

        DEBUG(llvm::dbgs() << "SC: Old = " << *I << '\n'
                           << "    New = " << *Result << '\n');

        // Everything uses the new instruction now.
        replaceInstUsesWith(*I, Result);

        // Push the new instruction and any users onto the worklist.
        Worklist.add(Result);
        Worklist.addUsersToWorklist(Result);


        eraseInstFromFunction(*I);
      } else {
        DEBUG(llvm::dbgs() << "SC: Mod = " << OrigI << '\n'
                     << "    New = " << *I << '\n');

        // If the instruction was modified, it's possible that it is now dead.
        // if so, remove it.
        if (isInstructionTriviallyDead(I)) {
          eraseInstFromFunction(*I);
        } else {
          Worklist.add(I);
          Worklist.addUsersToWorklist(I);
        }
      }
      MadeChange = true;
    }

    // Our tracking list has been accumulating instructions created by the
    // SILBuilder during this iteration. Go through the tracking list and add
    // its contents to the worklist and then clear said list in preparation for
    // the next iteration.
    for (SILInstruction *I : TrackingList) {
      if (!DeletedInstSet.count(I))
        Worklist.add(I);
    }
    TrackingList.clear();
    DeletedInstSet.clear();
  }

  Worklist.zap();
  return MadeChange;
}

void SILCombineWorklist::addInitialGroup(ArrayRef<SILInstruction *> List) {
  assert(Worklist.empty() && "Worklist must be empty to add initial group");
  Worklist.reserve(List.size()+16);
  WorklistMap.resize(List.size());
  DEBUG(llvm::dbgs() << "SC: ADDING: " << List.size()
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

  // Create a SILBuilder for F, initialize the tracking list, and add the
  // callback so we can update the worklist if the SILBuilder deletes
  // instructions.
  SILBuilder B(F);
  B.setTrackingList(&TrackingList);
  Builder = &B;

  bool Changed = false;
  // Perform iterations until we do not make any changes.
  while (doOneIteration(F, Iteration)) {
    Changed = true;
    Iteration++;
  }

  // Cleanup the builder and return whether or not we made any changes.
  Builder = nullptr;
  return Changed;
}

// Insert the instruction New before instruction Old in Old's parent BB. Add
// New to the worklist.
SILInstruction *SILCombiner::insertNewInstBefore(SILInstruction *New,
                                                 SILInstruction &Old) {
  assert(New && New->getParent() == 0 &&
         "New instruction already inserted into a basic block!");
  SILBasicBlock *BB = Old.getParent();
  BB->getInstList().insert(&Old, New);  // Insert inst
  Worklist.add(New);
  return New;
}

// This method is to be used when an instruction is found to be dead,
// replacable with another preexisting expression. Here we add all uses of I
// to the worklist, replace all uses of I with the new value, then return I,
// so that the combiner will know that I was modified.
SILInstruction *SILCombiner::replaceInstUsesWith(SILInstruction &I,
                                                 ValueBase *V) {
  Worklist.addUsersToWorklist(&I);   // Add all modified instrs to worklist.

  DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
        "    with " << *V << '\n');

  I.replaceAllUsesWith(V);

  return &I;
}

/// This is meant to be used when one is attempting to replace only one of the
/// results of I with a result of V.
SILInstruction *
SILCombiner::
replaceInstUsesWith(SILInstruction &I, ValueBase *V, unsigned IIndex,
                    unsigned VIndex) {
  assert(IIndex < I.getNumTypes() && "Can not have more results than "
         "types.");
  assert(VIndex < V->getNumTypes() && "Can not have more results than "
         "types.");

  // Add all modified instrs to worklist.
  Worklist.addUsersToWorklist(&I, IIndex);

  DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
        "    with " << *V << '\n');

  SILValue(&I, IIndex).replaceAllUsesWith(SILValue(V, VIndex));

  return &I;
}

// Some instructions can never be "trivially dead" due to side effects or
// producing a void value. In those cases, since we can not rely on
// SILCombines trivially dead instruction DCE in order to delete the
// instruction, visit methods should use this method to delete the given
// instruction and upon completion of their peephole return the value returned
// by this method.
SILInstruction *SILCombiner::eraseInstFromFunction(SILInstruction &I,
                                            SILBasicBlock::iterator &InstIter,
                                            bool AddOperandsToWorklist) {
  DEBUG(llvm::dbgs() << "SC: ERASE " << I << '\n');

  assert(hasNoUsesExceptDebug(&I) && "Cannot erase instruction that is used!");
  // Make sure that we reprocess all operands now that we reduced their
  // use counts.
  if (I.getNumOperands() < 8 && AddOperandsToWorklist)
    for (auto &OpI : I.getAllOperands())
      if (SILInstruction *Op = llvm::dyn_cast<SILInstruction>(&*OpI.get()))
        Worklist.add(Op);

  // If we have a call graph and we've removing an apply, remove the
  // associated edges from the call graph.
  if (CG)
    if (auto *AI = dyn_cast<ApplyInst>(&I))
      if (auto *Edge = CG->getCallGraphEdge(AI))
        CG->removeEdge(Edge);

  for (Operand *DU : getDebugUses(I))
    Worklist.remove(DU->getUser());

  Worklist.remove(&I);
  eraseFromParentWithDebugInsts(&I, InstIter);
  DeletedInstSet.insert(&I);
  MadeChange = true;
  return nullptr;  // Don't do anything with I
}

//===----------------------------------------------------------------------===//
//                                Entry Points
//===----------------------------------------------------------------------===//

namespace {

class SILCombine : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() override {
    auto *AA = PM->getAnalysis<AliasAnalysis>();

    // Call Graph Analysis in case we need to perform Call Graph updates.
    auto *CGA = PM->getAnalysis<CallGraphAnalysis>();
    SILCombiner Combiner(AA, CGA->getCallGraphOrNull(),
                         getOptions().RemoveRuntimeAsserts);
    bool Changed = Combiner.runOnFunction(*getFunction());

    if (Changed) {
      // Ignore invalidation messages for all analyses that we keep up to date
      // manually.
      CGA->lockInvalidation();

      // Invalidate everything else.
      invalidateAnalysis(SILAnalysis::PreserveKind::Nothing);

      // Unlock all of the analyses that we locked.
      CGA->unlockInvalidation();
    }
  }

  StringRef getName() override { return "SIL Combine"; }
};

} // end anonymous namespace

SILTransform *swift::createSILCombine() {
  return new SILCombine();
}

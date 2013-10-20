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
// small combining operations/peepholes at the SIL level. It additionally DCEs
// before in order to reduce compile time.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-combine"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/Subsystems.h"

using namespace swift;

STATISTIC(NumCombined, "Number of instructions combined.");
STATISTIC(NumDeadInst, "Num dead insts eliminated.");

//===----------------------------------------------------------------------===//
//                             SILCombineWorklist
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the worklist management logic for SILCombine.
class SILCombineWorklist {
  llvm::SmallVector<SILInstruction*, 256> Worklist;
  llvm::DenseMap<SILInstruction*, unsigned> WorklistMap;
  llvm::SmallVector<SILInstruction *, 8> TrackingList;

  void operator=(const SILCombineWorklist&RHS) = delete;
  SILCombineWorklist(const SILCombineWorklist&) = delete;
public:
  SILCombineWorklist() {}

  /// Returns true if the worklist is empty.
  bool isEmpty() const { return Worklist.empty(); }

  /// Add the specified instruction to the worklist if it isn't already in it.
  void add(SILInstruction *I) {
    if (WorklistMap.insert(std::make_pair(I, Worklist.size())).second) {
      DEBUG(llvm::dbgs() << "SC: ADD: " << *I << '\n');
      Worklist.push_back(I);
    }
  }

  /// Add the given ValueBase if it is an instruction to the worklist if it is a
  /// SILInstruction.
  void addValue(ValueBase *V) {
    if (SILInstruction *I = llvm::dyn_cast<SILInstruction>(V))
      add(I);
  }

  /// Add the specified batch of stuff in reverse order.  which should only be
  /// done when the worklist is empty and when the group has no duplicates.
  void addInitialGroup(SILInstruction *const *List, unsigned NumEntries) {
    assert(Worklist.empty() && "Worklist must be empty to add initial group");
    Worklist.reserve(NumEntries+16);
    WorklistMap.resize(NumEntries);
    DEBUG(llvm::dbgs() << "SC: ADDING: " << NumEntries
                       << " instrs to worklist\n");
    for (unsigned Idx = 0; NumEntries; --NumEntries) {
      SILInstruction *I = List[NumEntries-1];
      WorklistMap.insert(std::make_pair(I, Idx++));
      Worklist.push_back(I);
    }
  }

  // Remove I from the worklist if it exists.
  void remove(SILInstruction *I) {
    auto It = WorklistMap.find(I);
    if (It == WorklistMap.end()) return; // Not in worklist.

    // Don't bother moving everything down, just null out the slot. We will
    // check before we process any instruction if it is 0 allowing us to not
    // have to shift all of the elements of our worklist down everytime an
    // instruction is removed.
    Worklist[It->second] = 0;

    WorklistMap.erase(It);
  }

  /// Remove the top element from the worklist.
  SILInstruction *removeOne() {
    SILInstruction *I = Worklist.pop_back_val();
    WorklistMap.erase(I);
    return I;
  }

  /// When an instruction is simplified, add all users of the instruction to the
  /// work lists because they might get more simplified now.
  void addUsersToWorklist(SILInstruction &I) {
    for (auto UI : I.getUses())
      add(llvm::cast<SILInstruction>(UI->getUser()));
  }

  /// Check that the worklist is empty and nuke the backing store for the map if
  /// it is large.
  void zap() {
    assert(WorklistMap.empty() && "Worklist empty, but the map is not?");

    // Do an explicit clear, this shrinks the map if needed.
    WorklistMap.clear();
  }
};

} // end namespace swift

//===----------------------------------------------------------------------===//
//                                SILCombiner
//===----------------------------------------------------------------------===//

namespace swift {

/// This is a class which maintains the state of the combine and simplifies many
/// operations such as removing/adding instructions and syncing that with the
/// worklist.
class SILCombiner :
    public SILInstructionVisitor<SILCombiner, SILInstruction *> {
public:
  SILCombiner() : Worklist(), MadeChange(false), Iteration(0), Builder(0) { }

  bool runOnFunction(SILFunction &F) {
    bool MadeActualChange = false;
    Iteration = 0;

    // Create a SILBuilder for F and initialize the TrackingList.
    SILBuilder B(F);
    B.setTrackingList(&TrackingList);
    Builder = &B;

    // Perform iterations until we do not make any changes.
    while (doOneIteration(F, Iteration)) {
      Iteration++;
      MadeActualChange = true;
    }

    // Cleanup builder and return whether or not we made any changes.
    Builder = 0;
    return MadeActualChange;
  }

  void clear() {
    Iteration = 0;
    Worklist.zap();
    MadeChange = false;
  }

  // Insert the instruction New before instruction Old in the program.  Add the
  // new instruction to the worklist.
  SILInstruction *insertNewInstBefore(SILInstruction *New,
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
  // so that the inst combiner will know that I was modified.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V) {
    Worklist.addUsersToWorklist(I);   // Add all modified instrs to worklist.

    DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
          "    with " << *V << '\n');

    SILValue(&I, 0).replaceAllUsesWith(V);
    return &I;
  }

  // When dealing with an instruction that has side effects or produces a void
  // value, we can't rely on DCE to delete the instruction.  Instead, visit
  // methods should return the value returned by this function.
  SILInstruction *eraseInstFromFunction(SILInstruction &I) {
    DEBUG(llvm::dbgs() << "SC: ERASE " << I << '\n');

    assert(I.use_empty() && "Cannot erase instruction that is used!");
    // Make sure that we reprocess all operands now that we reduced their
    // use counts.
    if (I.getNumOperands() < 8) {
      for (auto &OpI : I.getAllOperands())
        if (SILInstruction *Op = llvm::dyn_cast<SILInstruction>(&*OpI.get()))
          Worklist.add(Op);
    }
    Worklist.remove(&I);
    I.eraseFromParent();
    MadeChange = true;
    return 0;  // Don't do anything with FI
  }

  void addInitialGroup(SILInstruction *const *List, unsigned NumEntries) {
    Worklist.addInitialGroup(List, NumEntries);
  }

  /// Base visitor that does not do anything.
  SILInstruction *visitValueBase(ValueBase *V) { return nullptr; }

private:
  /// Perform one SILCombine iteration.
  bool doOneIteration(SILFunction &F, unsigned Iteration);

  /// All of the instructions that need to be simplified.
  SILCombineWorklist Worklist;
  /// Did we make a chanbge?
  bool MadeChange;
  /// What is the current iteration of the SILCombine we are on.
  unsigned Iteration;
  /// Builder used to insert instructions.
  SILBuilder *Builder;
  /// A list that the builder puts newly created instructions into. Its contents
  /// are added to the worklist after every iteration and then is cleared.
  llvm::SmallVector<SILInstruction *, 64> TrackingList;
};

} // end namespace swift

//===----------------------------------------------------------------------===//
//                         SILCombine Implementation
//===----------------------------------------------------------------------===//

/// addReachableCodeToWorklist - Walk the function in depth-first order, adding
/// all reachable code to the worklist.
///
/// This has a couple of tricks to make the code faster and more powerful.  In
/// particular, we DCE instructions as we go, to avoid adding them to the
/// worklist (this significantly speeds up instcombine on code where many
/// instructions are dead or constant).
static void addReachableCodeToWorklist(SILBasicBlock *BB, SILCombiner &IC) {
  llvm::SmallVector<SILBasicBlock*, 256> Worklist;
  llvm::SmallVector<SILInstruction*, 128> InstrsForInstCombineWorklist;
  llvm::SmallPtrSet<SILBasicBlock*, 64> Visited;

  Worklist.push_back(BB);
  do {
    BB = Worklist.pop_back_val();

    // We have now visited this block!  If we've already been here, ignore it.
    if (!Visited.insert(BB)) continue;

    for (SILBasicBlock::iterator BBI = BB->begin(), E = BB->end(); BBI != E; ) {
      SILInstruction *Inst = BBI++;

      // DCE instruction if trivially dead.
      if (isInstructionTriviallyDead(Inst)) {
        ++NumDeadInst;
        DEBUG(llvm::dbgs() << "SC: DCE: " << *Inst << '\n');
        Inst->eraseFromParent();
        continue;
      }

      InstrsForInstCombineWorklist.push_back(Inst);
    }

    // Recursively visit successors.
    for (auto SI = BB->succ_begin(), SE = BB->succ_end(); SI != SE; ++SI)
      Worklist.push_back(*SI);
  } while (!Worklist.empty());

  // Once we've found all of the instructions to add to SILCombine's worklist,
  // add them in reverse order.  This way SILCombine will visit from the top of
  // the function down.  This jives well with the way that it adds all uses of
  // instructions to the worklist after doing a transformation, thus avoiding
  // some N^2 behavior in pathological cases.
  IC.addInitialGroup(&InstrsForInstCombineWorklist[0],
                     InstrsForInstCombineWorklist.size());
}

bool SILCombiner::doOneIteration(SILFunction &F, unsigned Iteration) {
  MadeChange = false;

  DEBUG(llvm::dbgs() << "\n\nSILCOMBINE ITERATION #" << Iteration << " on "
                     << F.getName() << "\n");

  // Add reachable instructions to our worklist.
  addReachableCodeToWorklist(F.begin(), *this);

  // Process until we run out of items in our worklist.
  while (!Worklist.isEmpty()) {
    SILInstruction *I = Worklist.removeOne();

    // When we erase an instruction, we use the map in the worklist to check if
    // the instruction is in the worklist. If it is, we replace it with 0
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

    // Now that we have an instruction, try simplifying it.
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
        DEBUG(llvm::dbgs() << "SC: Old = " << *I << '\n'
                     << "    New = " << *Result << '\n');

        // Everything uses the new instruction now.
        replaceInstUsesWith(*I, Result);

        // Push the new instruction and any users onto the worklist.
        Worklist.add(Result);
        Worklist.addUsersToWorklist(*Result);

        // Insert the new instruction into the basic block...
        SILBasicBlock *InstParent = I->getParent();
        SILBasicBlock::iterator InsertPos = I;

        InstParent->getInstList().insert(InsertPos, Result);

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
          Worklist.addUsersToWorklist(*I);
        }
      }
      MadeChange = true;
    }

    // Our tracking list which has been accumulating instructions created by the
    // SILBuilder doing this iteration. Go through the tracking list and add
    // them to the worklist and then clear the TrackingList in preparation for
    // the next iteration.
    for (SILInstruction *I : TrackingList) {
      Worklist.add(I);
    }
    TrackingList.clear();
  }

  Worklist.zap();
  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

bool swift::performSILCombine(SILModule *M) {
  bool MadeChange = false;
  SILCombiner Combiner;

  // Process each function in M.
  for (SILFunction &F : *M) {
    // If F is just a declaration without any basic blocks, skip it.
    if (!F.size())
      continue;

    // Clear the combiner just in case.
    Combiner.clear();
    // Record if we made any changes.
    MadeChange |= Combiner.runOnFunction(F);
  }

  // Return true if we made any changes.
  return MadeChange;
}

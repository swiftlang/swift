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
#include "swift/SIL/PatternMatch.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;
using namespace swift::PatternMatch;

STATISTIC(NumSimplified, "Number of instructions simplified");
STATISTIC(NumCombined, "Number of instructions combined");
STATISTIC(NumDeadInst, "Number of dead insts eliminated");

//===----------------------------------------------------------------------===//
//                             SILCombineWorklist
//===----------------------------------------------------------------------===//

namespace swift {

/// This is the worklist management logic for SILCombine.
class SILCombineWorklist {
  llvm::SmallVector<SILInstruction *, 256> Worklist;
  llvm::DenseMap<SILInstruction *, unsigned> WorklistMap;
  llvm::SmallVector<SILInstruction *, 8> TrackingList;

  void operator=(const SILCombineWorklist &RHS) = delete;
  SILCombineWorklist(const SILCombineWorklist &Worklist) = delete;
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

  /// If the given ValueBase is a SILInstruction add it to the worklist.
  void addValue(ValueBase *V) {
    if (SILInstruction *I = llvm::dyn_cast<SILInstruction>(V))
      add(I);
  }

  /// Add the given list of instructions in reverse order to the worklist. This
  /// routine assumes that the worklist is empty and the given list has no
  /// duplicates.
  void addInitialGroup(ArrayRef<SILInstruction *> List) {
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

  // If I is in the worklist, remove it.
  void remove(SILInstruction *I) {
    auto It = WorklistMap.find(I);
    if (It == WorklistMap.end()) return; // Not in worklist.

    // Don't bother moving everything down, just null out the slot. We will
    // check before we process any instruction if it is null.
    Worklist[It->second] = 0;

    WorklistMap.erase(It);
  }

  /// Remove the top element from the worklist.
  SILInstruction *removeOne() {
    SILInstruction *I = Worklist.pop_back_val();
    WorklistMap.erase(I);
    return I;
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist since additional simplifications of its users may have been
  /// exposed.
  void addUsersToWorklist(ValueBase *I) {
    for (auto UI : I->getUses())
      add(UI->getUser());
  }

  /// If only one result of an instruction has been simplified, add all of the
  /// users of that result to the worklist since additional simplifications of
  /// its users may have been exposed.
  void addUsersToWorklist(ValueBase *I, unsigned Index) {
    for (auto UI : SILValue(I, Index).getUses())
      add(UI->getUser());
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

/// This is a class which maintains the state of the combiner and simplifies
/// many operations such as removing/adding instructions and syncing them with
/// the worklist.
class SILCombiner :
    public SILInstructionVisitor<SILCombiner, SILInstruction *> {
public:
  SILCombiner() : Worklist(), MadeChange(false), Iteration(0), Builder(0) { }

  void runOnFunction(SILFunction &F) {
    Iteration = 0;

    // Create a SILBuilder for F and initialize the tracking list.
    SILBuilder B(F);
    B.setTrackingList(&TrackingList);
    Builder = &B;

    // Perform iterations until we do not make any changes.
    while (doOneIteration(F, Iteration)) {
      Iteration++;
    }

    // Cleanup the builder and return whether or not we made any changes.
    Builder = 0;
  }

  void clear() {
    Iteration = 0;
    Worklist.zap();
    MadeChange = false;
  }

  // Insert the instruction New before instruction Old in Old's parent BB. Add
  // New to the worklist.
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
  // so that the combiner will know that I was modified.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V) {
    Worklist.addUsersToWorklist(&I);   // Add all modified instrs to worklist.

    DEBUG(llvm::dbgs() << "SC: Replacing " << I << "\n"
          "    with " << *V << '\n');

    I.replaceAllUsesWith(V);

    return &I;
  }

  /// This is meant to be used when one is attempting to replace only one of the
  /// results of I with a result of V.
  SILInstruction *replaceInstUsesWith(SILInstruction &I, ValueBase *V,
                                      unsigned IIndex, unsigned VIndex=0) {
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
    return 0;  // Don't do anything with I
  }

  void addInitialGroup(ArrayRef<SILInstruction *> List) {
    Worklist.addInitialGroup(List);
  }

  /// Base visitor that does not do anything.
  SILInstruction *visitValueBase(ValueBase *V) { return nullptr; }
  SILInstruction *visitStructExtractInst(StructExtractInst *V);

private:
  /// Perform one SILCombine iteration.
  bool doOneIteration(SILFunction &F, unsigned Iteration);

  /// Worklist containing all of the instructions primed for simplification.
  SILCombineWorklist Worklist;
  /// Variable to track if the SILCombiner made any changes.
  bool MadeChange;
  /// The current iteration of the SILCombine.
  unsigned Iteration;
  /// Builder used to insert instructions.
  SILBuilder *Builder;
  /// A list that the builder inserts newly created instructions into. Its
  /// contents are added to the worklist after every iteration and then the list
  /// is cleared.
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
/// worklist (this significantly speeds up SILCombine on code where many
/// instructions are dead or constant).
static void addReachableCodeToWorklist(SILBasicBlock *BB, SILCombiner &SC) {
  llvm::SmallVector<SILBasicBlock*, 256> Worklist;
  llvm::SmallVector<SILInstruction*, 128> InstrsForSILCombineWorklist;
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
  SC.addInitialGroup(InstrsForSILCombineWorklist);
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
      replaceInstUsesWith(*I, Result.getDef());

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
        DEBUG(llvm::dbgs() << "SC: Old = " << *I << '\n'
                     << "    New = " << *Result << '\n');

        // Everything uses the new instruction now.
        replaceInstUsesWith(*I, Result);

        // Push the new instruction and any users onto the worklist.
        Worklist.add(Result);
        Worklist.addUsersToWorklist(Result);

        // Insert the new instruction into the basic block.
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
      Worklist.add(I);
    }
    TrackingList.clear();
  }

  Worklist.zap();
  return MadeChange;
}

//===----------------------------------------------------------------------===//
//                                  Visitors
//===----------------------------------------------------------------------===//

SILInstruction *SILCombiner::visitStructExtractInst(StructExtractInst *SEI) {
  // (struct_extract (load %x) #vardecl)
  //   ->
  // (load (struct_element_addr %x), #vardecl)
  LoadInst *LI;
  if (match(SEI->getOperand(), m_LoadInst(LI))) {
    // Move our insertion point to the load so we insert the new
    // struct_element_addr and load there.
    //
    // This is to ensure that in a situation like the following:
    //
    // %y = (load %x)
    // (do_stuff)
    // (struct_extract %y #vardecl)
    //
    // if (do_stuff) modifies the memory at %x, we get the original value.
    Builder->setInsertionPoint(LI);
    StructElementAddrInst *SEA =
      Builder->createStructElementAddr(SEI->getLoc(), LI->getOperand(),
                                       SEI->getField());
    LoadInst *Result = Builder->createLoad(SEI->getLoc(), SEA);

    return replaceInstUsesWith(*SEI, Result, 0);
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILCombine(SILModule *M) {
  SILCombiner Combiner;

  // Process each function in M.
  for (SILFunction &F : *M) {
    // If F is just a declaration without any basic blocks, skip it.
    if (F.empty())
      continue;

    // Clear the combiner just in case.
    Combiner.clear();
    // Record if we made any changes.
    Combiner.runOnFunction(F);
  }
}

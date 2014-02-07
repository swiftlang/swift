//===- SILCodeMotion.cpp - Code Motion Optimizations ----------------------===//
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

#define DEBUG_TYPE "codemotion"
#include "swift/SILPasses/Passes.h"
#include "swift/SIL/Dominance.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/RecyclingAllocator.h"

STATISTIC(NumSunk,   "Number of instructions sunk");
STATISTIC(NumDeadStores, "Number of dead stores removed");
STATISTIC(NumDupLoads,   "Number of dup loads removed");

using namespace swift;

static const int SinkSearchWindow = 6;

static bool isWriteMemBehavior(SILInstruction::MemoryBehavior B) {
  switch (B) {
  case SILInstruction::MemoryBehavior::MayWrite:
  case SILInstruction::MemoryBehavior::MayReadWrite:
  case SILInstruction::MemoryBehavior::MayHaveSideEffects:
    return true;
  case SILInstruction::MemoryBehavior::None:
  case SILInstruction::MemoryBehavior::MayRead:
    return false;
  }
}

namespace {

/// An abstract representation of a SIL Projection that allows one to refer to
/// either nominal fields or tuple indices.
class Projection {
  SILType Type;
  VarDecl *Decl;
  unsigned Index;

public:
  Projection(SILType T, VarDecl *D) : Type(T), Decl(D), Index(-1) { }
  Projection(SILType T, unsigned I) : Type(T), Decl(nullptr), Index(I) { }

  SILType getType() const { return Type; }
  VarDecl *getDecl() const { return Decl; }
  unsigned getIndex() const { return Index; }

  bool operator==(Projection &Other) const {
    if (Decl)
      return Decl == Other.getDecl();
    else
      return !Other.getDecl() && Index == Other.getIndex();
  }

  bool operator<(Projection Other) const {
    // If Proj1 is a decl...
    if (Decl) {
      // It should be sorted before Proj2 is Proj2 is not a decl. Otherwise
      // compare the pointers.
      if (auto OtherDecl = Other.getDecl())
        return uintptr_t(Decl) < uintptr_t(OtherDecl);
      return true;
    }

    // If Proj1 is not a decl, then if Proj2 is a decl, Proj1 is not before
    // Proj2. If Proj2 is not a decl, compare the indices.
    return !Other.getDecl() && (Index < Other.Index);
  }
};

} // end anonymous namespace.

static bool isAddressProjection(SILValue V) {
  switch (V->getKind()) {
  case ValueKind::StructElementAddrInst:
  case ValueKind::TupleElementAddrInst:
    return true;
  default:
    return false;
  }
}

// Given an already emitted load PrevLd, see if we can
static SILValue findExtractPathBetweenValues(LoadInst *PrevLI, LoadInst *LI) {
  SILValue PrevLIOp = PrevLI->getOperand();
  SILValue LIOp = LI->getOperand();

  // If they are equal, just return PrevLI.
  if (PrevLIOp == LIOp)
    return PrevLI;

  // Otherwise see if LI can be projection extracted from PrevLI. First see if
  // LI is a projection at all.
  llvm::SmallVector<Projection, 4> Projections;
  auto Iter = LIOp;
  while (isAddressProjection(Iter) && PrevLIOp != Iter) {
    if (auto *SEA = dyn_cast<StructElementAddrInst>(Iter.getDef()))
      Projections.push_back(Projection(Iter.getType(), SEA->getField()));
    else
      Projections.push_back(
        Projection(Iter.getType(),
                   cast<TupleElementAddrInst>(*Iter).getFieldNo()));
    Iter = cast<SILInstruction>(*Iter).getOperand(0);
  }

  // We could not find an extract path in between the two values.
  if (Projections.empty() || PrevLIOp != Iter)
    return SILValue();

  // Use the projection list we created to create the relevant extracts
  SILValue LastExtract = PrevLI;
  SILBuilder Builder(LI);
  while (!Projections.empty()) {
    auto P = Projections.pop_back_val();
    if (auto *D = P.getDecl()) {
      LastExtract = Builder.createStructExtract(LI->getLoc(), LastExtract,
                                                D,
                                                P.getType().getObjectType());
      cast<StructExtractInst>(*LastExtract).getStructDecl();
    } else {
      LastExtract = Builder.createTupleExtract(LI->getLoc(), LastExtract,
                                               P.getIndex(),
                                               P.getType().getObjectType());
      cast<TupleExtractInst>(*LastExtract).getTupleType();
    }
  }

  // Return the last extract we created.
  return LastExtract;
}

static void
invalidateAliasingLoads(SILInstruction *Inst,
                        llvm::SmallPtrSetImpl<LoadInst *> &Loads,
                        AliasAnalysis *AA) {
  llvm::SmallVector<LoadInst *, 4> InvalidatedLoadList;
  for (auto *LI : Loads)
    if (isWriteMemBehavior(AA->getMemoryBehavior(Inst, LI->getOperand())))
      InvalidatedLoadList.push_back(LI);
  for (auto *LI : InvalidatedLoadList) {
    DEBUG(llvm::dbgs() << "    Found an instruction that writes to memory "
          "such that a load is invalidated:" << *LI);
    Loads.erase(LI);
  }
}

/// \brief Promote stored values to loads, remove dead stores and merge
/// duplicated loads.
bool promoteMemoryOperationsInBlock(SILBasicBlock *BB, AliasAnalysis *AA) {
  bool Changed = false;
  StoreInst *PrevStore = 0;
  llvm::SmallPtrSet<LoadInst *, 8> Loads;

  auto II = BB->begin(), E = BB->end();
  while (II != E) {
    SILInstruction *Inst = II++;

    DEBUG(llvm::dbgs() << "Visiting: " << *Inst);

    // This is a StoreInst. Let's see if we can remove the previous stores.
    if (StoreInst *SI = dyn_cast<StoreInst>(Inst)) {
      // Invalidate any load that we can not prove does not read from the stores
      // destination.
      invalidateAliasingLoads(Inst, Loads, AA);

      // If we are storing to the previously stored address then delete the old
      // store.
      if (PrevStore && PrevStore->getDest() == SI->getDest()) {
        DEBUG(llvm::dbgs() << "    Found a dead previous store... Removing...:"
              << *PrevStore);
        Changed = true;
        recursivelyDeleteTriviallyDeadInstructions(PrevStore, true);
        PrevStore = SI;
        NumDeadStores++;
        continue;
      }
      PrevStore = SI;
      continue;
    }

    if (LoadInst *LI = dyn_cast<LoadInst>(Inst)) {
      // If we are loading a value that we just saved then use the saved value.
      if (PrevStore && PrevStore->getDest() == LI->getOperand()) {
        DEBUG(llvm::dbgs() << "    Forwarding store from: " << *PrevStore);
        SILValue(LI, 0).replaceAllUsesWith(PrevStore->getSrc());
        recursivelyDeleteTriviallyDeadInstructions(LI, true);
        Changed = true;
        NumDupLoads++;
        continue;
      }

      // Search the previous loads and replace the current load with one of the
      // previous loads.
      for (auto PrevLI : Loads) {
        SILValue ForwardingExtract = findExtractPathBetweenValues(PrevLI, LI);
        if (!ForwardingExtract)
          continue;

        DEBUG(llvm::dbgs() << "    Replacing with previous load: "
              << *ForwardingExtract);
        SILValue(LI, 0).replaceAllUsesWith(ForwardingExtract);
        recursivelyDeleteTriviallyDeadInstructions(LI, true);
        Changed = true;
        LI = 0;
        NumDupLoads++;
        break;
      }

      if (LI)
        Loads.insert(LI);
      continue;
    }

    // Retains write to memory but they don't affect loads and stores.
    if (isa<StrongRetainInst>(Inst)) {
      DEBUG(llvm::dbgs() << "    Found strong retain, does not affect loads and"
            " stores.\n");
      continue;
    }

    // Dealloc stack does not affect loads and stores.
    if (isa<DeallocStackInst>(Inst)) {
      DEBUG(llvm::dbgs() << "Found a dealloc stack. Does not affect loads and "
            "stores.\n");
      continue;
    }

    if (auto *AI = dyn_cast<ApplyInst>(Inst))
      if (auto *BI = dyn_cast<BuiltinFunctionRefInst>(&*AI->getCallee()))
        if (isReadNone(BI)) {
          DEBUG(llvm::dbgs() << "    Found readnone builtin, does not affect "
                "loads and stores.\n");
          continue;
        }

    // cond_fail does not read/write memory in a manner that we care about.
    if (isa<CondFailInst>(Inst)) {
      DEBUG(llvm::dbgs() << "    Found a cond fail, does not affect "
            "loads and stores.\n");
      continue;
    }

    // All other instructions that read from memory invalidate the store.
    if (Inst->mayReadFromMemory()) {
      DEBUG(llvm::dbgs() << "    Found an instruction that reads from memory."
            " Invalidating store.\n");
      PrevStore = 0;
    }

    // If we have an instruction that may write to memory and we can not prove
    // that it and its operands can not alias a load we have visited, invalidate
    // that load.
    if (Inst->mayWriteToMemory())
      // Invalidate any load that we can not prove does not read from one of the
      // writing instructions operands.
      invalidateAliasingLoads(Inst, Loads, AA);
  }

  return Changed;
}

/// \brief Returns True if we can sink this instruction to another basic block.
static bool canSinkInstruction(SILInstruction *Inst) {
  return Inst->use_empty() && !isa<TermInst>(Inst);
}

/// \brief Returns true if this instruction is a skip barrier, which means that
/// we can't sink other instructions past it.
static bool isSinkBarrier(SILInstruction *Inst) {
  // We know that some calls do not have side effects.
  if (const ApplyInst *AI = dyn_cast<ApplyInst>(Inst))
    if (BuiltinFunctionRefInst *FR =
        dyn_cast<BuiltinFunctionRefInst>(AI->getCallee().getDef()))
      return !isSideEffectFree(FR);

  if (isa<TermInst>(Inst))
    return false;

  if (Inst->mayHaveSideEffects())
    return true;

  return false;
}

/// \brief Search for an instruction that is identical to \p Iden by scanning
/// \p BB starting at the end of the block, stopping on sink barriers.
SILInstruction *findIdenticalInBlock(SILBasicBlock *BB, SILInstruction *Iden) {
  int SkipBudget = SinkSearchWindow;

  SILBasicBlock::iterator InstToSink = BB->getTerminator();

  while (SkipBudget) {
    // If we found a sinkable instruction that is identical to our goal
    // then return it.
    if (canSinkInstruction(InstToSink) && Iden->isIdenticalTo(InstToSink)) {
      DEBUG(llvm::dbgs() << "Found an identical instruction.");
      return InstToSink;
    }

    // If this instruction is a skip-barrier end the scan.
    if (isSinkBarrier(InstToSink))
      return nullptr;

    // If this is the first instruction in the block then we are done.
    if (InstToSink == BB->begin())
      return nullptr;

    SkipBudget--;
    InstToSink = std::prev(InstToSink);
    DEBUG(llvm::dbgs() << "Continuing scan. Next inst: " << *InstToSink);
  }

  return nullptr;
}

static bool sinkCodeFromPredecessors(SILBasicBlock *BB) {
  bool Changed = false;
  if (BB->pred_empty())
    return Changed;

  // This block must be the only successor of all the predecessors.
  for (auto P : BB->getPreds())
    if (P->getSingleSuccessor() != BB)
      return Changed;

  SILBasicBlock *FirstPred = *BB->pred_begin();
  // The first Pred must have at least one non-terminator.
  if (FirstPred->getTerminator() == FirstPred->begin())
    return Changed;

  DEBUG(llvm::dbgs() << " Sinking values from predecessors.\n");

  unsigned SkipBudget = SinkSearchWindow;

  // Start scanning backwards from the terminator.
  SILBasicBlock::iterator InstToSink = FirstPred->getTerminator();

  while (SkipBudget) {
    DEBUG(llvm::dbgs() << "Processing: " << *InstToSink);

    // Save the duplicated instructions in case we need to remove them.
    SmallVector<SILInstruction *, 4> Dups;

    if (canSinkInstruction(InstToSink)) {
      // For all preds:
      for (auto P : BB->getPreds()) {
        if (P == FirstPred)
          continue;

        // Search the duplicated instruction in the predecessor.
        if (SILInstruction *DupInst = findIdenticalInBlock(P, InstToSink)) {
          Dups.push_back(DupInst);
        } else {
          DEBUG(llvm::dbgs() << "Instruction mismatch.\n");
          Dups.clear();
          break;
        }
      }

      // If we found duplicated instructions, sink one of the copies and delete
      // the rest.
      if (Dups.size()) {
        DEBUG(llvm::dbgs() << "Moving: " << *InstToSink);
        InstToSink->moveBefore(BB->begin());
        Changed = true;
        for (auto I : Dups) {
          I->replaceAllUsesWith(InstToSink);
          I->eraseFromParent();
          NumSunk++;
        }

        // Restart the scan.
        InstToSink = FirstPred->getTerminator();
        DEBUG(llvm::dbgs() << "Restarting scan. Next inst: " << *InstToSink);
        continue;
      }
    }

    // If this instruction was a barrier then we can't sink anything else.
    if (isSinkBarrier(InstToSink)) {
      DEBUG(llvm::dbgs() << "Aborting on barrier: " << *InstToSink);
      return Changed;
    }

    // This is the first instruction, we are done.
    if (InstToSink == FirstPred->begin()) {
      DEBUG(llvm::dbgs() << "Reached the first instruction.");
      return Changed;
    }

    SkipBudget--;
    InstToSink = std::prev(InstToSink);
    DEBUG(llvm::dbgs() << "Continuing scan. Next inst: " << *InstToSink);
  }

  return Changed;
}

class SILCodeMotion : public SILFunctionTransform {

  /// The entry point to the transformation.
  void run() {
    SILFunction &F = *getFunction();

    DEBUG(llvm::dbgs() << "***** CodeMotion on function: " << F.getName() <<
          " *****\n");

    AliasAnalysis *AA = PM->getAnalysis<AliasAnalysis>();

    bool Changed = false;

    // Remove dead stores and merge duplicate loads.
    for (auto &BB : F)
      Changed |= promoteMemoryOperationsInBlock(&BB, AA);

    // Sink duplicated code from predecessors.
    for (auto &BB : F)
      Changed |= sinkCodeFromPredecessors(&BB);

    if (Changed)
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

SILTransform *swift::createCodeMotion() {
  return new SILCodeMotion();
}

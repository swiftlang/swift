//===-- DeadObjectElimination.h - Remove unused objects  ------------------===//
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
// This pass eliminates store only alloc_ref objects that have destructors
// without side effects.
//
// The high level overview of the algorithm is that first it visits the
// destructor and attempts to prove that the destructor is well behaved, i.e. it
// does not have any side effects outside of the destructor itself. If the
// destructor can be proven to be well behaved, it then goes through the use
// list of the alloc_ref and attempts to prove that the alloc_ref does not
// escape or is used in a way that could cause side effects. If both of those
// conditions apply, the alloc_ref and its entire use graph is eliminated.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "allocref-elim"
#include "swift/SILPasses/Passes.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Transforms.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(DeadAllocRefEliminated,
          "number of AllocRef instructions removed");

STATISTIC(DeadAllocStackEliminated,
          "number of AllocStack instructions removed");

static SILFunction *getDestructor(AllocationInst* AI) {
  if (auto *ARI = dyn_cast<AllocRefInst>(AI)) {
    // We only support classes.
    ClassDecl *ClsDecl = ARI->getType().getClassOrBoundGenericClass();
    if (!ClsDecl)
      return nullptr;

    // Look up the destructor of ClsDecl.
    DestructorDecl *Destructor = ClsDecl->getDestructor();
    assert(Destructor && "getDestructor() should never return a nullptr.");

    // Find the destructor name via SILDeclRef.
    // FIXME: When destructors get moved into vtables, update this to use the
    // vtable for the class.
    SmallVector<char, 128> buffer;
    StringRef Name = SILDeclRef(Destructor).mangle(buffer);
    DEBUG(llvm::dbgs() << "    Looking up destructor: " << Name << "\n");

    // Then try to lookup the destructor from the module.
    SILFunction *Fn = ARI->getModule().lookUpFunction(Name);
    if (!Fn || Fn->empty()) {
      DEBUG(llvm::dbgs() << "    Could not find destructor.\n");
      return nullptr;
    }

    DEBUG(llvm::dbgs() << "    Found destructor!\n");

    // If the destructor has an objc_method calling convention, we can not
    // analyze it since it could be swapped out from under us at runtime.
    if (Fn->getRepresentation() == SILFunctionTypeRepresentation::ObjCMethod) {
      DEBUG(llvm::dbgs() << "        Found objective-c destructor. Can't "
            "analyze!\n");
      return nullptr;
    }

    return Fn;
  }

  return nullptr;
}

/// Analyze the destructor for the class of ARI to see if any instructions in it
/// could have side effects on the program outside the destructor. If it does
/// not, then we can eliminate the destructor.
static bool doesDestructorHaveSideEffects(AllocRefInst *ARI) {
  SILFunction *Fn = getDestructor(ARI);
  // If we can't find a constructor then assume it has side effects.
  if (!Fn)
    return true;

  // A destructor only has one argument, self.
  assert(Fn->begin()->getNumBBArg() == 1 &&
         "Destructor should have only one argument, self.");
  SILArgument *Self = Fn->begin()->getBBArg(0);

  DEBUG(llvm::dbgs() << "    Analyzing destructor.\n");

  // For each BB in the destructor...
  for (auto &BB : *Fn)
    // For each instruction I in BB...
    for (auto &I : BB) {
      DEBUG(llvm::dbgs() << "        Visiting: " << I);

      // If I has no side effects, we can ignore it.
      if (!I.mayHaveSideEffects()) {
        DEBUG(llvm::dbgs() << "            SAFE! Instruction has no side "
              "effects.\n");
        continue;
      }

      // RefCounting operations on Self are ok since we are already in the
      // destructor. RefCountingOperations on other instructions could have side
      // effects though.
      if (auto *RefInst = dyn_cast<RefCountingInst>(&I)) {
        if (RefInst->getOperand(0).stripCasts().getDef() == Self) {
          // For now all ref counting insts have 1 operand. Put in an assert
          // just in case.
          assert(RefInst->getNumOperands() == 1 &&
                 "Make sure RefInst only has one argument.");
          DEBUG(llvm::dbgs() << "            SAFE! Ref count operation on "
                "Self.\n");
          continue;
        } else {
          DEBUG(llvm::dbgs() << "            UNSAFE! Ref count operation not on"
                " self.\n");
          return true;
        }
      }

      // dealloc_stack can be ignored.
      if (isa<DeallocStackInst>(I)) {
        DEBUG(llvm::dbgs() << "            SAFE! dealloc_stack can be "
              "ignored.\n");
        continue;
      }

      // dealloc_ref on self can be ignored, but dealloc_ref on anything else
      // can not be eliminated.
      if (auto *DeallocRef = dyn_cast<DeallocRefInst>(&I)) {
        if (DeallocRef->getOperand().stripCasts().getDef() == Self) {
          DEBUG(llvm::dbgs() << "            SAFE! dealloc_ref on self.\n");
          continue;
        } else {
          DEBUG(llvm::dbgs() << "            UNSAFE! dealloc_ref on value "
                "besides self.\n");
          return true;
        }
      }

      // Storing into the object can be ignored.
      if (auto *SI = dyn_cast<StoreInst>(&I))
        if (SI->getDest().stripAddressProjections().getDef() == Self) {
          DEBUG(llvm::dbgs() << "            SAFE! Instruction is a store into "
                "self.\n");
          continue;
        }

      DEBUG(llvm::dbgs() << "            UNSAFE! Unknown instruction.\n");
      // Otherwise, we can't remove the deallocation completely.
      return true;
    }

  // We didn't find any side effects.
  return false;
}

//===----------------------------------------------------------------------===//
//                             Use Graph Analysis
//===----------------------------------------------------------------------===//

/// Returns false if Inst is an instruction that would require us to keep the
/// alloc_ref alive.
static bool canZapInstruction(SILInstruction *Inst) {
  // It is ok to eliminate various retains/releases. We are either removing
  // everything or nothing.
  if (isa<RefCountingInst>(Inst))
    return true;

  // If we see a store here, we have already checked that we are storing into
  // the pointer before we added it to the worklist, so we can skip it.
  if (isa<StoreInst>(Inst))
    return true;

  // If Inst does not read or write to memory, have side effects, and is not a
  // terminator, we can zap it.
  if (!Inst->mayHaveSideEffects() && !Inst->mayReadFromMemory() &&
      !isa<TermInst>(Inst))
    return true;

  // We know that the destructor has no side effects so we can remove the
  // deallocation instruction too.
  if (isa<DeallocationInst>(Inst))
    return true;

  // Much like deallocation, destroy addr is safe.
  if (isa<DestroyAddrInst>(Inst))
    return true;

  // Otherwise we do not know how to handle this instruction. Be conservative
  // and don't zap it.
  return false;
}

/// Analyze the use graph of AllocRef for any uses that would prevent us from
/// zapping it completely.
static bool
hasUnremoveableUsers(SILInstruction *AllocRef,
                llvm::SmallSetVector<SILInstruction *, 16> &Users) {
  SmallVector<SILInstruction *, 16> Worklist;
  Worklist.push_back(AllocRef);

  DEBUG(llvm::dbgs() << "    Analyzing Use Graph.");

  while (!Worklist.empty()) {
    SILInstruction *I = Worklist.pop_back_val();

    DEBUG(llvm::dbgs() << "        Visiting: " << *I);

    // Insert the instruction into our InvolvedInstructions set.  If we have
    // already seen it, then don't reprocess all of the uses.
    if (!Users.insert(I)) {
      DEBUG(llvm::dbgs() << "        Already seen skipping...\n");
      continue;
    }

    // If we can't zap this instruction... bail...
    if (!canZapInstruction(I)) {
      DEBUG(llvm::dbgs() << "        Found instruction we can't zap...\n");
      return true;
    }

    // At this point, we can remove the instruction as long as all of its users
    // can be removed as well. Scan its users and add them to the worklist for
    // recursive processing.
    for (auto *Op : I->getUses()) {
      auto *User = Op->getUser();

      // Make sure that we are only storing into our users, not storing our
      // users which would be an escape.
      if (auto *SI = dyn_cast<StoreInst>(User))
        if (Op->get() == SI->getSrc()) {
          DEBUG(llvm::dbgs() << "        Found store of pointer. Failure: " <<
                *SI);
          return true;
        }

      // Otherwise, add normal instructions to the worklist for processing.
      Worklist.push_back(User);
    }
  }

  return false;
}

namespace {
class DeadObjectElimination : public SILFunctionTransform {
  llvm::DenseMap<SILType, bool> DestructorAnalysisCache;
  llvm::SmallVector<AllocationInst*, 16> Allocations;

  void collectAllocations(SILFunction &Fn) {
    for (auto &BB : Fn)
      for (auto &II : BB)
        if (auto *AI = dyn_cast<AllocationInst>(&II))
          Allocations.push_back(AI);
    }

  bool processAllocRef(AllocRefInst *ARI);
  bool processAllocStack(AllocStackInst *ASI);
  bool processAllocBox(AllocBoxInst *ABI){ return false;}

  bool processFunction(SILFunction &Fn) {
    Allocations.clear();
    DestructorAnalysisCache.clear();
    bool Changed = false;
    collectAllocations(Fn);
    for (auto *II : Allocations) {
      if (auto *A = dyn_cast<AllocRefInst>(II))
        Changed |= processAllocRef(A);
      else if (auto *A = dyn_cast<AllocStackInst>(II))
        Changed |= processAllocStack(A);
      else if (auto *A = dyn_cast<AllocBoxInst>(II))
        Changed |= processAllocBox(A);
    }
    return Changed;
  }

  void run() override {
    if (processFunction(*getFunction()))
      invalidateAnalysis(SILAnalysis::PreserveKind::ProgramFlow);
  }

  StringRef getName() override { return "Dead Object Elimination"; }
};
} // end anonymous namespace


//===----------------------------------------------------------------------===//
//                            Function Processing
//===----------------------------------------------------------------------===//

void static
removeInstructions(llvm::SmallSetVector<SILInstruction *, 16> &UsersToRemove) {
  for (auto *I : UsersToRemove) {
    if (!I->use_empty())
      for (unsigned i = 0, e = I->getNumTypes(); i != e; ++i)
        SILValue(I, i).replaceAllUsesWith(SILUndef::get(I->getType(i),
                                                        I->getModule()));
    // Now we know that I should not have any uses... erase it from its parent.
    I->eraseFromParent();
  }
}

bool DeadObjectElimination::processAllocRef(AllocRefInst *ARI) {
  // Ok, we have an alloc_ref. Check the cache to see if we have already
  // computed the destructor behavior for its SILType.
  bool HasSideEffects;
  SILType Type = ARI->getType();
  auto CacheSearchResult = DestructorAnalysisCache.find(Type);
  if (CacheSearchResult != DestructorAnalysisCache.end()) {
    // Ok we found a value in the cache.
    HasSideEffects = CacheSearchResult->second;
  } else {
    // We did not find a value in the cache for our destructor. Analyze the
    // destructor to make sure it has no side effects. For now this only
    // supports alloc_ref of classes so any alloc_ref with a reference type
    // that is not a class this will return false for. Once we have analyzed
    // it, set Behavior to that value and insert the value into the Cache.
    HasSideEffects = doesDestructorHaveSideEffects(ARI);
    DestructorAnalysisCache[Type] = HasSideEffects;
  }

  if (HasSideEffects) {
    DEBUG(llvm::dbgs() << " Destructor had side effects. \n");
    return false;
  }

  // Our destructor has no side effects, so if we can prove that no loads
  // escape, then we can completely remove the use graph of this alloc_ref.
  llvm::SmallSetVector<SILInstruction *, 16> UsersToRemove;
  if (hasUnremoveableUsers(ARI, UsersToRemove)) {
    DEBUG(llvm::dbgs() << "    Found a use that can not be zapped...\n");
    return false;
  }

  // Remove the AllocRef and all of its users.
  removeInstructions(UsersToRemove);
  DEBUG(llvm::dbgs() << "    Success! Eliminating alloc_ref.\n");

  ++DeadAllocRefEliminated;
  return true;
}

bool DeadObjectElimination::processAllocStack(AllocStackInst *ASI) {
  // Trivial types don't have destructors. Let's try to zap this AllocStackInst.
  if (!ASI->getElementType().isTrivial(ASI->getModule()))
    return false;

  llvm::SmallSetVector<SILInstruction *, 16> UsersToRemove;
  if (hasUnremoveableUsers(ASI, UsersToRemove)) {
    DEBUG(llvm::dbgs() << "    Found a use that can not be zapped...\n");
    return false;
  }

  // Remove the AllocRef and all of its users.
  removeInstructions(UsersToRemove);
  DEBUG(llvm::dbgs() << "    Success! Eliminating alloc_stack.\n");

  ++DeadAllocStackEliminated;
  return true;
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

SILTransform *swift::createDeadObjectElimination() {
  return new DeadObjectElimination();
}

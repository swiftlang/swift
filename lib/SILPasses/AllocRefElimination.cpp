//===-- AllocRefElimination.h - Remove unused well-behaved AllocRefInsts --===//
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

STATISTIC(NumStoreOnlyAllocRefEliminated,
          "number of store only alloc ref eliminated.");

//===----------------------------------------------------------------------===//
//                            Destructor Analysis
//===----------------------------------------------------------------------===//

/// Analyze the destructor for the class of ARI to see if any instructions in it
/// could have side effects on the program outside the destructor. If it does
/// not, then we can eliminate the destructor.
static bool doesDestructorHaveSideEffects(AllocRefInst *ARI) {
  // We only support classes. Assume all other cases have side effects.
  ClassDecl *ClsDecl = ARI->getType().getClassOrBoundGenericClass();
  if (!ClsDecl)
    return true;

  // Look up the destructor of ClsDecl.
  DestructorDecl *Destructor = ClsDecl->getDestructor();
  assert(Destructor && "getDestructor() should never return a nullptr.");

  // Ok we have a destructor. Compute its name (our lookup key) via SILDeclRef.
  //
  // FIXME: When destructors get moved into vtables, update this to use the
  // vtable for the class.
  SmallVector<char, 128> buffer;
  StringRef Name = SILDeclRef(Destructor).mangle(buffer,
                                                 ResilienceExpansion::Minimal);
  DEBUG(llvm::dbgs() << "    Looking up destructor: " << Name << "\n");

  // Then try to lookup the destructor from the module. If we can't the
  // destructor is unable to be found or external, be conservative and assume
  // that it does have side effects.
  SILFunction *Fn = ARI->getModule().lookUpFunction(Name);
  if (!Fn || Fn->empty()) {
    DEBUG(llvm::dbgs() << "    Could not find destructor...\n");
    return true;
  }

  DEBUG(llvm::dbgs() << "    Found destructor!\n");

  // If the destructor has an objc_method calling convention, we can not analyze
  // it since it could be swapped out from under us at runtime.
  if (Fn->getAbstractCC() == AbstractCC::ObjCMethod) {
    DEBUG(llvm::dbgs() << "        Found objective-c destructor. Can't "
          "analyze!\n");
    return true;
  }

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

      // If I is an apply calling a builtin that does not have side effects, we
      // can ignore it.
      if (auto *AI = dyn_cast<ApplyInst>(&I))
        if (auto *FR = dyn_cast<BuiltinFunctionRefInst>(&*AI->getCallee()))
          if (isSideEffectFree(FR)) {
            DEBUG(llvm::dbgs() << "            SAFE! No side effect "
                  "builtin.\n");
            continue;
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

/// Returns true if Inst is an instruction that would require us to keep the
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
  if (Inst->getMemoryBehavior() == SILInstruction::MemoryBehavior::None &&
      !isa<TermInst>(Inst))
    return true;

  // Otherwise we do not know how to handle this instruction. Be conservative
  // and don't zap it.
  return false;
}

/// Analyze the use graph of AllocRef for any uses that would prevent us from
/// zapping it completely.
static bool
analyzeUseGraph(AllocRefInst *AllocRef,
                llvm::SmallSetVector<SILInstruction *, 16> &InvolvedInsts) {
  SmallVector<SILInstruction *, 16> Worklist;
  Worklist.push_back(AllocRef);

  DEBUG(llvm::dbgs() << "    Analyzing Use Graph.");

  while (!Worklist.empty()) {
    SILInstruction *I = Worklist.pop_back_val();

    DEBUG(llvm::dbgs() << "        Visiting: " << *I);

    // Insert the instruction into our InvolvedInstructions set.  If we have
    // already seen it, then don't reprocess all of the uses.
    if (!InvolvedInsts.insert(I)) {
      DEBUG(llvm::dbgs() << "        Already seen skipping...\n");
      continue;
    }

    // If we can't zap this instruction... bail...
    if (!canZapInstruction(I)) {
      DEBUG(llvm::dbgs() << "        Found instruction we can't zap...\n");
      return false;
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
          return false;
        }

      // Otherwise, add normal instructions to the worklist for processing.
      Worklist.push_back(User);
    }
  }

  return true;
}

//===----------------------------------------------------------------------===//
//                            Function Processing
//===----------------------------------------------------------------------===//

static void
processFunction(SILFunction &Fn, llvm::DenseMap<SILType, bool> &Cache) {
  DEBUG(llvm::dbgs() << "***** Processing Function " << Fn.getName()
        << " *****\n");

  llvm::SmallVector<SILInstruction *, 16> DeleteList;

  // For each BB in Fn...
  for (auto &BB : Fn)

    // For each inst in BB...
    for (auto II = BB.begin(), IE = BB.end(); II != IE;) {

      // If the inst is not an alloc_ref inst, skip it...
      auto *AllocRef = dyn_cast<AllocRefInst>(II);
      if (!AllocRef) {
        ++II;
        continue;
      }

      DEBUG(llvm::dbgs() << "Visiting: " << *AllocRef);

      // Ok, we have an alloc_ref. Check the cache to see if we have already
      // computed the destructor behavior for its SILType.
      bool HasSideEffects;
      SILType Type = AllocRef->getType();
      auto CacheSearchResult = Cache.find(Type);
      if (CacheSearchResult != Cache.end())
        // Ok we found a value in the cache.
        HasSideEffects = CacheSearchResult->second;
      else
        // We did not find a value in the cache for our destructor. Analyze the
        // destructor to make sure it has no side effects. For now this only
        // supports alloc_ref of classes so any alloc_ref with a reference type
        // that is not a class this will return false for. Once we have analyzed
        // it, set Behavior to that value and insert the value into the Cache.
        Cache[Type] = HasSideEffects = doesDestructorHaveSideEffects(AllocRef);

      if (HasSideEffects) {
        DEBUG(llvm::dbgs() << "    Destructor had side effects. Can't "
              "eliminate alloc_ref\n");
        ++II;
        continue;
      }

      // Our destructor has no side effects, so if we can prove no loads or
      // escapes, then we can completely remove the use graph of this alloc_ref.
      llvm::SmallSetVector<SILInstruction *, 16> InvolvedInstructions;
      if (!analyzeUseGraph(AllocRef, InvolvedInstructions)) {
        DEBUG(llvm::dbgs() << "    Found a use that can not be zapped...\n");
        ++II;
        continue;
      }

      // Ok, we succeeded! Add all of the involved instructions to our delete
      // list.
      for (auto *I : InvolvedInstructions)
        DeleteList.push_back(I);
      DEBUG(llvm::dbgs() << "    Success! Eliminating alloc_ref.\n");

      ++NumStoreOnlyAllocRefEliminated;
      ++II;
    }

  // Erase all instructions that are in DeleteList.
  for (auto *I : DeleteList) {
    // If I has any uses left, just set them to be undef.
    if (!I->use_empty())
      for (unsigned i = 0, e = I->getNumTypes(); i != e; ++i)
        SILValue(I, i).replaceAllUsesWith(SILUndef::get(I->getType(i),
                                                        I->getModule()));
    // Now we know that I should not have any uses... erase it from its parent.
    I->eraseFromParent();
  }
}

//===----------------------------------------------------------------------===//
//                              Top Level Driver
//===----------------------------------------------------------------------===//

class AllocRefElimination : public SILFunctionTransform {
  /// The entry point to the transformation.
  void run() {
    llvm::DenseMap<SILType, bool> DestructorAnalysisCache;

    processFunction(*getFunction(), DestructorAnalysisCache);

    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
};

SILTransform *swift::createAllocRefElimination() {
  return new AllocRefElimination();
}

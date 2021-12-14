//===--- MoveFunctionCanonicalization.cpp ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-function-canonicalization"

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/Consumption.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SILOptimizer/Analysis/ClosureScope.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CanonicalOSSALifetime.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerSumType.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

static bool findInitAndDestroyForAllocation(
    AllocStackInst *asi, MarkUnresolvedMoveAddrInst *markMoveAddr,
    CopyAddrInst *&cai, DestroyAddrInst *&dai, StoreInst *&si) {
  for (auto *use : asi->getUses()) {
    auto *user = use->getUser();
    LLVM_DEBUG(llvm::dbgs() << "    Visiting User: " << *user);

    // If we find our own instruction or a dealloc stack, just skip.
    if (user == markMoveAddr || isa<DeallocStackInst>(user)) {
      LLVM_DEBUG(
          llvm::dbgs()
          << "        Found our original inst or a dealloc stack... Ok!\n");
      continue;
    }

    if (auto *destroyAddrInst = dyn_cast<DestroyAddrInst>(user)) {
      if (dai)
        return false;
      dai = destroyAddrInst;
      continue;
    }

    if (auto *newCAI = dyn_cast<CopyAddrInst>(user)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "        Found copy_addr... checking if legal...\n");
      // We require that our copy_addr be an init into our temp and in the same
      // block as markMoveAddr.
      if (newCAI->getDest() == asi && bool(newCAI->isInitializationOfDest()) &&
          !bool(newCAI->isTakeOfSrc()) &&
          newCAI->getParent() == markMoveAddr->getParent()) {
        if (cai || si)
          return false;
        cai = newCAI;
        continue;
      }
    }

    if (auto *newSI = dyn_cast<StoreInst>(user)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "        Found store... checking if legal...\n");
      // We require that our copy_addr be an init into our temp and in the same
      // block as markMoveAddr.
      if (newSI->getDest() == asi &&
          newSI->getOwnershipQualifier() == StoreOwnershipQualifier::Init &&
          newSI->getParent() == markMoveAddr->getParent()) {
        if (cai || si)
          return false;
        si = newSI;
        continue;
      }
    }

    // If we do not find an instruction that we know about, return we can't
    // optimize.
    LLVM_DEBUG(
        llvm::dbgs()
        << "        Found instruction we did not understand! Bailing!\n");
    return false;
  }

  return true;
}

static bool
tryHandlingLoadableVarMovePattern(MarkUnresolvedMoveAddrInst *markMoveAddr,
                                  StoreInst *si, AliasAnalysis *aa) {
  auto *li = dyn_cast<LoadInst>(si->getSrc());
  if (!li || li->getOwnershipQualifier() != LoadOwnershipQualifier::Copy ||
      li->getParent() != si->getParent())
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Found LI: " << *li);
  SILValue operand = stripAccessMarkers(li->getOperand());
  if (auto *originalASI = dyn_cast<AllocStackInst>(operand)) {
    if (!originalASI->isLexical() ||
        !(originalASI->isVar() || originalASI->isLet()))
      return false;
    LLVM_DEBUG(llvm::dbgs() << "Found OriginalASI: " << *originalASI);
  } else {
    auto *fArg = dyn_cast<SILFunctionArgument>(operand);
    if (!fArg || !fArg->hasConvention(SILArgumentConvention::Indirect_Inout))
      return false;
    LLVM_DEBUG(llvm::dbgs() << "Found fArg: " << *fArg);
  }

  // Make sure that there aren't any side-effect having instructions in
  // between our load/store.
  LLVM_DEBUG(llvm::dbgs() << "Checking for uses in between LI and SI.\n");
  auto range =
      llvm::make_range(std::next(li->getIterator()), si->getIterator());
  if (!llvm::none_of(range, [&](SILInstruction &iter) {
        if (!iter.mayHaveSideEffects()) {
          LLVM_DEBUG(llvm::dbgs() << "Found no side effect inst: " << iter);
          return false;
        }

        if (auto *dvi = dyn_cast<DestroyAddrInst>(&iter)) {
          if (aa->isNoAlias(dvi->getOperand(), operand)) {
            // We are going to be extending the lifetime of our
            // underlying value, not shrinking it so we can ignore
            // destroy_addr on other non-aliasing values.
            LLVM_DEBUG(llvm::dbgs() << "Found no alias destroy_addr: " << iter);
            return false;
          }
        }

        // Ignore end of scope markers with side-effects.
        if (isEndOfScopeMarker(&iter)) {
          LLVM_DEBUG(llvm::dbgs() << "Found end of scope marker: " << iter);
          return false;
        }

        LLVM_DEBUG(llvm::dbgs()
                   << "        Found side-effect inst... Bailing!: " << iter);
        return true;
      })) {
    return false;
  }

  // Ok, we know our original lexical alloc_stack is not written to in between
  // the load/store. Move the mark_move_addr onto the lexical alloc_stack.
  LLVM_DEBUG(llvm::dbgs() << "        Doing loadable var!\n");
  markMoveAddr->setSrc(operand);
  return true;
}

/// Attempts to perform several small optimizations to setup both the address
/// and object checkers. Returns true if we made a change to the IR.
static bool tryConvertSimpleMoveFromAllocStackTemporary(
    MarkUnresolvedMoveAddrInst *markMoveAddr, AliasAnalysis *aa,
    InstructionDeleter &deleter) {
  LLVM_DEBUG(llvm::dbgs() << "Trying to fix up: " << *markMoveAddr);

  // We need a non-lexical alloc_stack as our source.
  auto *asi = dyn_cast<AllocStackInst>(markMoveAddr->getSrc());
  if (!asi || asi->isLexical()) {
    LLVM_DEBUG(llvm::dbgs()
               << "    Source isnt an alloc_stack or is lexical... Bailing!\n");
    return false;
  }

  DestroyAddrInst *dai = nullptr;
  CopyAddrInst *cai = nullptr;
  StoreInst *si = nullptr;
  if (!findInitAndDestroyForAllocation(asi, markMoveAddr, cai, dai, si))
    return false;

  // If we did not find an (init | store) or destroy_addr, just bail.
  if (!(cai || si) || !dai) {
    LLVM_DEBUG(llvm::dbgs()
               << "        Did not find a single init! Bailing!\n");
    return false;
  }

  assert(bool(cai) != bool(si));

  // Otherwise, lets walk from cai/si to markMoveAddr and make sure there aren't
  // any side-effect having instructions in between them.
  //
  // NOTE: We know that cai must be before the markMoveAddr in the block since
  // otherwise we would be moving from uninitialized memory.
  SILInstruction *init = nullptr;
  if (cai)
    init = cai;
  else
    init = si;

  auto range = llvm::make_range(std::next(init->getIterator()),
                                markMoveAddr->getIterator());
  if (llvm::any_of(range, [&](SILInstruction &iter) {
        if (!iter.mayHaveSideEffects()) {
          return false;
        }

        if (auto *dvi = dyn_cast<DestroyAddrInst>(&iter)) {
          if (aa->isNoAlias(dvi->getOperand(), asi)) {
            // We are going to be extending the lifetime of our
            // underlying value, not shrinking it so we can ignore
            // destroy_addr on other non-aliasing values.
            return false;
          }
        }

        // Ignore end of scope markers with side-effects.
        if (isEndOfScopeMarker(&iter)) {
          return false;
        }

        LLVM_DEBUG(llvm::dbgs()
                   << "        Found side-effect inst... Bailing!: " << iter);
        return true;
      }))
    return false;

  // Ok, we can perform our optimization! Change move_addr's source to be the
  // original copy_addr's src and add add uses of the stack location to an
  // instruction deleter. We will eliminate them later.
  if (cai) {
    LLVM_DEBUG(llvm::dbgs() << "        Success! Performing optimization!\n");
    markMoveAddr->setSrc(cai->getSrc());
    return true;
  }

  // If we have a store [init], see if our src is a load [copy] from an
  // alloc_stack that is lexical var or an inout argument. In this case, we want
  // to move our mark_unresolved_move_addr onto that lexical var. This pattern
  // occurs due to SILGen always loading loadable values from memory when
  // retrieving an RValue. Calling _move then since _move is generic forces the
  // value to be re-materialized into an alloc_stack. In this example
  // remembering that mark_unresolved_move_addr is a copy_addr [init], we try to
  // move the MUMA onto the original lexical alloc_stack.
  if (tryHandlingLoadableVarMovePattern(markMoveAddr, si, aa))
    return true;

  // If we do not have a load [copy], transform this mark_resolved_move_addr
  // into a move_value [diagnostic] + store [init]. Predictable mem opts is
  // smart enough to handle this case and promote away loads from the
  // allocation. This runs before the value move checker runs.
  SILBuilderWithScope builder(si);
  auto *newValue = builder.createMoveValue(si->getLoc(), si->getSrc());
  newValue->setAllowsDiagnostics(true);
  si->setSrc(newValue);
  si->setDest(markMoveAddr->getDest());
  deleter.forceTrackAsDead(markMoveAddr);
  deleter.forceTrackAsDead(dai);

  LLVM_DEBUG(llvm::dbgs() << "        Success! Performing optimization!\n");
  return true;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MoveFunctionCanonicalization : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    bool madeChange = false;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");

    auto *aa = getAnalysis<AliasAnalysis>(fn);
    InstructionDeleter deleter;

    for (auto &block : *fn) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *inst = &*ii;
        ++ii;

        // See if we see a mark_unresolved_move_addr inst from a simple
        // temporary and move it onto the temporary's source. This ensures that
        // the mark_unresolved_move_addr is always on the operand regardless if
        // in the caller we materalized the address into a temporary.
        if (auto *markMoveAddr = dyn_cast<MarkUnresolvedMoveAddrInst>(inst)) {
          madeChange |= tryConvertSimpleMoveFromAllocStackTemporary(
              markMoveAddr, aa, deleter);
          continue;
        }
      }
    }

    deleter.cleanupDeadInstructions();

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveFunctionCanonicalization() {
  return new MoveFunctionCanonicalization();
}

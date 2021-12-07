//===--- MoveKillsCopyableAddressesChecker.cpp ----------------------------===//
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
///
/// NOTE: This pass is assumed to run before all memory optimizations that
/// manipulate the lifetimes of values in memory such as any of the -Onone
/// predictable memory optimizations. allocbox to stack is ok since it doesn't
/// effect the actual memory itself, just whether the memory is boxed or not.
///
/// In this file, we implement a checker that for memory objects in SIL checks
/// that after a call to _move, one can no longer use a var or a let with an
/// address only type. If you use it after that point, but before a known
/// destroy point, you get an error. Example:
///
///   var x = Class()
///   let y = _move(x)
///   _use(x) // error!
///   x = Class()
///   _use(x) // Ok, we reinitialized the memory.
///
/// Below, I describe in detail the algorithm. NOTE: The design below will be
/// updated to support inits once we actually support vars. Currently, we only
/// support lets.
///
/// # Design
///
/// ## Introduction
///
/// At its heart this checker is a dataflow checker that first understands the
/// lifetime of a specific address and then optimizes based off of that
/// information. It uses AccessUseVisitor to reliably get all users of an
/// address. Then it partitions those uses into three sets:
///
///  * A set of mark_unresolved_move_addr. A mark_unresolved_move_addr is an
///    instruction that marks an invocation by _move on a value. Since we have
///    not yet proven using dataflow that it can be a move, semantically this
///    instruction is actually a copy_addr [init] so we maintain a valid IR if
///    we have uses later that we want to error upon. This is where we always
///    begin tracking dataflow.
///
///  * A set of destroy_value operations. These are points where we stop
///    tracking dataflow.
///
///  * A set of misc uses that just require liveness, We call the last category
///    "livenessUses". These are the uses that can not exist in between the move
///    and the destroy_addr operation and if we see any, we emit an error to the
///    user.
///
/// ## Gathering information to prepare for the Dataflow
///
/// We perform several different dataflow operations:
///
/// 1. First mark_unresolved_move_addr are propagated downwards to determine if
///    they propagate downwards out of blocks. When this is done, we perform the
///    single basic block form of this diagnostic. If we emit a diagnostic while
///    doing that, we exit. Otherwise, we know that there must not be any uses
///    or consumes in the given block, so it propagates move out.
///
/// 2. Then mark_unresolved_move_addr and all liveness uses are propagated up to
///    determine if a block propagates liveness up. We always use the earliest
///    user in the block. Once we find that user, we insert it into a
///    DenseMap<SILBasicBlock *, SILInstruction *>. This is so we can emit a
///    nice diagnostic.
///
/// 3. Then we propagate destroy_addr upwards stopping if we see an init. If we
///    do not see an init, then we know that we propagated that destroy upward
///    out of the block. We then insert that into a DenseMap<SILBasicBlock *,
///    DestroyAddrInst *> we are maintaining. NOTE: We do not need to check for
///    moves here since any move in our block would have either resulted in the
///    destroy_addr being eliminated earlier by the single block form of the
///    diagnostic and us exiting early or us emitting an error diagnostic and
///    exiting early.
///
/// NOTE: The reason why we do not track init information on a per block is that
/// in our dataflow, we are treating inits as normal uses and if we see an init
/// without seeing a destroy_addr, we will error on the use and bail. Once we
/// decide to support vars, this will change.
///
/// NOTE: The reason why we do not track all consuming operations, just destroy
/// operations is that instead we are treating a consuming operation as a
/// liveness use. Since we are always going to just exit on the first error for
/// any specific move (all moves will be checked individually of course), we
/// know that we will stop processing blocks at that point.
///
/// ## Performing the Global Dataflow
///
/// Finally using this information we gathered above, for each markMoveAddrOut
/// block individually, we walk the CFG downwards starting with said block's
/// successors looking for liveness uses and destroy_addr blocks.
///
/// 1. If we visit any "liveness block", immediately emit an error diagnostic as
///    the user requested and return. We can not convert the
///    mark_unresolved_move_addr into a move safely.
///
/// 2. If we visit a DestroyAddr block instead, we mark the destroy_addr as
///    being a destroy_addr that is associated with a move. This is done a per
///    address basis.
///
/// Once we have finished visiting mark_unresolved_move_addr, if we found /any/
/// mark_unresolved_move_addr that were safe to convert to a take (and that we
/// did convert to a take), we need to then cleanup destroys.
///
/// ## Cleaning up Destroys
///
/// We do this simultaneously for all of the mark_unresolved_move_addr applied
/// to a single address. Specifically, we place into a worklist all of the
/// predecessor blocks of all destroy_addr blocks that we found while performing
/// global dataflow. Then for each block b until we run out of blocks:
///
/// 1. If b is a block associated with one of our converted
///    mark_unresolved_move_addr, continue. Along that path, we are shortening
///    the lifetime of the address as requested by the user.
///
/// 2. Then we check if b is a block that was not visited when processing the
///    new moves. In such a case, we have found the dominance frontier of one or
///    many of the moves and insert a destroy_addr at the end of the b and
///    continue.
///
/// 3. Finally, if b is not on our dominance frontier and isn't a stopping
///    point, we add its predecessors to the worklist and continue.
///
/// Once these steps have been completed, we delete all of the old destroy_addr
/// since the lifetime of the address has now been handled appropriately along
/// all paths through the program.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-move-kills-copyable-addresses-checker"

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

static llvm::cl::opt<bool>
    DisableUnhandledMoveDiagnostic("sil-disable-unknown-moveaddr-diagnostic");

//===----------------------------------------------------------------------===//
//                            Diagnostic Utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
//                                  Utility
//===----------------------------------------------------------------------===//

/// Returns false if we found a malformed
static void tryConvertSimpleMoveFromAllocStackTemporary(
    MarkUnresolvedMoveAddrInst *markMoveAddr) {
  LLVM_DEBUG(llvm::dbgs() << "Trying to fix up: " << *markMoveAddr);

  // We need a non-lexical alloc_stack as our source.
  auto *asi = dyn_cast<AllocStackInst>(markMoveAddr->getSrc());
  if (!asi || asi->isLexical()) {
    LLVM_DEBUG(llvm::dbgs()
               << "    Source isnt an alloc_stack or is lexical... Bailing!\n");
    return;
  }

  DestroyAddrInst *dai = nullptr;
  CopyAddrInst *init = nullptr;
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
        return;
      dai = destroyAddrInst;
      continue;
    }

    if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
      LLVM_DEBUG(llvm::dbgs()
                 << "        Found copy_addr... checking if legal...\n");
      // We require that our copy_addr be an init into our temp and in the same
      // block as markMoveAddr.
      if (cai->getDest() == asi && bool(cai->isInitializationOfDest()) &&
          !bool(cai->isTakeOfSrc()) &&
          cai->getParent() == markMoveAddr->getParent()) {
        if (init)
          return;
        init = cai;
        continue;
      }
    }

    // If we do not find an instruction that we know about, return we can't
    // optimize.
    LLVM_DEBUG(
        llvm::dbgs()
        << "        Found instruction we did not understand! Bailing!\n");
    return;
  }

  // If we did not find an init or destroy_addr, just bail.
  if (!init || !dai) {
    LLVM_DEBUG(llvm::dbgs()
               << "        Did not find a single init! Bailing!\n");
    return;
  }

  // Otherwise, lets walk from cai to markMoveAddr and make sure there aren't
  // any side-effect having instructions in between them.
  //
  // NOTE: We know that cai must be before the markMoveAddr in the block since
  // otherwise we would be moving from uninitialized memory.
  if (llvm::any_of(llvm::make_range(std::next(init->getIterator()),
                                    markMoveAddr->getIterator()),
                   [](SILInstruction &iter) {
                     if (!iter.mayHaveSideEffects()) {
                       return false;
                     }

                     if (isa<DestroyAddrInst>(iter)) {
                       // We are going to be extending the lifetime of our
                       // underlying value, not shrinking it so we can ignore
                       // destroy_addr on other values.
                       return false;
                     }

                     // Ignore end of scope markers with side-effects.
                     if (isEndOfScopeMarker(&iter)) {
                       return false;
                     }

                     LLVM_DEBUG(
                         llvm::dbgs()
                         << "        Found side-effect inst... Bailing!: "
                         << iter);
                     return true;
                   }))
    return;

  LLVM_DEBUG(llvm::dbgs() << "        Success! Performing optimization!\n");
  // Ok, we can perform our optimization! Change move_addr's source to be the
  // original copy_addr's src and add add uses of the stack location to an
  // instruction deleter. We will eliminate them later.
  markMoveAddr->setSrc(init->getSrc());
}

//===----------------------------------------------------------------------===//
//                               Use Gathering
//===----------------------------------------------------------------------===//

namespace {

struct UseState {
  SILValue address;
  SmallVector<MarkUnresolvedMoveAddrInst *, 8> markMoves;
  SmallPtrSet<SILInstruction *, 8> seenMarkMoves;
  SmallSetVector<SILInstruction *, 8> inits;
  SmallSetVector<SILInstruction *, 8> livenessUses;
  SmallBlotSetVector<DestroyAddrInst *, 8> destroys;
  llvm::SmallDenseMap<SILInstruction *, unsigned, 8> destroyToIndexMap;

  void insertMarkUnresolvedMoveAddr(MarkUnresolvedMoveAddrInst *inst) {
    if (!seenMarkMoves.insert(inst).second)
      return;
    markMoves.emplace_back(inst);
  }

  void insertDestroy(DestroyAddrInst *dai) {
    destroyToIndexMap[dai] = destroys.size();
    destroys.insert(dai);
  }

  void clear() {
    address = SILValue();
    markMoves.clear();
    seenMarkMoves.clear();
    inits.clear();
    livenessUses.clear();
    destroys.clear();
    destroyToIndexMap.clear();
  }
};

/// Visit all of the uses of a lexical lifetime, initializing useState as we go.
struct GatherLexicalLifetimeUseVisitor : public AccessUseVisitor {
  UseState &useState;

  GatherLexicalLifetimeUseVisitor(UseState &useState)
      : AccessUseVisitor(AccessUseType::Overlapping,
                         NestedAccessType::IgnoreAccessBegin),
        useState(useState) {}

  bool visitUse(Operand *op, AccessUseType useTy) override;
  void reset(SILValue address) { useState.address = address; }
  void clear() { useState.clear(); }
};

} // end anonymous namespace

// Filter out recognized uses that do not write to memory.
//
// TODO: Ensure that all of the conditional-write logic below is encapsulated in
// mayWriteToMemory and just call that instead. Possibly add additional
// verification that visitAccessPathUses recognizes all instructions that may
// propagate pointers (even though they don't write).
bool GatherLexicalLifetimeUseVisitor::visitUse(Operand *op,
                                               AccessUseType useTy) {
  // If this operand is for a dependent type, then it does not actually access
  // the operand's address value. It only uses the metatype defined by the
  // operation (e.g. open_existential).
  if (op->isTypeDependent()) {
    return true;
  }

  // If we have a move from src, this is a mark_move we want to visit.
  if (auto *move = dyn_cast<MarkUnresolvedMoveAddrInst>(op->getUser())) {
    if (move->getSrc() == op->get()) {
      LLVM_DEBUG(llvm::dbgs() << "Found move: " << *move);
      useState.insertMarkUnresolvedMoveAddr(move);
      return true;
    }
  }

  if (memInstMustInitialize(op)) {
    LLVM_DEBUG(llvm::dbgs() << "Found init: " << *op->getUser());
    useState.inits.insert(op->getUser());
    return true;
  }

  if (auto *dvi = dyn_cast<DestroyAddrInst>(op->getUser())) {
    // If we see a destroy_addr not on our base address, bail! Just error and
    // say that we do not understand the code.
    if (dvi->getOperand() != useState.address) {
      LLVM_DEBUG(llvm::dbgs()
                 << "!!! Error! Found destroy_addr no on base address: "
                 << *dvi);
      return false;
    }
    LLVM_DEBUG(llvm::dbgs() << "Found destroy_addr: " << *dvi);
    useState.insertDestroy(dvi);
    return true;
  }

  // Ignore dealloc_stack.
  if (isa<DeallocStackInst>(op->getUser()))
    return true;

  LLVM_DEBUG(llvm::dbgs() << "Found liveness use: " << *op->getUser());
  useState.livenessUses.insert(op->getUser());

  return true;
}

//===----------------------------------------------------------------------===//
//                                  Dataflow
//===----------------------------------------------------------------------===//

static SILInstruction *downwardScanForMoveOut(MarkUnresolvedMoveAddrInst *mvi,
                                              UseState &useState) {
  // Forward scan looking for uses or reinits.
  for (auto &next : llvm::make_range(std::next(mvi->getIterator()),
                                     mvi->getParent()->end())) {
    LLVM_DEBUG(llvm::dbgs() << "DownwardScan. Checking: " << next);

    // If we hit a non-destroy_addr, then we immediately know that we found an
    // error. Return the special result with the next stashed within it.
    if (useState.livenessUses.count(&next) ||
        useState.seenMarkMoves.count(&next) || useState.inits.count(&next)) {
      // Emit a diagnostic error and process the next mark_unresolved_move_addr.
      LLVM_DEBUG(llvm::dbgs() << "SingleBlock liveness user: " << next);
      return &next;
    }

    // If we see a destroy_addr, then stop processing since it pairs directly
    // with our move.
    auto iter = useState.destroyToIndexMap.find(&next);
    if (iter != useState.destroyToIndexMap.end()) {
      return cast<DestroyAddrInst>(iter->first);
    }
  }

  // We are move out!
  LLVM_DEBUG(llvm::dbgs() << "DownwardScan. We are move out!\n");
  return nullptr;
}

/// Scan backwards from \p inst to the beginning of its parent block looking for
/// uses. We return true if \p inst is the first use that we are tracking for
/// the given block. This means it propagates liveness upwards through the CFG.
///
/// This works only for an instruction expected to be a normal use.
static bool upwardScanForUseOut(SILInstruction *inst, UseState &useState) {
  // We scan backwards from the instruction before \p inst to the beginning of
  // the block.
  for (auto &iter : llvm::make_range(std::next(inst->getReverseIterator()),
                                     inst->getParent()->rend())) {
    // If we hit another liveness use, then this isn't the first use in the
    // block. We want to store only the first use in the block. In such a case,
    // we bail since when we visit that earlier instruction, we will do the
    // appropriate check.
    if (useState.livenessUses.contains(&iter))
      // If we are not tracking a destroy, we stop at liveness uses. If we have
      // a destroy_addr, we use the destroy blocks to ignore the liveness uses
      // since we use the destroy_addr to signal we should stop tracking when we
      // use dataflow and to pair/delete with a move.
      return false;
    if (useState.destroyToIndexMap.count(&iter))
      return false;
    if (auto *mmai = dyn_cast<MarkUnresolvedMoveAddrInst>(&iter))
      if (useState.seenMarkMoves.count(mmai))
        return false;
    if (useState.inits.contains(&iter))
      return false;
  }
  return true;
}

/// Scan backwards from \p inst to the beginning of its parent block looking for
/// uses. We return true if \p inst is the first use that we are tracking for
/// the given block. This means it propagates liveness upwards through the CFG.
static bool upwardScanForDestroys(DestroyAddrInst *inst, UseState &useState) {
  // We scan backwards from the instruction before \p inst to the beginning of
  // the block.
  for (auto &iter : llvm::make_range(std::next(inst->getReverseIterator()),
                                     inst->getParent()->rend())) {
    // If we find a destroy_addr earlier in the block, do not mark this block as
    // being associated with this destroy. We always want to associate the move
    // with the earliest destroy_addr.
    if (useState.destroyToIndexMap.count(&iter))
      return false;

    // If we see an init, then we return found other use to not track this
    // destroy_addr up since it is balanced by the init.
    if (useState.inits.contains(&iter))
      return false;

    // Otherwise, we have a normal use, just ignore it.
  }

  // Ok, this instruction is the first use in the block of our value. So return
  // true so we track it as such.
  return true;
}

//===----------------------------------------------------------------------===//
//                              Address Checker
//===----------------------------------------------------------------------===//

namespace {

struct MoveKillsCopyableAddressesObjectChecker {
  SmallSetVector<SILValue, 32> addressesToCheck;
  SILFunction *fn;
  UseState useState;
  GatherLexicalLifetimeUseVisitor visitor;
  llvm::DenseMap<SILBasicBlock *, SILInstruction *> useBlocks;
  llvm::DenseSet<SILBasicBlock *> initBlocks;
  llvm::DenseMap<SILBasicBlock *, SILInstruction *> destroyBlocks;
  SmallVector<MarkUnresolvedMoveAddrInst *, 8> markMovesToDataflow;

  MoveKillsCopyableAddressesObjectChecker(SILFunction *fn)
      : fn(fn), useState(), visitor(useState) {}
  bool performSingleBasicBlockAnalysisForAllMarkMoves(SILValue address);
  bool performGlobalDataflow(SILValue address);

  bool check();

  void emitDiagnosticForMove(SILValue borrowedValue,
                             StringRef borrowedValueName, MoveValueInst *mvi);

  ASTContext &getASTContext() const { return fn->getASTContext(); }
};

} // namespace

static SourceLoc getSourceLocFromValue(SILValue value) {
  if (auto *defInst = value->getDefiningInstruction())
    return defInst->getLoc().getSourceLoc();
  if (auto *arg = dyn_cast<SILFunctionArgument>(value))
    return arg->getDecl()->getLoc();
  llvm_unreachable("Do not know how to get source loc for value?!");
}

// Returns true if we emitted a diagnostic and handled the single block
// case. Returns false if we visited all of the uses and seeded the UseState
// struct with the information needed to perform our interprocedural dataflow.
bool MoveKillsCopyableAddressesObjectChecker::
    performSingleBasicBlockAnalysisForAllMarkMoves(SILValue address) {
  bool didEmitSingleBlockDiagnostic = false;
  for (auto *mvi : useState.markMoves) {
    // First scan downwards to make sure we are move out of this block.

    if (auto *interestingUser = downwardScanForMoveOut(mvi, useState)) {
      // If we found a destroy, then we found a single block case that we can
      // handle. Remove the destroy and convert the mark_unresolved_move_addr
      // into a true move.
      if (auto *dvi = dyn_cast<DestroyAddrInst>(interestingUser)) {
        SILBuilderWithScope builder(mvi);
        builder.createCopyAddr(mvi->getLoc(), mvi->getSrc(), mvi->getDest(),
                               IsTake, IsInitialization);
        useState.destroys.erase(dvi);
        mvi->eraseFromParent();
        dvi->eraseFromParent();
        continue;
      }

      // Then check if we found a user that violated our dataflow rules. In such
      // a case, emit an error, cleanup our mark_unresolved_move_addr, and
      // finally continue.
      didEmitSingleBlockDiagnostic = true;

      {
        auto diag =
            diag::sil_movekillscopyablevalue_value_consumed_more_than_once;
        StringRef name = getDebugVarName(address);
        diagnose(getASTContext(), getSourceLocFromValue(address), diag, name);
      }

      {
        auto diag = diag::sil_movekillscopyablevalue_move_here;
        diagnose(getASTContext(), mvi->getLoc().getSourceLoc(), diag);
      }

      {
        auto diag = diag::sil_movekillscopyablevalue_use_here;
        diagnose(getASTContext(), interestingUser->getLoc().getSourceLoc(),
                 diag);
      }

      // We purposely continue to see if at least in simple cases, we can flag
      // mistakes from other moves. Since we are setting emittedDiagnostic to
      // true, we will not perform the actual dataflow due to a check after
      // the loop.
      //
      // We also clean up mvi by converting it to a copy_addr init so we do not
      // emit fail errors later.
      //
      // TODO: Can we handle multiple errors in the same block for a single
      // move?
      SILBuilderWithScope builder(mvi);
      builder.createCopyAddr(mvi->getLoc(), mvi->getSrc(), mvi->getDest(),
                             IsNotTake, IsInitialization);
      mvi->eraseFromParent();
      continue;
    }

    // If we did not found any uses later in the block that was an interesting
    // use, we need to perform dataflow.
    LLVM_DEBUG(llvm::dbgs() << "Our move is live out, so we need to process "
                               "it with the dataflow.\n");
    markMovesToDataflow.emplace_back(mvi);

    // Now scan up to see if mvi is also a use to seed the dataflow. This could
    // happen if we have an earlier move.
    if (upwardScanForUseOut(mvi, useState)) {
      LLVM_DEBUG(llvm::dbgs() << "MVI projects a use up");
      useBlocks[mvi->getParent()] = mvi;
    }
  }

  return didEmitSingleBlockDiagnostic;
}

static bool
cleanupAllDestroyAddr(SILFunction *fn, SmallBitVector &destroyIndices,
                      UseState &useState,
                      BasicBlockSet &blocksVisitedWhenProcessingNewTakes,
                      BasicBlockSet &blocksWithMovesThatAreNowTakes) {
  bool madeChange = false;
  BasicBlockWorklist worklist(fn);
  SILValue daiOperand;
  for (int index = destroyIndices.find_first(); index != -1;
       index = destroyIndices.find_next(index)) {
    auto dai = useState.destroys[index];
    if (!dai)
      continue;
    SILValue op = (*dai)->getOperand();
    assert(daiOperand == SILValue() || op == daiOperand);
    daiOperand = op;
    for (auto *predBlock : (*dai)->getParent()->getPredecessorBlocks()) {
      worklist.pushIfNotVisited(predBlock);
    }
  }

  while (auto *next = worklist.pop()) {
    LLVM_DEBUG(llvm::dbgs()
               << "Looking at block: bb" << next->getDebugID() << "\n");
    // Any blocks that contained processed moves are stop points.
    if (blocksWithMovesThatAreNowTakes.contains(next))
      continue;

    if (!blocksVisitedWhenProcessingNewTakes.contains(next)) {
      // Insert a destroy_addr here since the block isn't reachable from any of
      // our moves.
      SILBuilderWithScope builder(next->getTerminator());
      auto *dvi = builder.createDestroyAddr(
          RegularLocation::getAutoGeneratedLocation(), daiOperand);
      useState.destroys.insert(dvi);
      continue;
    }

    // Otherwise, this block is reachable from one of our move blocks, visit
    // further predecessors.
    for (auto *predBlock : next->getPredecessorBlocks()) {
      worklist.pushIfNotVisited(predBlock);
    }
  }

  for (int index = destroyIndices.find_first(); index != -1;
       index = destroyIndices.find_next(index)) {
    auto dvi = useState.destroys[index];
    if (!dvi)
      continue;
    (*dvi)->eraseFromParent();
    madeChange = true;
  }

  return madeChange;
}

bool MoveKillsCopyableAddressesObjectChecker::performGlobalDataflow(
    SILValue address) {
  bool madeChange = false;

  SmallBitVector indicesOfPairedDestroys;
  auto getIndicesOfPairedDestroys = [&]() -> SmallBitVector & {
    if (indicesOfPairedDestroys.size() != useState.destroys.size())
      indicesOfPairedDestroys.resize(useState.destroys.size());
    return indicesOfPairedDestroys;
  };

  BasicBlockSet visitedByNewMove(fn);
  BasicBlockSet blocksWithMovesThatAreNowTakes(fn);
  bool convertedMarkMoveToTake = false;
  for (auto *mvi : markMovesToDataflow) {
    // At the end of this if we already initialized indices of paired destroys,
    // clear it.
    SWIFT_DEFER { getIndicesOfPairedDestroys().clear(); };

    bool emittedSingleDiagnostic = false;

    LLVM_DEBUG(llvm::dbgs() << "Checking Multi Block Dataflow for: " << *mvi);

    BasicBlockWorklist worklist(fn);
    BasicBlockSetVector setVector(fn);
    for (auto *succBlock : mvi->getParent()->getSuccessorBlocks()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "    SuccBlocks: " << succBlock->getDebugID() << "\n");
      worklist.pushIfNotVisited(succBlock);
      setVector.insert(succBlock);
    }

    while (auto *next = worklist.pop()) {
      LLVM_DEBUG(llvm::dbgs() << "Visiting: bb" << next->getDebugID() << "\n");

      // Before we check if we are supposed to stop processing here, check if we
      // have a use block. In such a case, emit an error.
      //
      // NOTE: We do this before since we could have a use before a destroy_addr
      // in this block. We would like to error in such a case.
      auto iter = useBlocks.find(next);
      if (iter != useBlocks.end()) {
        LLVM_DEBUG(llvm::dbgs() << "    Is Use Block! Emitting Error!\n");
        // We found one! Emit the diagnostic and continue and see if we can get
        // more diagnostics.
        auto &astContext = getASTContext();
        {
          auto diag =
              diag::sil_movekillscopyablevalue_value_consumed_more_than_once;
          StringRef name = getDebugVarName(address);
          diagnose(astContext, getSourceLocFromValue(address), diag, name);
        }

        {
          auto diag = diag::sil_movekillscopyablevalue_move_here;
          diagnose(astContext, mvi->getLoc().getSourceLoc(), diag);
        }

        {
          auto diag = diag::sil_movekillscopyablevalue_use_here;
          diagnose(astContext, iter->second->getLoc().getSourceLoc(), diag);
        }

        emittedSingleDiagnostic = true;
        break;
      }

      // Then see if this is a destroy block. If so, do not add successors and
      // continue. This is because we stop processing at destroy_addr. This
      // destroy_addr is paired with the mark_unresolved_move_addr.
      {
        auto iter = destroyBlocks.find(next);
        if (iter != destroyBlocks.end()) {
          LLVM_DEBUG(llvm::dbgs() << "    Is Destroy Block! Setting up for "
                                     "later deletion if possible!\n");
          auto indexIter = useState.destroyToIndexMap.find(iter->second);
          assert(indexIter != useState.destroyToIndexMap.end());
          getIndicesOfPairedDestroys().set(indexIter->second);
          continue;
        }
      }

      LLVM_DEBUG(
          llvm::dbgs()
          << "    No match! Pushing unvisited successors onto the worklist!\n");
      // Otherwise, add successors if we haven't visited them to the worklist.
      for (auto *succBlock : next->getSuccessorBlocks()) {
        worklist.pushIfNotVisited(succBlock);
        setVector.insert(succBlock);
      }
    }

    // At this point, we know that if we emitted a diagnostic, we need to
    // convert the move to a copy_addr [init] since we found a use that violates
    // the move. We just want to emit correct IR without the
    // mark_unresolved_move_addr within it.
    blocksWithMovesThatAreNowTakes.insert(mvi->getParent());
    SILBuilderWithScope builder(mvi);
    if (emittedSingleDiagnostic) {
      builder.createCopyAddr(mvi->getLoc(), mvi->getSrc(), mvi->getDest(),
                             IsNotTake, IsInitialization);
    } else {
      builder.createCopyAddr(mvi->getLoc(), mvi->getSrc(), mvi->getDest(),
                             IsTake, IsInitialization);
      // Flush our SetVector into the visitedByNewMove.
      for (auto *block : setVector) {
        visitedByNewMove.insert(block);
      }
      convertedMarkMoveToTake = true;
    }
    mvi->eraseFromParent();
    madeChange = true;
  }

  if (!convertedMarkMoveToTake)
    return madeChange;

  // Now that we have processed all of our mark_moves, eliminate all of the
  // destroy_addr.
  madeChange |=
      cleanupAllDestroyAddr(fn, getIndicesOfPairedDestroys(), useState,
                            visitedByNewMove, blocksWithMovesThatAreNowTakes);

  return madeChange;
}

bool MoveKillsCopyableAddressesObjectChecker::check() {
  if (addressesToCheck.empty())
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Visiting Function: " << fn->getName() << "\n");
  auto addressToProcess =
      llvm::makeArrayRef(addressesToCheck.begin(), addressesToCheck.end());

  bool madeChange = false;

  while (!addressToProcess.empty()) {
    auto address = addressToProcess.front();
    addressToProcess = addressToProcess.drop_front(1);
    LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *address);

    auto accessPathWithBase = AccessPathWithBase::compute(address);
    auto accessPath = accessPathWithBase.accessPath;

    // Bail on an invalid AccessPath.
    //
    // AccessPath completeness is verified independently--it may be invalid in
    // extraordinary situations. When AccessPath is valid, we know all its uses
    // are recognizable.
    //
    // NOTE: If due to an invalid access path we fail here, we will just error
    // on the _move since the _move would not have been handled.
    if (!accessPath.isValid())
      continue;

    SWIFT_DEFER { visitor.clear(); };
    visitor.reset(address);
    if (!visitAccessPathUses(visitor, accessPath, fn))
      continue;

    // Now initialize our data structures.
    SWIFT_DEFER {
      useBlocks.clear();
      initBlocks.clear();
      destroyBlocks.clear();
      markMovesToDataflow.clear();
    };

    // Perform the single basic block analysis emitting a diagnostic/pairing
    // mark_unresolved_move_addr and destroys if needed. If we discover a
    // mark_move that propagates its state out of the current block, this
    // routine also prepares the pass for running the multi-basic block
    // diagnostic.
    if (performSingleBasicBlockAnalysisForAllMarkMoves(address)) {
      LLVM_DEBUG(llvm::dbgs() << "Performed single block analysis!\n");
      madeChange = true;
      continue;
    }

    // Go through all init uses and if we don't see any other of our uses, then
    // mark this as an "init block".
    for (auto *init : useState.inits) {
      if (upwardScanForUseOut(init, useState)) {
        LLVM_DEBUG(llvm::dbgs() << "    Found init block at: " << *init);
        useBlocks[init->getParent()] = init;
      }
    }

    // Then go through all normal uses and do upwardScanForUseOut.
    for (auto *user : useState.livenessUses) {
      if (upwardScanForUseOut(user, useState)) {
        LLVM_DEBUG(llvm::dbgs() << "    Found liveness block at: " << *user);
        useBlocks[user->getParent()] = user;
      }
    }

    for (auto destroyOpt : useState.destroys) {
      // Any destroys we eliminated when processing single basic blocks will be
      // nullptr. Skip them!
      if (!destroyOpt)
        continue;

      auto *destroy = *destroyOpt;

      LLVM_DEBUG(llvm::dbgs() << "    Found destroy block at: " << *destroy);
      auto iter = useState.destroyToIndexMap.find(destroy);
      assert(iter != useState.destroyToIndexMap.end());

      if (upwardScanForDestroys(destroy, useState)) {
        LLVM_DEBUG(llvm::dbgs() << "    Found destroy block at: " << *destroy);
        destroyBlocks[destroy->getParent()] = destroy;
      }
    }

    // Ok, we are setup. Perform the global dataflow!
    madeChange |= performGlobalDataflow(address);
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MoveKillsCopyableAddressesCheckerPass : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();
    auto &astContext = fn->getASTContext();

    // If we do not have experimental move only enabled, do not emit
    // diagnostics.
    if (!astContext.LangOpts.EnableExperimentalMoveOnly)
      return;

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    bool madeChange = false;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");

    MoveKillsCopyableAddressesObjectChecker checker(getFunction());

    for (auto *arg : fn->front().getSILFunctionArguments()) {
      if (arg->getType().isAddress() &&
          (arg->hasConvention(SILArgumentConvention::Indirect_In) ||
           arg->hasConvention(SILArgumentConvention::Indirect_In)))
        checker.addressesToCheck.insert(arg);
    }

    for (auto &block : *fn) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *inst = &*ii;
        ++ii;

        if (auto *asi = dyn_cast<AllocStackInst>(inst)) {
          // Only check lexical alloc_stack that were not emitted as vars.
          if (asi->isLexical() && !asi->isVar()) {
            LLVM_DEBUG(llvm::dbgs() << "Found lexical alloc_stack: " << *asi);
            checker.addressesToCheck.insert(asi);
            continue;
          }
        }

        // See if we see a mark_unresolved_move_addr inst from a simple
        // temporary and move it onto the temporary's source. This ensures that
        // the mark_unresolved_move_addr is always on the operand regardless if
        // in the caller we materalized the address into a temporary.
        if (auto *markMoveAddr = dyn_cast<MarkUnresolvedMoveAddrInst>(inst)) {
          tryConvertSimpleMoveFromAllocStackTemporary(markMoveAddr);
          continue;
        }
      }
    }

    madeChange |= checker.check();

    if (madeChange) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }

    // Now search through our function one last time and any move_value
    // [allows_diagnostics] that remain are ones that we did not know how to
    // check so emit a diagnostic so the user doesn't assume that they have
    // guarantees. This gives us the guarantee that any moves written by the
    // user must have been properly resolved and thus maintain that all move
    // uses have been resolved appropriately.
    //
    // TODO: Emit specific diagnostics here (e.x.: _move of global).
    if (DisableUnhandledMoveDiagnostic)
      return;
    for (auto &block : *fn) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *inst = &*ii;
        ++ii;

        if (auto *mai = dyn_cast<MarkUnresolvedMoveAddrInst>(inst)) {
          auto diag =
              diag::sil_movekillscopyablevalue_move_applied_to_unsupported_move;
          diagnose(astContext, mai->getLoc().getSourceLoc(), diag);

          // Now that we have emitted the error, replace the move_addr with a
          // copy_addr so that future passes never see it. We mark it as a
          // copy_addr [init].
          SILBuilderWithScope builder(mai);
          builder.createCopyAddr(mai->getLoc(), mai->getSrc(), mai->getDest(),
                                 IsNotTake, IsInitialization);
          mai->eraseFromParent();
        }
      }
    }
  }
};

} // anonymous namespace

SILTransform *swift::createMoveKillsCopyableAddressesChecker() {
  return new MoveKillsCopyableAddressesCheckerPass();
}

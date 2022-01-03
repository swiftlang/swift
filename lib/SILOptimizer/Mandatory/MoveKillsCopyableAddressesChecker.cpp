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
//                                 Utilities
//===----------------------------------------------------------------------===//

template <typename... T, typename... U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

namespace llvm {
llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const SmallBitVector &bv) {
  for (unsigned index : range(bv.size())) {
    os << (bv[index] ? 1 : 0);
  }
  return os;
}
} // namespace llvm

static SourceLoc getSourceLocFromValue(SILValue value) {
  if (auto *defInst = value->getDefiningInstruction())
    return defInst->getLoc().getSourceLoc();
  if (auto *arg = dyn_cast<SILFunctionArgument>(value))
    return arg->getDecl()->getLoc();
  llvm_unreachable("Do not know how to get source loc for value?!");
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
  SmallBlotSetVector<SILInstruction *, 8> reinits;
  llvm::SmallDenseMap<SILInstruction *, unsigned, 8> reinitToIndexMap;

  void insertMarkUnresolvedMoveAddr(MarkUnresolvedMoveAddrInst *inst) {
    if (!seenMarkMoves.insert(inst).second)
      return;
    markMoves.emplace_back(inst);
  }

  void insertDestroy(DestroyAddrInst *dai) {
    destroyToIndexMap[dai] = destroys.size();
    destroys.insert(dai);
  }

  void insertReinit(SILInstruction *inst) {
    reinitToIndexMap[inst] = reinits.size();
    reinits.insert(inst);
  }

  void clear() {
    address = SILValue();
    markMoves.clear();
    seenMarkMoves.clear();
    inits.clear();
    livenessUses.clear();
    destroys.clear();
    destroyToIndexMap.clear();
    reinits.clear();
    reinitToIndexMap.clear();
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

static void convertMemoryReinitToInitForm(SILInstruction *memInst) {
  switch (memInst->getKind()) {
  default:
    llvm_unreachable("unsupported?!");

  case SILInstructionKind::CopyAddrInst: {
    auto *cai = cast<CopyAddrInst>(memInst);
    cai->setIsInitializationOfDest(IsInitialization_t::IsInitialization);
    return;
  }
  case SILInstructionKind::StoreInst: {
    auto *si = cast<StoreInst>(memInst);
    si->setOwnershipQualifier(StoreOwnershipQualifier::Init);
    return;
  }
  }
}

static bool memInstMustReinitialize(Operand *memOper) {
  SILValue address = memOper->get();
  auto *memInst = memOper->getUser();
  switch (memInst->getKind()) {
  default:
    return false;

  case SILInstructionKind::CopyAddrInst: {
    auto *CAI = cast<CopyAddrInst>(memInst);
    return CAI->getDest() == address && !CAI->isInitializationOfDest();
  }
  case SILInstructionKind::StoreInst: {
    auto *si = cast<StoreInst>(memInst);
    return si->getDest() == address &&
           si->getOwnershipQualifier() == StoreOwnershipQualifier::Assign;
  }
  }
}

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

  if (memInstMustReinitialize(op)) {
    LLVM_DEBUG(llvm::dbgs() << "Found reinit: " << *op->getUser());
    useState.insertReinit(op->getUser());
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

namespace {

enum class DownwardScanResult {
  Invalid,
  Destroy,
  Reinit,
  UseForDiagnostic,
  MoveOut
};

}

/// Returns true if we are move out, false otherwise. If we find an interesting
/// inst, we return it in foundInst. If no inst is returned, one must continue.
static DownwardScanResult
downwardScanForMoveOut(MarkUnresolvedMoveAddrInst *mvi, UseState &useState,
                       SILInstruction **foundInst) {
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
      *foundInst = &next;
      return DownwardScanResult::UseForDiagnostic;
    }

    {
      auto iter = useState.reinitToIndexMap.find(&next);
      if (iter != useState.reinitToIndexMap.end()) {
        LLVM_DEBUG(llvm::dbgs() << "DownwardScan: reinit: " << next);
        *foundInst = &next;
        return DownwardScanResult::Reinit;
      }
    }

    // If we see a destroy_addr, then stop processing since it pairs directly
    // with our move.
    {
      auto iter = useState.destroyToIndexMap.find(&next);
      if (iter != useState.destroyToIndexMap.end()) {
        auto *dai = cast<DestroyAddrInst>(iter->first);
        LLVM_DEBUG(llvm::dbgs() << "DownwardScan: Destroy: " << *dai);
        *foundInst = dai;
        return DownwardScanResult::Destroy;
      }
    }
  }

  // We are move out!
  LLVM_DEBUG(llvm::dbgs() << "DownwardScan. We are move out!\n");
  return DownwardScanResult::MoveOut;
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
    if (useState.reinitToIndexMap.count(&iter))
      return false;
  }
  return true;
}

/// Scan backwards from \p inst to the beginning of its parent block looking for
/// uses. We return true if \p inst is the first use that we are tracking for
/// the given block. This means it propagates liveness upwards through the CFG.
static bool upwardScanForDestroys(SILInstruction *inst, UseState &useState) {
  // We scan backwards from the instruction before \p inst to the beginning of
  // the block.
  for (auto &iter : llvm::make_range(std::next(inst->getReverseIterator()),
                                     inst->getParent()->rend())) {
    // If we find a destroy_addr earlier in the block, do not mark this block as
    // being associated with this destroy. We always want to associate the move
    // with the earliest destroy_addr.
    if (useState.destroyToIndexMap.count(&iter))
      return false;
    if (useState.reinitToIndexMap.count(&iter))
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

/// Search for the first init in the block.
static bool upwardScanForInit(SILInstruction *inst, UseState &useState) {
  // We scan backwards from the instruction before \p inst to the beginning of
  // the block.
  for (auto &iter : llvm::make_range(std::next(inst->getReverseIterator()),
                                     inst->getParent()->rend())) {
    if (useState.inits.contains(&iter))
      return false;
  }
  return true;
}

//===----------------------------------------------------------------------===//
//                              Global Dataflow
//===----------------------------------------------------------------------===//

namespace {

struct DataflowState {
  llvm::DenseMap<SILBasicBlock *, SILInstruction *> useBlocks;
  llvm::DenseSet<SILBasicBlock *> initBlocks;
  llvm::DenseMap<SILBasicBlock *, SILInstruction *> destroyBlocks;
  llvm::DenseMap<SILBasicBlock *, SILInstruction *> reinitBlocks;
  SmallVector<MarkUnresolvedMoveAddrInst *, 8> markMovesToDataflow;
  UseState &useState;

  DataflowState(UseState &useState) : useState(useState) {}
  void init();
  bool process(SILValue address);
  void clear() {
    useBlocks.clear();
    initBlocks.clear();
    destroyBlocks.clear();
    reinitBlocks.clear();
    markMovesToDataflow.clear();
  }
};

} // namespace

static bool
cleanupAllDestroyAddr(SILFunction *fn, SmallBitVector &destroyIndices,
                      SmallBitVector &reinitIndices, UseState &useState,
                      BasicBlockSet &blocksVisitedWhenProcessingNewTakes,
                      BasicBlockSet &blocksWithMovesThatAreNowTakes) {
  bool madeChange = false;
  BasicBlockWorklist worklist(fn);
  SILValue daiOperand;
  LLVM_DEBUG(llvm::dbgs() << "Cleanup up destroy addr!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Visiting destroys!\n");
  LLVM_DEBUG(llvm::dbgs() << "    Destroy Indices: " << destroyIndices << "\n");
  for (int index = destroyIndices.find_first(); index != -1;
       index = destroyIndices.find_next(index)) {
    LLVM_DEBUG(llvm::dbgs() << "    Index: " << index << "\n");
    auto dai = useState.destroys[index];
    if (!dai)
      continue;
    LLVM_DEBUG(llvm::dbgs() << "    Destroy: " << *dai);
    SILValue op = (*dai)->getOperand();
    assert(daiOperand == SILValue() || op == daiOperand);
    daiOperand = op;
    for (auto *predBlock : (*dai)->getParent()->getPredecessorBlocks()) {
      worklist.pushIfNotVisited(predBlock);
    }
  }
  LLVM_DEBUG(llvm::dbgs() << "    Visiting reinit!\n");
  for (int index = reinitIndices.find_first(); index != -1;
       index = reinitIndices.find_next(index)) {
    auto reinit = useState.reinits[index];
    if (!reinit)
      continue;
    LLVM_DEBUG(llvm::dbgs() << "  Reinit: " << *reinit);
    for (auto *predBlock : (*reinit)->getParent()->getPredecessorBlocks()) {
      worklist.pushIfNotVisited(predBlock);
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "Processing worklist!\n");
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

  for (int index = reinitIndices.find_first(); index != -1;
       index = reinitIndices.find_next(index)) {
    auto reinit = useState.reinits[index];
    if (!reinit)
      continue;
    convertMemoryReinitToInitForm(*reinit);
    madeChange = true;
  }

  return madeChange;
}

bool DataflowState::process(SILValue address) {
  SILFunction *fn = address->getFunction();
  assert(fn);

  bool madeChange = false;
  SmallBitVector indicesOfPairedDestroys;
  auto getIndicesOfPairedDestroys = [&]() -> SmallBitVector & {
    if (indicesOfPairedDestroys.size() != useState.destroys.size())
      indicesOfPairedDestroys.resize(useState.destroys.size());
    return indicesOfPairedDestroys;
  };
  SmallBitVector indicesOfPairedReinits;
  auto getIndicesOfPairedReinits = [&]() -> SmallBitVector & {
    if (indicesOfPairedReinits.size() != useState.reinits.size())
      indicesOfPairedReinits.resize(useState.reinits.size());
    return indicesOfPairedReinits;
  };

  BasicBlockSet visitedByNewMove(fn);
  BasicBlockSet blocksWithMovesThatAreNowTakes(fn);
  bool convertedMarkMoveToTake = false;
  for (auto *mvi : markMovesToDataflow) {
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
        auto &astContext = fn->getASTContext();
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

      {
        auto iter = reinitBlocks.find(next);
        if (iter != reinitBlocks.end()) {
          LLVM_DEBUG(llvm::dbgs() << "    Is reinit Block! Setting up for "
                                     "later deletion if possible!\n");
          auto indexIter = useState.reinitToIndexMap.find(iter->second);
          assert(indexIter != useState.reinitToIndexMap.end());
          getIndicesOfPairedReinits().set(indexIter->second);
          continue;
        }
      }

      // Then see if this is an init block. If so, do not add successors and
      // continue. We already checked that we are not destroy up in this block
      // by the check a few lines up. So we know that we are in one of the
      // following situations:
      //
      // 1. We are the only use in the block. In this case, we must have
      //    consumed the value with a non-destroy_addr earlier (e.x.: apply). In
      //    such a case, we need to just stop processing since we are re-initing
      //    memory for a var.
      //
      // 2. There is a consuming use that is treated as a liveness use before
      //    us. In that case, we will have already errored upon it.
      if (initBlocks.count(next)) {
        LLVM_DEBUG(llvm::dbgs() << "    Is Init Block!\n");
        continue;
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
  madeChange |= cleanupAllDestroyAddr(
      fn, getIndicesOfPairedDestroys(), getIndicesOfPairedReinits(), useState,
      visitedByNewMove, blocksWithMovesThatAreNowTakes);

  return madeChange;
}

void DataflowState::init() {
  // Go through all init uses and if we don't see any other of our uses, then
  // mark this as an "init block".
  for (auto *init : useState.inits) {
    if (upwardScanForInit(init, useState)) {
      LLVM_DEBUG(llvm::dbgs() << "    Found use block at: " << *init);
      initBlocks.insert(init->getParent());
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

    auto iter = useState.destroyToIndexMap.find(destroy);
    assert(iter != useState.destroyToIndexMap.end());

    if (upwardScanForDestroys(destroy, useState)) {
      LLVM_DEBUG(llvm::dbgs() << "    Found destroy block at: " << *destroy);
      destroyBlocks[destroy->getParent()] = destroy;
    }
  }

  for (auto reinitOpt : useState.reinits) {
    // Any destroys we eliminated when processing single basic blocks will be
    // nullptr. Skip them!
    if (!reinitOpt)
      continue;

    auto *reinit = *reinitOpt;
    auto iter = useState.reinitToIndexMap.find(reinit);
    assert(iter != useState.reinitToIndexMap.end());

    if (upwardScanForDestroys(reinit, useState)) {
      LLVM_DEBUG(llvm::dbgs() << "    Found reinit block at: " << *reinit);
      reinitBlocks[reinit->getParent()] = reinit;
    }
  }
}

// Returns true if we emitted a diagnostic and handled the single block
// case. Returns false if we visited all of the uses and seeded the UseState
// struct with the information needed to perform our interprocedural dataflow.
static bool performSingleBasicBlockAnalysis(DataflowState &dataflowState,
                                            SILValue address,
                                            MarkUnresolvedMoveAddrInst *mvi) {
  // First scan downwards to make sure we are move out of this block.
  auto &useState = dataflowState.useState;
  SILInstruction *interestingUser = nullptr;
  switch (downwardScanForMoveOut(mvi, useState, &interestingUser)) {
  case DownwardScanResult::Invalid:
    llvm_unreachable("invalid");
  case DownwardScanResult::Destroy: {
    // If we found a destroy, then we found a single block case that we can
    // handle. Remove the destroy and convert the mark_unresolved_move_addr
    // into a true move.
    auto *dvi = cast<DestroyAddrInst>(interestingUser);
    SILBuilderWithScope builder(mvi);
    builder.createCopyAddr(mvi->getLoc(), mvi->getSrc(), mvi->getDest(), IsTake,
                           IsInitialization);
    useState.destroys.erase(dvi);
    mvi->eraseFromParent();
    dvi->eraseFromParent();
    return false;
  }
  case DownwardScanResult::UseForDiagnostic: {
    // Then check if we found a user that violated our dataflow rules. In such
    // a case, emit an error, cleanup our mark_unresolved_move_addr, and
    // finally continue.
    auto &astCtx = mvi->getFunction()->getASTContext();
    {
      auto diag =
          diag::sil_movekillscopyablevalue_value_consumed_more_than_once;
      StringRef name = getDebugVarName(address);
      diagnose(astCtx, getSourceLocFromValue(address), diag, name);
    }

    {
      auto diag = diag::sil_movekillscopyablevalue_move_here;
      diagnose(astCtx, mvi->getLoc().getSourceLoc(), diag);
    }

    {
      auto diag = diag::sil_movekillscopyablevalue_use_here;
      diagnose(astCtx, interestingUser->getLoc().getSourceLoc(), diag);
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
    return true;
  }
  case DownwardScanResult::Reinit: {
    convertMemoryReinitToInitForm(interestingUser);
    useState.reinits.erase(interestingUser);
    SILBuilderWithScope builder(mvi);
    builder.createCopyAddr(mvi->getLoc(), mvi->getSrc(), mvi->getDest(), IsTake,
                           IsInitialization);
    mvi->eraseFromParent();
    return false;
  }
  case DownwardScanResult::MoveOut:
    break;
  }

  // If we did not found any uses later in the block that was an interesting
  // use, we need to perform dataflow.
  LLVM_DEBUG(llvm::dbgs() << "Our move is live out, so we need to process "
                             "it with the dataflow.\n");
  dataflowState.markMovesToDataflow.emplace_back(mvi);

  // Now scan up to see if mvi is also a use to seed the dataflow. This could
  // happen if we have an earlier move.
  if (upwardScanForUseOut(mvi, dataflowState.useState)) {
    LLVM_DEBUG(llvm::dbgs() << "MVI projects a use up");
    dataflowState.useBlocks[mvi->getParent()] = mvi;
  }
  return false;
}

//===----------------------------------------------------------------------===//
//                              Address Checker
//===----------------------------------------------------------------------===//

namespace {

struct MoveKillsCopyableAddressesObjectChecker {
  SILFunction *fn;
  UseState useState;
  DataflowState dataflowState;

  MoveKillsCopyableAddressesObjectChecker(SILFunction *fn)
      : fn(fn), useState(), dataflowState(useState) {}

  bool check(SILValue address);

  void emitDiagnosticForMove(SILValue borrowedValue,
                             StringRef borrowedValueName, MoveValueInst *mvi);

  ASTContext &getASTContext() const { return fn->getASTContext(); }
};

} // namespace

bool MoveKillsCopyableAddressesObjectChecker::check(SILValue address) {
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
    return false;

  GatherLexicalLifetimeUseVisitor visitor(useState);
  SWIFT_DEFER { visitor.clear(); };
  visitor.reset(address);
  if (!visitAccessPathUses(visitor, accessPath, fn))
    return false;

  // See if our base address is an inout. If we found any moves, add as a
  // liveness use all function terminators.
  if (auto *fArg = dyn_cast<SILFunctionArgument>(address)) {
    if (fArg->hasConvention(SILArgumentConvention::Indirect_Inout)) {
      if (visitor.useState.markMoves.size()) {
        SmallVector<SILBasicBlock *, 2> exitingBlocks;
        fn->findExitingBlocks(exitingBlocks);
        for (auto *block : exitingBlocks) {
          visitor.useState.livenessUses.insert(block->getTerminator());
        }
      }
    }
  }

  // Now initialize our data structures.
  SWIFT_DEFER { dataflowState.clear(); };

  // Perform the single basic block analysis emitting a diagnostic/pairing
  // mark_unresolved_move_addr and destroys if needed. If we discover a
  // mark_move that propagates its state out of the current block, this
  // routine also prepares the pass for running the multi-basic block
  // diagnostic.
  bool emittedSingleBBDiagnostic = false;
  for (auto *mvi : useState.markMoves) {
    emittedSingleBBDiagnostic |=
        performSingleBasicBlockAnalysis(dataflowState, address, mvi);
  }
  if (emittedSingleBBDiagnostic) {
    LLVM_DEBUG(llvm::dbgs() << "Performed single block analysis!\n");
    return true;
  }

  // Ok, we need to perform global dataflow. Initialize our dataflow state
  // engine and then run the dataflow itself.
  dataflowState.init();
  return dataflowState.process(address);
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

class MoveKillsCopyableAddressesCheckerPass : public SILFunctionTransform {
  void run() override {
    auto *fn = getFunction();
    auto &astContext = fn->getASTContext();

    // Don't rerun diagnostics on deserialized functions.
    if (getFunction()->wasDeserializedCanonical())
      return;

    assert(fn->getModule().getStage() == SILStage::Raw &&
           "Should only run on Raw SIL");

    SmallSetVector<SILValue, 32> addressesToCheck;

    for (auto *arg : fn->front().getSILFunctionArguments()) {
      if (arg->getType().isAddress() &&
          (arg->hasConvention(SILArgumentConvention::Indirect_In) ||
           arg->hasConvention(SILArgumentConvention::Indirect_Inout)))
        addressesToCheck.insert(arg);
    }

    for (auto &block : *fn) {
      for (auto ii = block.begin(), ie = block.end(); ii != ie;) {
        auto *inst = &*ii;
        ++ii;

        if (auto *asi = dyn_cast<AllocStackInst>(inst)) {
          // Only check lexical alloc_stack that were not emitted as vars.
          if (asi->isLexical()) {
            LLVM_DEBUG(llvm::dbgs() << "Found lexical alloc_stack: " << *asi);
            addressesToCheck.insert(asi);
            continue;
          }
        }
      }
    }

    LLVM_DEBUG(llvm::dbgs() << "Visiting Function: " << fn->getName() << "\n");
    auto addressToProcess =
        llvm::makeArrayRef(addressesToCheck.begin(), addressesToCheck.end());

    MoveKillsCopyableAddressesObjectChecker checker(getFunction());
    bool madeChange = false;

    while (!addressToProcess.empty()) {
      auto address = addressToProcess.front();
      addressToProcess = addressToProcess.drop_front(1);
      LLVM_DEBUG(llvm::dbgs() << "Visiting: " << *address);
      madeChange |= checker.check(address);
    }

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

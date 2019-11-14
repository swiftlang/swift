//===--- DestroyHoisting.cpp - Hoisting of address destroys ---------------===//
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
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-destroy-hoisting"
#include "swift/SIL/MemoryLifetime.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

//===----------------------------------------------------------------------===//
//                               DestroyHoisting
//===----------------------------------------------------------------------===//

/// Hoist destroy_addr instructions up the control flow.
///
/// This has two benefits:
///
/// * Combine a destroy_addr with a preceeding copy. For example, replace
///   /code
///     %v = load [copy] %a
///     ...
///     destroy_addr %a
///   /endcode
///   with
///   /code
///     %v = load [take] %a
///     ...
///   /endcode

/// * Shorten the lifetime of memory-values. This can avoid copy-on-write
///   operations. Specifically, optimize the enum payload in-place modification
///   pattern:
///   /code
///     // inside a mutating enum member function
///     switch self {
///       case .A(var cow_container)
///         cow_container.modify()
///         self = .A(cow_container)
///       ...
///     }
///   /endcode
///   DestroyHoisting moves the destroy-part of the self assignment up so that
///   the cow_container is only single-referenced and will not trigger a copy-
///   on-write operation when calling modify().
///
/// Note that this optimization does _not_ try to split a destroy_addr of a
/// struct/tuple into destroys of its sub-locations. Also, vice versa, it does
/// not try to combine multiple destroy_addr of sub-locations into a single
/// destroy_addr of the aggregate.
class DestroyHoisting {

  using Bits = MemoryLocations::Bits;
  using BlockState = MemoryDataflow::BlockState;

  SILFunction *function;
  MemoryLocations locations;

  /// A cache of generated address projection instructions. The key is the
  /// location index.
  llvm::DenseMap<unsigned, SingleValueInstruction *> addressProjections;

  DominanceAnalysis *DA;
  DominanceInfo *domTree = nullptr;
  bool madeChanges = false;

  void expandStores(MemoryDataflow &dataFlow);

  void initDataflow(MemoryDataflow &dataFlow);

  void initDataflowInBlock(BlockState &state);

  bool canIgnoreUnreachableBlock(SILBasicBlock *block,
                                 MemoryDataflow &dataFlow);

  int getDestroyedLoc(SILInstruction *I) {
    if (auto *DAI = dyn_cast<DestroyAddrInst>(I))
        return locations.getLocationIdx(DAI->getOperand());
    return -1;
  }

  void getUsedLocationsOfAddr(Bits &bits, SILValue addr);

  void getUsedLocationsOfOperands(Bits &bits, SILInstruction *I);

  void getUsedLocationsOfInst(Bits &bits, SILInstruction *Inst);

  void moveDestroys(MemoryDataflow &dataFlow);

  void moveDestroysInBlock(SILBasicBlock *block, Bits &activeDestroys,
                           SmallVectorImpl<SILInstruction *> &toRemove);

  void insertDestroys(Bits &toInsert, Bits &aliveDestroys,
                      SmallVectorImpl<SILInstruction *> &removeList,
                      SILInstruction *afterInst, SILBasicBlock *inBlock);

  void insertDestroy(int locIdx, Bits &insertBits, SILInstruction *atInst);

  bool combineWith(SILInstruction *inst, unsigned locIdx);

  SILValue createAddress(unsigned locIdx, SILBuilder &builder);

  void tailMerging(MemoryDataflow &dataFlow);

  bool tailMergingInBlock(SILBasicBlock *block, MemoryDataflow &dataFlow);

public:
  DestroyHoisting(SILFunction *function, DominanceAnalysis *DA) :
    function(function), DA(DA) { }

  bool hoistDestroys();
};

static bool isExpansionEnabler(SILInstruction *I) {
  return isa<SwitchEnumInst>(I);
}

// A pre-pass which expands
//    store %s to [assign] %a
// to
//    destroy_addr %a
//    store %s to [init] %a
//
// This is not a good thing in general. If we would do it unconditionally, it
// would result in some benchmark regressions. Therefore we only expand stores
// where we expect that we can move the destroys to or across a switch_enum.
// This enables the switch-enum optimization (see above).
// TODO: investigate the benchmark regressions and enable store-expansion more
//       aggressively.
void DestroyHoisting::expandStores(MemoryDataflow &dataFlow) {
  Bits usedLocs(locations.getNumLocations());

  // Initialize the dataflow, which tells us which destroy_addr instructions are
  // reachable through a switch_enum, without a use of the location in between.
  // The gen-sets are initialized at all expansion-enabler instructions
  // (currently only switch_enum instructions).
  // The kill-sets are initialized with the used locations, because we would not
  // move a destroy across a use.
  bool expansionEnablerFound = false;
  for (BlockState &state : dataFlow) {
    state.entrySet.reset();
    state.genSet.reset();
    state.killSet.reset();
    state.exitSet.reset();
    for (SILInstruction &I : *state.block) {
      if (isExpansionEnabler(&I)) {
        expansionEnablerFound = true;
        state.genSet.set();
        state.killSet.reset();
      }
      usedLocs.reset();
      getUsedLocationsOfInst(usedLocs, &I);
      state.genSet.reset(usedLocs);
      state.killSet |= usedLocs;
    }
  }
  if (!expansionEnablerFound)
    return;

  // Solve the dataflow, which tells us if a destroy_addr is reachable from
  // a expansion-enabler on _any_ path (therefore "Union" and not "Intersect")
  // without hitting a use of that location.
  dataFlow.solveForwardWithUnion();

  Bits activeLocs(locations.getNumLocations());
  for (BlockState &st : dataFlow) {
    activeLocs = st.entrySet;
    for (SILInstruction &I : *st.block) {
      if (isExpansionEnabler(&I)) {
        // Set all bits: an expansion-enabler enables expansion for all
        // locations.
        activeLocs.set();
      }
      if (auto *SI = dyn_cast<StoreInst>(&I)) {
        if (SI->getOwnershipQualifier() == StoreOwnershipQualifier::Assign) {
          int locIdx = locations.getLocationIdx(SI->getDest());
          if (locIdx >= 0 && activeLocs.test(locIdx)) {
            // Expand the store.
            SILBuilder builder(SI);
            builder.createDestroyAddr(SI->getLoc(), SI->getDest());
            SI->setOwnershipQualifier(StoreOwnershipQualifier::Init);
            madeChanges = true;
          }
        }
      }
      // Clear the bits of used locations.
      usedLocs.reset();
      getUsedLocationsOfInst(usedLocs, &I);
      activeLocs.reset(usedLocs);
    }
  }
}

// Initialize the dataflow for moving destroys up the control flow.
void DestroyHoisting::initDataflow(MemoryDataflow &dataFlow) {
  for (BlockState &st : dataFlow) {
    st.entrySet.set();
    st.genSet.reset();
    st.killSet.reset();
    if (isa<UnreachableInst>(st.block->getTerminator())) {
      if (canIgnoreUnreachableBlock(st.block, dataFlow)) {
        st.exitSet.set();
      } else {
        st.exitSet.reset();
      }
    } else if (st.block->getTerminator()->isFunctionExiting()) {
      st.exitSet.reset();
    } else {
      st.exitSet.set();
    }
    initDataflowInBlock(st);
  }
}

// Initialize the gen- and kill-sets.
// It's a backward dataflow problem. A bit <n> in the genSet means: we have seen
// a destroy_addr of location <n> (when walking backwards).
// As we are not trying to split or combine destroy_addr instructions, a
// destroy_addr just sets its "self" bit and not the location's subLocations
// set (this is different compared to the detaflow in MemoryLifetimeVerifier).
void DestroyHoisting::initDataflowInBlock(BlockState &state) {
  Bits usedLocs(state.entrySet.size());

  for (SILInstruction &I : llvm::reverse(*state.block)) {
    usedLocs.reset();
    getUsedLocationsOfInst(usedLocs, &I);
    state.genSet.reset(usedLocs);
    state.killSet |= usedLocs;

    // For terminators clear the bits in the exit set instead in the genSet,
    // because we cannot insert destroy_addrs after the terminator in the block.
    if (isa<TermInst>(&I))
      state.exitSet.reset(usedLocs);

    int destroyedLoc = getDestroyedLoc(&I);
    if (destroyedLoc >= 0) {
      state.killSet.reset(destroyedLoc);
      state.genSet.set(destroyedLoc);
    }
  }
}

// Handling blocks which eventually end up in an unreachable is surprisingly
// complicated, because without special treatment it would block destroy-
// hoisting in the main control flow. Example:
//
//   bb1:
//     cond_br %error_condition, bb2, bb3
//   bb2:
//     // some unrelated error processing, which doesn't use any locations
//     unreachable
//   bb3:
//     // continue with main control flow
//     destroy_addr %some_location
//
// We have to initialize the exit-set of bb2 with zeroes. Otherwise we would
// insert wrong destroys in case there are any location-uses in the unreachable-
// block (or sub-graph).
// But having a zero-set at the bb2-entry would block hoisting of the
// destroy_addr from bb3 into bb1.
// Therefore we special case the common scenario of a single block with
// unreachable which does not touch any of our memory locations. We can just
// ignore those blocks.
bool DestroyHoisting::canIgnoreUnreachableBlock(SILBasicBlock *block,
                                                MemoryDataflow &dataFlow) {
  assert(isa<UnreachableInst>(block->getTerminator()));

  // Is it a single unreachable-block (i.e. it has a single predecessor from
  // which there is a path to the function exit)?
  SILBasicBlock *singlePred = block->getSinglePredecessorBlock();
  if (!singlePred)
    return false;
  if (!dataFlow.getState(singlePred)->exitReachable)
    return false;

  // Check if none of the locations are touched in the unreachable-block.
  for (SILInstruction &I : *block) {
    if (isa<EndBorrowInst>(&I))
      return false;
    for (Operand &op : I.getAllOperands()) {
      if (locations.getLocation(op.get()))
        return false;
    }
  }
  return true;
}

void DestroyHoisting::getUsedLocationsOfAddr(Bits &bits, SILValue addr) {
  if (auto *loc = locations.getLocation(addr)) {
    // destroy_addr locations are "killed" by parent _and_ sublocations. In
    // other words: a destroy_addr must not be moved across an instruction which
    // uses the location itself, an aggregate containing the location, or a
    // sub-field of the location.
    bits |= loc->selfAndParents;
    bits |= loc->subLocations;
  }
}

void DestroyHoisting::getUsedLocationsOfOperands(Bits &bits, SILInstruction *I) {
  for (Operand &op : I->getAllOperands()) {
    getUsedLocationsOfAddr(bits, op.get());
  }
}

// Set all bits of locations which instruction \p I is using. It's including
// parent and sub-locations (see comment in getUsedLocationsOfAddr).
void DestroyHoisting::getUsedLocationsOfInst(Bits &bits, SILInstruction *I) {
  switch (I->getKind()) {
    case SILInstructionKind::EndBorrowInst:
      if (auto *LBI = dyn_cast<LoadBorrowInst>(
                                        cast<EndBorrowInst>(I)->getOperand())) {
        getUsedLocationsOfAddr(bits, LBI->getOperand());
      }
      break;
    case SILInstructionKind::EndApplyInst:
      // Operands passed to begin_apply are alive throughout an end_apply ...
      getUsedLocationsOfOperands(bits, cast<EndApplyInst>(I)->getBeginApply());
      break;
    case SILInstructionKind::AbortApplyInst:
      // ... or abort_apply.
      getUsedLocationsOfOperands(bits, cast<AbortApplyInst>(I)->getBeginApply());
      break;
    case SILInstructionKind::LoadInst:
    case SILInstructionKind::StoreInst:
    case SILInstructionKind::CopyAddrInst:
    case SILInstructionKind::ApplyInst:
    case SILInstructionKind::TryApplyInst:
    case SILInstructionKind::YieldInst:
      getUsedLocationsOfOperands(bits, I);
      break;
    case SILInstructionKind::DebugValueAddrInst:
    case SILInstructionKind::DestroyAddrInst:
      // destroy_addr and debug_value_addr are handled specially.
      break;
    default:
      break;
  }
}

static void processRemoveList(SmallVectorImpl<SILInstruction *> &toRemove) {
  for (SILInstruction *I : toRemove) {
    I->eraseFromParent();
  }
  toRemove.clear();
}

// Do the actual moving of destroy_addr instructions.
void DestroyHoisting::moveDestroys(MemoryDataflow &dataFlow) {
  // Don't eagerly delete destroy_addr instructions, instead put them into this
  // list. When we are about to "move" a destroy_addr just over some sideeffect-
  // free instructions, we'll keep it at the current location.
  llvm::SmallVector<SILInstruction *, 8> toRemove;

  Bits activeDestroys(locations.getNumLocations());

  for (BlockState &state : dataFlow) {
    SILBasicBlock *block = state.block;

    // Is it an unreachable-block we can ignore?
    if (isa<UnreachableInst>(block->getTerminator()) && state.exitSet.any())
      continue;

    // Do the inner-block processing.
    activeDestroys = state.exitSet;
    moveDestroysInBlock(block, activeDestroys, toRemove);
    assert(activeDestroys == state.entrySet);

    if (block->pred_empty()) {
      // The function entry block: insert all destroys which are still active.
      insertDestroys(activeDestroys, activeDestroys, toRemove,
                     nullptr, block);
    } else if (SILBasicBlock *pred = block->getSinglePredecessorBlock()) {
      // Insert destroys which are active at the entry of this block, but not
      // on another successor block of the predecessor.
      Bits usedLocs = activeDestroys;
      usedLocs.reset(dataFlow.getState(pred)->exitSet);
      insertDestroys(usedLocs, activeDestroys,
                     toRemove, nullptr, block);
    }
    // Note that this condition relies on not having critical edges in the CFG.
    assert(std::all_of(block->getPredecessorBlocks().begin(),
                       block->getPredecessorBlocks().end(),
                       [&](SILBasicBlock *P) {
                         return activeDestroys == dataFlow.getState(P)->exitSet;
                       }));

    // Delete all destroy_addr and debug_value_addr which are scheduled for
    // removal.
    processRemoveList(toRemove);
  }
}

void DestroyHoisting::moveDestroysInBlock(
                                SILBasicBlock *block, Bits &activeDestroys,
                                SmallVectorImpl<SILInstruction *> &toRemove) {
  Bits usedLocs(locations.getNumLocations());
  assert(toRemove.empty());

  for (SILInstruction &I : llvm::reverse(*block)) {
    usedLocs.reset();
    getUsedLocationsOfInst(usedLocs, &I);
    usedLocs &= activeDestroys;
    insertDestroys(usedLocs, activeDestroys, toRemove, &I, block);

    int destroyedLoc = getDestroyedLoc(&I);
    if (destroyedLoc >= 0) {
      activeDestroys.set(destroyedLoc);
      toRemove.push_back(&I);
    } else if (auto *DVA = dyn_cast<DebugValueAddrInst>(&I)) {
      // debug_value_addr does not count as real use of a location. If we are
      // moving a destroy_addr above a debug_value_addr, just delete that
      // debug_value_addr.
      if (auto *dvaLoc = locations.getLocation(DVA->getOperand())) {
        if (activeDestroys.anyCommon(dvaLoc->subLocations) ||
            activeDestroys.anyCommon(dvaLoc->selfAndParents))
          toRemove.push_back(DVA);
      }
    } else if (I.mayHaveSideEffects()) {
      // Delete all destroy_addr and debug_value_addr which are scheduled for
      // removal.
      processRemoveList(toRemove);
    }
  }
}

// Insert destroy_addr which are in \p toInsert. Also update \p activeDestroys.
// But exclude instruction from \p removeList (see below).
// If \p afterInst is null, insert the destroys at the begin of \p inBlock.
void DestroyHoisting::insertDestroys(Bits &toInsert, Bits &activeDestroys,
                          SmallVectorImpl<SILInstruction *> &removeList,
                          SILInstruction *afterInst, SILBasicBlock *inBlock) {
  if (toInsert.none())
    return;

  // The removeList contains destroy_addr (and debug_value_addr) instructions
  // which we want to delete, but we didn't see any side-effect instructions
  // since then. There is no value in moving a destroy_addr over side-effect-
  // free instructions (it could even trigger creating redundant address
  // projections).
  // Therefore we remove all such destroy_addr from removeList, i.e. we will
  // keep them at their original locations and will not move them.
  Bits keepDestroyedLocs(toInsert.size());
  auto end = std::remove_if(removeList.begin(), removeList.end(),
    [&](SILInstruction *I){
      int destroyedLoc = getDestroyedLoc(I);
      if (destroyedLoc >= 0 && toInsert.test(destroyedLoc)) {
        keepDestroyedLocs.set(destroyedLoc);
        toInsert.reset(destroyedLoc);
        activeDestroys.reset(destroyedLoc);
        return true;
      }
      if (auto *DVA = dyn_cast<DebugValueAddrInst>(I)) {
        // Also keep debug_value_addr instructions, located before a
        // destroy_addr which we won't move.
        auto *dvaLoc = locations.getLocation(DVA->getOperand());
        if (dvaLoc && dvaLoc->selfAndParents.anyCommon(keepDestroyedLocs))
          return true;
      }
      return false;
    });
  removeList.erase(end, removeList.end());

  if (toInsert.none())
    return;

  SILInstruction *insertionPoint =
    (afterInst ? &*std::next(afterInst->getIterator()) : &*inBlock->begin());
  SILBuilder builder(insertionPoint);
  SILLocation loc = RegularLocation(insertionPoint->getLoc().getSourceLoc());

  // Insert destroy_addr instructions for all bits in toInsert.
  for (int locIdx = toInsert.find_first(); locIdx >= 0;
       locIdx = toInsert.find_next(locIdx)) {
    if (!combineWith(afterInst, locIdx)) {
      SILValue addr = createAddress(locIdx, builder);
      builder.createDestroyAddr(loc, addr);
    }
    activeDestroys.reset(locIdx);
  }
  madeChanges = true;
}

// Instead of inserting a destroy_addr, try to combine it with \p inst, in case
// \p inst is a load [copy] or a non taking copy_addr.
// We can just convert the load/copy_addr to a taking load/copy_addr.
bool DestroyHoisting::combineWith(SILInstruction *inst, unsigned locIdx) {
  if (!inst)
    return false;
  switch (inst->getKind()) {
    case SILInstructionKind::LoadInst: {
      auto *LI = cast<LoadInst>(inst);
      int srcIdx = locations.getLocationIdx(LI->getOperand());
      if (srcIdx == (int)locIdx) {
        assert(LI->getOwnershipQualifier() == LoadOwnershipQualifier::Copy);
        LI->setOwnershipQualifier(LoadOwnershipQualifier::Take);
        return true;
      }
      break;
    }
    case SILInstructionKind::CopyAddrInst: {
      auto *CAI = cast<CopyAddrInst>(inst);
      int srcIdx = locations.getLocationIdx(CAI->getSrc());
      if (srcIdx == (int)locIdx) {
        assert(!CAI->isTakeOfSrc());
        CAI->setIsTakeOfSrc(IsTake);
        return true;
      }
      break;
    }
    default:
      break;
  }
  return false;
}

// Create an address projection for the sub-location \p locIdx, in case the
// representativeValue is a begin_access or does not dominate the insertion
// point of \p builder.
SILValue DestroyHoisting::createAddress(unsigned locIdx, SILBuilder &builder) {
  auto *loc = locations.getLocation(locIdx);
  if (loc->parentIdx < 0)
    return loc->representativeValue;

  assert(!isa<BeginAccessInst>(loc->representativeValue) &&
         "only a root location can be a begin_access");

  SingleValueInstruction *&cachedProj = addressProjections[locIdx];
  if (cachedProj)
    return cachedProj;

  if (!domTree)
    domTree = DA->get(function);

  auto *projInst = cast<SingleValueInstruction>(loc->representativeValue);
  if (domTree->properlyDominates(projInst, &*builder.getInsertionPoint())) {
    cachedProj = projInst;
    return projInst;
  }

  // Recursively create (or get) the address of the parent location.
  SILValue baseAddr = createAddress(loc->parentIdx, builder);

  // Insert the new projection instruction right below the parent address.
  // This ensures that it dominates the builder's insertion point.
  SILBuilder projBuilder(baseAddr->getParentBlock()->begin());
  if (auto *addrInst = dyn_cast<SingleValueInstruction>(baseAddr))
    projBuilder.setInsertionPoint(std::next(addrInst->getIterator()));
  SingleValueInstruction *newProj;

  if (auto *SEA = dyn_cast<StructElementAddrInst>(projInst)) {
    newProj = projBuilder.createStructElementAddr(SEA->getLoc(), baseAddr,
                                             SEA->getField(), SEA->getType());
  } else {
    auto *TEA = dyn_cast<TupleElementAddrInst>(projInst);
    newProj = projBuilder.createTupleElementAddr(TEA->getLoc(), baseAddr,
                                            TEA->getFieldNo(), TEA->getType());
  }
  assert(domTree->properlyDominates(newProj, &*builder.getInsertionPoint()) &&
         "new projection does not dominate insert point");
  // We need to remember the new projection instruction because in tailMerging
  // we might call locations.getLocationIdx() on such a new instruction.
  locations.registerProjection(newProj, locIdx);
  cachedProj = newProj;
  return newProj;
}

// Perform a simple tail merging optimization: at control flow merges, move
// identical destroy_addr instructions down to the successor block.
// The hoisting optimization can create such situations, e.g.
// \code
//   bb1:
//     // use of location %a
//     br bb3
//   bb2:
//     br bb3
//   bb3:
//     destroy_addr %a  // will be hoisted (duplicated) into bb2 and bb2
// \endcode
// This is mainly a code size reduction optimization.
void DestroyHoisting::tailMerging(MemoryDataflow &dataFlow) {

  // TODO: we could do a worklist algorithm here instead of iterating through
  // all the function blocks.
  bool changed = false;
  do {
    changed = false;
    for (SILBasicBlock &block : *function) {
      changed |= tailMergingInBlock(&block, dataFlow);
    }
  } while (changed);
}

bool DestroyHoisting::tailMergingInBlock(SILBasicBlock *block,
                                         MemoryDataflow &dataFlow) {
  if (block->pred_empty() || block->getSinglePredecessorBlock())
    return false;

  BlockState *state = dataFlow.getState(block);

  // Only if the entry set of the block has some bit sets, it's even possible
  // that hoisting has moved destroy_addr up to the predecessor blocks.
  if (state->entrySet.empty())
    return false;

  Bits canHoist = state->entrySet;
  Bits destroysInPred(canHoist.size());
  Bits killedLocs(canHoist.size());

  // Collect all common destroy_addr of the predecessor blocks.
  for (SILBasicBlock *pred : block->getPredecessorBlocks()) {
    destroysInPred.reset();
    killedLocs.reset();
    for (SILInstruction &I : llvm::reverse(*pred)) {
      int destroyedLoc = getDestroyedLoc(&I);
      if (destroyedLoc >= 0 && !killedLocs.test(destroyedLoc))
        destroysInPred.set(destroyedLoc);
      getUsedLocationsOfInst(killedLocs, &I);
      if (!canHoist.test(killedLocs))
        break;
    }
    canHoist &= destroysInPred;
  }
  if (canHoist.none())
    return false;

  // Create the common destroy_addr at the block entry.
  SILBuilder builder(&*block->begin());
  SILLocation loc = RegularLocation(block->begin()->getLoc().getSourceLoc());
  for (int locIdx = canHoist.find_first(); locIdx >= 0;
       locIdx = canHoist.find_next(locIdx)) {
    SILValue addr = createAddress(locIdx, builder);
    builder.createDestroyAddr(loc, addr);
  }

  // Remove the common destroy_addr from the predecessor blocks.
  for (SILBasicBlock *pred : block->getPredecessorBlocks()) {
    destroysInPred = canHoist;
    SILInstruction *I = pred->getTerminator();
    while (destroysInPred.any()) {
      assert(I && "not all destroys fround in predecessor block");
      SILInstruction *nextI = (I == &pred->front() ?
                                 nullptr : &*std::prev(I->getIterator()));
      int destroyedLoc = getDestroyedLoc(I);
      if (destroyedLoc >= 0 && destroysInPred.test(destroyedLoc)) {
        destroysInPred.reset(destroyedLoc);
        I->eraseFromParent();
      }
      I = nextI;
    }
  }
  return true;
}

bool DestroyHoisting::hoistDestroys() {
  locations.analyzeLocations(function);
  if (locations.getNumLocations() > 0) {
    MemoryDataflow dataFlow(function, locations.getNumLocations());
    dataFlow.exitReachableAnalysis();

    // Step 1: pre-processing: expand store instructions
    expandStores(dataFlow);

    // Step 2: hoist destroy_addr instructions.
    // (reuse dataFlow to avoid re-allocating all the data structures)
    initDataflow(dataFlow);
    dataFlow.solveBackwardWithIntersect();
    moveDestroys(dataFlow);

    // Step 3: post-processing: tail merge inserted destroy_addr instructions.
    tailMerging(dataFlow);
    addressProjections.clear();
  }

  // Finally: handle alloc_stack locations within blocks (no need for store-
  // expansion and tail-merging for such locations).
  locations.handleSingleBlockLocations([this](SILBasicBlock *block) {
    llvm::SmallVector<SILInstruction *, 8> toRemove;
    Bits activeDestroys(locations.getNumLocations());
    moveDestroysInBlock(block, activeDestroys, toRemove);
    addressProjections.clear();
    assert(activeDestroys.none());
    assert(toRemove.empty());
  });

  return madeChanges;
}

//===----------------------------------------------------------------------===//
//                          DestroyHoistingPass
//===----------------------------------------------------------------------===//

class DestroyHoistingPass : public SILFunctionTransform {
public:
  DestroyHoistingPass() {}

  /// The entry point to the transformation.
  void run() override {
    SILFunction *F = getFunction();
    if (!F->shouldOptimize())
      return;

    if (!F->hasOwnership())
      return;

    // If we are not supposed to perform ossa optimizations, bail.
    if (!F->getModule().getOptions().EnableOSSAOptimizations)
      return;

    LLVM_DEBUG(llvm::dbgs() << "*** DestroyHoisting on function: "
                            << F->getName() << " ***\n");

    bool EdgeChanged = splitAllCriticalEdges(*F, nullptr, nullptr);

    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();

    DestroyHoisting CM(F, DA);
    bool InstChanged = CM.hoistDestroys();

    if (EdgeChanged) {
      // We split critical edges.
      invalidateAnalysis(SILAnalysis::InvalidationKind::FunctionBody);
      return;
    }
    if (InstChanged) {
      // We moved instructions.
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createDestroyHoisting() {
  return new DestroyHoistingPass();
}

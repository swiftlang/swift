//===---- DeadStoreElimination.cpp - SIL Dead Store Elimination -----===//
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
///
/// \file
///
/// This pass eliminates dead stores across basic blocks.
///
/// A store is dead if after the store has occurred:
///
/// 1. The store to pointer is not used along any path to program exit.
/// 2. The store to pointer is overwritten by another store before any
/// potential use of the pointer.
///
/// Dead store elimination (DSE) eliminates such stores by:
///
/// 1. Introducing a notion of a LSLocation that is used to model objects
/// fields. (See below for more details).
///
/// 2. Performing a post-order walk over the control flow graph, tracking any
/// LSLocations that are read from or stored into in each basic block. After
/// eliminating any dead stores in single blocks, it computes a kill set for
/// each block. The kill set tracks what LSLocations are stored into by this
/// basic block and its successors.
///
/// 3. An optimistic iterative dataflow is performed on the gen sets and kill
/// sets until convergence.
///
/// At the core of DSE, there is the LSLocation class. a LSLocation is an
/// abstraction of an object field in program. It consists of a base and a
/// projection path to the field accessed.
///
/// When a load or store instruction is encountered, the memory is broken down
/// to the indivisible components, i.e aggregates are broken down to their
/// individual fields using the expand function. This gives the flexibility to
/// find exactly which part of the store is alive and which part is dead.
///
/// After the live parts of the store are determined, they are merged into the
/// minimum number of stores possible using the reduce function. This is done
/// so that we do not bloat SIL IR.
///
/// Another way to implement the DSE optimization is to keep the instructions
/// that read and/or write memory without breaking the memory read/written
/// using the ProjectionPath. However, this can easily lead to loss of
/// opportunities, e.g. a read that only kills part of a store may need to be
/// treated as killing the entire store.  However, using ProjectionPath does
/// lead to more memory uses.
///
/// TODO: Handle same value store in DSE, currently handled in RLE.
///
/// e.g.
/// %0 = load %A
/// ... nothing happens in middle and the %A contains the value of %0.
/// store %0 to %A  <---- no need to do this store.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-dead-store-elim"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILValueProjection.h"
#include "swift/SILOptimizer/Analysis/AliasAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/Analysis/ValueTracking.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include <algorithm>

using namespace swift;
using swift::LSLocation;

static llvm::cl::opt<bool> DisableLocalStoreDSE("disable-local-store-dse",
                                                llvm::cl::init(true));

STATISTIC(NumDeadStores, "Number of dead stores removed");
STATISTIC(NumPartialDeadStores, "Number of partial dead stores removed");

/// ComputeMaxStoreSet - If we ignore all reads, what is the max store set that
/// can reach the beginning of a basic block. This helps generating the genset
/// and killset. i.e. if there is no upward visible store that can reach the
/// beginning of a basic block, then we know that the genset and killset for
/// the stored location need not be set.
///
/// BuildGenKillSet - Build the genset and killset of the basic block.
///
/// PerformDSE - Perform the actual dead store elimination.
enum class DSEKind : unsigned {
  ComputeMaxStoreSet = 0,
  BuildGenKillSet = 1,
  PerformDSE = 2,
};

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

static inline bool isComputeMaxStoreSet(DSEKind Kind) {
  return Kind == DSEKind::ComputeMaxStoreSet;
}

static inline bool isBuildingGenKillSet(DSEKind Kind) {
  return Kind == DSEKind::BuildGenKillSet;
}

static inline bool isPerformingDSE(DSEKind Kind) {
  return Kind == DSEKind::PerformDSE;
}

/// Returns true if this is an instruction that may have side effects in a
/// general sense but are inert from a load store perspective.
static bool isDeadStoreInertInstruction(SILInstruction *Inst) {
  switch (Inst->getKind()) {
  case ValueKind::StrongRetainInst:
  case ValueKind::StrongRetainUnownedInst:
  case ValueKind::UnownedRetainInst:
  case ValueKind::RetainValueInst:
  case ValueKind::DeallocStackInst:
  case ValueKind::CondFailInst:
  case ValueKind::IsUniqueInst:
  case ValueKind::IsUniqueOrPinnedInst:
    return true;
  default:
    return false;
  }
}

//===----------------------------------------------------------------------===//
//                       Basic Block Location State
//===----------------------------------------------------------------------===//

namespace {

/// If a large store is broken down to too many smaller stores, bail out.
/// Currently, we only do partial dead store if we can form a single contiguous
/// non-dead store.
constexpr unsigned MaxPartialDeadStoreCountLimit = 1;

/// BlockState summarizes how LSLocations are used in a basic block.
///
/// Initially the BBWriteSetOut is empty. Before a basic block is processed, it
/// is
/// initialized to the intersection of BBWriteSetIns of all successors of the
/// basic block.
///
/// Initially BBWriteSetIn is set to true. After the basic block is processed,
/// if
/// its BBWriteSetOut is different from BBWriteSetIn, BBWriteSetIn is
/// initialized to
/// the value of BBWriteSetOut and the data flow is rerun.
///
/// Instructions in each basic block are processed in post-order as follows:
///
/// 1. When a store instruction is encountered, the stored location is tracked.
///
/// 2. When a load instruction is encountered, remove the loaded location and
///    any location it may alias with from the BBWriteSetOut.
///
/// 3. When an instruction reads from  memory in an unknown way, the
/// BBWriteSetOut
///    bit is cleared if the instruction can read the corresponding LSLocation.
///
class BlockState {
public:
  /// The basic block this BlockState represents.
  SILBasicBlock *BB;

  /// Keep the number of LSLocations in the LocationVault.
  unsigned LocationNum;

  /// A bit vector for which the ith bit represents the ith LSLocation in
  /// LocationVault. If the bit is set, then the location currently has an
  /// upward visible store.
  llvm::BitVector BBWriteSetOut;

  /// A bit vector for which the ith bit represents the ith LSLocation in
  /// LocationVault. If a bit in the vector is set, then the location has an
  /// upward visible store at the beginning of the basic block.
  llvm::BitVector BBWriteSetIn;

  /// A bit vector for which the ith bit represents the ith LSLocation in
  /// LocationVault. If the bit is set, then the current basic block
  /// generates an upward visible store.
  llvm::BitVector BBGenSet;

  /// A bit vector for which the ith bit represents the ith LSLocation in
  /// LocationVault. If the bit is set, then the current basic block
  /// kills an upward visible store.
  llvm::BitVector BBKillSet;

  /// A bit vector to keep the maximum number of stores that can reach the
  /// beginning of the basic block. If a bit is set, that means there is
  /// potentially a upward visible store to the location at the beginning
  /// of the basic block.
  llvm::BitVector BBMaxStoreSet;

  /// The dead stores in the current basic block.
  llvm::DenseSet<SILInstruction *> DeadStores;

  /// Keeps track of what stores to generate after the data flow stabilizes.
  /// these stores come from partial dead stores.
  llvm::DenseMap<SILValue, SILValue> LiveStores;

  /// Constructors.
  BlockState() : BB(nullptr) {}
  BlockState(SILBasicBlock *B) : BB(B) {}

  /// Return the current basic block.
  SILBasicBlock *getBB() const { return BB; }

  void init(unsigned lcnt) {
    LocationNum = lcnt;
    // The initial state of BBWriteSetIn should be all 1's. Otherwise the
    // dataflow solution could be too conservative.
    //
    // consider this case, the dead store by var a = 10 before the loop will not
    // be eliminated if the BBWriteSetIn is set to 0 initially.
    //
    //   var a = 10
    //   for _ in 0...1024 {}
    //   a = 10
    //
    // However, by doing so, we can only eliminate the dead stores after the
    // data flow stabilizes.
    //
    BBWriteSetIn.resize(LocationNum, true);
    BBWriteSetOut.resize(LocationNum, false);

    // GenSet and KillSet initially empty.
    BBGenSet.resize(LocationNum, false);
    BBKillSet.resize(LocationNum, false);

    // MaxStoreSet is optimistically set to true initially.
    BBMaxStoreSet.resize(LocationNum, true);
  }

  /// Check whether the BBWriteSetIn has changed. If it does, we need to rerun
  /// the data flow on this block's predecessors to reach fixed point.
  bool updateBBWriteSetIn();

  /// Functions to manipulate the write set.
  void startTrackingLocation(llvm::BitVector &BV, unsigned bit);
  void stopTrackingLocation(llvm::BitVector &BV, unsigned bit);
  bool isTrackingLSLocation(llvm::BitVector &BV, unsigned bit);
};

} // end anonymous namespace

bool BlockState::updateBBWriteSetIn() {
  bool Changed = (BBWriteSetIn != BBWriteSetOut);
  if (!Changed)
    return Changed;
  BBWriteSetIn = BBWriteSetOut;
  return Changed;
}

void BlockState::startTrackingLocation(llvm::BitVector &BV, unsigned i) {
  BV.set(i);
}

void BlockState::stopTrackingLocation(llvm::BitVector &BV, unsigned i) {
  BV.reset(i);
}

bool BlockState::isTrackingLSLocation(llvm::BitVector &BV, unsigned i) {
  return BV.test(i);
}

//===----------------------------------------------------------------------===//
//                          Top Level Implementation
//===----------------------------------------------------------------------===//

namespace {

class DSEContext {
  /// The module we are currently processing.
  SILModule *Mod;

  /// The function we are currently processing.
  SILFunction *F;

  /// Pass manager, used to get various analysis.
  SILPassManager *PM;

  /// Alias Analysis.
  AliasAnalysis *AA;

  /// Type Expansion Analysis.
  TypeExpansionAnalysis *TE;

  /// Allocator.
  llvm::BumpPtrAllocator BPA;

  /// Map every basic block to its location state.
  llvm::DenseMap<SILBasicBlock *, BlockState *> BBToLocState;

  /// Keeps the actual BlockStates.
  std::vector<BlockState> BlockStates;

  /// Keeps all the locations for the current function. The BitVector in each
  /// BlockState is then laid on top of it to keep track of which LSLocation
  /// has an upward visible store.
  std::vector<LSLocation> LocationVault;

  /// Contains a map between location to their index in the LocationVault.
  LSLocationIndexMap LocToBitIndex;

  /// Return the BlockState for the basic block this basic block belongs to.
  BlockState *getBlockState(SILBasicBlock *B) { return BBToLocState[B]; }

  /// Return the BlockState for the basic block this instruction belongs to.
  BlockState *getBlockState(SILInstruction *I) {
    return getBlockState(I->getParent());
  }

  /// LSLocation written has been extracted, expanded and mapped to the bit
  /// position in the bitvector. update the max store set using the bit
  /// position.
  void updateMaxStoreSetForWrite(BlockState *S, unsigned bit);

  /// LSLocation read has been extracted, expanded and mapped to the bit
  /// position in the bitvector. update the gen kill set using the bit
  /// position.
  void updateGenKillSetForRead(BlockState *S, unsigned bit);

  /// LSLocation written has been extracted, expanded and mapped to the bit
  /// position in the bitvector. update the gen kill set using the bit
  /// position.
  void updateGenKillSetForWrite(BlockState *S, unsigned bit);

  /// LSLocation read has been extracted, expanded and mapped to the bit
  /// position in the bitvector. process it using the bit position.
  void updateWriteSetForRead(BlockState *S, unsigned Bit);

  /// LSLocation written has been extracted, expanded and mapped to the bit
  /// position in the bitvector. process it using the bit position.
  bool updateWriteSetForWrite(BlockState *S, unsigned Bit);

  /// There is a read to a location, expand the location into individual fields
  /// before processing them.
  void processRead(SILInstruction *Inst, BlockState *State, SILValue Mem,
                   DSEKind Kind);

  /// There is a write to a location, expand the location into individual fields
  /// before processing them.
  void processWrite(SILInstruction *Inst, BlockState *State, SILValue Val,
                    SILValue Mem, DSEKind Kind);

  /// Process Instructions. Extract LSLocations from SIL LoadInst.
  void processLoadInst(SILInstruction *Inst, DSEKind Kind);

  /// Process Instructions. Extract LSLocations from SIL StoreInst.
  void processStoreInst(SILInstruction *Inst, DSEKind Kind);

  /// Process Instructions. Extract LSLocations from SIL DebugValueAddrInst.
  /// DebugValueAddrInst maybe promoted to DebugValue, when this is done,
  /// DebugValueAddrInst is effectively a read on the LSLocation.
  void processDebugValueAddrInst(SILInstruction *I, DSEKind Kind);

  /// Process Instructions. Extract LSLocations from unknown memory inst.
  void processUnknownReadInst(SILInstruction *Inst, DSEKind Kind);

  /// Check whether the instruction invalidate any LSLocations due to change in
  /// its LSLocation Base.
  ///
  /// This is to handle a case like this.
  ///
  ///  class Foo { var a : Int = 12 }
  ///  for _ in 0 ...x {
  ///     x = Foo();
  ///     x.a = 13
  ///  }
  ///  x.a = 12
  ///
  /// In this case, DSE cannot remove the x.a = 13 inside the loop.
  ///
  /// To do this, when the algorithm reaches the beginning of the basic block in
  /// the loop it will need to invalidate the LSLocation in the BBWriteSetOut.
  /// i.e.
  /// the base of the LSLocation is changed.
  ///
  /// If not, on the second iteration, the intersection of the successors of
  /// the loop basic block will have store to x.a and therefore x.a = 13 can now
  /// be considered dead.
  void invalidateLSLocationBase(SILInstruction *Inst, DSEKind Kind);

  /// Get the bit representing the location in the LocationVault.
  ///
  /// NOTE: Adds the location to the location vault if necessary.
  unsigned getLocationBit(const LSLocation &L);

public:
  /// Constructor.
  DSEContext(SILFunction *F, SILModule *M, SILPassManager *PM,
             AliasAnalysis *AA, TypeExpansionAnalysis *TE)
      : Mod(M), F(F), PM(PM), AA(AA), TE(TE) {}

  /// Entry point for dead store elimination.
  bool run();

  /// Compute the kill set for the basic block. return true if the store set
  /// changes.
  bool processBasicBlockForDSE(SILBasicBlock *BB);

  /// Compute the genset and killset for the current basic block.
  void processBasicBlockForGenKillSet(SILBasicBlock *BB);

  /// Compute the max store set for the current basic block.
  void processBasicBlockForMaxStoreSet(SILBasicBlock *BB);

  /// Compute the BBWriteSetOut and BBWriteSetIn for the current basic
  /// block with the generated gen and kill set.
  bool processBasicBlockWithGenKillSet(SILBasicBlock *BB);

  /// Intersect the successor live-ins.
  void mergeSuccessorStates(SILBasicBlock *BB);

  /// Update the BlockState based on the given instruction.
  void processInstruction(SILInstruction *I, DSEKind Kind);
};

} // end anonymous namespace

unsigned DSEContext::getLocationBit(const LSLocation &Loc) {
  // Return the bit position of the given Loc in the LocationVault. The bit
  // position is then used to set/reset the bitvector kept by each BlockState.
  //
  // We should have the location populated by the enumerateLSLocation at this
  // point.
  auto Iter = LocToBitIndex.find(Loc);
  assert(Iter != LocToBitIndex.end() && "LSLocation should have been enum'ed");
  return Iter->second;
}

void DSEContext::processBasicBlockForGenKillSet(SILBasicBlock *BB) {
  for (auto I = BB->rbegin(), E = BB->rend(); I != E; ++I) {
    processInstruction(&(*I), DSEKind::BuildGenKillSet);
  }
}

void DSEContext::processBasicBlockForMaxStoreSet(SILBasicBlock *BB) {
  // Compute the MaxStoreSet at the end of the basic block.
  auto *BBState = getBlockState(BB);
  if (BB->succ_empty()) {
    BBState->BBMaxStoreSet.reset();
  } else {
    auto Iter = BB->succ_begin();
    BBState->BBMaxStoreSet = getBlockState(*Iter)->BBMaxStoreSet;
    Iter = std::next(Iter);
    for (auto EndIter = BB->succ_end(); Iter != EndIter; ++Iter) {
      BBState->BBMaxStoreSet &= getBlockState(*Iter)->BBMaxStoreSet;
    }
  }

  // Compute the MaxStoreSet at the beginning of the basic block.
  for (auto I = BB->rbegin(), E = BB->rend(); I != E; ++I) {
    // Only process store insts.
    if (!isa<StoreInst>(*I))
      continue;
    processStoreInst(&(*I), DSEKind::ComputeMaxStoreSet);
  }
}

bool DSEContext::processBasicBlockWithGenKillSet(SILBasicBlock *BB) {
  // Compute the BBWriteSetOut at the end of the basic block.
  mergeSuccessorStates(BB);

  // Compute the BBWriteSetOut at the beginning of the basic block.
  BlockState *S = getBlockState(BB);
  llvm::BitVector T = S->BBKillSet;
  S->BBWriteSetOut &= T.flip();
  S->BBWriteSetOut |= S->BBGenSet;

  // If BBWriteSetIn changes, then keep iterating until reached a fixed point.
  return S->updateBBWriteSetIn();
}

bool DSEContext::processBasicBlockForDSE(SILBasicBlock *BB) {
  // Intersect in the successor WriteSetIns. A store is dead if it is not read
  // from any path to the end of the program. Thus an intersection.
  mergeSuccessorStates(BB);

  // Process instructions in post-order fashion.
  for (auto I = BB->rbegin(), E = BB->rend(); I != E; ++I) {
    processInstruction(&(*I), DSEKind::PerformDSE);
  }

  // If BBWriteSetIn changes, then keep iterating until reached a fixed
  // point.
  return getBlockState(BB)->updateBBWriteSetIn();
}

void DSEContext::mergeSuccessorStates(SILBasicBlock *BB) {
  // First, clear the BBWriteSetOut for the current basicblock.
  BlockState *C = getBlockState(BB);
  C->BBWriteSetOut.reset();

  // If basic block has no successor, then all local writes can be considered
  // dead for block with no successor.
  if (BB->succ_empty()) {
    if (DisableLocalStoreDSE)
      return;
    for (unsigned i = 0; i < LocationVault.size(); ++i) {
      if (!LocationVault[i].isNonEscapingLocalLSLocation())
        continue;
      C->startTrackingLocation(C->BBWriteSetOut, i);
    }
    return;
  }

  // Use the first successor as the base condition.
  auto Iter = BB->succ_begin();
  C->BBWriteSetOut = getBlockState(*Iter)->BBWriteSetIn;

  /// Merge/intersection is very frequently performed, so it is important to make
  /// it as cheap as possible.
  ///
  /// To do so, we canonicalize LSLocations, i.e. traced back to the underlying
  /// object. Therefore, no need to do a O(N^2) comparison to figure out what is
  /// dead along all successors.
  ///
  /// NOTE: Canonicalization does not solve the problem entirely. i.e. it is
  /// still possible that 2 LSLocations with different bases that happen to be
  /// the same object and field. In such case, we would miss a dead store
  /// opportunity. But this happens less often with canonicalization.
  Iter = std::next(Iter);
  for (auto EndIter = BB->succ_end(); Iter != EndIter; ++Iter) {
    C->BBWriteSetOut &= getBlockState(*Iter)->BBWriteSetIn;
  }
}

void DSEContext::invalidateLSLocationBase(SILInstruction *I, DSEKind Kind) {
  // If this instruction defines the base of a location, then we need to
  // invalidate any locations with the same base.
  BlockState *S = getBlockState(I);

  // Are we building genset and killset.
  if (isBuildingGenKillSet(Kind)) {
    for (unsigned i = 0; i < S->LocationNum; ++i) {
      if (LocationVault[i].getBase().getDef() != I)
        continue;
      S->startTrackingLocation(S->BBKillSet, i);
      S->stopTrackingLocation(S->BBGenSet, i);
    }
    return;
  }

  // Are we performing dead store elimination.
  if (isPerformingDSE(Kind)) {
    for (unsigned i = 0; i < S->LocationNum; ++i) {
      if (!S->BBWriteSetOut.test(i))
        continue;
      if (LocationVault[i].getBase().getDef() != I)
        continue;
      S->stopTrackingLocation(S->BBWriteSetOut, i);
    }
    return;
  }

  llvm_unreachable("Unknown DSE compute kind");
}

void DSEContext::updateWriteSetForRead(BlockState *S, unsigned bit) {
  // Remove any may/must-aliasing stores to the LSLocation, as they cant be
  // used to kill any upward visible stores due to the interfering load.
  LSLocation &R = LocationVault[bit];
  for (unsigned i = 0; i < S->LocationNum; ++i) {
    if (!S->isTrackingLSLocation(S->BBWriteSetOut, i))
      continue;
    LSLocation &L = LocationVault[i];
    if (!L.isMayAliasLSLocation(R, AA))
      continue;
    S->stopTrackingLocation(S->BBWriteSetOut, i);
  }
}

bool DSEContext::updateWriteSetForWrite(BlockState *S, unsigned bit) {
  // If a tracked store must aliases with this store, then this store is dead.
  bool IsDead = false;
  LSLocation &R = LocationVault[bit];
  for (unsigned i = 0; i < S->LocationNum; ++i) {
    if (!S->isTrackingLSLocation(S->BBWriteSetOut, i))
      continue;
    // If 2 locations may alias, we can still keep both stores.
    LSLocation &L = LocationVault[i];
    if (!L.isMustAliasLSLocation(R, AA))
      continue;
    // There is a must alias store. No need to check further.
    IsDead = true;
    break;
  }

  // Track this new store.
  S->startTrackingLocation(S->BBWriteSetOut, bit);
  return IsDead;
}

void DSEContext::updateGenKillSetForRead(BlockState *S, unsigned bit) {
  // Start tracking the read to this LSLocation in the killset and update
  // the genset accordingly.
  //
  // Even though, LSLocations are canonicalized, we still need to consult
  // alias analysis to determine whether 2 LSLocations are disjointed.
  LSLocation &R = LocationVault[bit];
  for (unsigned i = 0; i < S->LocationNum; ++i) {
    if (!S->BBMaxStoreSet.test(i))
      continue;
    // Do nothing if the read location NoAlias with the current location.
    LSLocation &L = LocationVault[i];
    if (!L.isMayAliasLSLocation(R, AA))
      continue;
    // Update the genset and kill set.
    S->startTrackingLocation(S->BBKillSet, i);
    S->stopTrackingLocation(S->BBGenSet, i);
  }
}

void DSEContext::updateGenKillSetForWrite(BlockState *S, unsigned bit) {
  S->startTrackingLocation(S->BBGenSet, bit);
}

void DSEContext::updateMaxStoreSetForWrite(BlockState *S, unsigned bit) {
  S->startTrackingLocation(S->BBMaxStoreSet, bit);
}

void DSEContext::processRead(SILInstruction *I, BlockState *S, SILValue Mem,
                             DSEKind Kind) {
  // Construct a LSLocation to represent the memory read by this instruction.
  // NOTE: The base will point to the actual object this inst is accessing,
  // not this particular field.
  //
  // e.g. %1 = alloc_stack $S
  //      %2 = struct_element_addr %1, #a
  //      %3 = load %2 : $*Int
  //
  // Base will point to %1, but not %2. Projection path will indicate which
  // field is accessed.
  //
  // This will make comparison between locations easier. This eases the
  // implementation of intersection operator in the data flow.
  LSLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the read instruction,
  // process it as an unknown memory instruction for now.
  if (!L.isValid()) {
    processUnknownReadInst(I, Kind);
    return;
  }

  // Expand the given Mem into individual fields and process them as separate
  // reads.
  LSLocationList Locs;
  LSLocation::expand(L, &I->getModule(), Locs, TE);

  // Are we building the genset and killset.
  if (isBuildingGenKillSet(Kind)) {
    for (auto &E : Locs) {
      // Only building the gen and kill sets for now.
      updateGenKillSetForRead(S, getLocationBit(E));
    }
    return;
  }

  // Are we performing the actual DSE.
  if (isPerformingDSE(Kind)) {
    for (auto &E : Locs) {
      // This is the last iteration, compute BBWriteSetOut and perform DSE. 
      updateWriteSetForRead(S, getLocationBit(E));
    }
    return;
  }

  llvm_unreachable("Unknown DSE compute kind");
}

void DSEContext::processWrite(SILInstruction *I, BlockState *S, SILValue Val,
                              SILValue Mem, DSEKind Kind) {
  // Construct a LSLocation to represent the memory read by this instruction.
  // NOTE: The base will point to the actual object this inst is accessing,
  // not this particular field.
  //
  // e.g. %1 = alloc_stack $S
  //      %2 = struct_element_addr %1, #a
  //      store %3 to %2 : $*Int
  //
  // Base will point to %1, but not %2. Projection path will indicate which
  // field is accessed.
  //
  // This will make comparison between locations easier. This eases the
  // implementation of intersection operator in the data flow.
  LSLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the store
  // instruction, simply ignore it.
  if (!L.isValid())
    return;

  // Expand the given Mem into individual fields and process them as separate
  // writes.
  bool Dead = true;
  LSLocationList Locs;
  LSLocation::expand(L, Mod, Locs, TE);
  llvm::BitVector V(Locs.size());

  // Are we computing genset and killset.
  if (isBuildingGenKillSet(Kind)) {
    for (auto &E : Locs) {
      // Only building the gen and kill sets here.
      updateGenKillSetForWrite(S, getLocationBit(E));
    }
    // Data flow has not stabilized, do not perform the DSE just yet.
    return;
  }

  // Are we computing max store set.
  if (isComputeMaxStoreSet(Kind)) {
    for (auto &E : Locs) {
      // Update the max store set for the basic block.
      updateMaxStoreSetForWrite(S, getLocationBit(E));
    }
    return;
  }

  // We are doing the actual DSE.
  assert(isPerformingDSE(Kind) && "Invalid computation kind");
  unsigned idx = 0;
  for (auto &E : Locs) {
    // This is the last iteration, compute BBWriteSetOut and perform the dead
    // store elimination.
    if (updateWriteSetForWrite(S, getLocationBit(E)))
      V.set(idx);
    Dead &= V.test(idx);
    ++idx;
  }

  // Fully dead store - stores to all the components are dead, therefore this
  // instruction is dead.
  if (Dead) {
    DEBUG(llvm::dbgs() << "Instruction Dead: " << *I << "\n");
    S->DeadStores.insert(I);
    ++NumDeadStores;
    return;
  }

  // Partial dead store - stores to some locations are dead, but not all. This
  // is a partially dead store. Also at this point we know what locations are
  // dead.
  llvm::DenseSet<LSLocation> Alives;
  if (V.any()) {
    // Take out locations that are dead.
    for (unsigned i = 0; i < V.size(); ++i) {
      if (V.test(i))
        continue;
      // This location is alive.
      Alives.insert(Locs[i]);
    }

    // Try to create as few aggregated stores as possible out of the locations.
    LSLocation::reduce(L, Mod, Alives, TE);

    // Oops, we have too many smaller stores generated, bail out.
    if (Alives.size() > MaxPartialDeadStoreCountLimit)
      return;

    // At this point, we are performing a partial dead store elimination.
    //
    // Locations here have a projection path from their Base, but this
    // particular instruction may not be accessing the base, so we need to
    // *rebase* the locations w.r.t. to the current instruction.
    SILValue B = Locs[0].getBase();
    Optional<ProjectionPath> BP = ProjectionPath::getAddrProjectionPath(B, Mem);
    // Strip off the projection path from base to the accessed field.
    for (auto &X : Alives) {
      X.subtractPaths(BP);
    }

    // We merely setup the remain live stores, but do not materialize in IR yet,
    // These stores will be materialized when before the algorithm exits.
    for (auto &X : Alives) {
      SILValue Value =
          SILValueProjection::createExtract(Val, X.getPath(), I, true);
      SILValue Addr =
          SILValueProjection::createExtract(Mem, X.getPath(), I, false);
      S->LiveStores[Addr] = Value;
    }

    // Lastly, mark the old store as dead.
    DEBUG(llvm::dbgs() << "Instruction Partially Dead: " << *I << "\n");
    S->DeadStores.insert(I);
    ++NumPartialDeadStores;
  }
}

void DSEContext::processLoadInst(SILInstruction *I, DSEKind Kind) {
  processRead(I, getBlockState(I), cast<LoadInst>(I)->getOperand(), Kind);
}

void DSEContext::processStoreInst(SILInstruction *I, DSEKind Kind) {
  auto *SI = cast<StoreInst>(I);
  processWrite(I, getBlockState(I), SI->getSrc(), SI->getDest(), Kind);
}

void DSEContext::processDebugValueAddrInst(SILInstruction *I, DSEKind Kind) {
  BlockState *S = getBlockState(I);
  SILValue Mem = cast<DebugValueAddrInst>(I)->getOperand();
  // Are we building genset and killset.
  if (isBuildingGenKillSet(Kind)) {
    for (unsigned i = 0; i < S->LocationNum; ++i) {
      if (!S->BBMaxStoreSet.test(i))
        continue;
      if (AA->isNoAlias(Mem, LocationVault[i].getBase()))
        continue;
      S->stopTrackingLocation(S->BBGenSet, i);
      S->startTrackingLocation(S->BBKillSet, i);
    }
    return;
  }

  // Are we performing dead store elimination.
  if (isPerformingDSE(Kind)) {
    for (unsigned i = 0; i < S->LocationNum; ++i) {
      if (!S->isTrackingLSLocation(S->BBWriteSetOut, i))
        continue;
      if (AA->isNoAlias(Mem, LocationVault[i].getBase()))
        continue;
      S->stopTrackingLocation(S->BBWriteSetOut, i);
    }
    return;
  }

  llvm_unreachable("Unknown DSE compute kind");
}

void DSEContext::processUnknownReadInst(SILInstruction *I, DSEKind Kind) {
  BlockState *S = getBlockState(I);
  // Are we building genset and killset.
  if (isBuildingGenKillSet(Kind)) {
    for (unsigned i = 0; i < S->LocationNum; ++i) {
      if (!S->BBMaxStoreSet.test(i))
        continue;
      if (!AA->mayReadFromMemory(I, LocationVault[i].getBase()))
        continue;
      // Update the genset and kill set.
      S->startTrackingLocation(S->BBKillSet, i);
      S->stopTrackingLocation(S->BBGenSet, i);
    }
    return;
  }

  // Are we performing dead store elimination.
  if (isPerformingDSE(Kind)) {
    for (unsigned i = 0; i < S->LocationNum; ++i) {
      if (!S->isTrackingLSLocation(S->BBWriteSetOut, i))
        continue;
      if (!AA->mayReadFromMemory(I, LocationVault[i].getBase()))
        continue;
      S->stopTrackingLocation(S->BBWriteSetOut, i);
    }
    return;
  }

  llvm_unreachable("Unknown DSE compute kind");
}

void DSEContext::processInstruction(SILInstruction *I, DSEKind Kind) {
  // If this instruction has side effects, but is inert from a store
  // perspective, skip it.
  if (isDeadStoreInertInstruction(I))
    return;

  // A set of ad-hoc rules to process instructions.
  if (isa<LoadInst>(I)) {
    processLoadInst(I, Kind);
  } else if (isa<StoreInst>(I)) {
    processStoreInst(I, Kind);
  } else if (isa<DebugValueAddrInst>(I)) {
    processDebugValueAddrInst(I, Kind);
  } else if (I->mayReadFromMemory()) {
    processUnknownReadInst(I, Kind);
  }

  // Check whether this instruction will invalidate any other locations.
  invalidateLSLocationBase(I, Kind);
}

bool DSEContext::run() {
  // Walk over the function and find all the locations accessed by
  // this function.
  LSLocation::enumerateLSLocations(*F, LocationVault, LocToBitIndex, TE);

  // For all basic blocks in the function, initialize a BB state.
  //
  // DenseMap has a minimum size of 64, while many functions do not have more
  // than 64 basic blocks. Therefore, allocate the BlockState in a vector and
  // use pointer in BBToLocState to access them.
  for (auto &B : *F) {
    BlockStates.push_back(BlockState(&B));
    // Since we know all the locations accessed in this function, we can resize
    // the bit vector to the appropriate size.
    BlockStates.back().init(LocationVault.size());
  }

  // Initialize the BBToLocState mapping.
  for (auto &S : BlockStates) {
    BBToLocState[S.getBB()] = &S;
  }

  // We perform dead store elimination in the following phases.
  //
  // Phase 1. we compute the max store set at the beginning of the basic block.
  //
  // Phase 2. we compute the genset and killset for every basic block.
  //
  // Phase 3. we run the data flow with the genset and killset until
  // BBWriteSetIns stop changing.
  //
  // Phase 4. we run the data flow for the last iteration and perform the DSE.
  //
  // Phase 5. we remove the dead stores.

  // Compute the max store set at the beginning of the basic block.
  //
  // This helps generating the genset and killset. If there is no way a
  // location can have an upward visible store at a particular point in the
  // basic block, we do not need to turn on the genset and killset for the
  // location.
  //
  // Turning on the genset and killset can be costly as it involves querying
  // the AA interface.
  auto *PO = PM->getAnalysis<PostOrderAnalysis>()->get(F);
  for (SILBasicBlock *B : PO->getPostOrder()) {
    processBasicBlockForMaxStoreSet(B);
  }

  // Generate the genset and killset for each basic block. We can process the
  // basic blocks in any order.
  for (auto &B : *F) {
    processBasicBlockForGenKillSet(&B);
  }

  // Process each basic block with the gen and kill set. Every time the
  // BBWriteSetIn of a basic block changes, the optimization is rerun on its
  // predecessors.
  llvm::SmallVector<SILBasicBlock *, 16> WorkList;
  for (SILBasicBlock *B : PO->getPostOrder()) {
    WorkList.push_back(B);
  }
  while (!WorkList.empty()) {
    SILBasicBlock *BB = WorkList.pop_back_val();
    if (processBasicBlockWithGenKillSet(BB)) {
      for (auto X : BB->getPreds())
        WorkList.push_back(X);
    }
  }

  // The data flow has stabilized, run one last iteration over all the basic
  // blocks and try to remove dead stores.
  for (SILBasicBlock &BB : *F) {
    processBasicBlockForDSE(&BB);
  }

  // Finally, delete the dead stores and create the live stores.
  bool Changed = false;
  for (SILBasicBlock &BB : *F) {
    // Create the stores that are alive due to partial dead stores.
    for (auto &I : getBlockState(&BB)->LiveStores) {
      Changed = true;
      SILInstruction *IT = cast<SILInstruction>(I.first)->getNextNode();
      SILBuilderWithScope Builder(IT);
      Builder.createStore(I.first.getLoc().getValue(), I.second, I.first);
    }
    // Delete the dead stores.
    for (auto &I : getBlockState(&BB)->DeadStores) {
      Changed = true;
      DEBUG(llvm::dbgs() << "*** Removing: " << *I << " ***\n");
      // This way, we get rid of pass dependence on DCE.
      recursivelyDeleteTriviallyDeadInstructions(I, true);
    }
  }
  return Changed;
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class DeadStoreElimination : public SILFunctionTransform {
public:
  StringRef getName() override { return "SIL Dead Store Elimination"; }

  /// The entry point to the transformation.
  void run() override {
    auto *AA = PM->getAnalysis<AliasAnalysis>();
    auto *TE = PM->getAnalysis<TypeExpansionAnalysis>();
    SILFunction *F = getFunction();
    DEBUG(llvm::dbgs() << "*** DSE on function: " << F->getName() << " ***\n");

    DSEContext DSE(F, &F->getModule(), PM, AA, TE);
    if (DSE.run()) {
      invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createDeadStoreElimination() {
  return new DeadStoreElimination();
}

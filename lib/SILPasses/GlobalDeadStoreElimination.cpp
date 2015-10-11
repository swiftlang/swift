//===---- GlobalDeadStoreElimination.cpp - SIL Dead Store Elimination -----===//
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
/// DeadStoreElimination (DSE) eliminates such stores by:
///
/// 1. Introducing a notion of a MemLocation that is used to model objects
/// fields. (See below for more details).
///
/// 2. Performing a post-order walk over the control flow graph, tracking any
/// MemLocations that are read from or stored into in each basic block. After
/// eliminating any dead stores in single blocks, it computes a kill set for
/// each block. The kill set tracks what MemLocations are stored into by this
/// basic block and its successors.
///
/// 3. An optimistic iterative dataflow is performed on the kill sets until
/// convergence.
///
/// At the core of DSE, there is the MemLocation class. a MemLocation is an
/// abstraction of an object field in program. It consists of a base and a
/// projection path to the field accessed.
///
/// When a load or store instruction is encountered, the memory is broken down
/// to the indivisible components, i.e aggregates are broken down to their
/// individual fields using the expand function. This gives the flexbility to
/// find exactly which part of the store is alive and which part is dead.
///
/// After the live parts of the store are determined, they are merged into the
/// minimum number of stores possible using the reduce function. This is done
/// so that we do not bloat SIL IR.
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
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SIL/MemLocation.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Passes.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include <algorithm>

using namespace swift;
using swift::MemLocation;

STATISTIC(NumDeadStores, "Number of dead stores removed");
STATISTIC(NumPartialDeadStores, "Number of partial dead stores removed");

//===----------------------------------------------------------------------===//
//                             Utility Functions
//===----------------------------------------------------------------------===//

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

/// BBState summarizes how MemLocations are used in a basic block.
///
/// Initially the WriteSetOut is empty. Before a basic block is processed, it is
/// initialized to the intersection of WriteSetIns of all successors of the
/// basic block.
///
/// Initially WriteSetIn is empty. After the basic block is processed, if its
/// WriteSetOut is different from WriteSetIn, WriteSetIn is initialized to the
/// value of WriteSetOut and the data flow is rerun.
///
/// Instructions in each basic block are processed in post-order as follows:
///
/// 1. When a store instruction is encountered, the stored location is tracked.
///
/// 2. When a load instruction is encountered, remove the loaded location and
///    any location it may alias with from the WriteSetOut.
///
/// 3. When an instruction reads or writes to memory in an unknown way, the
///    WriteSetOut is cleared.
///
class BBState {
public:
  /// A bit vector for which the ith bit represents the ith MemLocation in
  /// MemLocationVault. If the bit is set, then the location currently has an
  /// upward visible store.
  llvm::BitVector WriteSetOut;

  /// If WriteSetIn changes while processing a basicblock, then all its
  /// predecessors needs to be rerun.
  llvm::BitVector WriteSetIn;

  /// The basic block this BBState represents.
  SILBasicBlock *BB;

  /// The dead stores in the current basic block.
  llvm::DenseSet<SILInstruction *> DeadStores;

  /// Constructors.
  BBState() : BB(nullptr) {}
  BBState(SILBasicBlock *B, unsigned lcnt) : BB(B) {
    // The initial state of WriteSetIn should be all 1's. Otherwise the
    // dataflow solution could be too conservative.
    //
    // consider this case, the dead store by var a = 10 before the loop will not
    // be eliminated if the WriteSetIn is set to 0 initially.
    //
    //   var a = 10
    //   for _ in 0...1024 {}
    //   a = 10
    //
    // However, by doing so, we can only eliminate the dead stores after the
    // data flow stablizes.
    //
    WriteSetIn.resize(lcnt, true);
    WriteSetOut.resize(lcnt, false);
  }

  /// Check whether the WriteSetIn has changed. If it does, we need to
  /// re-iterate to reach fixed point.
  bool updateWriteSetIn();

  /// Functions to manipulate the write set.
  void clearMemLocations();
  void startTrackingMemLocation(unsigned bit);
  void stopTrackingMemLocation(unsigned bit);
  bool isTrackingMemLocation(unsigned bit);
  void initialize(const BBState &L);
  void intersect(const BBState &L);
};

} // end anonymous namespace

bool BBState::updateWriteSetIn() {
  if (WriteSetIn != WriteSetOut) {
    WriteSetIn = WriteSetOut;
    return true;
  }
  return false;
}

void BBState::clearMemLocations() { WriteSetOut.reset(); }

void BBState::startTrackingMemLocation(unsigned bit) { WriteSetOut.set(bit); }

void BBState::stopTrackingMemLocation(unsigned bit) { WriteSetOut.reset(bit); }

bool BBState::isTrackingMemLocation(unsigned bit) {
  return WriteSetOut.test(bit);
}

void BBState::initialize(const BBState &Succ) { WriteSetOut = Succ.WriteSetIn; }

void BBState::intersect(const BBState &Succ) {
  for (unsigned i = 0; i < WriteSetOut.size(); ++i) {
    if (Succ.WriteSetIn.test(i))
      continue;
    // WriteSetIn is not set.
    stopTrackingMemLocation(i);
  }
}

//===----------------------------------------------------------------------===//
//                          Top Level Implementation
//===----------------------------------------------------------------------===//

namespace {

class GlobalDeadStoreEliminationImpl {
  /// Map every basic block to its location state.
  llvm::DenseMap<SILBasicBlock *, BBState> BBToLocState;

  /// The function we are currently processing.
  SILFunction *F;

  /// The module we are currently processing.
  SILModule *Mod;

  /// Pass manager, used to get various analysis.
  SILPassManager *PM;

  /// Alias Analysis.
  AliasAnalysis *AA;

  /// Allocator.
  llvm::BumpPtrAllocator BPA;

  /// Keeps all the locations for the current function. The BitVector in each
  /// BBState is then laid on top of it to keep track of which MemLocation
  /// has an upward visible store.
  std::vector<MemLocation> MemLocationVault;

  /// Contains a map between location to their index in the MemLocationVault.
  llvm::DenseMap<MemLocation, unsigned> LocToBitIndex;

  /// Return the BBState for the basic block this basic block belongs to.
  BBState *getBBLocState(SILBasicBlock *B) { return &BBToLocState[B]; }

  /// Return the BBState for the basic block this instruction belongs to.
  BBState *getBBLocState(SILInstruction *I) {
    return getBBLocState(I->getParent());
  }

  /// MemLocation read has been extracted, expanded and mapped to the bit
  /// position in the bitvector. process it using the bit position.
  void updateWriteSetForRead(SILInstruction *Inst, BBState *State,
                             unsigned Bit);

  /// MemLocation written has been extracted, expanded and mapped to the bit
  /// position in the bitvector. process it using the bit position.
  bool updateWriteSetForWrite(SILInstruction *Inst, BBState *State,
                              unsigned Bit);

  /// There is a read to a location, expand the location into individual fields
  /// before processing them.
  void processRead(SILInstruction *Inst, BBState *State, SILValue Mem);

  /// There is a write to a location, expand the location into individual fields
  /// before processing them.
  void processWrite(SILInstruction *Inst, BBState *State, SILValue Val,
                    SILValue Mem, bool PDSE);

  /// Process Instructions. Extract MemLocations from SIL LoadInst.
  void processLoadInst(SILInstruction *Inst);

  /// Process Instructions. Extract MemLocations from SIL StoreInst.
  void processStoreInst(SILInstruction *Inst, bool PDSE);

  /// Process Instructions. Extract MemLocations from SIL Unknown Memory Inst.
  void processUnknownReadMemInst(SILInstruction *Inst);

  /// Check whether the instruction invalidate any MemLocations due to change in
  /// its MemLocation Base.
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
  /// In this case, DSE can not remove the x.a = 13 inside the loop.
  ///
  /// To do this, when the algorithm reaches the beginning of the basic block in
  /// the loop it will need to invalidate the MemLocation in the WriteSetOut.
  /// i.e.
  /// the base of the MemLocation is changed.
  ///
  /// If not, on the second iteration, the intersection of the successors of
  /// the loop basic block will have store to x.a and therefore x.a = 13 can now
  /// be considered dead.
  ///
  void invalidateMemLocationBase(SILInstruction *Inst);

  /// Get the bit representing the location in the MemLocationVault.
  ///
  /// NOTE: Adds the location to the location vault if necessary.
  unsigned getMemLocationBit(const MemLocation &L);

public:
  /// Constructor.
  GlobalDeadStoreEliminationImpl(SILFunction *F, SILModule *M,
                                 SILPassManager *PM, AliasAnalysis *AA)
      : Mod(M), F(F), PM(PM), AA(AA) {}

  /// Compute the kill set for the basic block. return true if the store set
  /// changes.
  bool processBasicBlock(SILBasicBlock *BB, bool PDSE = false);

  /// Intersect the successor live-ins.
  void mergeSuccessorsWriteIn(SILBasicBlock *BB);

  /// Update the BBState based on the given instruction.
  void processInstruction(SILInstruction *I, bool PDSE);

  /// Entry point for global dead store elimination.
  void run();

  /// Create the value or address extraction based on the give Base and
  /// projection path.
  SILValue createExtract(SILValue VA, Optional<ProjectionPath> &Path,
                         SILInstruction *Inst, bool IsValExtract);
};

} // end anonymous namespace

unsigned
GlobalDeadStoreEliminationImpl::getMemLocationBit(const MemLocation &Loc) {
  // Return the bit position of the given Loc in the MemLocationVault. The bit
  // position is then used to set/reset the bitvector kept by each BBState.
  //
  // We should have the location populated by the enumerateMemLocation at this
  // point.
  //
  auto Iter = LocToBitIndex.find(Loc);
  assert(Iter != LocToBitIndex.end() &&
         "MemLocation should have been enumerated");
  return Iter->second;
}

bool GlobalDeadStoreEliminationImpl::processBasicBlock(SILBasicBlock *BB,
                                                       bool PDSE) {
  // Intersect in the successor live-ins. A store is dead if it is not read from
  // any path to the end of the program. Thus an intersection.
  mergeSuccessorsWriteIn(BB);

  // Process instructions in post-order fashion.
  for (auto I = BB->rbegin(), E = BB->rend(); I != E; ++I) {
    processInstruction(&(*I), PDSE);
  }

  // If WriteSetIn changes, then keep iterating until reached a fixed
  // point.
  return getBBLocState(BB)->updateWriteSetIn();
}

void GlobalDeadStoreEliminationImpl::mergeSuccessorsWriteIn(SILBasicBlock *BB) {
  // First, clear the WriteSetOut for the current basicblock.
  BBState *C = getBBLocState(BB);
  C->clearMemLocations();

  // If the basic block has no successor, then we do not need to do anything
  // for its WriteSetOut.
  if (BB->succ_empty())
    return;

  // Use the first successor as the base condition.
  auto Iter = BB->succ_begin();
  C->initialize(*getBBLocState(*Iter));
  Iter = std::next(Iter);
  for (auto EndIter = BB->succ_end(); Iter != EndIter; ++Iter) {
    C->intersect(*getBBLocState(*Iter));
  }
}

void GlobalDeadStoreEliminationImpl::invalidateMemLocationBase(
    SILInstruction *I) {
  BBState *S = getBBLocState(I);
  // If this instruction defines the base of a location, then we need to
  // invalidate any locations with the same base.
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->WriteSetOut.test(i))
      continue;
    if (MemLocationVault[i].getBase().getDef() != I)
      continue;
    S->stopTrackingMemLocation(i);
  }
}

void GlobalDeadStoreEliminationImpl::updateWriteSetForRead(SILInstruction *I,
                                                           BBState *S,
                                                           unsigned bit) {
  // Remove any may/must-aliasing stores to the MemLocation, as they cant be
  // used
  // to kill any upward visible stores due to the intefering load.
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->isTrackingMemLocation(i))
      continue;
    if (!MemLocationVault[i].isMayAliasMemLocation(MemLocationVault[bit], AA))
      continue;
    DEBUG(llvm::dbgs() << "Loc Removal: " << MemLocationVault[i].getBase()
                       << "Instruction: " << *I << "\n");
    S->stopTrackingMemLocation(i);
  }
}

bool GlobalDeadStoreEliminationImpl::updateWriteSetForWrite(SILInstruction *I,
                                                            BBState *S,
                                                            unsigned bit) {
  // If a tracked store must aliases with this store, then this store is dead.
  bool IsDead = false;
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->isTrackingMemLocation(i))
      continue;
    // If 2 locations may alias, we can still keep both stores.
    if (!MemLocationVault[i].isMustAliasMemLocation(MemLocationVault[bit], AA))
      continue;
    IsDead = true;
    // No need to check the rest of the upward visible stores as this store
    // is dead.
    break;
  }

  // Track this new store.
  DEBUG(llvm::dbgs() << "Loc Insertion: " << MemLocationVault[bit].getBase()
                     << "Instruction: " << *I << "\n");
  S->startTrackingMemLocation(bit);
  return IsDead;
}

void GlobalDeadStoreEliminationImpl::processRead(SILInstruction *I, BBState *S,
                                                 SILValue Mem) {
  // Construct a MemLocation to represent the memory read by this instruction.
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
  MemLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the read instruction,
  // process it as an unknown memory instruction for now.
  if (!L.isValid()) {
    processUnknownReadMemInst(I);
    return;
  }

  // Expand the given Mem into individual fields and process them as
  // separate reads.
  MemLocationList Locs;
  MemLocation::expand(L, &I->getModule(), Locs);
  for (auto &E : Locs) {
    updateWriteSetForRead(I, S, getMemLocationBit(E));
  }
}

SILValue GlobalDeadStoreEliminationImpl::createExtract(
    SILValue Base, Optional<ProjectionPath> &Path, SILInstruction *Inst,
    bool IsValExt) {
  // If we found a projection path, but there are no projections, then the two
  // loads must be the same, return PrevLI.
  if (!Path || Path->empty())
    return Base;

  // Ok, at this point we know that we can construct our aggregate projections
  // from our list of address projections.
  SILValue LastExtract = Base;
  SILBuilder Builder(Inst);

  // Construct the path!
  for (auto PI = Path->rbegin(), PE = Path->rend(); PI != PE; ++PI) {
    if (IsValExt) {
      LastExtract =
          PI->createValueProjection(Builder, Inst->getLoc(), LastExtract).get();
      continue;
    }
    LastExtract =
        PI->createAddrProjection(Builder, Inst->getLoc(), LastExtract).get();
  }

  // Return the last extract we created.
  return LastExtract;
}

void GlobalDeadStoreEliminationImpl::processWrite(SILInstruction *I, BBState *S,
                                                  SILValue Val, SILValue Mem,
                                                  bool PDSE) {
  // Construct a MemLocation to represent the memory read by this instruction.
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
  MemLocation L(Mem);

  // If we cant figure out the Base or Projection Path for the store
  // instruction,
  // simply ignore it for now.
  if (!L.isValid())
    return;

  // Expand the given Mem into individual fields and process them as separate
  // writes.
  bool Dead = true;
  MemLocationList Locs;
  MemLocation::expand(L, &I->getModule(), Locs);
  llvm::BitVector V(Locs.size());
  unsigned idx = 0;
  for (auto &E : Locs) {
    if (updateWriteSetForWrite(I, S, getMemLocationBit(E)))
      V.set(idx);
    Dead &= V.test(idx);
    ++idx;
  }

  // Data flow has not stablized, do not perform the DSE just yet.
  if (!PDSE)
    return;

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
  llvm::DenseSet<MemLocation> LocsAlive;
  if (V.any()) {
    // Take out locations that are dead.
    for (unsigned i = 0; i < V.size(); ++i) {
      if (V.test(i))
        continue;
      // We are already tracking all the stores to this Mem as dead.
      S->stopTrackingMemLocation(i);
      LocsAlive.insert(Locs[i]);
    }

    // Try to create as few aggregated stores as possible out of the locations.
    MemLocation::reduce(L, Mod, LocsAlive);

    // Oops, we have too many smaller stores generated, bail out.
    if (LocsAlive.size() > MaxPartialDeadStoreCountLimit)
      return;

    // Locations here have a projection path from their Base, but this
    // particular instruction may not be accessing the base, so we need to
    // *rebase*
    // the locations w.r.t. to the current instruction.
    SILValue B = Locs[0].getBase();
    Optional<ProjectionPath> BP = ProjectionPath::getAddrProjectionPath(B, Mem);
    // Strip off the projection path from base to the accessed field.
    for (auto &X : LocsAlive) {
      X.subtractPaths(BP);
    }

    // Create the individual stores that are alive.
    SILBuilderWithScope<16> Builder(I);
    for (auto &X : LocsAlive) {
      SILValue Value = createExtract(Val, X.getPath(), I, true);
      SILValue Addr = createExtract(Mem, X.getPath(), I, false);
      Builder.createStore(Addr.getLoc().getValue(), Value, Addr);
    }

    // Lastly, mark the old store as dead.
    DEBUG(llvm::dbgs() << "Instruction Partially Dead: " << *I << "\n");
    S->DeadStores.insert(I);
    ++NumPartialDeadStores;
  }
}

void GlobalDeadStoreEliminationImpl::processLoadInst(SILInstruction *I) {
  // Loading a loadable type.
  SILValue Mem = cast<LoadInst>(I)->getOperand();
  processRead(I, getBBLocState(I), Mem);
}

void GlobalDeadStoreEliminationImpl::processStoreInst(SILInstruction *I,
                                                      bool PDSE) {
  // Storing a loadable type.
  SILValue Val = cast<StoreInst>(I)->getSrc();
  SILValue Mem = cast<StoreInst>(I)->getDest();
  processWrite(I, getBBLocState(I), Val, Mem, PDSE);
}

void GlobalDeadStoreEliminationImpl::processUnknownReadMemInst(
    SILInstruction *I) {
  // We do not know what this instruction does or the memory that it *may*
  // touch. Hand it to alias analysis to see whether we need to invalidate
  // any MemLocation.
  BBState *S = getBBLocState(I);
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->isTrackingMemLocation(i))
      continue;
    if (!AA->mayReadFromMemory(I, MemLocationVault[i].getBase()))
      continue;
    getBBLocState(I)->stopTrackingMemLocation(i);
  }
}

void GlobalDeadStoreEliminationImpl::processInstruction(SILInstruction *I,
                                                        bool PDSE) {
  // If this instruction has side effects, but is inert from a store
  // perspective, skip it.
  if (isDeadStoreInertInstruction(I))
    return;

  // A set of ad-hoc rules to process instructions.
  //
  // TODO: process more instructions.
  //
  if (isa<LoadInst>(I)) {
    processLoadInst(I);
  } else if (isa<StoreInst>(I)) {
    processStoreInst(I, PDSE);
  } else if (I->mayReadFromMemory()) {
    processUnknownReadMemInst(I);
  }

  // Check whether this instruction will invalidate any other MemLocations.
  invalidateMemLocationBase(I);
}

void GlobalDeadStoreEliminationImpl::run() {
  // Walk over the function and find all the locations accessed by
  // this function.
  MemLocation::enumerateMemLocations(*F, MemLocationVault, LocToBitIndex);

  // For all basic blocks in the function, initialize a BB state. Since we
  // know all the locations accessed in this function, we can resize the bit
  // vector to the approproate size.
  for (auto &B : *F) {
    BBToLocState[&B] = BBState(&B, MemLocationVault.size());
  }

  auto *PO = PM->getAnalysis<PostOrderAnalysis>()->get(F);
  // Keep iterating over all basicblocks in a post-order fashion until
  // convergence. Everytime the WriteSetIn of a basic block changes, the
  // optimization is rerun.
  //
  // TODO: We only need to rerun basic blocks with successors changed.
  // use a worklist in the future.
  //
  bool Changed = false;
  do {
    Changed = false;
    for (SILBasicBlock *BB : PO->getPostOrder()) {
      Changed |= processBasicBlock(BB);
    }
  } while (Changed);

  // The data flow has stablized, run one last iteration over all the basic
  // blocks and try to remove dead stores.
  for (SILBasicBlock *BB : PO->getPostOrder()) {
    processBasicBlock(BB, true);
  }

  // Finally, delete the dead stores.
  for (SILBasicBlock *BB : PO->getPostOrder()) {
    for (auto &I : getBBLocState(BB)->DeadStores) {
      DEBUG(llvm::dbgs() << "*** Removing: " << *I << " ***\n");
      I->eraseFromParent();
    }
  }
}

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class GlobalDeadStoreElimination : public SILFunctionTransform {
public:
  StringRef getName() override { return "SIL Dead Store Elimination"; }

  /// The entry point to the transformation.
  void run() override {
    auto *AA = PM->getAnalysis<AliasAnalysis>();
    SILFunction *F = getFunction();
    DEBUG(llvm::dbgs() << "*** DSE on function: " << F->getName() << " ***\n");

    GlobalDeadStoreEliminationImpl DSE(F, &F->getModule(), PM, AA);
    DSE.run();
  }
};

} // end anonymous namespace

SILTransform *swift::createGlobalDeadStoreElimination() {
  return new GlobalDeadStoreElimination();
}

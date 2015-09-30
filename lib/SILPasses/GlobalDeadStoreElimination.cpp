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
/// 1. Introducing a notion of a Location that is used to model objects fields.
/// (See below for more details).
///
/// 2. Performing a post-order walk over the control flow graph, tracking any
/// Locations that are read from or stored into in each basic block. After
/// eliminating any dead stores in single blocks, it computes a kill set for
/// each block. The kill set tracks what Locations are stored into by this basic
/// block and its successors.
///
/// 3. An optimistic iterative dataflow is performed on the kill sets until
/// convergence.
///
/// At the core of DSE, there is the Location class. a Location is an
/// abstraction of an object field in program. It consists of currently a base
/// but will eventually include a path as well.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-dead-store-opt"
#include "swift/SILPasses/Passes.h"
#include "swift/SILAnalysis/AliasAnalysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILAnalysis/PostOrderAnalysis.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILPasses/Utils/SILSSAUpdater.h"
#include "swift/SILPasses/Transforms.h"
#include "swift/SILPasses/Utils/Local.h"
#include "swift/SILPasses/Utils/CFG.h"
#include "swift/SILAnalysis/ValueTracking.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MathExtras.h"
#include <algorithm>

using namespace swift;

static llvm::cl::opt<bool> EnableGDSE("sil-enable-global-dse",
                                      llvm::cl::init(true), llvm::cl::Hidden);

STATISTIC(NumDeadStores, "Number of dead stores removed");

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
//                                  Location
//===----------------------------------------------------------------------===//

namespace {

using ProjectionTreeNodeList = llvm::ArrayRef<ProjectionTreeNode *>;
using ProjectionPathList = llvm::SmallVector<ProjectionPath, 8>;
using hash_code = llvm::hash_code;

/// A Location is an abstraction of an object field in program. It consists of a
/// base that is the tracked SILValue. In a subsequent commit this will be
/// expanded to include a path.
class Location {
public:
  enum KeyKind : uint8_t { EmptyKey = 0, TombstoneKey, NormalKey };

private:
  /// Empty key, tombstone key or normal key.
  KeyKind Kind;
  /// The base of the object.
  SILValue Base;
  /// The path to reach the accessed field of the object.
  Optional<ProjectionPath> Path;

public:
  /// Constructors.
  Location() : Base(), Kind(NormalKey) {}
  Location(SILValue B) : Base(B), Kind(NormalKey) { initialize(B); }
  Location(SILValue B, ProjectionPath &P, KeyKind Kind = NormalKey)
      : Base(B), Path(std::move(P)), Kind(Kind) {}

  /// Copy constructor.
  Location(const Location &RHS) {
    Base = RHS.Base;
    Path.reset();
    Kind = RHS.Kind;
    if (!RHS.Path.hasValue())
      return;
    ProjectionPath X;
    X.append(RHS.Path.getValue());
    Path = std::move(X);
  }

  Location &operator=(const Location &RHS) {
    Base = RHS.Base;
    Path.reset();
    Kind = RHS.Kind;
    if (!RHS.Path.hasValue())
      return *this;
    ProjectionPath X;
    X.append(RHS.Path.getValue());
    Path = std::move(X);
    return *this;
  }

  /// Getters and setters for Location.
  KeyKind getKind() const { return Kind; }
  void setKind(KeyKind K) { Kind = K; }
  SILValue getBase() const { return Base; }
  Optional<ProjectionPath> &getPath() { return Path; }

  /// Returns the hashcode for the location.
  hash_code getHashCode() const {
    hash_code HC = llvm::hash_combine(Base.getDef(), Base.getResultNumber(),
                                      Base.getType());
    if (!Path.hasValue())
      return HC;
    HC = llvm::hash_combine(HC, Path.getValue().getHashCode());
    return HC;
  }

  /// Return true if the 2 locations have identical projection path.
  /// If both locations have empty paths, they are treated as having
  /// identical projection path.
  bool hasIdenticalProjectionPath(const Location &RHS) const {
    // If both Paths have no value, then locations different.
    if (!Path.hasValue() && !RHS.Path.hasValue())
      return false;
    // If 1 Path has value while the other does not, then the 2 locations
    // are different.
    if (Path.hasValue() != RHS.Path.hasValue())
      return false;
    // If both Paths are empty, then 2 locations are the same.
    if (Path.getValue().empty() && RHS.Path.getValue().empty())
      return true;
    // If 2 Paths have different values, then the 2 locations are different.
    if (Path.getValue() != RHS.Path.getValue())
      return false;
    return true;
  }

  /// Comparisons.
  bool operator!=(const Location &RHS) const { return !(*this == RHS); }
  bool operator==(const Location &RHS) const {
    // If type is not the same, then locations different.
    if (Kind != RHS.Kind)
      return false;
    // If Base is different, then locations different.
    if (Base != RHS.Base)
      return false;

    // If the projection path is different, then locations different.
    if (!hasIdenticalProjectionPath(RHS))
      return false;

    // These locations represent the same memory location.
    return true;
  }

  /// Trace the given SILValue till the base of the accessed object. Also
  /// construct the projection path to the field accessed.
  void initialize(SILValue val);

  /// Expand this location to its individual fields by performing a DFS on the
  /// Projection Tree to find all the fields.
  ///
  /// In SIL, we can have a store to an aggregate and loads from its individual
  /// fields. Therefore, we expands all the operations on aggregates into
  /// individual fields.
  void enumerateAggProjection(ProjectionPathList &Paths, ProjectionPath &Path,
                              ProjectionTreeNode *Root,
                              ProjectionTreeNodeList Nodes);

  /// Given a SILType, return a list of ProjectionPaths to its individual
  /// fields.
  void enumerateAgg(SILModule *M, SILType Ty, ProjectionPathList &P,
                    llvm::BumpPtrAllocator &BPA);

  /// Expand this location to all individual fields it contains.
  void expand(SILModule *Mod, llvm::SmallVector<Location, 8> &F,
              llvm::BumpPtrAllocator &BPA, SILType Ty);
};

} // end anonymous namespace

void Location::initialize(SILValue Dest) {
  Base = getUnderlyingObject(Dest);
  Path = ProjectionPath::getAddrProjectionPath(Base, Dest);
}

void Location::expand(SILModule *Mod, llvm::SmallVector<Location, 8> &Locs,
                      llvm::BumpPtrAllocator &BPA, SILType Ty) {
  // Using this location as template and expand the aggregates represented
  // by this Location.
  ProjectionPathList Paths;
  enumerateAgg(Mod, Ty, Paths, BPA);
  for (auto &P : Paths) {
    ProjectionPath X;
    X.append(Path.getValue());
    Locs.push_back(Location(Base, X.append(P)));
  }
}

void Location::enumerateAggProjection(ProjectionPathList &Paths,
                                      ProjectionPath &Path,
                                      ProjectionTreeNode *Root,
                                      ProjectionTreeNodeList Nodes) {
  // If this is the field. keep its projection tree.
  if (Root->getChildProjections().empty()) {
    ProjectionPath X;
    X.append(Path);
    Paths.push_back(std::move(X));
    return;
  }

  for (auto &I : Root->getChildProjections()) {
    Path.push_back(*Nodes[I]->getProjection().getPointer());
    enumerateAggProjection(Paths, Path, Nodes[I], Nodes);
    Path.pop_back(); // Backtrack.
  }
}

void Location::enumerateAgg(SILModule *M, SILType Ty, ProjectionPathList &Paths,
                            llvm::BumpPtrAllocator &Allocator) {
  // Get the projection tree.
  ProjectionTree PT(*M, Allocator, Ty);

  // Start a depth first search on the projection tree to enumerate
  // each field of the projection.
  ProjectionPath Path;
  ProjectionTreeNode *Root = PT.getRoot();
  enumerateAggProjection(Paths, Path, Root, PT.getProjectionTreeNodes());
}

//===----------------------------------------------------------------------===//
//                       Basic Block Location State
//===----------------------------------------------------------------------===//

namespace {

/// BBState summarizes how Locations are used in a basic block.
///
/// Initially the WriteSetOut is empty. Before a basic block is processed, it is
/// initialized to the intersection of WriteSetIns of all successors of the
/// basic block.
///
/// TODO: The initial state of WriteSetIn should be all 1's. Otherwise the
/// dataflow solution will be too conservative in case of loops.
/// consider this case, the dead store by var a = 10 before the loop will not
/// be eliminated if the WriteSetIn is set to 0 initially. However, we can only
/// eliminate the dead stores after the data flow stablizes.
///
///   var a = 10
///   for _ in 0...1024 {}
///   a = 10
///
/// Initially WriteSetIn is empty. After the basic block is processed, if its
/// WriteSetOut is different from WriteSetIn, WriteSetIn is initialized to the
/// value of WriteSetOut and the data flow is rerun.
///
/// Instructions in each basic block are processed in post-order as follows:
///
/// 1. When a store instruction is encountered, the stored location is tracked.
///
/// 2. When a load instruction is encountered, remove the loaded location from
///    the WriteSetOut.
///
/// 3. When an instruction reads or writes to memory in an unknown way, the
///    WriteSetOut is cleared.
///
class BBState {
public:
  /// A bit vector for which the ith bit represents the ith Location in
  /// LocationVault. If the bit is set, then the location currently has an
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
  BBState(SILBasicBlock *B) : BB(B) {}

  /// Check whether the WriteSetIn has changed. If it does, we need to
  /// re-iterate to reach fixed point.
  bool updateWriteSetIn();

  /// Functions to manipulate the write set.
  void clearLocations();
  void startTrackingLocation(unsigned bit);
  void stopTrackingLocation(unsigned bit);
  bool isTrackingLocation(unsigned bit);
  void initialize(const BBState &L);
  /// *NOTE* Intersect can modify the input BBState. Given a basic block with
  /// multiple successors each of which have differing bitvector sizes,
  /// intersect will resize the bit vectors to the maximum of all bit vector
  /// sizes.
  void intersect(BBState &L);
};

} // end anonymous namespace

bool BBState::updateWriteSetIn() {
  if (WriteSetIn != WriteSetOut) {
    WriteSetIn = WriteSetOut;
    return true;
  }
  return false;
}

void BBState::clearLocations() { WriteSetOut.clear(); }

void BBState::startTrackingLocation(unsigned bit) {
  if (WriteSetOut.size() < bit + 1)
    WriteSetOut.resize(bit + 1);
  WriteSetOut.set(bit);
}

void BBState::stopTrackingLocation(unsigned bit) {
  if (WriteSetOut.size() < bit + 1)
    WriteSetOut.resize(bit + 1);
  WriteSetOut.reset(bit);
}

bool BBState::isTrackingLocation(unsigned bit) {
  if (WriteSetOut.size() < bit + 1)
    WriteSetOut.resize(bit + 1);
  return WriteSetOut.test(bit);
}

void BBState::initialize(const BBState &Succ) { WriteSetOut = Succ.WriteSetIn; }

void BBState::intersect(BBState &Succ) {
  unsigned max = std::max(WriteSetOut.size(), Succ.WriteSetIn.size());
  WriteSetOut.resize(max);
  Succ.WriteSetIn.resize(max);
  for (unsigned i = 0; i < max; ++i) {
    if (Succ.WriteSetIn.test(i))
      continue;
    // WriteSetIn is not set.
    stopTrackingLocation(i);
  }
}

namespace llvm {

static inline llvm::hash_code hash_value(const Location &L) {
  return llvm::hash_combine(L.getBase().getDef(), L.getBase().getResultNumber(),
                            L.getBase().getType());
}

template <> struct DenseMapInfo<Location> {
  static inline Location getEmptyKey() {
    Location L;
    L.setKind(Location::EmptyKey);
    return L;
  }
  static inline Location getTombstoneKey() {
    Location L;
    L.setKind(Location::TombstoneKey);
    return L;
  }
  static unsigned getHashValue(const Location &Loc) {
    return llvm::hash_value(Loc);
  }
  static bool isEqual(const Location &LHS, const Location &RHS) {
    if (LHS.getKind() == Location::EmptyKey &&
        RHS.getKind() == Location::EmptyKey)
      return true;
    if (LHS.getKind() == Location::TombstoneKey &&
        RHS.getKind() == Location::TombstoneKey)
      return true;
    return LHS == RHS;
  }
};

} // namespace llvm

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
  /// BBState is then laid on top of it to keep track of which Location
  /// has an upward visible store.
  std::vector<Location> LocationVault;

  /// Contains a map between location to their index in the LocationVault.
  llvm::DenseMap<Location, unsigned> LocToBitIndex;

  /// Return the BBState for the basic block this basic block belongs to.
  BBState *getBBLocState(SILBasicBlock *B) { return &BBToLocState[B]; }

  /// Return the BBState for the basic block this instruction belongs to.
  BBState *getBBLocState(SILInstruction *I) {
    return getBBLocState(I->getParent());
  }

  /// Location read has been extracted, expanded and mapped to the bit
  /// position in the bitvector. process it using the bit position.
  void updateWriteSetForRead(SILInstruction *Inst, BBState *State,
                             unsigned Bit);

  /// Location written has been extracted, expanded and mapped to the bit
  /// position in the bitvector. process it using the bit position.
  bool updateWriteSetForWrite(SILInstruction *Inst, BBState *State,
                              unsigned Bit);

  /// There is a read to a location, expand the location into individual fields
  /// before processing them.
  void processRead(SILInstruction *Inst, BBState *State, SILValue Value);

  /// There is a write to a location, expand the location into individual fields
  /// before processing them.
  void processWrite(SILInstruction *Inst, BBState *State, SILValue Value);

  /// Process Instructions. Extract Locations from SIL LoadInst.
  void processLoadInst(SILInstruction *Inst);

  /// Process Instructions. Extract Locations from SIL StoreInst.
  void processStoreInst(SILInstruction *Inst);

  /// Process Instructions. Extract Locations from SIL Unknown Memory Inst.
  void processUnknownMemInst(SILInstruction *Inst);

  /// Check whether the instruction invalidate any Locations due to change in
  /// its Location Base.
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
  /// the loop it will need to invalidate the Location in the WriteSetOut. i.e.
  /// the base of the Location is changed.
  ///
  /// If not, on the second iteration, the intersection of the successors of
  /// the loop basic block will have store to x.a and therefore x.a = 13 can now
  /// be considered dead.
  ///
  void invalidateLocationBase(SILInstruction *Inst);

  /// Check whether the 2 Locations may alias each other or not.
  bool isMayAliasingLocation(unsigned src, unsigned dst);

  /// Check whether the 2 Locations must alias each other or not.
  bool isMustAliasingLocation(unsigned src, unsigned dst);

  /// Get the bit representing the location in the LocationVault.
  ///
  /// NOTE: Adds the location to the location vault if necessary.
  unsigned getLocationBit(const Location &L);

public:
  /// Constructor.
  GlobalDeadStoreEliminationImpl(SILFunction *F, SILModule *M,
                                SILPassManager *PM, AliasAnalysis *AA)
      : Mod(M), F(F), PM(PM), AA(AA)  {}

  /// Compute the kill set for the basic block. return true if the store set
  /// changes.
  bool processBasicBlock(SILBasicBlock *BB);

  /// Intersect the successor live-ins.
  void mergeSuccessorsWriteIn(SILBasicBlock *BB);

  /// Update the BBState based on the given instruction.
  void processInstruction(SILInstruction *I);

  /// Entry point for global dead store elimination.
  void run();
};

} // end anonymous namespace

bool GlobalDeadStoreEliminationImpl::isMayAliasingLocation(unsigned src,
                                                           unsigned dst) {
  const Location &SL = LocationVault[src], DL = LocationVault[dst];
  // If the base does not alias, then the location can not alias.
  if (AA->isNoAlias(SL.getBase(), DL.getBase()))
    return false;
  // If projection paths are different, then the locations can not alias.
  if (!SL.hasIdenticalProjectionPath(DL))
    return false;
  return true;
}

bool GlobalDeadStoreEliminationImpl::isMustAliasingLocation(unsigned src,
                                                            unsigned dst) {
  const Location &SL = LocationVault[src], &DL = LocationVault[dst];
  // If the base is not must alias, the location may not alias.
  if (!AA->isMustAlias(SL.getBase(), DL.getBase()))
    return false;
  // If projection paths are different, then the locations can not alias.
  if (!SL.hasIdenticalProjectionPath(DL))
    return false;
  return true;
}

unsigned GlobalDeadStoreEliminationImpl::getLocationBit(const Location &Loc) {
  // Return the bit position of the given Loc in the LocationVault. The bit
  // position is then used to set/reset the bitvector kept by each BBState.
  //
  // See whether we can find it on the fast path. i.e. bit position looked up
  // through the hash table.
  auto Iter = LocToBitIndex.find(Loc);
  if (Iter != LocToBitIndex.end())
    return Iter->second;

  // At this point, we know we do not have the location, insert it into the
  // LocationVault and update the LocToBitIndex.
  LocToBitIndex[Loc] = LocationVault.size();
  LocationVault.push_back(Loc);
  return getLocationBit(Loc);
}

bool GlobalDeadStoreEliminationImpl::processBasicBlock(SILBasicBlock *BB) {
  // Intersect in the successor live-ins. A store is dead if it is not read from
  // any path to the end of the program. Thus an intersection.
  mergeSuccessorsWriteIn(BB);

  // Process instructions in post-order fashion.
  for (auto I = BB->rbegin(), E = BB->rend(); I != E; ++I) {
    processInstruction(&(*I));
  }

  // If WriteSetIn changes, then keep iterating until reached a fixed
  // point.
  return getBBLocState(BB)->updateWriteSetIn();
}

void GlobalDeadStoreEliminationImpl::mergeSuccessorsWriteIn(SILBasicBlock *BB) {
  // First, clear the WriteSetOut for the current basicblock.
  BBState *C = getBBLocState(BB);
  C->clearLocations();

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

void GlobalDeadStoreEliminationImpl::invalidateLocationBase(SILInstruction *I) {
  BBState *S = getBBLocState(I);
  // If this instruction defines the base of a location, then we need to
  // invalidate any locations with the same base.
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->WriteSetOut.test(i))
      continue;
    if (LocationVault[i].getBase().getDef() != I)
      continue;
    S->stopTrackingLocation(i);
  }
}

void GlobalDeadStoreEliminationImpl::updateWriteSetForRead(SILInstruction *I,
                                                           BBState *S,
                                                           unsigned bit) {
  // Remove any may/must-aliasing stores to the Location, as they cant be used
  // to kill any upward visible stores due to the intefering load.
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->isTrackingLocation(i))
      continue;
    if (!isMayAliasingLocation(i, bit))
      continue;
    DEBUG(llvm::dbgs() << "Loc Removal: " << LocationVault[i].getBase()
                       << "Instruction: " << *I << "\n");
    S->stopTrackingLocation(i);
  }
}

bool GlobalDeadStoreEliminationImpl::updateWriteSetForWrite(SILInstruction *I,
                                                            BBState *S,
                                                            unsigned bit) {
  // If a tracked store must aliases with this store, then this store is dead.
  bool IsDead = false;
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->isTrackingLocation(i))
      continue;
    // If 2 locations may alias, we can still keep both stores.
    if (!isMustAliasingLocation(i, bit))
      continue;
    IsDead = true;
    // No need to check the rest of the upward visible stores as this store
    // is dead.
    break;
  }

  // Track this new store.
  DEBUG(llvm::dbgs() << "Loc Insertion: " << LocationVault[bit].getBase()
                     << "Instruction: " << *I << "\n");
  S->startTrackingLocation(bit);
  return IsDead;
}

void GlobalDeadStoreEliminationImpl::processRead(SILInstruction *I, BBState *S,
                                                 SILValue Mem) {
  // Construct a Location to represent the memory read by this instruction.
  Location L(Mem);

  // We cant figure out the Base or Projection Path for the read instruction,
  // process it as an unknown memory instruction for now.
  if (!L.getBase() || !L.getPath().hasValue()) {
    processUnknownMemInst(I);
    return;
  }

  // Expand the Mem with given into individual fields and process them as
  // separate reads.
  llvm::SmallVector<Location, 8> Locs;
  L.expand(&I->getModule(), Locs, BPA, Mem.getType().getObjectType());
  for (auto &E : Locs) {
    updateWriteSetForRead(I, S, getLocationBit(E));
  }
}

void GlobalDeadStoreEliminationImpl::processWrite(SILInstruction *I, BBState *S,
                                                  SILValue Mem) {
  // Construct a Location to represent the memory written by this instruction.
  Location L(Mem);

  // We cant figure out the Base or Projection Path for the store instruction,
  // simply ignore it for now.
  if (!L.getBase() || !L.getPath().hasValue())
    return;

  // Expand the Mem into individual fields and process them as separate writes.
  bool Dead = true;
  llvm::SmallVector<Location, 8> Locs;
  L.expand(&I->getModule(), Locs, BPA, Mem.getType().getObjectType());
  for (auto &E : Locs) {
    Dead &= updateWriteSetForWrite(I, S, getLocationBit(E));
  }

  // Stores to all the components are dead, therefore this instruction is dead.
  //
  // TODO: handle partially dead store.
  //
  if (Dead) {
    DEBUG(llvm::dbgs() << "Instruction Dead: " << *I << "\n");
    S->DeadStores.insert(I);
  }
}

void GlobalDeadStoreEliminationImpl::processLoadInst(SILInstruction *I) {
  // Loading a loadable type.
  SILValue Mem = cast<LoadInst>(I)->getOperand();
  processRead(I, getBBLocState(I), Mem);
}

void GlobalDeadStoreEliminationImpl::processStoreInst(SILInstruction *I) {
  // Storing a loadable type.
  SILValue Mem = cast<StoreInst>(I)->getDest();
  processWrite(I, getBBLocState(I), Mem);
}

void GlobalDeadStoreEliminationImpl::processUnknownMemInst(SILInstruction *I) {
  // We do not know what this instruction does or the memory that it *may*
  // touch. Hand it to alias analysis to see whether we need to invalidate
  // any Location.
  BBState *S = getBBLocState(I);
  for (unsigned i = 0; i < S->WriteSetOut.size(); ++i) {
    if (!S->isTrackingLocation(i))
      continue;
    if (!AA->mayReadFromMemory(I, LocationVault[i].getBase()))
      continue;
    getBBLocState(I)->stopTrackingLocation(i);
  }
}

void GlobalDeadStoreEliminationImpl::processInstruction(SILInstruction *I) {
  // If this instruction has side effects, but is inert from a store
  // perspective, skip it.
  if (isDeadStoreInertInstruction(I))
    return;

  // A set of ad-hoc rules to process instructions.
  if (isa<LoadInst>(I)) {
    processLoadInst(I);
  } else if (isa<StoreInst>(I)) {
    processStoreInst(I);
  } else if (I->mayReadOrWriteMemory()) {
    processUnknownMemInst(I);
  }

  // Check whether this instruction will invalidate any other Locations.
  invalidateLocationBase(I);
}

void GlobalDeadStoreEliminationImpl::run() {
  // For all basic blocks in the function, initialize a BB state.
  for (auto &B : *F) {
    BBToLocState[&B] = BBState(&B);
  }

  auto *PO = PM->getAnalysis<PostOrderAnalysis>()->get(F);
  // Keep iterating over all basicblocks in a post-order fashion until
  // convergence. Everytime the WriteSetIn of a basic block changes, the
  // optimization is rerun.
  //
  // TODO: We only need to rerun basicblocks with successors changed.
  // use a worklist in the future.
  bool Changed = false;
  do {
    Changed = false;
    for (SILBasicBlock *BB : PO->getPostOrder()) {
      Changed |= processBasicBlock(BB);
    }
  } while (Changed);

  // Finally, delete the dead stores.
  for (SILBasicBlock *BB : PO->getPostOrder()) {
    for (auto &I : getBBLocState(BB)->DeadStores) {
      DEBUG(llvm::dbgs() << "*** Removing: " << *I << " ***\n");
      I->eraseFromParent();
      ++NumDeadStores;
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
    if (!EnableGDSE)
      return;

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

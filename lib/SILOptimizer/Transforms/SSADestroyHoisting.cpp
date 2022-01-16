//===--- SSADestroyHoisting.cpp - SSA-based destroy hoisting --------------===//
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
/// This is a light-weight utility for hoisting destroy instructions for unique
/// storage--typically alloc_stac or owned incoming arguments. Shrinking an
/// object's memory lifetime can allow removal of copy_addr and other
/// optimization.
///
/// This algorithm is:
/// - Incremental
/// - SSA-based
/// - Canonical
/// - Free from alias analysis
///
/// Incremental: Handle a single in-memory value at a time. The value's address
/// typically originates from an alloc_stack or owned function argument
/// (@in). It does not depend on any analysis result, which would need to be
/// preserved by a pass.
///
/// SSA-based: Starting with uniquely identified (exclusive) storage,
/// discovers all known uses based on recognizable SIL patterns. Bails-out on
/// unknown uses. Derivation of a raw pointer is considered a "known use".
///
/// Canonical: Assumes that aggregate values, which are allocated in a single
/// operation, are also destroyed in a single operation. This canonical form is
/// not fully enforced, so violations result in a bail-out.
///
/// Free from alias analysis: this only handles exclusively identified
/// addresses to owned values, which cannot be derived from object references.
///
/// ----------------------------------------------------------------------------
///
/// DestroyAddr hoisting stops at either a direct use, or a deinitialization
/// barrier. Direct uses are checked by guaranteeing that all storage uses are
/// known.
///
/// Deinitialization barriers:
///
/// Case #1. Weak reference loads: Any load of a weak or unowned referenceto an
/// object that may be deallocated when this variable is destroyed. Any use of
/// the weak reference is considered a barrier, even if the referenced object is
/// not accessed. This only applies to loads within the current lexical
/// scope. Programmers must properly check escaping weak references for null.
///
/// Case #2. Derived pointers: Any memory access based on a raw pointer to
/// memory that may be deallocated when this variable is destroyed. This only
/// applies to pointer access within this variable's lexical scope. Programmers
/// must manage escaping pointers explicitly via Builtin.fixLifetime.
///
/// Case #3. Synchronization points: If the object potentially has a custom
/// deinitializer with side effects, then any external function call, which may
/// contain a memory barrier or system call, prevents hoisting. If the external
/// function call is annotated as "read-only", then it is safe. Since Swift does
/// not directly support atomics, no SIL instructions are currently considered
/// synchronization points.
///
/// ----------------------------------------------------------------------------
///
/// TODO: replace the destroy hoisting in CopyForwarding::forwardCopiesOf and
/// ensure related tests still pass. This requires hoisting over certain
/// calls. We can do this as long as the call takes a copy of the storage value
/// as an argument. The copy will be guarded by the callee's lexical scope, so
/// the deinits cannot be invoked by the hoisted destroy (in fact it should be
/// possible to eliminate the destroy).
///
/// TODO: As a utility, hoistDestroys should be repeatable. Subsequent runs
/// without changing input should have no effect, including putting new
/// instructions on a worklist. MergeDestroys currently breaks this because the
/// destroys are inserted first before they are merged. This will trigger the
/// createdNewInst callback and cause hadCallbackInvocation() to return true
/// even when the merged result is identical to the input. Fix this by keeping
/// track of the newly created destroys, defer calling createdNewInst, and defer
/// deleting dead instructions. When merging, check if the merged destroy is
/// inserted at the old destroy to reuse it and bypass triggering callbacks.
///
/// TODO: enforce an invariant that destroy_addrs jointly post-dominate any
/// exclusive owned address, that would simplify the algorithm.
///
/// ===--------------------------------------------------------------------===//

#define DEBUG_TYPE "ssa-destroy-hoisting"

#include "swift/Basic/GraphNodeWorklist.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/Reachability.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"

using namespace swift;

namespace {

/// Step #1: Find all known uses of the unique storage object.
struct KnownStorageUses : UniqueStorageUseVisitor {
  bool preserveDebugInfo;

  SmallPtrSet<SILInstruction *, 16> storageUsers;
  SmallVector<SILInstruction *, 4> originalDestroys;
  SmallPtrSet<SILInstruction *, 4> debugInsts;

  KnownStorageUses(AccessStorage storage, SILFunction *function)
      : UniqueStorageUseVisitor(storage, function),
        preserveDebugInfo(function->preserveDebugInfo()) {}

  bool empty() const {
    return storageUsers.empty() && originalDestroys.empty()
           && debugInsts.empty();
  }

  SILFunction *getFunction() const { return function; }

  AccessStorage getStorage() const { return storage; }

  // Return true if all leaf users of the root address are recognized.
  //
  // Populate addressUsers, originalDestroys, and debugInsts.
  bool findUses() {
    assert(empty() && "already initialized");

    return UniqueStorageUseVisitor::findUses(*this);
  }

protected:
  KnownStorageUses(KnownStorageUses const &) = delete;
  KnownStorageUses &operator=(KnownStorageUses const &) = delete;

  bool recordUser(SILInstruction *user) {
    storageUsers.insert(user);
    return true;
  }

  bool visitLoad(Operand *use) override { return recordUser(use->getUser()); }

  bool visitStore(Operand *use) override { return recordUser(use->getUser()); }

  bool visitDestroy(Operand *use) override {
    originalDestroys.push_back(use->getUser());
    return true;
  }

  bool visitDealloc(Operand *use) override { return true; }

  bool visitDebugUse(Operand *use) override {
    if (preserveDebugInfo) {
      storageUsers.insert(use->getUser());
    } else {
      debugInsts.insert(use->getUser());
    }
    return true;
  }

  bool visitUnknownUse(Operand *use) override {
    auto *user = use->getUser();
    // Recognize any leaf users not already recognized by UniqueAddressUses.
    //
    // Destroy hoisting considers address_to_pointer to be a leaf use because
    // any potential pointer access is already considered to be a
    // deinitializtion barrier.
    if (isa<PointerToAddressInst>(user)) {
      storageUsers.insert(use->getUser());
      return true;
    }
    LLVM_DEBUG(llvm::dbgs() << "Unknown user " << *user);
    return false;
  }
};

/// Step #2: Perform backward dataflow from KnownStorageUses.originalDestroys to
/// KnownStorageUses.storageUsers to find deinitialization barriers.
class DeinitBarriers {
public:
  // Data flow state: blocks whose beginning is backward reachable from a
  // destroy without first reaching a barrier or storage use.
  BasicBlockSetVector destroyReachesBeginBlocks;

  // Data flow state: blocks whose end is backward reachable from a destroy
  // without first reaching a barrier or storage use.
  BasicBlockSet destroyReachesEndBlocks;

  // Deinit barriers or storage uses within a block, reachable from a destroy.
  SmallVector<SILInstruction *, 4> barriers;

  // Debug instructions that are no longer within this lifetime after shrinking.
  SmallVector<SILInstruction *, 4> deadUsers;

  explicit DeinitBarriers(SILFunction *function)
    : destroyReachesBeginBlocks(function),
      destroyReachesEndBlocks(function)
  {}

  void compute(const KnownStorageUses &knownUses) {
    DestroyReachability(knownUses, *this).solveBackward();
  }

private:
  // Conforms to BackwardReachability::BlockReachability
  class DestroyReachability {
    const KnownStorageUses &knownUses;
    DeinitBarriers &result;
    SILInstruction *storageDefInst = nullptr; // null for function args

    BackwardReachability<DestroyReachability> reachability;

  public:
    DestroyReachability(const KnownStorageUses &knownUses,
                        DeinitBarriers &result)
        : knownUses(knownUses), result(result),
          reachability(knownUses.getFunction(), *this) {

      auto rootValue = knownUses.getStorage().getRoot();
      assert(rootValue && "HoistDestroys requires a single storage root");
      storageDefInst = rootValue->getDefiningInstruction();

      // Seed backward reachability with destroy points.
      for (SILInstruction *destroy : knownUses.originalDestroys) {
        reachability.initLastUse(destroy);
      }
    }

    bool hasReachableBegin(SILBasicBlock *block) {
      return result.destroyReachesBeginBlocks.contains(block);
    }

    void markReachableBegin(SILBasicBlock *block) {
      result.destroyReachesBeginBlocks.insert(block);
    }

    void markReachableEnd(SILBasicBlock *block) {
      result.destroyReachesEndBlocks.insert(block);
    }

    bool checkReachableBarrier(SILInstruction *inst);

    void solveBackward() { reachability.solveBackward(); }
  };
};

/// Return true if \p inst is a barrier.
///
/// Called exactly once for each reachable instruction. This is guaranteed to
/// hold as a barrier occurs between any original destroys that are reachable
/// from each. Any path reaching multiple destroys requires initialization,
/// which is a storageUser and therefore a barrier.
bool DeinitBarriers::DestroyReachability::checkReachableBarrier(
    SILInstruction *inst) {
  if (knownUses.debugInsts.contains(inst)) {
    result.deadUsers.push_back(inst);
    return false;
  }
  if (inst == storageDefInst) {
    result.barriers.push_back(inst);
    return true;
  }
  if (knownUses.storageUsers.contains(inst)) {
    result.barriers.push_back(inst);
    return true;
  }
  if (isDeinitBarrier(inst)) {
    result.barriers.push_back(inst);
    return true;
  }
  return false;
}

/// Algorithm for hoisting the destroys of a single uniquely identified storage
/// object.
class HoistDestroys {
  SILValue storageRoot;
  InstructionDeleter &deleter;

  // Book-keeping for the rewriting stage.
  SmallPtrSet<SILInstruction *, 4> reusedDestroys;

  BasicBlockSetVector destroyMergeBlocks;

public:
  HoistDestroys(SILValue storageRoot, InstructionDeleter &deleter)
    : storageRoot(storageRoot), deleter(deleter),
      destroyMergeBlocks(getFunction()) {}

  bool perform();

protected:
  SILFunction *getFunction() const { return storageRoot->getFunction(); }

  bool foldBarrier(SILInstruction *barrier);

  void insertDestroy(SILInstruction *barrier, SILInstruction *insertBefore,
                     const KnownStorageUses &knownUses);

  void createDestroy(SILInstruction *insertBefore,
                     const SILDebugScope *scope);

  void createSuccessorDestroys(SILBasicBlock *barrierBlock);

  bool rewriteDestroys(const KnownStorageUses &knownUses,
                       const DeinitBarriers &deinitBarriers);

  void mergeDestroys(SILBasicBlock *mergeBlock);
};

} // namespace

bool HoistDestroys::perform() {
  auto storage = AccessStorage::compute(storageRoot);
  if (!storage.isUniquelyIdentified())
    return false;

  KnownStorageUses knownUses(storage, getFunction());
  if (!knownUses.findUses())
    return false;

  DeinitBarriers deinitBarriers(getFunction());
  deinitBarriers.compute(knownUses);

  // No SIL changes happen before rewriting.
  return rewriteDestroys(knownUses, deinitBarriers);
}

bool HoistDestroys::rewriteDestroys(const KnownStorageUses &knownUses,
                                    const DeinitBarriers &deinitBarriers) {
  // Place a new destroy after each barrier instruction.
  for (SILInstruction *barrier : deinitBarriers.barriers) {
    auto *barrierBlock = barrier->getParent();
    if (barrier != barrierBlock->getTerminator()) {
      if (!foldBarrier(barrier))
        insertDestroy(barrier, barrier->getNextInstruction(), knownUses);
      continue;
    }
    for (auto *successor : barrierBlock->getSuccessorBlocks()) {
      insertDestroy(barrier, &successor->front(), knownUses);
    }
  }
  // Place a new destroy at each CFG edge in which the successor's beginning is
  // reached but the predecessors end is not reached.
  for (auto *beginReachedBlock : deinitBarriers.destroyReachesBeginBlocks) {
    SILInstruction *barrier = nullptr;
    if (auto *predecessor = beginReachedBlock->getSinglePredecessorBlock()) {
      if (deinitBarriers.destroyReachesEndBlocks.contains(predecessor))
        continue;

      barrier = predecessor->getTerminator();

    } else if (!beginReachedBlock->pred_empty()) {
      // This is the only successor, so the destroy must reach the predecessors.
      assert(llvm::all_of(
          beginReachedBlock->getPredecessorBlocks(), [&](auto *predecessor) {
            return deinitBarriers.destroyReachesEndBlocks.contains(predecessor);
          }));
      continue;
    }
    // The destroy does not reach the end of any predecessors.
    insertDestroy(barrier, &beginReachedBlock->front(), knownUses);
  }
  // Delete dead users before merging destroys.
  for (auto *deadInst : deinitBarriers.deadUsers) {
    deleter.forceDelete(deadInst);
  }
  for (auto *destroyInst : knownUses.originalDestroys) {
    if (reusedDestroys.contains(destroyInst))
      continue;

    deleter.forceDelete(destroyInst);
  }
  deleter.cleanupDeadInstructions();

  for (auto *mergeBlock : destroyMergeBlocks) {
    mergeDestroys(mergeBlock);
  }
  return deleter.hadCallbackInvocation();
}

bool HoistDestroys::foldBarrier(SILInstruction *barrier) {
  if (auto *load = dyn_cast<LoadInst>(barrier)) {
    if (load->getOperand() == storageRoot) {
      assert(load->getOwnershipQualifier() == LoadOwnershipQualifier::Copy);
      load->setOwnershipQualifier(LoadOwnershipQualifier::Take);
      return true;
    }
  }
  if (auto *copy = dyn_cast<CopyAddrInst>(barrier)) {
    if (copy->getSrc() == storageRoot) {
      assert(!copy->isTakeOfSrc());
      copy->setIsTakeOfSrc(IsTake);
      return true;
    }
  }
  return false;
}

// \p barrier may be null if the destroy is at function entry.
void HoistDestroys::insertDestroy(SILInstruction *barrier,
                                  SILInstruction *insertBefore,
                                  const KnownStorageUses &knownUses) {
  if (auto *branch = dyn_cast<BranchInst>(insertBefore)) {
    destroyMergeBlocks.insert(branch->getDestBB());
  }
  // Avoid mutating SIL for no reason. This could lead to infinite loops.
  if (isa<DestroyAddrInst>(insertBefore)
      || isa<DestroyValueInst>(insertBefore)) {
    if (llvm::find(knownUses.originalDestroys, insertBefore)
        != knownUses.originalDestroys.end()) {
      reusedDestroys.insert(insertBefore);
      return;
    }
  }
  const SILDebugScope *scope = barrier
    ? barrier->getDebugScope() : getFunction()->getDebugScope();
  createDestroy(insertBefore, scope);
}

void HoistDestroys::createDestroy(SILInstruction *insertBefore,
                                  const SILDebugScope *scope) {
  auto loc = RegularLocation::getAutoGeneratedLocation();
  SILInstruction *newDestroy;
  if (storageRoot->getType().isAddress()) {
    newDestroy =
      SILBuilder(insertBefore, scope).createDestroyAddr(loc, storageRoot);
  } else {
    newDestroy =
      SILBuilder(insertBefore, scope).createDestroyValue(loc, storageRoot);
  }
  deleter.getCallbacks().createdNewInst(newDestroy);
}

void HoistDestroys::mergeDestroys(SILBasicBlock *mergeBlock) {
  SmallVector<SILInstruction *, 4> deadDestroys;
  for (auto *predecessors : mergeBlock->getPredecessorBlocks()) {
    auto *tailDestroy = predecessors->getTerminator()->getPreviousInstruction();
    if (!tailDestroy || (!isa<DestroyAddrInst>(tailDestroy)
                         && !isa<DestroyValueInst>(tailDestroy))) {
      return;
    }
    if (tailDestroy->getOperand(0) != storageRoot)
      return;

    deadDestroys.push_back(tailDestroy);
  }
  if (deadDestroys.size() < 2) // ignore trivial fall-thru
    return;

  createDestroy(&mergeBlock->front(), deadDestroys[0]->getDebugScope());

  for (auto *deadDestroy : deadDestroys) {
    deleter.forceDelete(deadDestroy);
  }
}

// =============================================================================
// Top-Level API
// =============================================================================

bool hoistDestroys(SILValue root, InstructionDeleter &deleter) {
  LLVM_DEBUG(llvm::dbgs() << "Performing destroy hoisting on " << root);

  SILFunction *function = root->getFunction();
  if (!function)
    return false;

  // The algorithm assumes no critical edges.
  assert(function->hasOwnership() && "requires OSSA");

  return HoistDestroys(root, deleter).perform();
}

// =============================================================================
// Pipeline Pass
// =============================================================================

namespace {
class SSADestroyHoisting : public swift::SILFunctionTransform {
  void run() override;
};
} // end anonymous namespace

// TODO: Handle alloc_box the same way, as long as the box doesn't escape.
//
// TODO: Handle address and boxes that are captured in no-escape closures.
void SSADestroyHoisting::run() {
  if (!getFunction()->hasOwnership())
    return;

  InstructionDeleter deleter;
  bool changed = false;
  for (auto *arg : getFunction()->getArguments()) {
    if (arg->getType().isAddress()) {
      changed |= hoistDestroys(arg, deleter);
    }
  }
  for (auto &block : *getFunction()) {
    for (auto &inst : block) {
      if (auto *alloc = dyn_cast<AllocStackInst>(&inst)) {
        changed |= hoistDestroys(alloc, deleter);
      }
    }
  }
  if (changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

SILTransform *swift::createSSADestroyHoisting() {
  return new SSADestroyHoisting();
}

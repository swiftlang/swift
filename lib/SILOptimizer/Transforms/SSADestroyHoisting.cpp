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
/// storage--typically alloc_stack or owned incoming arguments. Shrinking an
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
#include "swift/Basic/SmallPtrSetVector.h"
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

  bool visitBeginAccess(Operand *use) override {
    auto *bai = cast<BeginAccessInst>(use->getUser());
    for (auto *eai : bai->getEndAccesses()) {
      storageUsers.insert(eai);
    }
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
  SmallPtrSetVector<SILBasicBlock *, 4> destroyReachesBeginBlocks;

  // Data flow state: blocks whose end is backward reachable from a destroy
  // without first reaching a barrier or storage use.
  SmallPtrSet<SILBasicBlock *, 4> destroyReachesEndBlocks;

  // Deinit barriers or storage uses within a block, reachable from a destroy.
  SmallVector<SILInstruction *, 4> barriers;

  // Debug instructions that are no longer within this lifetime after shrinking.
  SmallVector<SILInstruction *, 4> deadUsers;

  // The access scopes which are hoisting barriers.
  //
  // They are hoisting barriers if they include any barriers.  We need to be
  // sure not to hoist a destroy_addr into an access scope and by doing so cause
  // a deinit which had previously executed outside an access scope to start
  // executing within it--that could violate exclusivity.
  llvm::SmallPtrSet<BeginAccessInst *, 8> barrierAccessScopes;

  explicit DeinitBarriers(bool ignoreDeinitBarriers,
                          const KnownStorageUses &knownUses,
                          SILFunction *function)
      : ignoreDeinitBarriers(ignoreDeinitBarriers), knownUses(knownUses) {
    auto rootValue = knownUses.getStorage().getRoot();
    assert(rootValue && "HoistDestroys requires a single storage root");
    // null for function args
    storageDefInst = rootValue->getDefiningInstruction();
  }

  void compute() {
    FindBarrierAccessScopes(*this).solveBackward();
    if (barrierAccessScopes.size() == 0)
      return;
    destroyReachesBeginBlocks.clear();
    destroyReachesEndBlocks.clear();
    barriers.clear();
    deadUsers.clear();
    DestroyReachability(*this).solveBackward();
  }

  bool isBarrier(SILInstruction *instruction) const {
    return classificationIsBarrier(
        classifyInstruction(instruction, ignoreDeinitBarriers, storageDefInst,
                            barrierAccessScopes, knownUses));
  };

private:
  DeinitBarriers(DeinitBarriers const &) = delete;
  DeinitBarriers &operator=(DeinitBarriers const &) = delete;

  bool ignoreDeinitBarriers;
  const KnownStorageUses &knownUses;
  SILInstruction *storageDefInst = nullptr;

  enum class Classification { DeadUser, Barrier, Other };

  Classification classifyInstruction(SILInstruction *inst) {
    return classifyInstruction(inst, ignoreDeinitBarriers, storageDefInst,
                               barrierAccessScopes, knownUses);
  }

  static Classification classifyInstruction(
      SILInstruction *inst, bool ignoreDeinitBarriers,
      SILInstruction *storageDefInst,
      const llvm::SmallPtrSetImpl<BeginAccessInst *> &barrierAccessScopes,
      const KnownStorageUses &knownUses);

  void visitedInstruction(SILInstruction *instruction,
                          Classification classification);

  static bool classificationIsBarrier(Classification classification);

  // Implements BackwardReachability::BlockReachability
  //
  // Determine which end_access instructions must be treated as barriers.
  //
  // An end_access is a barrier if the access scope it ends contains any deinit
  // barriers.  Suppose that it weren't treated as a barrier.  Then the
  // destroy_addr would be hoisted up to the in-scope deinit barrier.  That
  // could result in a deinit being executed within the scope which was
  // previously executed outside it.  Executing a deinit in the scope could
  // violate exclusivity.
  //
  // So before determining what ALL the barriers are, we need to determine which
  // end_access instructions are barriers.  Do that by observing which access
  // scopes are open when encountering a barrier.  The access scopes which are
  // open are those for which we've seen an end_access instruction when walking
  // backwards from the destroy_addrs.  Add these access scopes to
  // DeinitBarriers::barrierAccessScopes.
  //
  // Tracking which access scopes are open consists of two parts:
  // (1) in-block analysis
  // (2) cross-block analysis
  // For (1), maintain a set of access scopes which are currently open.  Insert
  // and erase scopes when seeing begin_access and end_access instructions when
  // they're visited in checkReachableBarrier.  A stack can't be used here
  // because access scopes are not necessarily nested.
  // For (2), when entering a block, the access scope is the union of all the
  // open access scopes in the block's predecessors.
  class FindBarrierAccessScopes {
    DeinitBarriers &result;
    llvm::DenseMap<SILBasicBlock *, llvm::SmallPtrSet<BeginAccessInst *, 2>>
        liveInAccessScopes;
    llvm::SmallPtrSet<BeginAccessInst *, 2> runningLiveAccessScopes;

    BackwardReachability<FindBarrierAccessScopes> reachability;

  public:
    FindBarrierAccessScopes(DeinitBarriers &result)
        : result(result), reachability(result.knownUses.getFunction(), *this) {
      // Seed backward reachability with destroy points.
      for (SILInstruction *destroy : result.knownUses.originalDestroys) {
        reachability.initLastUse(destroy);
      }
    }

    void markLiveAccessScopesAsBarriers() {
      for (auto *scope : runningLiveAccessScopes) {
        result.barrierAccessScopes.insert(scope);
      }
    }

    bool hasReachableBegin(SILBasicBlock *block) {
      return result.destroyReachesBeginBlocks.contains(block);
    }

    void markReachableBegin(SILBasicBlock *block) {
      result.destroyReachesBeginBlocks.insert(block);
      if (!runningLiveAccessScopes.empty()) {
        liveInAccessScopes[block] = runningLiveAccessScopes;
      }
    }

    void markReachableEnd(SILBasicBlock *block) {
      result.destroyReachesEndBlocks.insert(block);
      runningLiveAccessScopes.clear();
      for (auto *predecessor : block->getPredecessorBlocks()) {
        auto iterator = liveInAccessScopes.find(predecessor);
        if (iterator != liveInAccessScopes.end()) {
          for (auto *bai : iterator->getSecond()) {
            runningLiveAccessScopes.insert(bai);
          }
        }
      }
    }

    bool checkReachableBarrier(SILInstruction *inst) {
      // For correctness, it is required that
      // FindBarrierAccessScopes::checkReachableBarrier return true whenever
      // DestroyReachability::checkReachableBarrier does, with one exception:
      // DestryReachability::checkReachableBarrier will also return true for any
      // end_access barrier that FindBarrierAccessScopes finds.
      if (auto *eai = dyn_cast<EndAccessInst>(inst)) {
        runningLiveAccessScopes.insert(eai->getBeginAccess());
      } else if (auto *bai = dyn_cast<BeginAccessInst>(inst)) {
        runningLiveAccessScopes.erase(bai);
      }
      auto classification = result.classifyInstruction(inst);
      result.visitedInstruction(inst, classification);
      auto isBarrier = result.classificationIsBarrier(classification);
      if (isBarrier) {
        markLiveAccessScopesAsBarriers();
      }
      // If we've seen a barrier, then we can stop looking for access scopes.
      // Any that were open already have now been marked as barriers.  And if
      // none are open, the second data flow won't get beyond this barrier to
      // face subsequent end_access instructions.
      return isBarrier;
    }

    bool checkReachablePhiBarrier(SILBasicBlock *block) {
      bool isBarrier =
          llvm::any_of(block->getPredecessorBlocks(), [&](auto *predecessor) {
            return result.isBarrier(predecessor->getTerminator());
          });
      if (isBarrier) {
        // If there's a barrier preventing us from hoisting out of this block,
        // then every open access scope contains a barrier, so all the
        // corresponding end_access instructions are barriers too.
        markLiveAccessScopesAsBarriers();
      }
      return isBarrier;
    }

    void solveBackward() { reachability.solveBackward(); }
  };

  // Conforms to BackwardReachability::BlockReachability
  class DestroyReachability {
    DeinitBarriers &result;

    BackwardReachability<DestroyReachability> reachability;

  public:
    DestroyReachability(DeinitBarriers &result)
        : result(result), reachability(result.knownUses.getFunction(), *this) {
      // Seed backward reachability with destroy points.
      for (SILInstruction *destroy : result.knownUses.originalDestroys) {
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

    bool checkReachableBarrier(SILInstruction *);

    bool checkReachablePhiBarrier(SILBasicBlock *);

    void solveBackward() { reachability.solveBackward(); }
  };
};

DeinitBarriers::Classification DeinitBarriers::classifyInstruction(
    SILInstruction *inst, bool ignoreDeinitBarriers,
    SILInstruction *storageDefInst,
    const llvm::SmallPtrSetImpl<BeginAccessInst *> &barrierAccessScopes,
    const KnownStorageUses &knownUses) {
  if (knownUses.debugInsts.contains(inst)) {
    return Classification::DeadUser;
  }
  if (inst == storageDefInst) {
    return Classification::Barrier;
  }
  if (knownUses.storageUsers.contains(inst)) {
    return Classification::Barrier;
  }
  if (!ignoreDeinitBarriers && isDeinitBarrier(inst)) {
    return Classification::Barrier;
  }
  if (auto *eai = dyn_cast<EndAccessInst>(inst)) {
    return barrierAccessScopes.contains(eai->getBeginAccess())
               ? Classification::Barrier
               : Classification::Other;
  }
  return Classification::Other;
}

bool DeinitBarriers::classificationIsBarrier(Classification classification) {
  switch (classification) {
  case Classification::DeadUser:
  case Classification::Other:
    return false;
  case Classification::Barrier:
    return true;
  }
  llvm_unreachable("exhaustive switch is not exhaustive?!");
}

void DeinitBarriers::visitedInstruction(SILInstruction *instruction,
                                        Classification classification) {
  assert(classifyInstruction(instruction) == classification);
  switch (classification) {
  case Classification::DeadUser:
    deadUsers.push_back(instruction);
    break;
  case Classification::Barrier:
    barriers.push_back(instruction);
    break;
  case Classification::Other:
    break;
  }
}

/// Return true if \p inst is a barrier.
///
/// Called exactly once for each reachable instruction. This is guaranteed to
/// hold as a barrier occurs between any original destroys that are reachable
/// from each. Any path reaching multiple destroys requires initialization,
/// which is a storageUser and therefore a barrier.
bool DeinitBarriers::DestroyReachability::checkReachableBarrier(
    SILInstruction *instruction) {
  // For correctness, it is required that
  // DestroyReachability::checkReachableBarrier return true whenever
  // FindBarrierAccessScopes::checkReachableBarrier does.  It must additionally
  // return true when encountering an end_access barrier that
  // FindBarrierAccessScope determined is a barrier.
  auto classification = result.classifyInstruction(instruction);
  result.visitedInstruction(instruction, classification);
  return result.classificationIsBarrier(classification);
}

bool DeinitBarriers::DestroyReachability::checkReachablePhiBarrier(
    SILBasicBlock *block) {
  assert(llvm::all_of(block->getArguments(),
                      [&](auto argument) { return PhiValue(argument); }));
  return llvm::any_of(block->getPredecessorBlocks(), [&](auto *predecessor) {
    return result.isBarrier(predecessor->getTerminator());
  });
}

/// Algorithm for hoisting the destroys of a single uniquely identified storage
/// object.
class HoistDestroys {
  SILValue storageRoot;
  bool ignoreDeinitBarriers;
  SmallPtrSetImpl<SILInstruction *> &remainingDestroyAddrs;
  InstructionDeleter &deleter;

  // Book-keeping for the rewriting stage.
  SmallPtrSet<SILInstruction *, 4> reusedDestroys;

  BasicBlockSetVector destroyMergeBlocks;

public:
  HoistDestroys(SILValue storageRoot, bool ignoreDeinitBarriers,
                SmallPtrSetImpl<SILInstruction *> &remainingDestroyAddrs,
                InstructionDeleter &deleter)
      : storageRoot(storageRoot), ignoreDeinitBarriers(ignoreDeinitBarriers),
        remainingDestroyAddrs(remainingDestroyAddrs), deleter(deleter),
        destroyMergeBlocks(getFunction()) {}

  bool perform();

protected:
  SILFunction *getFunction() const { return storageRoot->getFunction(); }

  bool foldBarrier(SILInstruction *barrier, SILValue accessScope);

  bool foldBarrier(SILInstruction *barrier, const KnownStorageUses &knownUses,
                   const DeinitBarriers &deinitBarriers);

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
  auto storage = AccessStorage::computeInScope(storageRoot);
  if (!storage.isUniquelyIdentified() &&
      storage.getKind() != AccessStorage::Kind::Nested)
    return false;

  KnownStorageUses knownUses(storage, getFunction());
  if (!knownUses.findUses())
    return false;

  DeinitBarriers deinitBarriers(ignoreDeinitBarriers, knownUses, getFunction());
  deinitBarriers.compute();

  // No SIL changes happen before rewriting.
  return rewriteDestroys(knownUses, deinitBarriers);
}

bool HoistDestroys::rewriteDestroys(const KnownStorageUses &knownUses,
                                    const DeinitBarriers &deinitBarriers) {
  // Place a new destroy after each barrier instruction.
  for (SILInstruction *barrier : deinitBarriers.barriers) {
    auto *barrierBlock = barrier->getParent();
    if (barrier != barrierBlock->getTerminator()) {
      if (!foldBarrier(barrier, knownUses, deinitBarriers))
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

    remainingDestroyAddrs.erase(destroyInst);
    deleter.forceDelete(destroyInst);
  }
  deleter.cleanupDeadInstructions();

  for (auto *mergeBlock : destroyMergeBlocks) {
    mergeDestroys(mergeBlock);
  }
  return deleter.hadCallbackInvocation();
}

bool HoistDestroys::foldBarrier(SILInstruction *barrier, SILValue storageRoot) {
  if (auto *load = dyn_cast<LoadInst>(barrier)) {
    if (stripAccessMarkers(load->getOperand()) ==
        stripAccessMarkers(storageRoot)) {
      if (load->getOwnershipQualifier() == LoadOwnershipQualifier::Copy) {
        load->setOwnershipQualifier(LoadOwnershipQualifier::Take);
        return true;
      } else {
        assert(load->getOperand()->getType().isTrivial(*load->getFunction()));
        return false;
      }
    }
  }
  if (auto *copy = dyn_cast<CopyAddrInst>(barrier)) {
    if (stripAccessMarkers(copy->getSrc()) == stripAccessMarkers(storageRoot)) {
      assert(!copy->isTakeOfSrc());
      copy->setIsTakeOfSrc(IsTake);
      return true;
    }
  }
  return false;
}

bool HoistDestroys::foldBarrier(SILInstruction *barrier,
                                const KnownStorageUses &knownUses,
                                const DeinitBarriers &deinitBarriers) {
  if (auto *eai = dyn_cast<EndAccessInst>(barrier)) {
    auto *bai = eai->getBeginAccess();
    // Don't hoist a destroy into an unrelated access scope.
    if (stripAccessMarkers(bai) != stripAccessMarkers(storageRoot))
      return false;
    SILInstruction *instruction = eai;
    while ((instruction = instruction->getPreviousInstruction())) {
      if (instruction == bai)
        return false;
      if (foldBarrier(instruction, storageRoot))
        return true;
      if (deinitBarriers.isBarrier(instruction))
        return false;
    }
  }
  return foldBarrier(barrier, storageRoot);
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
    remainingDestroyAddrs.erase(deadDestroy);
    deleter.forceDelete(deadDestroy);
  }
}

// =============================================================================
// Top-Level API
// =============================================================================

bool hoistDestroys(SILValue root, bool ignoreDeinitBarriers,
                   SmallPtrSetImpl<SILInstruction *> &remainingDestroyAddrs,
                   InstructionDeleter &deleter) {
  LLVM_DEBUG(llvm::dbgs() << "Performing destroy hoisting on " << root);

  SILFunction *function = root->getFunction();
  if (!function)
    return false;

  // The algorithm assumes no critical edges.
  assert(function->hasOwnership() && "requires OSSA");

  return HoistDestroys(root, ignoreDeinitBarriers, remainingDestroyAddrs,
                       deleter)
      .perform();
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

  llvm::SmallVector<AllocStackInst *, 4> asis;
  llvm::SmallVector<BeginAccessInst *, 4> bais;
  llvm::SmallVector<StoreInst *, 4> sis;
  llvm::SmallVector<CopyAddrInst *, 4> cais;

  // Collect the instructions that we'll be transforming.
  for (auto &block : *getFunction()) {
    for (auto &inst : block) {
      if (auto *asi = dyn_cast<AllocStackInst>(&inst)) {
        asis.push_back(asi);
      } else if (auto *bai = dyn_cast<BeginAccessInst>(&inst)) {
        if (bai->getAccessKind() == SILAccessKind::Modify) {
          bais.push_back(bai);
        }
      } else if (auto *si = dyn_cast<StoreInst>(&inst)) {
        if (si->getOwnershipQualifier() == StoreOwnershipQualifier::Assign) {
          sis.push_back(si);
        }
      } else if (auto *cai = dyn_cast<CopyAddrInst>(&inst)) {
        if (cai->isInitializationOfDest() == IsNotInitialization) {
          cais.push_back(cai);
        }
      }
    }
  }

  // Before hoisting, expand all
  //
  //     store [assign]
  //
  // instructions into
  //
  //     destroy_addr
  //     store [init]
  //
  // sequences to create more destroy_addrs to hoist.
  //
  // Record the newly created destroy_addrs and the stores they were split off
  // of.  After hoisting, if they have not been hoisted away from the store
  // instruction, we will merge them back together.
  llvm::SmallVector<std::pair<DestroyAddrInst *, StoreInst *>, 8>
      splitDestroysAndStores;
  // The destroy_addrs that were created that have not been deleted.  Items are
  // erased from the set as the destroy_addrs are deleted.
  SmallPtrSet<SILInstruction *, 8> remainingDestroyAddrs;
  // The number of destroys that were split off of store [init]s and not
  // recombined.
  int splitDestroys = 0;
  for (auto *si : sis) {
    auto builder = SILBuilderWithScope(si);
    auto *dai = builder.createDestroyAddr(
        RegularLocation::getAutoGeneratedLocation(si->getLoc()),
        si->getOperand(1));
    si->setOwnershipQualifier(StoreOwnershipQualifier::Init);
    splitDestroysAndStores.push_back({dai, si});
    remainingDestroyAddrs.insert(dai);
    ++splitDestroys;
  }
  // Similarly, also expand each
  //
  //     copy_addr to
  //
  // instruction into
  //
  //     destroy_addr
  //     copy_addr to [initialization]
  //
  // sequences to create still more destroy_addrs to hoist.
  //
  // As above, record the newly created destroy_addrs and copy_addrs off of
  // which they were split.  After hoisting, we'll merge them back together when
  // possible.
  llvm::SmallVector<std::pair<DestroyAddrInst *, CopyAddrInst *>, 8>
      splitDestroysAndCopies;
  for (auto *cai : cais) {
    auto builder = SILBuilderWithScope(cai);
    auto *dai = builder.createDestroyAddr(
        RegularLocation::getAutoGeneratedLocation(cai->getLoc()),
        cai->getOperand(1));
    cai->setIsInitializationOfDest(IsInitialization);
    splitDestroysAndCopies.push_back({dai, cai});
    remainingDestroyAddrs.insert(dai);
    ++splitDestroys;
  }

  // We assume that the function is in reverse post order so visiting the
  // blocks and pushing begin_access as we see them and then popping them off
  // the end will result in hoisting inner begin_access' destroy_addrs first.
  while (!bais.empty()) {
    auto *bai = bais.pop_back_val();
    changed |= hoistDestroys(bai, /*ignoreDeinitBarriers=*/true,
                             remainingDestroyAddrs, deleter);
  }
  // Alloc stacks always enclose their accesses.
  for (auto *asi : asis) {
    changed |= hoistDestroys(asi, /*ignoreDeinitBarriers=*/false,
                             remainingDestroyAddrs, deleter);
  }
  // Arguments enclose everything.
  for (auto *arg : getFunction()->getArguments()) {
    if (arg->getType().isAddress()) {
      auto convention = cast<SILFunctionArgument>(arg)->getArgumentConvention();
      // This is equivalent to writing
      //
      //     convention == SILArgumentConvention::Indirect_Inout
      //
      // but communicates the rationale: in order to ignore deinit barriers, the
      // address must be exclusively accessed and be a modification.
      bool ignoreDeinitBarriers = convention.isInoutConvention() &&
                                  convention.isExclusiveIndirectParameter();
      changed |= hoistDestroys(arg, ignoreDeinitBarriers, remainingDestroyAddrs,
                               deleter);
    }
  }

  for (auto pair : splitDestroysAndStores) {
    auto *dai = pair.first;
    if (!remainingDestroyAddrs.contains(dai))
      continue;
    auto *si = pair.second;
    if (dai->getNextInstruction() != si)
      continue;
    // No stores should have been rewritten during hoisting.  Their ownership
    // qualifiers were set to [init] when splitting off the destroy_addrs.
    assert(si->getOwnershipQualifier() == StoreOwnershipQualifier::Init);
    // If a newly created destroy_addr has not been hoisted from its previous
    // location, combine it back together with the store [init] which it was
    // split off from.
    deleter.forceDelete(dai);
    si->setOwnershipQualifier(StoreOwnershipQualifier::Assign);
    --splitDestroys;
  }
  for (auto pair : splitDestroysAndCopies) {
    auto *dai = pair.first;
    if (!remainingDestroyAddrs.contains(dai))
      continue;
    auto *cai = pair.second;
    if (dai->getNextInstruction() != cai)
      continue;
    assert(cai->isInitializationOfDest() == IsInitialization);
    deleter.forceDelete(dai);
    cai->setIsInitializationOfDest(IsNotInitialization);
    --splitDestroys;
  }
  // If there were any destroy_addrs split off of stores and not recombined
  // with them, then the function has changed.
  changed |= splitDestroys > 0;

  if (changed) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

SILTransform *swift::createSSADestroyHoisting() {
  return new SSADestroyHoisting();
}

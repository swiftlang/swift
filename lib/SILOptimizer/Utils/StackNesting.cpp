//===--- StackNesting.cpp - Utility for stack nesting  --------------------===//
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

#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/Test.h"
#include "llvm/Support/Debug.h"

using namespace swift;

/// Run the given function exactly once on each of the reachable blocks in
/// a SIL function. Blocks will be visited in a post-order consistent with
/// dominance, which is to say, after all dominating blocks but otherwise
/// in an unspecified order.
///
/// The function is passed a state value, which it can freely mutate. The
/// initial value of the state will be the same as the value left in the
/// state for an unspecified predecessor (or the initial value passed in,
/// for the entry block of the function). Since the predecessor choice is
/// arbitrary, you should only use this if the state is guaranteed to be
/// the same for all predecessors. The state type must be copyable, but
/// the algorithm makes a reasonable effort to avoid copying it.
///
/// This function assumes you don't change the CFG during its operation.
template <class Fn, class State>
void runInDominanceOrder(SILFunction &F, State &&state, const Fn &fn) {
  // The set of blocks that have been visited, or at least added to the
  // worklist. We only bother to track blocks with multiple predecessors.
  llvm::SmallPtrSet<SILBasicBlock*, 8> visitedBlocks;

  // The next basic block to operate on. We always operate on `state`.
  SILBasicBlock *curBB = F.getEntryBlock();

  // We need to copy `state` whenever we enqueue a block onto the worklist.
  // We'll then move-assign it back to `state` when we dequeue it.
  using StateValue = std::remove_reference_t<State>;
  SmallVector<std::pair<SILBasicBlock *, StateValue>> worklist;

  while (true) {
    // Run the function on the current block, updating the current state.
    fn(curBB, state);

    // Enqueue the successors.
    SILBasicBlock *nextBB = nullptr;
    for (SILBasicBlock *succBB : curBB->getSuccessorBlocks()) {
      // Check to see if we've already visited/enqueued this block.
      // As a very important fast path, we can skip this if the block
      // has a single predecessor.
      if (!succBB->hasSinglePredecessorBlock()) {
        // If this insertion fails, it's because the block has already
        // been visited/enqueued.
        if (!visitedBlocks.insert(succBB).second)
          continue;
      }

      // Set this successor as the next block if possible, but if we
      // already have one, add it to the worklist instead.
      if (nextBB) {
        worklist.emplace_back(succBB, /*copied*/ state);
      } else {
        nextBB = succBB;
      }
    }

    // If there's a viable successor, continue with the current state.
    if (nextBB) {
      curBB = nextBB;
      continue;
    }

    // Otherwise, if the worklist is empty, we're done.
    if (worklist.empty()) {
      return;
    }

    // Otherwise, pull the next item off the worklist.
    auto &nextItem = worklist.back();
    curBB = nextItem.first;
    state = std::move(nextItem.second);
    worklist.pop_back();
  }
}

/// Returns the stack allocation instruction for a stack deallocation
/// instruction.
static SILInstruction *getAllocForDealloc(SILInstruction *dealloc) {
  SILValue op = dealloc->getOperand(0);
  while (auto *mvi = dyn_cast<MoveValueInst>(op)) {
    op = mvi->getOperand();
  }
  return op->getDefiningInstruction();
}

/// Create a dealloc for a particular allocation.
///
/// This is expected to work for all allocations that don't have
/// properly-nested deallocations. It's fine to have a kind of allocation
/// that you can't do this for, as long as as it's always explicitly
/// deallocated on all paths. This pass doesn't change any allocations
/// or deallocations that are properly nested already.
///
/// Only allocations whose deallocations return true from canMoveDealloc
/// need to support this.
static void createDealloc(SILBuilder &B, SILLocation loc, SILInstruction *alloc) {
  switch (alloc->getKind()) {
  case SILInstructionKind::PartialApplyInst:
  case SILInstructionKind::AllocStackInst:
    assert((isa<AllocStackInst>(alloc) ||
            cast<PartialApplyInst>(alloc)->isOnStack()) &&
           "wrong instruction");
    B.createDeallocStack(loc, cast<SingleValueInstruction>(alloc));
    return;
  case SILInstructionKind::BeginApplyInst: {
    auto *bai = cast<BeginApplyInst>(alloc);
    assert(bai->isCalleeAllocated());
    B.createDeallocStack(loc, bai->getCalleeAllocationResult());
    return;
  }
  case SILInstructionKind::AllocRefDynamicInst:
  case SILInstructionKind::AllocRefInst:
    assert(cast<AllocRefInstBase>(alloc)->canAllocOnStack());
    B.createDeallocStackRef(loc, cast<AllocRefInstBase>(alloc));
    return;
  case SILInstructionKind::AllocPackInst:
    B.createDeallocPack(loc, cast<AllocPackInst>(alloc));
    return;
  case SILInstructionKind::BuiltinInst: {
    auto *bi = cast<BuiltinInst>(alloc);
    auto &ctx = alloc->getFunction()->getModule().getASTContext();

    switch (*bi->getBuiltinKind()) {
    case BuiltinValueKind::StackAlloc:
    case BuiltinValueKind::UnprotectedStackAlloc: {
      auto identifier =
        ctx.getIdentifier(getBuiltinName(BuiltinValueKind::StackDealloc));
      B.createBuiltin(loc, identifier,
                      SILType::getEmptyTupleType(ctx),
                      SubstitutionMap(), {bi});
      return;
    }
    default:
      llvm_unreachable("unknown stack allocation builtin");
    }
  }
  case SILInstructionKind::AllocPackMetadataInst:
    B.createDeallocPackMetadata(loc, cast<AllocPackMetadataInst>(alloc));
    return;
  default:
    llvm_unreachable("unknown stack allocation");
  }
}

namespace {
class ActiveAllocation {
  llvm::PointerIntPair<SILInstruction*, 1, bool> valueAndIsPending;

public:
  ActiveAllocation(SILInstruction *value) : valueAndIsPending(value, false) {}

  SILInstruction *getValue() const {
    return valueAndIsPending.getPointer();
  }

  bool isPending() const {
    return valueAndIsPending.getInt();
  }

  void setPending() {
    assert(!isPending());
    valueAndIsPending.setInt(true);
  }
};

struct State {
  // The active allocations and whether they're pending deallocation.
  SmallVector<ActiveAllocation, 4> allocations;

#ifndef NDEBUG
  SWIFT_ATTRIBUTE_NORETURN
  void abortForUnknownAllocation(SILInstruction *alloc,
                                 SILInstruction *dealloc) {
    llvm::errs() << "fatal error: StackNesting could not find record of "
                    "allocation for deallocation:\n  "
                 << *dealloc
                 << "Allocation might not be jointly post-dominated. "
                    "Current stack:\n";
    for (auto i : indices(allocations)) {
      llvm::errs() << "[" << i << "] "
                   << (allocations[i].isPending() ? "(pending) " : "")
                   << *allocations[i].getValue();
    }
    llvm::errs() << "Complete function:\n";
    alloc->getFunction()->dump();
    abort();
  }
#endif
};

} // end anonymous namespace

using IndexForAllocationMap = llvm::DenseMap<SILInstruction*, size_t>;

/// Flag that a particular allocation is pending.
static void setAllocationAsPending(State &state, SILInstruction *alloc,
                                   SILInstruction *dealloc,
                                   IndexForAllocationMap &indexForAllocation) {
  auto stack = MutableArrayRef(state.allocations);
  assert(!stack.empty());

  // Just ignore the top entry in all of this; we know it doesn't match
  // the allocation.
  assert(stack.back().getValue() != alloc);
  stack = stack.drop_back();

  // Ultimately, we're just calling setPending() on the entry matching
  // `alloc` in the allocations stack. All the complexity has to do with
  // trying to avoid super-linear behavior while also trying very hard
  // to avoid actually using indexForAllocation for simple cases.

  // It's very common for allocations to never be improperly nested,
  // so we don't want to eagerly add allocations to indexForAllocation
  // when we encounter them. This means we can't rely on it having
  // an entry for `alloc` now.

  // `alloc` is very likely to be close to the top of the stack. Just do
  // a short linear scan there first. This might be slightly slower than
  // a hash lookup in the worst case, but usually it means we can avoid
  // adding any entries to indexForAllocation at all. Even for this case
  // where nesting is broken, that's still worthwhile to do.
  const size_t linearScanLimit = 8;
  auto linearScanEntries = stack.take_back(linearScanLimit);
  for (auto &entry : linearScanEntries) {
    if (entry.getValue() == alloc) {
      entry.setPending();
      return;
    }
  }

  // Okay, so much for that, time for the hashtable.

#ifndef NDEBUG
  if (stack.size() <= linearScanLimit) {
    state.abortForUnknownAllocation(alloc, dealloc);
  }
#endif

  // We don't need to consider entries that we've already linearly scanned.
  stack = stack.drop_back(linearScanLimit);

  // Check if the entry's already in the hashtable.
  if (auto it = indexForAllocation.find(alloc); it != indexForAllocation.end()) {
    auto index = it->second;
    assert(stack[index].getValue() == alloc);
    stack[index].setPending();
    return;
  }

  // Fill in any missing entries in indexForAllocations.
  //
  // The invariant we maintain is that there may be allocations at the
  // top of the stack that aren't hashed, but once we reach a hashed
  // entry, everything beneath it is hashed. The first half of this
  // is necessary because we don't eagerly add allocations to the table,
  // but it's also what makes it okay that we skip the entries we
  // linearly scanned. The second half of this means that, if we start
  // adding entries from the top down, we can stop hashing once we find
  // that the entries we're adding are redundant. That's what keeps this
  // O(N).
  //
  // All of this caching is relying on us (1) never revisiting a block
  // and (2) never changing the active-allocations stack except via push
  // and pop.

  // Look for the target allocation index in this loop rather than doing
  // a hash lookup at the end.
  std::optional<size_t> foundIndexForAlloc;

  for (size_t onePast = stack.size(); onePast != 0; --onePast) {
    size_t entryIndex = onePast - 1;
    auto entryAlloc = stack[entryIndex].getValue();

    // Remember this if it's the allocation we're looking for.
    if (entryAlloc == alloc) {
      foundIndexForAlloc = entryIndex;
    }

    // Add this entry to the hashtable. Stop hashing as soon as this fails.
    auto insertResult = indexForAllocation.insert({entryAlloc, entryIndex});
    if (!insertResult.second) {
      continue;
    }
  }

#ifndef NDEBUG
  if (!foundIndexForAlloc) {
    state.abortForUnknownAllocation(alloc, dealloc);
  }
#endif

  stack[*foundIndexForAlloc].setPending();
}

/// Pop and emit deallocations for any allocations on top of the
/// active allocations stack that are pending deallocation.
///
/// This operation is called whenever we pop an allocation; it
/// restores the invariant that the top of the stack is never in a
/// pending state.
static void emitPendingDeallocations(State &state,
                                     SILInstruction *insertAfterDealloc,
                                     bool &madeChanges) {
  std::optional<SILBuilderWithScope> builder;

  while (!state.allocations.empty() &&
         state.allocations.back().isPending()) {
    auto entry = state.allocations.pop_back_val();
    SILInstruction *alloc = entry.getValue();

    // Create a builder that inserts after the initial dealloc, if we
    // haven't already. Re-using the same builder for subsequent deallocs
    // means we order them correctly w.r.t each other, which we wouldn't
    // if we made a fresh builder after the initial dealloc each time.
    if (!builder) {
      // We want to use the location of (and inherit debug scopes from)
      // the initial dealloc that we're inserting after.
      builder.emplace(/*insertion point*/
                        std::next(insertAfterDealloc->getIterator()),
                      /*inherit scope from*/insertAfterDealloc);
    }

    createDealloc(*builder, insertAfterDealloc->getLoc(), alloc);
    madeChanges = true;
  }
}

/// The main entrypoint for clients.
StackNesting::Changes StackNesting::fixNesting(SILFunction *F) {
  bool madeChanges = false;

  // The index in the allocation stack for each allocation. Multiple
  // allocations can map to the same index, since ultimately it's a stack;
  // we should be looking up an allocation while it's still in use.
  // This is very lazily filled in, because we don't want to do unnecessary
  // work if nothing is unscoped. See setAllocationAsPending for invariants.
  // This function never accesses it directly.
  IndexForAllocationMap indexForAllocation;

  // Visit each block of the function in an order consistent with dominance.
  // The state represents the stack of active allocations, so it's appropriate
  // that it starts with an empty stack. We're not worried about states
  // potentially being different for different paths to the same block because
  // that can only happen if deallocations don't properly post-dominate
  // their allocations.
  runInDominanceOrder(*F, State(), [&](SILBasicBlock *B, State &state) {

    // We can't use a foreach loop because we sometimes remove the
    // current instruction or add instructions (that we shouldn't visit)
    // after it. Advancing the iterator immediately within the loop is
    // sufficient to protect against both.
    for (auto II = B->begin(), IE = B->end(); II != IE; ) {
      SILInstruction *I = &*II++;

      // Invariant: the top of the stack is never pending.
      assert(state.allocations.empty() ||
             !state.allocations.back().isPending());

      // Push allocations onto the current stack in the non-pending state.
      if (I->isAllocatingStack()) {
        state.allocations.push_back(I);
        continue;
      }

      // Ignore instructions other than allocations and deallocations.
      if (!I->isDeallocatingStack()) {
        continue;
      }

      // Get the allocation for the deallocation.
      SILInstruction *dealloc = I;
      SILInstruction *alloc = getAllocForDealloc(dealloc);

#ifndef NDEBUG
      if (state.allocations.empty()) {
        state.abortForUnknownAllocation(alloc, dealloc);
      }
#endif

      // If the allocation is the top of the allocations stack, we can
      // leave it alone.
      if (alloc == state.allocations.back().getValue()) {
        // Pop off our record of the allocation.
        state.allocations.pop_back();

        // We may need to emit any pending deallocations still on the stack.
        // Pop and emit them in order.
        emitPendingDeallocations(state, /*after*/ dealloc, madeChanges);

        continue;
      }

      // Otherwise, just remove the deallocation and set the allocation
      // as having a pending deallocation on this path.
      //
      // When we mark `alloc` as having a pending deallocation, we are
      // deferring its deallocation on this path to the deallocation
      // points of some non-pending allocation that's on top of it on the
      // allocation stack. That may not end up being the current top of
      // the stack:
      //
      // - the current top may itself get deferred later, e.g.
      //
      //     %alloc = alloc_stack $Int
      //     %top = alloc_stack $Int
      //     dealloc_stack %alloc       // gets deferred
      //     %new = alloc_stack $Int
      //     dealloc_stack %top         // gets deferred
      //     dealloc_stack %new         // dealloc %alloc and %top after this
      //
      // - there might be some other allocation between `alloc` and the
      //   current that "inherits" the deferral after we dealloc the
      //   current top, e.g.:
      //
      //     %alloc = alloc_stack $Int
      //     %middle = alloc_stack $Int
      //     %top = alloc_stack $Int
      //     dealloc_stack %alloc       // gets deferred
      //     dealloc_stack %top
      //     dealloc_stack %middle      // dealloc %alloc after this
      //
      // The key is that, no matter what happens, there's always a last
      // thing on the stack above `alloc` that hasn't been deferred yet.
      // Joint post-dominance means that, on every path we can reach
      // from this point, we'll either eventually reach a dealloc for
      // that last undeferred allocation (in which case, we'll deallocate
      // `alloc` then), or we'll reach a dead-end (in which case it's fine
      // that we never deallocated `alloc`). And whatever those points
      // are, replacing this deallocation of `alloc` with those points
      // will re-establish the joint post-dominance of `alloc` by its
      // deallocations with respect to this path.

      dealloc->eraseFromParent();
      madeChanges = true;
      setAllocationAsPending(state, alloc, dealloc, indexForAllocation);
    }
  });

  // We never make changes to the CFG.
  return (madeChanges ? Changes::Instructions : Changes::None);
}

namespace swift::test {
static FunctionTest MyNewTest("stack_nesting_fixup",
                              [](auto &function, auto &arguments, auto &test) {
                                StackNesting::fixNesting(&function);
                                function.print(llvm::outs());
                              });
} // end namespace swift::test

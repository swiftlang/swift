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
#include "swift/SIL/Dominance.h"
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
/// state for an unspecified predecessor. For the entry block of the
/// function, this is the initial state passed to runInDominanceOrder.
///
/// Essentially, runInDominanceOrder finds an arbitrary simple path to
/// the block and runs the callback function for each block in that path
/// in order. As long as the callback:
/// - only looks at instructions in the current block and the blocks
///   it dominates,
/// - has no dependencies outside of the state (which must have "value
///   semantics"), and
/// - can handle the arbitrariness of the choice of path,
/// then the callback can act as if only the work done along the current
/// path has happened and ignore the impact of arbitrary visitation order.
///
/// This function assumes you don't change the CFG during its operation.
template <class Fn, class State>
void runInDominanceOrder(SILFunction &F, State &&state, const Fn &fn) {
  // The set of blocks that have ever been enqueued onto the worklist.
  // (We actually skip the worklist a lot, but *abstractly* they're
  // enqueued, and everything but the entry block does get added to this
  // set.)
  BasicBlockSet visitedBlocks(&F);

  // The next basic block to operate on. We always operate on `state`.
  SILBasicBlock *curBB = F.getEntryBlock();

  // We need to copy `state` whenever we enqueue a block onto the worklist.
  // We'll move-assign it back to `state` when we dequeue it.
  using StateValue = std::remove_reference_t<State>;
  SmallVector<std::pair<SILBasicBlock *, StateValue>> worklist;

  // Count the remaining edges to each dead-end region, and keep track
  // of a current merged state for each.
  DeadEndEdges deadEndEdges(&F);
  SmallVector<std::optional<StateValue>> deadEndRegionStates;
  deadEndRegionStates.resize(deadEndEdges.getNumDeadEndRegions());
  auto visitedDeadEndEdges = deadEndEdges.createVisitingSet();

  while (true) {
    // Run the function on the current block, updating the current state.
    fn(curBB, state);

    // Enqueue the successors.
    SILBasicBlock *nextBB = nullptr;
    for (SILBasicBlock *succBB : curBB->getSuccessorBlocks()) {
      // If this edge visits a dead-end region, we may need to conservatively
      // merge the input state before visiting the region.
      if (auto deadEndRegionIndex =
            deadEndEdges.entersDeadEndRegion(curBB, succBB)) {
        auto &existingRegionState = deadEndRegionStates[*deadEndRegionIndex];

        bool isLastEdgeToRegion = visitedDeadEndEdges.visitEdgeTo(succBB);

        // If this is not the last edge to the dead-end region, we can't
        // enqueue yet. Merge the current state into the existing state we're
        // tracking for the region.
        if (!isLastEdgeToRegion) {
          if (!existingRegionState) {
            existingRegionState.emplace(/*copied*/ state);
          } else {
            existingRegionState->merge(state);
          }
          continue;
        }

        // Otherwise, this is the last edge to the dead-end region.

        // The dead-end-edges counting means we know we haven't visited the
        // successor block yet (or anything else in its region), so we're
        // going to end up enqueuing it. But we do need to add it to
        // visitedBlocks so we don't re-enter it if its region is cyclic.
        auto inserted = visitedBlocks.insert(succBB);
        assert(inserted); (void) inserted;

        // If we have an existing state for the region, merge the current
        // state into it, then enqueue that state.
        if (existingRegionState) {
          // In theory we could merge into state and then just continue if
          // this is the current block's only reasonable successor, but
          // avoiding a few move-assignments wouldn't really justify the
          // more complicated bookkeeping that would require.
          existingRegionState->merge(state);
          worklist.emplace_back(succBB, std::move(*existingRegionState));
          continue;
        }

        // Otherwise there was just a single edge into the dead-end region,
        // so there's no conservative state merge required, and we can just
        // fall into the code below.

      // If this edge doesn't visit a dead-end region, just check whether
      // we've already enqueued the successor block. If so, we can just
      // skip it.
      } else {
        // Insertion returns false if it the block was already in the set.
        // We could avoid this set operation by checking whether the block
        // has a unique predecessor edge, but BasicBlockSet is fast enough
        // that that's not worthwhile.
        if (!visitedBlocks.insert(succBB))
          continue;
      }

      // If we haven't found a successor to visit yet, pick this one.
      if (!nextBB) {
        nextBB = succBB;

      // Otherwise, add it to the worklist, copying the current state.
      } else {
        worklist.emplace_back(succBB, /*copied*/ state);
      }
    }

    // If there's a viable direct successor, just continue along this
    // path, editing the current state in-place.
    if (nextBB) {
      curBB = nextBB;
      continue;
    }

    // Otherwise, if the worklist is empty, we're done.
    if (worklist.empty()) {
      return;
    }

    // Otherwise, pull the next item off the worklist and overwrite the
    // current state with the state we saved for it before.
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
enum class AllocationStatus {
  Allocated, Pending, Undeallocatable
};

class ActiveAllocation {
  llvm::PointerIntPair<SILInstruction*, 2, AllocationStatus> valueAndStatus;

public:
  ActiveAllocation(SILInstruction *value)
    : valueAndStatus(value, AllocationStatus::Allocated) {}

  SILInstruction *getValue() const {
    return valueAndStatus.getPointer();
  }

  AllocationStatus getStatus() const {
    return valueAndStatus.getInt();
  }

  void setPending() {
    assert(getStatus() == AllocationStatus::Allocated);
    valueAndStatus.setInt(AllocationStatus::Pending);
  }

  void setUndeallocatable() {
    valueAndStatus.setInt(AllocationStatus::Undeallocatable);
  }

  bool operator==(const ActiveAllocation &other) const {
    return valueAndStatus == other.valueAndStatus;
  }
  bool operator!=(const ActiveAllocation &other) const {
    return valueAndStatus != other.valueAndStatus;
  }
};

using IndexForAllocationMap = llvm::DenseMap<SILInstruction*, size_t>;

struct State {
  /// The stack of active allocations and their statuses.
  SmallVector<ActiveAllocation, 4> allocations;

  ActiveAllocation &getEntryForNonTop(SILInstruction *alloc,
                                      SILInstruction *dealloc,
                                      IndexForAllocationMap &indexForAllocation);

  /// Given that these are the end states of two blocks with edges into
  /// a dead-end region R, merge them.
  ///
  /// In the formal presentation of the algorithm, we decide whether PSS(R)
  /// is non-singleton and, if so, apply STATE_MERGE. We can do this
  /// iteratively by checking for pairwise equality and doing a pairwise
  /// CONSERVATIVE_MERGE if they're not.
  void merge(const State &other) {
    // If the states match exactly, there's nothing to do.
    if (ArrayRef(allocations) == ArrayRef(other.allocations)) {
      return;
    }

    // Otherwise, find the longest common prefix of allocations. We can start
    // from the end and stop as soon as we find a match because all states
    // that contain A have a common prefix of allocations; see
    // [allocation-index] in the proof.
    auto commonPrefixLength =
      std::min(allocations.size(), other.allocations.size());
    while (commonPrefixLength > 0 &&
           allocations[commonPrefixLength - 1].getValue()
             != other.allocations[commonPrefixLength - 1].getValue()) {
      commonPrefixLength--;
    }

    // Truncate to the common prefix length and set all entries to
    // undeallocatable.
    allocations.truncate(commonPrefixLength);
    for (auto &entry : allocations) {
      entry.setUndeallocatable();
    }
  } 

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
      auto status = allocations[i].getStatus();
      llvm::errs() << "[" << i << "] "
                   << (status == AllocationStatus::Allocated ? "" :
                       status == AllocationStatus::Pending ? "(pending) " :
                       "(undeallocatable) ")
                   << *allocations[i].getValue();
    }
    llvm::errs() << "Complete function:\n";
    alloc->getFunction()->dump();
    abort();
  }
#endif
};

} // end anonymous namespace

ActiveAllocation &
State::getEntryForNonTop(SILInstruction *alloc,
                         SILInstruction *dealloc,
                         IndexForAllocationMap &indexForAllocation) {
  auto stack = MutableArrayRef(allocations);
  assert(!stack.empty());

  // By precondition, we know the top entry doesn't match the allocation,
  // so we can drop it.
  assert(stack.back().getValue() != alloc);
  stack = stack.drop_back();

  // This is really just searching for the allocation in the stack.
  // All the complexity has to do with trying to avoid super-linear
  // behavior while also trying very hard to avoid actually filling
  // in indexForAllocation for simple cases.

  // It's very common for allocations to never be improperly nested,
  // so we don't want to eagerly add allocations to indexForAllocation
  // when we encounter them. This means we can't rely on it having
  // an entry for `alloc` now.

  // `alloc` is very likely to be close to the top of the stack. Just do
  // a short linear scan there first. This is quick, and it means we
  // can probably avoid ever filling in indexForAllocation.
  const size_t linearScanLimit = 8;
  auto linearScanEntries = stack.take_back(linearScanLimit);
  for (auto &entry : linearScanEntries) {
    if (entry.getValue() == alloc) {
      return entry;
    }
  }

  // Okay, so much for that, time for the hashtable.

#ifndef NDEBUG
  if (stack.size() <= linearScanLimit) {
    abortForUnknownAllocation(alloc, dealloc);
  }
#endif

  // We don't need to consider entries that we've already linearly scanned.
  stack = stack.drop_back(linearScanLimit);

  // Check if the entry's already in the hashtable.
  if (auto it = indexForAllocation.find(alloc); it != indexForAllocation.end()) {
    auto index = it->second;
    assert(stack[index].getValue() == alloc);
    return stack[index];
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
  // This caching works because allocations have a constant index as long
  // as they're in the stack; see [allocation-index] in the proof.

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

    // Add this entry to the hashtable. We can stop adding entries as soon as
    // we find something that already has an entry.
    auto insertResult = indexForAllocation.insert({entryAlloc, entryIndex});
    if (!insertResult.second) {
      continue;
    }
  }

#ifndef NDEBUG
  if (!foundIndexForAlloc) {
    abortForUnknownAllocation(alloc, dealloc);
  }
#endif

  return stack[*foundIndexForAlloc];
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
  // The builder we use for inserting deallocations. Initialized lazily
  // to insert after the initial dealloc. We have to reuse the same
  // builder so that, if we pop multiple deallocations, we order them
  // correctly w.r.t each other.
  std::optional<SILBuilderWithScope> builder;

  while (!state.allocations.empty() &&
         state.allocations.back().getStatus() == AllocationStatus::Pending) {
    auto entry = state.allocations.pop_back_val();
    SILInstruction *alloc = entry.getValue();

    // Create a builder if necessary.
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
///
/// We use a straightforward, single-pass algorithm:
///
///   enum AllocationStatus {
///     case allocated
///     case pendingDeallocation
///     case undeallocatable
///   }
///   struct State {
///     var stack = [(StackAllocationInst, AllocationStatus)]()
///   }
///   F.searchBlocksForJointPostDominance(initialState: State()) {
///     (block, state) in
///     for inst in block.instructions {
///       if inst.isStackAllocation {
///         state.stack.push((inst, .allocated))
///       } else if inst.isStackDeallocation {
///         let allocation = inst.stackAllocation
///         if state.stack.top == (A, .allocated) {
///           _ = state.stack.pop()
///           let builder = SILBuilder(insertAfter: I)
///           while !state.stack.isEmpty &&
///                 state.stack.top!.1 == .pendingDeallocation {
///             builder.createStackDeallocation(for: state.stack.pop().0)
///           }
///         } else {
///           I.remove()
///           let curStatus = state.findStatus(A)
///           assert(curStatus != .pendingDeallocation)
///           if curStatus == .allocated) {
///             state.setStatus(A, .pendingDeallocation)
///           }
///         }
///       }
///     }
///   }
///
/// The expectation is that searchBlocksForJointPostDominance performs
/// a depth-first search, passing a state that reflects the current
/// simple path from the entry block that is being explored. However,
/// there's a twist to this from a standard DFS; see below.
///
/// For the most part, the value in `state` at the end of processing
/// a block is the initial value of `state` at the start of processing
/// its successor as visited by the DFS, but this also has a twist;
/// see below.
///
/// The state consists of (1) an active stack of allocations which
/// haven't yet been deallocated on this path and (2) an active
/// status for each allocation.
///
/// It has five invariants:
///
/// 1. Allocations on the stack dominate all allocations above them.
/// 2. Every allocation on the stack dominates the current point
///    being considered.
/// 3. The top of the stack never has pending status.
/// 4. If a stack item has undeallocatable status, every item
///    below it also has undeallocatable status.
/// 5. If an allocation has pending status, there is an allocation
///    above it on the stack which has allocated status.
///
/// The twist about the search order and state relates to the
/// non-coherence of SIL's joint post-dominance requirement for stack
/// allocations. Specifically, SIL permits the current state of the
/// stack to vary on different edges into a dead-end region; this just
/// means it is not permitted to pop any of those deallocations from
/// the stack. (More allocations can be pushed and popped, but the
/// existing ones become untouchable.) StackNesting therefore permits
/// its input to be non-coherent: whether an allocation has been
/// deallocated is allowed to vary across the entries to a dead-end
/// region.
///
/// To handle this, the search delays considering edges into a dead-end
/// region until it has seen the last such edge. Furthermore, the
/// initial state of the destination block is set to a conservative
/// merger of the final states of all of the predecessors: if two
/// states differ in any way, the contents of the stack are pared
/// back to the common prefix, and all allocations are placed in the
/// undeallocatable state.
///
/// A proof of correctness can be found in StackNestingProof.txt.
StackNesting::Changes StackNesting::fixNesting(SILFunction *F) {
  bool madeChanges = false;

  // The index in the allocation stack for each allocation.
  // This function never uses this directly; it's just a cache for
  // setAllocationAsPending.
  IndexForAllocationMap indexForAllocation;

  // Visit each block of the function in an order consistent with dominance.
  // The state represents the stack of active allocations; it starts
  // with an empty stack because so does the function.
  runInDominanceOrder(*F, State(), [&](SILBasicBlock *B, State &state) {

    // We can't use a foreach loop because we sometimes remove the
    // current instruction or add instructions (that we don't want to
    // visit) after it. Advancing the iterator immediately within the
    // loop is sufficient to protect against both.
    for (auto II = B->begin(), IE = B->end(); II != IE; ) {
      SILInstruction *I = &*II;
      ++II;

      // Invariant: the top of the stack is never pending.
      assert(state.allocations.empty() ||
             state.allocations.back().getStatus() != AllocationStatus::Pending);

      // Push allocations onto the current stack in the non-pending state.
      //
      // In the formal presentation, the state change is
      //   state = STATE_PUSH(state, alloc)
      if (I->isAllocatingStack()) {
        // Only handle nested stack allocations.
        if (I->isStackAllocationNested() == StackAllocationIsNested)
          state.allocations.push_back(I);
        continue;
      }

      // Ignore instructions other than allocations and deallocations.
      if (!I->isDeallocatingStack()) {
        continue;
      }

      // Get the allocation for the deallocation. The allocation should be
      // in the state, and it should not have pending status; see
      // [deallocation-preconditions] in the proof.
      SILInstruction *dealloc = I;
      SILInstruction *alloc = getAllocForDealloc(dealloc);

      // Since we only processed nested allocations above, ignore deallocations
      // from non-nested allocations.
      if (alloc->isStackAllocationNested() == StackAllocationIsNotNested)
        continue;

#ifndef NDEBUG
      if (state.allocations.empty()) {
        state.abortForUnknownAllocation(alloc, dealloc);
      }
#endif

      // If the allocation is the top of the allocations stack:
      if (alloc == state.allocations.back().getValue()) {
        auto status = state.allocations.back().getStatus();
        assert(status == AllocationStatus::Allocated ||
               status == AllocationStatus::Undeallocatable);

        // If the allocation has allocated status, leave the deallocation
        // alone, pop the record of it off the stack, and pop and emit any
        // pending allocations beneath it.
        //
        // In the formal presentation, the state change is
        //   state = STATE_POP(state, alloc)
        if (status == AllocationStatus::Allocated) {
          state.allocations.pop_back();
          emitPendingDeallocations(state, /*after*/ dealloc, madeChanges);

        // Otherwise, it must have undeallocatable status; remove the
        // deallocation, but make no changes to the state.
        } else {
          dealloc->eraseFromParent();
          madeChanges = true;
        }

      // If the allocation is not the top of the allocations stack:
      } else {
        // Find its entry.
        auto &entry = state.getEntryForNonTop(alloc, dealloc,
                                              indexForAllocation);

        auto status = entry.getStatus();
        assert(status == AllocationStatus::Allocated ||
               status == AllocationStatus::Undeallocatable);

        // If the allocation has allocated status, remove the deallocation
        // and update the allocation status to pending.
        //
        // In the formal presentation, the state change is
        //   state = STATE_PEND(state, alloc)
        if (status == AllocationStatus::Allocated) {
          entry.setPending();

        // Otherwise, it has undeallocatable status; just remove the
        // deallocation but make no changes to the state.
        }

        // In any case, remove the deallocation.
        dealloc->eraseFromParent();
        madeChanges = true;
      }
    }
  });

  // We never make changes to the CFG.
  return (madeChanges ? Changes::Instructions : Changes::None);
}

namespace swift::test {
static FunctionTest StackNestingTests("stack_nesting_fixup",
                                      [](auto &function, auto &arguments,
                                         auto &test) {
                                        StackNesting::fixNesting(&function);
                                        function.print(llvm::outs());
                                      });
} // end namespace swift::test

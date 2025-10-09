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

#ifndef NDEBUG
static void checkPreconditions(SILFunction *F) {
  // StackNesting has two preconditions: first, dominance still has
  // to hold among allocations and deallocations; and second,
  // allocations must be jointly post-dominated by their deallocations.
  //
  // Dominance is the more important precondition to check, because we
  // don't want this pass to be held responsible if it ends up
  // generating non-dominating loads and stores because its input didn't
  // obey dominance either.
  //
  // Joint post-dominance is likely to just result in join
  // If the input just violates joint post-dominance, on the other
  // hand, we generally maintain most of that structure, and it's
  // relatively easy to debug.

  DominanceInfo dominance(F);

  for (auto &BB: *F) {
    for (auto &I : BB) {
      if (I.isDeallocatingStack()) {
        SILInstruction *dealloc = &I;
        SILInstruction *alloc = getAllocForDealloc(dealloc);
        if (!dominance.properlyDominates(alloc, dealloc)) {
          llvm::errs() << "FATAL ERROR: prior to StackNesting, deallocation\n  "
                       << *dealloc
                       << "is not properly dominated by its allocation\n  "
                       << *alloc
                       << "Complete function:\n" << *F;
          abort();
        }
      }
    }
  }

}
#endif

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

  while (true) {
    // Run the function on the current block, updating the current state.
    fn(curBB, state);

    // Enqueue the successors.
    SILBasicBlock *nextBB = nullptr;
    for (SILBasicBlock *succBB : curBB->getSuccessorBlocks()) {
      // If this insertion returns true, we've already enqueued the
      // successor block, so we can skip it. This is fast enough because
      // of BasicBlockSet that there's no point in avoiding it for
      // single-predecessor blocks.
      if (!visitedBlocks.insert(succBB))
        continue;

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
  // The builder we use for inserting deallocations. Initialized lazily
  // to insert after the initial dealloc. We have to reuse the same
  // builder so that, if we pop multiple deallocations, we order them
  // correctly w.r.t each other.
  std::optional<SILBuilderWithScope> builder;

  while (!state.allocations.empty() &&
         state.allocations.back().isPending()) {
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
///   struct State {
///     var stack = [StackAllocationInst]()
///     var pending = Set<StackAllocationInst>()
///   }
///   F.searchBlocks(initialState: State()) { (B, state) in
///     for I in B.instructions {
///       if I.isStackAllocation {
///         state.stack.push(I)
///       } else if I.isStackDeallocation {
///         let A = I.stackAllocation
///         if A == state.stack.top {
///           _ = state.stack.pop()
///           let builder = SILBuilder(insertAfter: I)
///           while !state.stack.isEmpty &&
///                 state.pending.contains(state.stack.top) {
///             builder.createStackDeallocation(for: state.stack.pop())
///           }
///         } else {
///           I.remove()
///           state.pending.insert(A)
///         }
///       }
///     }
///   }
///
/// The exact order of block visitation doesn't matter as long as
/// A is visited before B whenever A dominates B, which naturally
/// happens with any forward search through the CFG.
///
/// The state is maintained for the current simple path from the
/// entry block. (Forward searchs through the CFG naturally visit
/// blocks along a simple path from the entry block, i.e. a path
/// that doesn't repeat blocks.) It consists of (1) an active stack
/// of allocations which haven't yet been deallocated on this path
/// and (2) a set of those allocations which are pending deallocation
/// on this path. (We actually store this set using a flag on the
/// stack entry.) There are three invariants:
///
/// 1. If A dominates B, A precedes B (is further from the top) in
///    the allocation stack.
/// 2. The top of the stack is never in the pending set.
/// 3. If an allocation is in the pending set, there is an allocation
///    which it dominates that is still on the stack and not in the
///    pending set.
///
/// Invariant #1 is established by visiting blocks and instructions
/// in an order consistent with dominance. Invariant #2 can be proven
/// easily from how the stack is manipulated in the pseudocode.
/// Invariant #3 is more subtle and relies on proper dominance within
/// the function.
///
/// Theorem.
///   The algorithm maintains invariant #3.
///
/// Proof: This property is fairly obvious while the CFG search
/// is still walking code that is dominated by an allocation, because
/// it must dominate *all* allocations above it on the stack.
/// But a CFG search is not a dominance-tree walk; it it is possible
/// to visit blocks that are not dominated by earlier blocks on the
/// current path from entry. In principle, the algorithm could try
/// to emit deallocations for allocations within these blocks.
/// However:
///
///   Lemma. If a CFG search visits a block A, then later visits a
///     block B that is not dominated by A, it cannot subsequently
///     visit (on the same path from entry) a block C that *is*
///     dominated by A.
///
///   Proof: By construction of the search, there is a simple path
///   from entry to A to B to C. The portion P_BC of this path
///   from B to C cannot pass through A because it is a subpath
///   of a simple path that includes A earlier. Meanwhile, if A
///   does not dominate B, by definition there is a path P_EB
///   that does not include A. The concatenation of P_EB and P_BC
///   is therefore a path from entry that does not include A, so
///   A does not dominate C.
///
/// Deallocations of an allocation must be dominated by the
/// allocation. If an allocation A is added to the pending set,
/// it must not be the top of the current stack, and the current
/// point must be dominated by the allocation. By the lemma, all
/// of the code visited after A must have been dominated by A,
/// and so all of the allocations currently on the stack above
/// A are dominated by A. Suppose that the search subsequently
/// transitions to a point not dominated by A. At this moment,
/// if A has not yet been popped, it must not be the top of the
/// stack since it is pending. Let B be the allocation that is
/// currently the top of the stack. B must be dominated by A
/// because all code visited after A up to this point was
/// dominated by A. B must also not be in the pending set because
/// it is the top of the stack. After this moment, by the lemma,
/// the search can never return to code that is dominated by A.
/// But A dominates B, so the search can also never return to
/// code dominated by B, because that code would also by dominated
/// by A. That means that a deallocation for B can no longer be
/// reached on this path, so B can never be popped or marked
/// pending.
///
/// In "normal" cases, this situation is impossible for a different
/// reason: SIL's joint post-dominance rule normally requires that
/// join points in the CFG agree about whether an instruction is
/// currently in scope. This means that A must be deallocated
/// before the edge to a block not dominated by A, because the
/// path from entry which does not pass through A cannot possibly
/// have A currently in scope. But this rule is relaxed in dead-end
/// blocks, which can disagree about whether a scope is active
/// as long as they don't try to end it. StackNesting must handle
/// this and not try to emit deallocations in a position not
/// dominated by the deallocation.
///
/// This is simple to prove. Suppose that we are emitting a
/// deallocation for A. The pseudocode only inserts deallocations
/// for allocations when they are in the pending set and it is
/// popping them from the stack. To do this, all instructions above
/// A must already have been popped. Prior to the current instruction,
/// invariant #3 says that there must have been an allocation B that
/// was dominated by A but which was not in the pending set. B must
/// be above A by invariant #1, so if we are popping A, we must have
/// already popped B. Since B was not in the pending set, it can
/// only have been popped because the current instruction is a
/// deallocation of B. Since this deallocation must be dominated
/// by B, and since B is dominated by A, A must dominate the current
/// point.
///
/// Theorem.
///   The algorithm preserves the joint post-dominance of
///   allocations and deallocations.
/// 
/// Proof: Prior to the algorithm running, each allocation is jointly
/// post-dominated by its deallocations. This means that every simple
/// path from the allocation must pass through at most one of its
/// deallocations, and there must be a deallocation if the path reaches
/// the exit.
///
/// Given an allocation A, consider its set of deallocations.
/// Replacing any of these deallocations with a set of deallocations
/// that jointly post-dominate the original deallocation point
/// preserves the joint post-dominance of the original allocation.
/// This is because 

/// The
/// algorithm finds each of these deallocations on some simple path
/// from the entry block. 


// By the definition of joint post-dominance, for each of the
/// deallocations, there cannot be a path from that deallocation to
/// another deallocation 
/// a simple path from the 
/// 
/// 
/// 
///
/// The last things to prove are whether the insertion points of the
/// deallocations jointly post-dominate the allocation and whether
/// they are properly nested with each other.
///
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
StackNesting::Changes StackNesting::fixNesting(SILFunction *F) {
#ifndef NDEBUG
  checkPreconditions(F);
#endif

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

        // Emit any pending deallocations that are on top of the stack.
        emitPendingDeallocations(state, /*after*/ dealloc, madeChanges);

        continue;
      }

      // Otherwise, just remove the deallocation and set the allocation
      // as having a pending deallocation on this path.
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

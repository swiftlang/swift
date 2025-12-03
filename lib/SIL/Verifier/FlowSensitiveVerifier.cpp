//===--- FlowSensitiveVerification.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "FlowSensitiveVerifier.h"

#include "swift/AST/ASTContext.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;
using namespace swift::silverifier;

namespace {

enum class CFGState {
  /// No special rules are in play.
  Normal,
  /// We've followed the resume edge of a yield in a yield_once coroutine.
  YieldOnceResume,
  /// We've followed the unwind edge of a yield.
  YieldUnwind
};

struct BBState {
  std::vector<SILValue> Stack;

  /// Contents: Results of BeginAccessInst*, BeginApplyInst*.
  std::set<SILValue> ActiveOps;

  CFGState CFG = CFGState::Normal;

  GetAsyncContinuationInstBase *GotAsyncContinuation = nullptr;

  BBState() = default;

  // Clang (as of LLVM 22) does not elide the final move for this;
  // see https://github.com/llvm/llvm-project/issues/34037. But
  // GCC and MSVC do, and the clang issue will presumably get fixed
  // eventually, and the move is not an outrageous cost to bear
  // compared to actually copying it.
  BBState(maybe_movable_ref<BBState> other)
      : BBState(std::move(other).construct()) {}

  /// Print for dumping all information.
  void print(llvm::raw_ostream &os) const {
    SILPrintContext ctx(os);
    printStack(ctx, "stack");
    printActiveOps(ctx, "active_ops");
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void printStack(SILPrintContext &ctx, StringRef label) const {
    ctx.OS() << label << ": [";
    if (!Stack.empty())
      ctx.OS() << "\n";
    for (auto allocation : Stack) {
      allocation->print(ctx);
    }
    ctx.OS() << "]\n";
  }

  void printActiveOps(SILPrintContext &ctx, StringRef label) const {
    ctx.OS() << label << ": [";
    if (!ActiveOps.empty())
      ctx.OS() << "\n";
    for (auto op : ActiveOps) {
      op->print(ctx);
    }
    ctx.OS() << "]\n";
  }

  /// Given that we have two edges to the same block or dead-end region,
  /// handle any potential mismatch between the states we were in on
  /// those edges.
  ///
  /// For the most part, we don't allow mismatches and immediately report
  /// them as errors. However, we do allow certain mismatches on edges
  /// that enter dead-end regions,
  /// in which case the states need to be conservatively merged.
  ///
  /// This state is the previously-recorded state of the block, which
  /// is also what needs to be updated for the merge.
  ///
  /// Note that, when we have branches to a dead-end region, we merge
  /// state across *all* branches into the region, not just to a single
  /// block. The existence of multiple blocks in a dead-end region
  /// implies that all of the blocks are in a loop. This means that
  /// flow-sensitive states that were begun externally to the region
  /// cannot possibly change within the region in any well-formed way,
  /// which is why we can merge across all of them.
  ///
  /// For example, consider this function excerpt:
  ///
  ///   bb5:
  ///     %alloc = alloc_stack $Int
  ///     cond_br %cond1, bb6, bb100
  ///   bb6:
  ///     dealloc_stack %alloc
  ///     cond_br %cond2, bb7, bb101
  ///
  /// Now suppose that the branches to bb100 and bb101 are branches
  /// into the same dead-end region. We will conservatively merge the
  /// BBState heading into this region by recognizing that there's
  /// a stack mismatch and therefore clearing the stack (preventing
  /// anything currently on the stack from being deallocated).
  ///
  /// One might think that this is problematic because, e.g.,
  /// bb100 might deallocate %alloc before proceeding to bb101. But
  /// for bb100 and bb101 to be in the *same* dead-end region, they
  /// must be in a strongly-connected component, which means there
  /// must be a path from bb101 back to bb100. That path cannot
  /// possibly pass through %alloc again, or else bb5 would be a
  /// branch *within* the region, not *into* it. So the loop from
  /// bb100 -> bb101 -> ... -> bb100 repeatedly deallocates %alloc
  /// and should be ill-formed.
  ///
  /// That's why it's okay (and necessary) to merge state across
  /// all paths to the dead-end region.
  void handleJoinPoint(const BBState &otherState, bool isDeadEndEdge,
                       SILInstruction *term, SILBasicBlock *succBB) {
    // A helper function for reporting a failure in the edge.

    // Note that this doesn't always abort, e.g. when running under
    // -verify-continue-on-failure. So if there's a mismatch, we do the
    // conservative merge regardless of failure so that we're in a
    // coherent state in the successor block.
    auto fail = [&](StringRef complaint,
                    llvm::function_ref<void(SILPrintContext & out)> extra =
                        nullptr) {
      verificationFailure(complaint, term, [&](SILPrintContext &ctx) {
        ctx.OS() << "Entering basic block " << ctx.getID(succBB) << "\n";

        if (extra)
          extra(ctx);
      });
    };

    // These rules are required to hold unconditionally; there is
    // no merge rule for dead-end edges.
    if (CFG != otherState.CFG) {
      fail("inconsistent coroutine states entering basic block");
    }
    if (GotAsyncContinuation != otherState.GotAsyncContinuation) {
      fail("inconsistent active async continuations entering basic block");
    }

    // The stack normally has to agree exactly, but we allow stacks
    // to disagree on different edges into a dead-end region.
    // Intersecting the stacks would be wrong because we actually
    // cannot safely allow anything to be popped in this state;
    // instead, we simply clear the stack completely. This would
    // allow us to incorrectly pass the function-exit condition,
    // but we know we cannot reach an exit from succBB because it's
    // dead-end.
    if (Stack != otherState.Stack) {
      if (!isDeadEndEdge) {
        fail("inconsistent stack states entering basic block",
             [&](SILPrintContext &ctx) {
               otherState.printStack(ctx, "Current stack state");
               printStack(ctx, "Recorded stack state");
             });
      }

      Stack.clear();
    }

    // The set of active operations normally has to agree exactly,
    // but we allow sets to diverge on different edges into a
    // dead-end region.
    if (ActiveOps != otherState.ActiveOps) {
      if (!isDeadEndEdge) {
        fail("inconsistent active-operations sets entering basic block",
             [&](SILPrintContext &ctx) {
               otherState.printActiveOps(ctx, "Current active operations");
               printActiveOps(ctx, "Recorded active operations");
             });
      }

      // Conservatively remove any operations that aren't also found
      // in the other state's active set.
      for (auto i = ActiveOps.begin(), e = ActiveOps.end(); i != e;) {
        if (otherState.ActiveOps.count(*i)) {
          ++i;
        } else {
          i = ActiveOps.erase(i);
        }
      }
    }
  }

  void handleScopeInst(SILValue i) {
    if (!ActiveOps.insert(i).second) {
      verificationFailure("operation was not ended before re-beginning it", i,
                          nullptr);
    }
  }

  void handleScopeEndingInst(const SILInstruction &i) {
    if (!ActiveOps.erase(i.getOperand(0))) {
      verificationFailure("operation has already been ended", &i, nullptr);
    }
  }

  void handleScopeEndingUse(SILValue value, SILInstruction *origUser) {
    if (!ActiveOps.erase(value)) {
      verificationFailure("operation has already been ended", origUser,
                          nullptr);
    }
  }
};

struct DeadEndRegionState {
  BBState sharedState;
  llvm::SmallPtrSet<SILBasicBlock *, 4> entryBlocks;

  DeadEndRegionState(maybe_movable_ref<BBState> state)
      : sharedState(std::move(state).construct()) {}
};
} // namespace

/// If this is a scope ending inst, return the result from the instruction
/// that provides the scoped value whose lifetime must be ended by some other
/// scope ending instruction.
static SILValue getScopeEndingValueOfScopeInst(SILInstruction *i) {
  if (auto *bai = dyn_cast<BeginAccessInst>(i))
    return bai;
  if (auto *bai = dyn_cast<BeginApplyInst>(i))
    return bai->getTokenResult();
  if (auto *sbi = dyn_cast<StoreBorrowInst>(i))
    return sbi;

  if (auto bi = dyn_cast<BuiltinInst>(i)) {
    if (auto bk = bi->getBuiltinKind()) {
      switch (*bk) {
      case BuiltinValueKind::StartAsyncLetWithLocalBuffer:
        return bi->getResult(0);

      default:
        return SILValue();
      }
    }
  }

  return SILValue();
}

static bool isScopeEndingInst(SILInstruction *i) {
  if (isa<EndAccessInst>(i) || isa<AbortApplyInst>(i) || isa<EndApplyInst>(i)) {
    return true;
  }

  if (auto bi = dyn_cast<BuiltinInst>(i)) {
    if (auto bk = bi->getBuiltinKind()) {
      switch (*bk) {
      case BuiltinValueKind::FinishAsyncLet:
        return true;

      default:
        return false;
      }
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

void swift::silverifier::verifyFlowSensitiveRules(SILFunction *F) {
  if (F->getASTContext().hadError())
    return;

  // Compute which blocks are part of dead-end regions, and start tracking
  // all of the edges to those regions.
  DeadEndEdges deadEnds(F);
  auto visitedDeadEndEdges = deadEnds.createVisitingSet();

  llvm::DenseMap<SILBasicBlock *, BBState> visitedBBs;

  SmallVector<std::optional<DeadEndRegionState>> deadEndRegionStates;
  deadEndRegionStates.resize(deadEnds.getNumDeadEndRegions());

  // Do a traversal of the basic blocks.
  // Note that we intentionally don't verify these properties in blocks
  // that can't be reached from the entry block.
  SmallVector<SILBasicBlock *, 16> Worklist;
  visitedBBs.try_emplace(&*F->begin());
  Worklist.push_back(&*F->begin());
  while (!Worklist.empty()) {
    SILBasicBlock *BB = Worklist.pop_back_val();
    BBState state = visitedBBs[BB];

    for (SILInstruction &i : *BB) {
      auto _require = [&](bool value, StringLiteral msg) {
        if (value)
          return;
        verificationFailure(msg, &i, nullptr);
      };
#define require(condition, complaint)                                          \
  _require(bool(condition), complaint ": " #condition)

      if (i.maySuspend()) {
        // Instructions that may suspend an async context must not happen
        // while the continuation is being accessed, with the exception of
        // the AwaitAsyncContinuationInst that completes suspending the task.
        if (auto aaci = dyn_cast<AwaitAsyncContinuationInst>(&i)) {
          require(state.GotAsyncContinuation == aaci->getOperand(),
                  "encountered await_async_continuation that doesn't match "
                  "active gotten continuation");
          state.GotAsyncContinuation = nullptr;
        } else {
          require(!state.GotAsyncContinuation,
                  "cannot suspend async task while unawaited continuation is "
                  "active");
        }
      }

      if (i.isAllocatingStack()) {
        // Nested stack allocations are pushed on the stack. Non-nested stack
        // allocations are treated as active ops so that we can at least
        // verify their joint post-dominance.
        if (i.isStackAllocationNested()) {
          state.Stack.push_back(i.getStackAllocation());
        } else {
          state.handleScopeInst(i.getStackAllocation());
        }

        // Also track begin_apply's token as an ActiveOp so we can also verify
        // its joint dominance.
        if (auto *bai = dyn_cast<BeginApplyInst>(&i)) {
          state.handleScopeInst(bai->getTokenResult());
        }
        continue;
      }

      if (i.isDeallocatingStack()) {
        SILValue op = i.getOperand(0);
        while (auto *mvi = dyn_cast<MoveValueInst>(op)) {
          op = mvi->getOperand();
        }

        auto *definingInst = op->getDefiningInstruction();

        // First see if we have a begin_inst. In such a case, we need to handle
        // the token result first.
        if (auto *beginInst =
                llvm::dyn_cast_if_present<BeginApplyInst>(definingInst)) {
          // Check if our value is the token result... in such a case, we need
          // to handle scope ending inst and continue.
          if (op == beginInst->getTokenResult()) {
            state.handleScopeEndingUse(op, &i);
            continue;
          }
        }

        // Ok, we have some sort of memory. Lets check if our definingInst is
        // unnested stack memory. In such a case, we want to just verify post
        // dominance and not validate that we are in stack order.
        if (definingInst && definingInst->isAllocatingStack() &&
            !definingInst->isStackAllocationNested()) {
          state.handleScopeEndingUse(op, &i);
          continue;
        }

        // Ok, we have nested stack memory. Lets try to pop the stack. If we
        // fail due to an empty stack or a lack of a match, emit an error.
        if (!state.Stack.empty() && op == state.Stack.back()) {
          state.Stack.pop_back();
          continue;
        }

        verificationFailure(
            "deallocating allocation that is not the top of the stack", &i,
            [&](SILPrintContext &ctx) {
              state.printStack(ctx, "Current stack state");
              ctx.OS() << "Stack allocation:\n" << *op;
              // The deallocation is printed out as the focus of the
              // failure.
            });
        continue;
      }

      if (auto scopeEndingValue = getScopeEndingValueOfScopeInst(&i)) {
        state.handleScopeInst(scopeEndingValue);
        continue;
      }

      if (isScopeEndingInst(&i)) {
        state.handleScopeEndingInst(i);
        continue;
      }

      if (auto *endBorrow = dyn_cast<EndBorrowInst>(&i)) {
        if (isa<StoreBorrowInst>(endBorrow->getOperand())) {
          state.handleScopeEndingInst(i);
        }
        continue;
      }

      if (auto gaci = dyn_cast<GetAsyncContinuationInstBase>(&i)) {
        require(!state.GotAsyncContinuation,
                "get_async_continuation while unawaited continuation is "
                "already active");
        state.GotAsyncContinuation = gaci;
        continue;
      }

      auto *term = dyn_cast<TermInst>(&i);
      if (!term)
        continue;

      if (term->isFunctionExiting()) {
        require(state.Stack.empty(),
                "return with stack allocs that haven't been deallocated");
        require(state.ActiveOps.empty(), "return with operations still active");
        require(!state.GotAsyncContinuation,
                "return with unawaited async continuation");

        if (isa<UnwindInst>(term)) {
          require(state.CFG == CFGState::YieldUnwind,
                  "encountered 'unwind' when not on unwind path");
        } else {
          require(state.CFG != CFGState::YieldUnwind,
                  "encountered 'return' or 'throw' when on unwind path");
          if (isa<ReturnInst>(term) &&
              F->getLoweredFunctionType()->getCoroutineKind() ==
                  SILCoroutineKind::YieldOnce &&
              F->getModule().getStage() != SILStage::Raw) {
            require(state.CFG == CFGState::YieldOnceResume,
                    "encountered 'return' before yielding a value in "
                    "yield_once coroutine");
          }
        }
      }

      if (isa<YieldInst>(term)) {
        require(state.CFG != CFGState::YieldOnceResume,
                "encountered multiple 'yield's along single path");
        require(state.CFG == CFGState::Normal,
                "encountered 'yield' on abnormal CFG path");
        require(!state.GotAsyncContinuation,
                "encountered 'yield' while an unawaited continuation is "
                "active");
      }

      auto successors = term->getSuccessors();
      for (auto i : indices(successors)) {
        SILBasicBlock *succBB = successors[i].getBB();
        auto succStateRef = move_if(state, i + 1 == successors.size());

        // Some successors (currently just `yield`) have state
        // transitions on the edges themselves. Fortunately,
        // these successors all require their destination blocks
        // to be uniquely referenced, so we never have to combine
        // the state change with merging or consistency checking.

        // Check whether this edge enters a dead-end region.
        if (auto deadEndRegion = deadEnds.entersDeadEndRegion(BB, succBB)) {
          // If so, record it in the visited set, which will tell us
          // whether it's the last remaining edge to the region.
          bool isLastDeadEndEdge = visitedDeadEndEdges.visitEdgeTo(succBB);

          // Check for an existing shared state for the region.
          auto &regionInfo = deadEndRegionStates[*deadEndRegion];

          // If we don't have an existing shared state, and this is the
          // last edge to the region, just fall through and process it
          // like a normal edge.
          if (!regionInfo && isLastDeadEndEdge) {
            // This can only happen if there's exactly one edge to the
            // block, so we will end up in the insertion success case below.
            // Note that the state-changing terminators like `yield`
            // always take this path: since this must be the unique edge
            // to the successor, it must be in its own dead-end region.

            // fall through to the main path

            // Otherwise, we need to merge this into the shared state.
          } else {
            require(!isa<YieldInst>(term),
                    "successor of 'yield' should not be encountered twice");

            // Copy/move our current state into the shared state if it
            // doesn't already exist.
            if (!regionInfo) {
              regionInfo.emplace(std::move(succStateRef));

              // Otherwise, merge our current state into the shared state.
            } else {
              regionInfo->sharedState.handleJoinPoint(
                  succStateRef.get(), /*dead end*/ true, term, succBB);
            }

            // Add the successor block to the state's set of entry blocks.
            regionInfo->entryBlocks.insert(succBB);

            // If this was the last branch to the region, act like we
            // just saw the edges to each of its entry blocks.
            if (isLastDeadEndEdge) {
              for (auto ebi = regionInfo->entryBlocks.begin(),
                        ebe = regionInfo->entryBlocks.end();
                   ebi != ebe;) {
                auto *regionEntryBB = *ebi++;

                // Copy/move the shared state to be the state for the
                // region entry block.
                auto insertResult = visitedBBs.try_emplace(
                    regionEntryBB,
                    move_if(regionInfo->sharedState, ebi == ebe));
                assert(insertResult.second &&
                       "already visited edge to dead-end region!");
                (void)insertResult;

                // Add the region entry block to the worklist.
                Worklist.push_back(regionEntryBB);
              }
            }

            // Regardless, don't fall through to the main path.
            continue;
          }
        }

        // Okay, either this isn't an edge to a dead-end region or it
        // was a unique edge to it.

        // Optimistically try to set our current state as the state
        // of the successor.  We can use a move on the final successor;
        // note that if the insertion fails, the move won't actually
        // happen, which is important because we'll still need it
        // to compare against the already-recorded state for the block.
        auto insertResult =
            visitedBBs.try_emplace(succBB, std::move(succStateRef));

        // If the insertion was successful, we need to add the successor
        // block to the worklist.
        if (insertResult.second) {
          auto &insertedState = insertResult.first->second;

          // 'yield' has successor-specific state updates, so we do that
          // now. 'yield' does not permit critical edges, so we don't
          // have to worry about doing this in the case below where
          // insertion failed.
          if (isa<YieldInst>(term)) {
            // Enforce that the unwind logic is segregated in all stages.
            if (i == 1) {
              insertedState.CFG = CFGState::YieldUnwind;

              // We check the yield_once rule in the mandatory analyses,
              // so we can't assert it yet in the raw stage.
            } else if (F->getLoweredFunctionType()->getCoroutineKind() ==
                           SILCoroutineKind::YieldOnce &&
                       F->getModule().getStage() != SILStage::Raw) {
              insertedState.CFG = CFGState::YieldOnceResume;
            }
          }

          // Go ahead and add the block.
          Worklist.push_back(succBB);
          continue;
        }

        // This rule is checked elsewhere, but we'd want to assert it
        // here anyway.
        require(!isa<YieldInst>(term),
                "successor of 'yield' should not be encountered twice");

        // Okay, we failed to insert. That means there's an existing
        // state for the successor block. That existing state generally
        // needs to match the current state, but certain rules are
        // relaxed for branches that enter dead-end regions.
        auto &foundState = insertResult.first->second;

        // Join the states into `foundState`. We can still validly use
        // succStateRef here because the insertion didn't work.
        foundState.handleJoinPoint(succStateRef.get(), /*dead-end*/ false, term,
                                   succBB);
      }
    }
  }

  assert(visitedDeadEndEdges.visitedAllEdges() && "didn't visit all edges");
}

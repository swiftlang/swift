//===--- Reachability.h ---------------------------------------------------===//
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
/// Reachability data flow analysis using a path-discovery worklist. For
/// efficient data flow propagation based on a single SSA value and its uses.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_REACHABILITY_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_REACHABILITY_H

#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILSuccessor.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// Pessimistic, non-iterative data flow for analyzing backward reachability
/// from a set of last uses to their dominating def or nearest barrier.
///
/// Intended for frequently called utilities where minimizing the cost of data
/// flow is more important than analyzing reachability across loops. Expected to
/// visit very few blocks because barriers often occur close to a last use.
///
/// BlockReachability {
///   // True if the beginning of \p block is reachable.
///   // Typically a BasicBlockSet wrapper.
///   bool hasReachableBegin(SILBasicBlock *block);
///
///   // Mark the beginning of a block reachable. Only called once per block.
///   // Typically a BasicBlockSet wrapper.
///   void markReachableBegin(SILBasicBlock *block);
///
///   // Mark the end of a block reachable. Only called once per block.
///   // Typically a BasicBlockSet wrapper.
///   void markReachableEnd(SILBasicBlock *block);
///
///   // Return true if \p inst is a barrier. Called once for each reachable
///   // instruction, assuming that each lastUsePoint is itself a barrier.
///   // Used by the data flow client to perform additional book-keeping,
///   // such as recording debug_value instructions.
///   bool checkReachableBarrier(SILInstruction *inst);
///
///   // Return true if any of \p block's phis are barriers.
///   bool checkReachablePhiBarrier(SILBasicBlock *block);
/// };
template <typename BlockReachability>
class BackwardReachability {
  SILFunction *function;
  BlockReachability &reachableBlocks;
  BasicBlockWorklist cfgWorklist;
  bool solved = false;

public:
  BackwardReachability(SILFunction *function,
                       BlockReachability &reachableBlocks)
      : function(function), reachableBlocks(reachableBlocks),
        cfgWorklist(function) {}

  // Initialize data flow starting points before running solveBackward.
  void initLastUse(SILInstruction *lastUsePoint) {
    assert(!solved && "adding last use after solving!?");
    auto *lastUseBlock = lastUsePoint->getParent();
    auto *previous = lastUsePoint->getPreviousInstruction();
    if (!previous || canReachBlockBegin(previous)) {
      pushPreds(lastUseBlock);
    }
  }

  // Data flow "meet": intersection of successor reachability.
  void solveBackward() {
    while (SILBasicBlock *block = cfgWorklist.popAndForget()) {
      if (!meetOverSuccessors(block))
        continue;

      reachableBlocks.markReachableEnd(block);

      if (canReachBlockBegin(block->getTerminator())) {
        pushPreds(block);
      }
    }
    solved = true;
  }

protected:
  BackwardReachability(BackwardReachability const &) = delete;
  BackwardReachability &operator=(BackwardReachability const &) = delete;

  // Perform a "meet" over successor begin reachability.
  // Return true if \p predecessor's end is pessimistically reachable.
  //
  // Meet:
  // ReachableEnd(predecessor) := intersection(ReachableBegin, successors)
  bool meetOverSuccessors(SILBasicBlock *block) {
    return llvm::all_of(block->getSuccessorBlocks(), [this](auto *successor) {
      return reachableBlocks.hasReachableBegin(successor);
    });
  }

  // Local data flow. Computes the block's flow function.
  bool canReachBlockBegin(SILInstruction *lastReachablePoint) {
    do {
      if (reachableBlocks.checkReachableBarrier(lastReachablePoint))
        return false;
      lastReachablePoint = lastReachablePoint->getPreviousInstruction();
    } while (lastReachablePoint);
    return true;
  }

  // Propagate global data flow from \p succBB to its predecessors.
  void pushPreds(SILBasicBlock *succBB) {
    if (succBB->hasPhi())
      if (reachableBlocks.checkReachablePhiBarrier(succBB))
        return;

    reachableBlocks.markReachableBegin(succBB);

    for (SILBasicBlock *predBB : succBB->getPredecessorBlocks()) {
      cfgWorklist.pushIfNotVisited(predBB);
    }
  }
};

/// Iterative, optimistic data flow for analyzing backward reachability from a
/// set of gens to their dominating def or nearest barrier.
///
/// Uses a worklist approach to avoid extraneous iteration.  Additionally,
/// avoids analyzing reachability in blocks which are known to be unreachable.
///
/// interface Effects {
///     /// The effect, if any, of the specified instruction.
///     Effect effectForInstruction(SILInstruction *);
///
///     /// The effect, if any, of the phis of the specified block.
///     Effect effectForPhi(SILBasicBlock *);
///
///     /// The uses from which reachability will begin.
///     iterable gens();
/// }
template <typename Effects>
class IterativeBackwardReachability final {
public:
  /// How an instruction or phi can modify the state of a block.
  ///
  /// NoEffect - the state is unmodified
  /// Kill - the state becomes unreachable
  /// Gen - the state becomes reachable
  struct Effect final {
    enum class Value {
      NoEffect,
      Kill,
      Gen,
    };
    Value value;

    Effect(Value value) : value(value) {}
    Effect() : value(Value::NoEffect) {}

    static Effect NoEffect() { return {Value::NoEffect}; }
    static Effect Kill() { return {Value::Kill}; }
    static Effect Gen() { return {Value::Gen}; }

    bool isNoEffect() const { return value == Value::NoEffect; }

    /// Form the effect equivalent to first applying this effect and then
    /// applying the specified effect.
    Effect composing(Effect other) const {
      return other.isNoEffect() ? *this : other;
    }

    explicit operator bool() const { return !isNoEffect(); }
    bool operator==(const Effect &other) const { return value == other.value; }
    bool operator!=(const Effect &other) const { return value != other.value; }
  };

  /// How reachable a point in the function is:
  /// - Unreachable: Some path from a gen to here passes through a kill.
  /// - Reachable: No path from a gen point to here passes through a kill.
  /// - Unknown: This point is the unexplored end of a block which contains a
  ///            gen.
  ///
  /// These states form a simple lattice:
  ///
  ///       Unknown (top)
  ///          |
  ///      Reachable
  ///          |
  ///     Unreachable (bottom)
  ///
  /// During initialization, certain points may be set to have the Unknown
  /// state.  After that point, all blocks which have been discovered are
  /// optimistically assumed to have Reachable begin and end states until it is
  /// recorded otherwise.
  struct State final {
    enum class Value {
      Unreachable,
      Reachable,
      Unknown,
    };
    Value value;

    static State Unreachable() { return {Value::Unreachable}; }
    static State Reachable() { return {Value::Reachable}; }
    static State Unknown() { return {Value::Unknown}; }
    State meet(State other) { return value < other.value ? *this : other; }
    State apply(Effect effect) {
      switch (effect.value) {
      case Effect::Value::NoEffect:
        return *this;
      case Effect::Value::Kill:
        return Unreachable();
      case Effect::Value::Gen:
        return Reachable();
      };
    };
    bool operator==(const State &other) { return value == other.value; }
    bool operator!=(const State &other) { return value != other.value; }
  };

  /// The output of the dataflow.
  class Result final {
    /// Blocks whose instructions+phis are summarized as gen.
    BasicBlockSet genBlocks;
    /// Blocks whose instructions+phis are summarized as kill.
    BasicBlockSet killBlocks;
    /// Blocks whose ends are in the ::Unreachable state.
    BasicBlockSet unreachableEndBlocks;
    /// Blocks whose ends are in the ::Unknown state.
    BasicBlockSet unknownEndBlocks;

  public:
    /// The blocks found between the gens and the initialBlocks into which
    /// reachability may extend.
    BasicBlockSetVector discoveredBlocks;
    /// The sublist of gens which are killed within the blocks where they occur.
    llvm::SmallSetVector<SILInstruction *, 32> localGens;

    /// Construct a result for running IterativeBackwardReachability in a given
    /// function.
    Result(SILFunction *function)
        : genBlocks(function), killBlocks(function),
          unreachableEndBlocks(function), unknownEndBlocks(function),
          discoveredBlocks(function){};

    /// The previously recorded end state of the specified block.
    State getEndStateForBlock(SILBasicBlock *block);

    /// The begin state of the specified block.
    State getBeginStateForBlock(SILBasicBlock *block) {
      auto endState = getEndStateForBlock(block);
      return transferEndToBegin(block, endState);
    }

    /// The summary effect of the specified block on reachability, as previously
    /// recorded.
    Effect getEffectForBlock(SILBasicBlock *block);

    /// Record the summary effect of the specified block.
    template <bool SkipOverwrite>
    void setEffectForBlock(SILBasicBlock *block, Effect effect);

    /// Transfer the specified block's provided end state to its begin state by
    /// applying its effect.
    State transferEndToBegin(SILBasicBlock *block, State state) {
      auto effect = getEffectForBlock(block);
      return state.apply(effect);
    }

    /// During initialization, set the specified block's initial state to be
    /// ::Unknown.
    void initializeEndStateToUnknown(SILBasicBlock *block) {
      unknownEndBlocks.insert(block);
    }

    /// Record a transition (down-lattice) of the indicated block to the
    /// specified state.
    bool setEndStateforBlock(SILBasicBlock *block, State state);

  private:
    Result(Result const &) = delete;
    Result &operator=(Result const &) = delete;
  };

  /// Construct a dataflow for the specified function to run from the gens
  /// provided by \p effects to the specified \p initialBlocks.  So that the
  /// result structure can be owned by the caller, it is taken by reference
  /// here.
  ///
  /// If \p initialBlocks is empty, the dataflow may run up to the begin of the
  /// function.
  IterativeBackwardReachability(SILFunction *function,
                                ArrayRef<SILBasicBlock *> initialBlocks,
                                Effects &effects, Result &result)
      : function(function), initialBlocks(function), effects(effects),
        result(result), dataflowWorklist(function) {
    for (auto *block : initialBlocks) {
      this->initialBlocks.insert(block);
    }
  }

  /// Convenience constructor to pass a single initial block.
  static IterativeBackwardReachability
  untilInitialBlock(SILFunction *function, SILBasicBlock *initialBlock,
                    Effects &effects, Result &result) {
    using InitialBlocks = ArrayRef<SILBasicBlock *>;
    InitialBlocks initialBlocks =
        initialBlock ? InitialBlocks(initialBlock) : InitialBlocks();
    return IterativeBackwardReachability(function, initialBlocks, effects,
                                         result);
  }

  /// Step 1: Prepare to run the global dataflow: discover and summarize the
  /// blocks in the relevant region.
  ///
  /// Effects are queried in order to summarize.
  void initialize();

  /// Step 2 (Optional): Add kills beyond those that were already added during
  /// initialization, requerying effects as needed.
  void addKill(SILInstruction *instruction);

  /// Step 3: Propagate reachability through the blocks in the identified
  /// region.
  void solve();

  /// Step 4 (Optional): Visit each barrier instruction, phi, and block.
  ///
  /// Effects are requeried.
  ///
  /// A barrier block here means the target block of a barrier edge.
  ///
  /// interface Visitor {
  ///     void visitBarrierInstruction(SILInstruction *)
  ///     void visitBarrierPhi(SILBasicBlock *)
  ///     void visitBarrierBlock(SILBasicBlock *)
  ///     void visitInitialBlock(SILBasicBlock *)
  /// }
  template <typename Visitor>
  void findBarriers(Visitor &visitor);

private:
  /// How far along the dataflow is.
  enum class Stage {
    /// The dataflow has not done any work.
    Unstarted,
    /// Performing local, sub-block dataflow from gens, determining which gens
    /// are local to and should be stored in \p localGens.
    Initializing,
    /// Discovering blocks into which reachability may extend and lazily doing
    /// local dataflows of those blocks as they are seen.
    SolvingLocal,
    /// Discovery has completed and local dataflow has been performed for all
    /// discovered blocks.  At this point, clients are free to examine the
    /// Result's \p discoveredBlocks add more kills.
    SolvedLocal,
    /// Running the iterative dataflow, draining the worklist of blocks which
    /// are unreachable-at-begin.
    SolvingGlobal,
    /// The dataflow has finished.
    SolvedGlobal,
  };

  /// The function in which the dataflow will run.
  SILFunction *function;
  /// The blocks beyond which the dataflow will not propagate.
  BasicBlockSet initialBlocks;

  /// Input to the dataflow.
  Effects &effects;
  /// Output from the dataflow.
  Result &result;

  /// The worklist for the global data flow.  It consists of blocks whose begin
  /// states have been found to be unreachable; the unreachable-at-begin-ness of
  /// each such block is to be propagated into unreachable-at-end-ness of its
  /// predecessors.
  BasicBlockWorklist dataflowWorklist;
  /// Current activity of the dataflow.
  Stage stage = Stage::Unstarted;

  /// Whether dataflow continues beyond this block.
  bool stopAtBlock(SILBasicBlock *block) {
    return initialBlocks.contains(block) || &*function->begin() == block;
  }

  /// Form the meet of the end state of the provided predecessor with the begin
  /// states of all its successors.
  State meetOverSuccessors(SILBasicBlock *predecessor);

  /// Whether the first (non-NoEffect) effect before the specified instruction
  /// is of the indicated kind.
  bool findLocalEffectBefore(SILInstruction *from, Effect target);

  /// Summarize the effect of the block.
  Effect summarizeLocalEffect(SILBasicBlock *block);

  /// Carry out the portion of initialization corresponding to a single gen.
  void initializeFromGen(SILInstruction *gen);

  /// Propagates the unreachable-at-begin-ness of the specified block into
  /// unreachable-at-end-ness of its predecessors.
  void propagateIntoPredecessors(SILBasicBlock *successor);

  /// Find and visit a single barrier instruction, phi, or block.  Returns true
  /// if one is found.
  ///
  /// A barrier block here means the target block of a barrier edge.
  ///
  /// interface Visitor {
  ///     void visitBarrierInstruction(SILInstruction *)
  ///     void visitBarrierPhi(SILBasicBlock *)
  ///     void visitBarrierBlock(SILBasicBlock *)
  ///     void visitInitialBlock(SILBasicBlock *)
  /// }
  template <typename Visitor>
  bool findBarrier(SILInstruction *from, SILBasicBlock *block,
                   Visitor &visitor);
};

//===----------------------------------------------------------------------===//
// MARK: IterativeBackwardReachability - Initialization
//===----------------------------------------------------------------------===//

/// Prepare the dataflow to run.
///
/// Simultaneously (1) discover the blocks which could be reached and within
/// which consequently the data flow must run and (2) summarize the local
/// effect of each block for use by the dataflow.
///
/// Starting from the gens, find all blocks which might be reached up to and
/// including the initialBlocks.  Summarize the effects of these blocks along
/// the way.
template <typename Effects>
void IterativeBackwardReachability<Effects>::initialize() {
  assert(stage == Stage::Unstarted);
  stage = Stage::Initializing;
  for (auto *gen : effects.gens()) {
    initializeFromGen(gen);
  }
  stage = Stage::SolvingLocal;
  /// Thanks to StackList's iterator guarantees (BasicBlockSetVector uses
  /// StackList's iterator), the iterator won't be invalidated by the
  /// modification that is done within the loop.
  for (auto iterator = result.discoveredBlocks.begin();
       iterator != result.discoveredBlocks.end(); ++iterator) {
    auto *block = *iterator;
    auto effect = summarizeLocalEffect(block);
    result.template setEffectForBlock</*SkipOverwrite=*/true>(block, effect);
    if (effect == Effect::Kill()) {
      // This block is a kill, so it is unreachable at its begin.  Propagate
      // that unreachable-at-begin-ness to unreachable-at-end-ness of its
      // predecessors, if any of them are ever discovered.
      dataflowWorklist.push(block);
      // This block is a kill, so dataflow can't propagate a reachable state
      // through this block to its predecessors.  Don't add its predecessors
      // to the list of discoverable blocks--they can't be reachable by way of
      // this block.  Note though that they may become reachable through other
      // adjacent successors.
      continue;
    }
    if (stopAtBlock(block)) {
      // If dataflow is to stop at this block, it mustn't propagate a reachable
      // state through this block to its predecessors.
      continue;
    }
    for (auto *predecessor : block->getPredecessorBlocks())
      result.discoveredBlocks.insert(predecessor);
  }
  stage = Stage::SolvedLocal;
}

/// Initialize the dataflow for a single gen from effect.gens.
///
/// Summarize and record the local effect of the partial instruction range
/// starting at gen and running to the begin of the block.
template <typename Effects>
void IterativeBackwardReachability<Effects>::initializeFromGen(
    SILInstruction *gen) {
  auto *block = gen->getParent();

  if (findLocalEffectBefore(gen, Effect::Kill())) {
    // If there is a local kill before this gen, then it is a local gen.
    result.localGens.insert(gen);
  } else {
    result.template setEffectForBlock</*SkipOverwrite=*/true>(block,
                                                              Effect::Gen());
    // Only add blocks whose begins are reached to the list of blocks which
    // must participate in the global dataflow.
    result.discoveredBlocks.insert(block);
    // When considering gen in isolation, we don't have enough information to
    // decide whether it is reachable.  It may be reachable, from some other
    // gen, but it may not be.  Set its state to ::Unknown, the top state of the
    // lattice.  During the dataflow, its state may be updated downwards in the
    // lattice.
    result.initializeEndStateToUnknown(block);
  }
}

/// Summarize the effect of the block.
///
/// Compose all the effects in the block, starting from the last instruction and
/// working backwards through the first instruction and the phi if any.
template <typename Effects>
typename IterativeBackwardReachability<Effects>::Effect
IterativeBackwardReachability<Effects>::summarizeLocalEffect(
    SILBasicBlock *block) {
  Effect runningEffect;
  for (auto &instruction : llvm::reverse(*block)) {
    auto effect = effects.effectForInstruction(&instruction);
    runningEffect = runningEffect.composing(effect);
  }
  if (block->hasPhi()) {
    auto effect = effects.effectForPhi(block);
    runningEffect = runningEffect.composing(effect);
  }
  return runningEffect;
}

/// Whether an effect of the indicated kind occurs before the specified
/// instruction.
///
/// Scan the effects in the block backwards from the specified instruction.  If
/// any effect along the way is of the indicated kind, return true.
template <typename Effects>
bool IterativeBackwardReachability<Effects>::findLocalEffectBefore(
    SILInstruction *from, Effect target) {
  assert(target != Effect::NoEffect());
  for (auto *instruction = from->getPreviousInstruction(); instruction;
       instruction = instruction->getPreviousInstruction()) {
    auto effect = effects.effectForInstruction(instruction);
    if (effect == target)
      return true;
  }
  auto *block = from->getParent();
  if (block->hasPhi()) {
    auto effect = effects.effectForPhi(block);
    if (effect == target)
      return true;
  }
  return false;
}

//===----------------------------------------------------------------------===//
// MARK: IterativeBackwardReachability - Modification
//===----------------------------------------------------------------------===//

/// Record a new kill.
///
/// To be called only after initialization.
template <typename Effects>
void IterativeBackwardReachability<Effects>::addKill(
    SILInstruction *instruction) {
  // TODO: Trim discovered set if possible.
  assert(stage == Stage::SolvedLocal);
  auto *block = instruction->getParent();

  // If the kill isn't even in a discovered block, it has no effect.
  if (!result.discoveredBlocks.contains(block))
    return;

  if (result.getEffectForBlock(block) != Effect::Gen()) {
    // If the block wasn't previously a gen block, without further work, we
    // already know that adding a kill to the block makes it a kill block.
    result.template setEffectForBlock</*SkipOverwrite=*/true>(block,
                                                              Effect::Kill());
    return;
  }

  // The block was previously a gen block, so the new kill may or may not make
  // it a kill block; it depends on whether the new kill is above the gen which
  // made the block a gen.  We could resummarize the block, starting from the
  // new kill.  As an optimization, instead, we just walk until we find a gen.

  // Search for a previous gen.
  if (findLocalEffectBefore(instruction, Effect::Gen()))
    // If one is found, the new kill doesn't alter the block's effect.
    return;

  // Since no previous gen was found, the block is now a kill.
  result.template setEffectForBlock</*SkipOverwrite=*/false>(block,
                                                             Effect::Kill());
}

//===----------------------------------------------------------------------===//
// MARK: IterativeBackwardReachability - Solving
//===----------------------------------------------------------------------===//

/// Iteratively solve the global dataflow.
///
/// Drain the worklist which consists of blocks each of whose begins are
/// unreachable, propagating that unreachable-at-begin-ness into
/// unreachable-at-end-ness of its predecessors.
template <typename Effects>
void IterativeBackwardReachability<Effects>::solve() {
  assert(stage == Stage::SolvedLocal);
  stage = Stage::SolvingGlobal;
  // Now that all discovered blocks have been summarized, seed the worklist
  // with those discovered blocks which we can already tell are unreachable
  // at begin.
  for (auto *block : result.discoveredBlocks) {
    auto endState = meetOverSuccessors(block);
    if (result.setEndStateforBlock(block, endState)) {
      // The end state was updated.  See whether that resulted in the begin
      // state becoming unreachable.
      auto beginState = result.getBeginStateForBlock(block);
      if (beginState == State::Unreachable()) {
        // The begin state is now unreachable.  Add the block to the worklist
        // so that its unreachable-at-begin-ness gets propagated to
        // unreachable-at-end-ness of its predecessors.
        dataflowWorklist.pushIfNotVisited(block);
      }
    }
  }
  // Drain the worklist.
  while (auto *block = dataflowWorklist.popAndForget()) {
    // Propagate block's unreachable-at-begin-ness into
    // unreachable-at-end-ness of its predecessors.
    propagateIntoPredecessors(block);
  }
  stage = Stage::SolvedGlobal;
}

/// Propagates the unreachable-at-begin-ness of the specified block into
/// unreachable-at-end-ness of its predecessors.
///
/// If that new unreachable-at-end-ness of any predecessor results in that
/// predecessor being newly unreachable at begin, add the predecessor to the
/// worklist.
///
/// Called with each block from the worklist as it is drained.  A block is added
/// to the worklist only when it is known to be unreachable at begin;
/// consequently, when forming the meet of that block's begin state with its
/// predecessor's end state, we can skip requerying the block's end state--it
/// must be ::Unreachable.
template <typename Effects>
void IterativeBackwardReachability<Effects>::propagateIntoPredecessors(
    SILBasicBlock *successor) {
  // State isn't tracked above the blocks dataflow stops at.  Don't propagate
  // state changes into its predecessors.
  if (stopAtBlock(successor))
    return;
  assert(result.getBeginStateForBlock(successor) == State::Unreachable() &&
         "propagating unreachability into predecessors of block whose begin is "
         "reachable?!");
  for (auto *predecessor : successor->getPredecessorBlocks()) {
    // Blocks are added to the worklist whenever they are found to be
    // unreachable at begin.  Specifically, no checks are done at that time that
    // the block's predecessors are discovered.  Check now.
    if (!result.discoveredBlocks.contains(predecessor))
      continue;
    auto oldEndState = result.getEndStateForBlock(predecessor);
    auto newEndState = State::Unreachable().meet(oldEndState);
    if (result.setEndStateforBlock(predecessor, newEndState)) {
      // The end state of predecessor was changed.  Its begin state may be newly
      // ::Unreachable.
      if (result.getBeginStateForBlock(predecessor) == State::Unreachable()) {
        // Propagate predecessor's new unreachable-at-begin-ness to
        // unreachable-at-end-ness of _its_ predecessors.
        dataflowWorklist.pushIfNotVisited(predecessor);
      }
    }
  }
}

template <typename Effects>
typename IterativeBackwardReachability<Effects>::State
IterativeBackwardReachability<Effects>::meetOverSuccessors(
    SILBasicBlock *predecessor) {
  auto state = result.getEndStateForBlock(predecessor);
  for (auto *successor : predecessor->getSuccessorBlocks()) {
    state = state.meet(result.getBeginStateForBlock(successor));
  }
  return state;
}

//===----------------------------------------------------------------------===//
// MARK: IterativeBackwardReachability - Barrier Discovery
//===----------------------------------------------------------------------===//

/// Backwards walk discovered blocks and partial blocks starting at gens,
/// visiting barrier instructions, phis, and blocks.
///
/// interface Visitor {
///     /// Invoked with an instruction which is a barrier.
///     void visitBarrierInstruction(SILInstruction *)
///
///     /// Invoked with a block one of whose phi arguments is a barrier.
///     void visitBarrierPhi(SILBasicBlock *)
///
///     /// Invoked with target of an edge which is a barrier.
///     ///
///     /// Given a block P only some of whose successors S are
///     /// reachable-at-begin, an edge E from P to S is a barrier.  This
///     /// function is invoked for each such edge E with the block S.  Thanks
///     /// to the lack of critical edges in SIL, that uniquely identifies the
///     /// edge as the only edge to S.
///     ///
///     /// Additionally, this may be invoked with the effective def block if
///     /// no barriers are found between the gens and it.
///     void visitBarrierBlock(SILBasicBlock *)
///
///     /// Invoked with either the function entry block or one of the specified
///     /// initial blocks.
///     void visitInitialBlock(SILBasicBlock *)
/// }
template <typename Effects>
template <typename Visitor>
void IterativeBackwardReachability<Effects>::findBarriers(Visitor &visitor) {
  assert(stage == Stage::SolvedGlobal);
  for (auto *block : result.discoveredBlocks) {
    switch (result.getEndStateForBlock(block).value) {
    case State::Value::Reachable:
      findBarrier(&block->back(), block, visitor);
      break;
    case State::Value::Unreachable:
      for (auto *successor : block->getSuccessorBlocks()) {
        if (result.getBeginStateForBlock(successor) == State::Reachable()) {
          visitor.visitBarrierBlock(successor);
        }
      }
      break;
    case State::Value::Unknown:
      // A block which contained a gen which was never overwritten by global
      // data flow.
      break;
    }
  }
  for (auto *gen : effects.gens()) {
    bool foundBarrier =
        findBarrier(gen->getPreviousInstruction(), gen->getParent(), visitor);
    assert((foundBarrier || !result.localGens.contains(gen)) &&
           "local gen without in-block kill?!");
    (void)foundBarrier;
  }
}

/// Find a single barrier, starting from the specified instruction (nullable)
/// within the indicated block (non-nullable) and visit it.
///
/// Return whether a barrier was found.
///
/// interface Visitor {
///     /// Invoked with an instruction which is a barrier.
///     void visitBarrierInstruction(SILInstruction *)
///
///     /// Invoked with a block one of whose phi arguments is a barrier.
///     void visitBarrierPhi(SILBasicBlock *)
///
///     /// Invoked with target of an edge which is a barrier.
///     ///
///     /// Given a block P only some of whose successors S are
///     /// reachable-at-begin, an edge E from P to S is a barrier.  This
///     /// function is invoked for each such edge E with the block S.  Thanks
///     /// to the lack of critical edges in SIL, that uniquely identifies the
///     /// edge as the only edge to S.
///     ///
///     /// Additionally, this may be invoked with the effective def block if
///     /// no barriers are found between the gens and it.
///     void visitBarrierBlock(SILBasicBlock *)
///
///     /// Invoked with either the function entry block or one of the specified
///     /// initial blocks.
///     void visitInitialBlock(SILBasicBlock *)
/// }
template <typename Effects>
template <typename Visitor>
bool IterativeBackwardReachability<Effects>::findBarrier(SILInstruction *from,
                                                         SILBasicBlock *block,
                                                         Visitor &visitor) {
  for (auto *instruction = from; instruction;
       instruction = instruction->getPreviousInstruction()) {
    auto effect = effects.effectForInstruction(instruction);
    if (!effect)
      continue;
    if (effect == Effect::Gen()) {
      continue;
    }
    // effect == Effect::Kill
    visitor.visitBarrierInstruction(instruction);
    return true;
  }
  if (block->hasPhi()) {
    if (auto effect = effects.effectForPhi(block)) {
      if (effect == Effect::Kill()) {
        visitor.visitBarrierPhi(block);
        return true;
      }
    }
  }
  assert(result.getEffectForBlock(block) != Effect::Kill());
  if (stopAtBlock(block)) {
    visitor.visitInitialBlock(block);
    return true;
  }
  return false;
}

//===----------------------------------------------------------------------===//
// MARK: IterativeBackwardReachability::Result - State
//===----------------------------------------------------------------------===//

/// The previously recorded end state of the specified block.
template <typename Effects>
typename IterativeBackwardReachability<Effects>::State
IterativeBackwardReachability<Effects>::Result::getEndStateForBlock(
    SILBasicBlock *block) {
  if (!discoveredBlocks.contains(block))
    return State::Unreachable();
  if (unreachableEndBlocks.contains(block))
    return State::Unreachable();
  if (unknownEndBlocks.contains(block))
    return State::Unknown();
  return State::Reachable();
}

/// Record a transition of the indicated block to the specified state.
///
/// States must transition down the State lattice.
///
/// The state is recorded in:
/// - unreachableEndBlocks; blocks contained here are ::Unreachable
/// - unknownEndBlocks; blocks contained here are ::Unknown
/// Blocks that are in neither are ::Reachable.
template <typename Effects>
bool IterativeBackwardReachability<Effects>::Result::setEndStateforBlock(
    SILBasicBlock *block, State state) {
  switch (state.value) {
  case State::Value::Unreachable: {
    auto previouslyUnknown = unknownEndBlocks.contains(block);
    unknownEndBlocks.erase(block);
    auto notPreviouslyUnreachable = unreachableEndBlocks.insert(block);
    assert(!previouslyUnknown ||
           notPreviouslyUnreachable &&
               "block was previously in both unknown and unreachable?!");
    return previouslyUnknown || notPreviouslyUnreachable;
  }
  case State::Value::Reachable: {
    auto previouslyUnknown = unknownEndBlocks.contains(block);
    unknownEndBlocks.erase(block);
    assert(!unreachableEndBlocks.contains(block) &&
           "transitioning up lattice?! unreachable->reachable");
    return previouslyUnknown;
  }
  case State::Value::Unknown: {
    auto notPreviouslyUnknown = unknownEndBlocks.insert(block);
    assert(!unreachableEndBlocks.contains(block) &&
           "transitioning up lattice?! unreachable->unknown");
    assert(!notPreviouslyUnknown &&
           "transitioning up lattice?! reachable->unknown");
    return notPreviouslyUnknown;
  }
  }
}

//===----------------------------------------------------------------------===//
// MARK: IterativeBackwardReachability::Result - Effects
//===----------------------------------------------------------------------===//

/// The summary effect of the specified block on reachability, as previously
/// recorded.
template <typename Effects>
typename IterativeBackwardReachability<Effects>::Effect
IterativeBackwardReachability<Effects>::Result::getEffectForBlock(
    SILBasicBlock *block) {
  if (genBlocks.contains(block))
    return Effect::Gen();
  if (killBlocks.contains(block))
    return Effect::Kill();
  return Effect::NoEffect();
}

/// Record the summary effect of the specified block.
///
/// If it is statically known that the block has not already had its effect set
/// to an incompatible value, we can avoid clearing the old effect.  For
/// example, if this is the first time a block's state has been set, or if we
/// are setting a block's state to ::Gen and we already know it isn't ::Kill.
template <typename Effects>
template <bool SkipOverwrite>
void IterativeBackwardReachability<Effects>::Result::setEffectForBlock(
    SILBasicBlock *block, Effect effect) {
  switch (effect.value) {
  case Effect::Value::NoEffect:
    if (!SkipOverwrite) {
      genBlocks.erase(block);
      killBlocks.erase(block);
    }
    return;
  case Effect::Value::Gen:
    genBlocks.insert(block);
    if (!SkipOverwrite)
      killBlocks.erase(block);
    return;
  case Effect::Value::Kill:
    killBlocks.insert(block);
    if (!SkipOverwrite)
      genBlocks.erase(block);
    return;
  }
}

//===----------------------------------------------------------------------===//
// MARK: findBarriersBackward
//===----------------------------------------------------------------------===//

using llvm::ArrayRef;
using llvm::function_ref;

struct ReachableBarriers final {
  /// Instructions which are barriers.
  llvm::SmallVector<SILInstruction *, 4> instructions;

  /// Blocks one of whose phis is a barrier.
  llvm::SmallVector<SILBasicBlock *, 4> phis;

  /// Boundary edges; edges such that
  /// (1) the target block is reachable-at-begin
  /// (2) at least one adjacent edge's target is not reachable-at-begin.
  llvm::SmallVector<SILBasicBlock *, 4> edges;

  /// Terminal blocks that were reached; blocks such that
  /// (1) the block is reachable-at-begin
  /// (2) it is either the function's entry block or one of the blocks passed
  /// to findBarriersBackward's \p initialBlocks parameter.
  llvm::SmallVector<SILBasicBlock *, 2> initialBlocks;

  ReachableBarriers() {}
  ReachableBarriers(ReachableBarriers const &) = delete;
  ReachableBarriers &operator=(ReachableBarriers const &) = delete;
};

/// Walk backwards from the specified \p roots through at the earliest \p
/// initialBlocks to populate \p barriers by querying \p isBarrier along the
/// way.
///
/// If \p initialBlocks is empty, dataflow continues to the begin of the
/// function.
void findBarriersBackward(ArrayRef<SILInstruction *> roots,
                          ArrayRef<SILBasicBlock *> initialBlocks,
                          SILFunction &function, ReachableBarriers &barriers,
                          function_ref<bool(SILInstruction *)> isBarrier);

} // end namespace swift

#endif

//===--- SSAUpdater.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

/// Utility for constructing SSA form for a value which is redefined in multiple basic blocks,
/// inserting phi arguments (block arguments fed by `br` instructions) wherever control flow
/// merges values coming from different definitions.
///
/// Usage:
/// 1. Register the values that are already available at the end of certain blocks via
///    `addAvailableValue`.
/// 2. Query the value that is visible at the end of, or at the beginning of, some block via
///    `getValue(atEndOf:)` or `getValue(atBeginOf:)`. Missing values are constructed on demand by
///    inserting phi arguments where needed.
///
/// All available values must be registered with `addAvailableValue` before the first `getValue`
/// call that could reach the corresponding block.
///
public struct SSAUpdater<Context: MutatingContext> {
  private let context: Context
  private let type: Type
  private let ownership: Ownership

  /// All blocks between available-value blocks (exclusive) and getValue blocks (inclusive)
  private var liverange: BasicBlockWorklist

  /// Values for blocks which are explicitly set via `addAvailableValue`
  private var availableValues = Dictionary<BasicBlock, Value>()

  private var computedValuesAtBeginOfBlocks = Dictionary<BasicBlock, Value>()

  /// The phi arguments which were inserted so far by `getValue`, in the order they were created.
  public var insertedPhis = [Phi]()

  /// Creates a new SSA-updater for values of `type` with the given `ownership`.
  ///
  /// All values that are constructed or merged by this updater (available values passed to
  /// `addAvailableValue` and inserted phi arguments) must have this `type` and `ownership`.
  public init(type: Type, ownership: Ownership, _ context: Context) {
    self.context = context
    self.type = type
    self.ownership = ownership
    self.liverange = BasicBlockWorklist(context)
  }

  public mutating func deinitialize() {
    liverange.deinitialize()
  }

  /// Registers `value` as the available value at the *end* of `block`.
  ///
  /// Must be called before any `getValue` call which could reach `block` in its search for
  /// available values, i.e. before `block` is included in the liverange of a query.
  public mutating func addAvailableValue(_ value: Value, in block: BasicBlock) {
    precondition(!liverange.hasBeenPushed(block), "added an available value after a value has been queried")
    availableValues[block] = value
  }

  /// Returns the value that is live at the *end* of `block`, i.e. the value seen by a use in a
  /// successor block.
  ///
  /// If `block` has a registered available value, that value is returned directly. Otherwise
  /// this falls back to `getValue(atBeginOf:)`.
  public mutating func getValue(atEndOf block: BasicBlock) -> Value {
    if let value = availableValues[block] {
      return value
    }
    return getValue(atBeginOf: block)
  }

  /// Returns the value that is live at the *beginning* of `block`, constructing SSA form for it.
  ///
  /// If `block` is not reachable from any available value along some path (e.g. it's the
  /// function's entry block, or an unreachable block), that path contributes `Undef`.
  ///
  public mutating func getValue(atBeginOf block: BasicBlock) -> Value {
    let numExistingPhis = insertedPhis.count

    // Walk backwards from `block` along predecessors to find the (already computed or available)
    // values that reach `block`.
    // and returns the (possibly newly inserted phi) value which is visible at
    // the beginning of `block`.
    //
    let blocksInLiverange = computeLiverange(startingAt: block)

    // Populate values for all blocks in the liverange and insert phi arguments in blocks
    // where control flow merges different values.
    while propagateValuesThrough(blocks: blocksInLiverange) {
    }

    for i in (numExistingPhis ..< insertedPhis.count) {
      addBranchArguments(for: insertedPhis[i])
    }

    return computedValuesAtBeginOfBlocks[block]!
  }

  /// Collects all blocks which need a computed value in order to determine the value at
  /// `startBlock`, by walking backwards from `startBlock` along predecessors, stopping at blocks
  /// which already have an available or computed value.
  private mutating func computeLiverange(startingAt startBlock: BasicBlock) -> [BasicBlock] {
    var blocksInLiverange = [BasicBlock]()

    liverange.pushIfNotVisited(startBlock)
    while let block = liverange.pop() {
      blocksInLiverange.append(block)
      for pred in block.predecessors where availableValues[pred] == nil {
        liverange.pushIfNotVisited(pred)
      }
    }
    return blocksInLiverange
  }

  /// Computes the value at the beginning of each block in `blocks`, deriving it from the
  /// (already available or already computed) values at the end of its predecessors, inserting a
  /// phi argument whenever predecessors disagree.
  ///
  /// This must be called repeatedly until it returns `false`, because a block's value can depend
  /// on a not yet computed value of a not yet processed block (e.g. in a loop). Returns whether
  /// any block's value changed in this call, i.e. whether another call is needed to reach a
  /// fixed point.
  private mutating func propagateValuesThrough(blocks: [BasicBlock]) -> Bool {
    var changed = false

    // The `blocks` contains block in backward control flow order. We want to propagate values in
    // forward order. Therefore we have to reverse the list.
    for block in blocks.reversed() {
      if let existingValue = computedValuesAtBeginOfBlocks[block] {
        if existingValue.isPhi(in: block) {
          // Avoid creating another phi if we already have one created in this block.
          continue
        }
        guard let value = deriveValueFromPredecessors(of: block) else {
          assert(block.predecessors.isEmpty, "should have a predecessor value here, except for the entry block")
          continue
        }
        if value != existingValue {
          assert(Phi(value) != nil, "a mismatching value can only be a phi argument")
          // `existingValue` can be stale: it may have been computed before a phi was inserted
          // further up the control flow graph, and so far that phi has only been propagated into
          // some of `block`'s predecessors. Any block that merely forwarded `existingValue`
          // (instead of deriving a phi of its own) must be invalidated as well, so it gets
          // recomputed from the now-current predecessor values on the next iteration instead of
          // continuing to reference the stale one.
          clearValues(value: existingValue, startingAt: block)

          computedValuesAtBeginOfBlocks[block] = value
          changed = true
        }
      } else {
        if block.predecessors.isEmpty {
          // The liverange computation can only reach the function entry (or an unreachable block)
          // if an available value is missing on some path. This is a corner case.
          let undef = Undef.get(type: type, context)
          computedValuesAtBeginOfBlocks[block] = undef
        } else if let value = deriveValueFromPredecessors(of: block) {
          computedValuesAtBeginOfBlocks[block] = value
          changed = true
        }
      }
    }
    return changed
  }

  /// Returns the single value which reaches `block` from all of its predecessors, inserting a
  /// new phi argument in `block` if predecessors disagree. Returns `nil` if no predecessor has
  /// a value yet (which means `block` cannot yet be resolved in this fixed-point iteration).
  private mutating func deriveValueFromPredecessors(of block: BasicBlock) -> Value? {
    var uniqueValue: Value?
    for predecessor in block.predecessors {
      if let v = getComputedValue(atEndOf: predecessor) {
        if let previous = uniqueValue {
          if previous != v {
            let phiArg = block.addArgument(type: type, ownership: ownership, context)
            insertedPhis.append(Phi(phiArg)!)
            return phiArg
          }
        } else {
          uniqueValue = v
        }
      }
    }
    return uniqueValue
  }

  /// Returns the value at the end of `block`: either its available value, or its already
  /// computed value at the beginning of the block.
  private func getComputedValue(atEndOf block: BasicBlock) -> Value? {
    if let availableValue = availableValues[block] {
      return availableValue
    }
    return computedValuesAtBeginOfBlocks[block]
  }

  /// Removes the cached value of `startBlock` and, transitively, of all successors whose cached
  /// value is exactly `value` (i.e. blocks which merely forwarded `value` rather than deriving a
  /// phi of their own), so they get recomputed from their (now possibly different) predecessor
  /// values in the next `propagateValuesThrough` iteration.
  private mutating func clearValues(value: Value, startingAt startBlock: BasicBlock) {
    var worklist = BasicBlockWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(startBlock)
    while let block = worklist.pop() {
      if let v = computedValuesAtBeginOfBlocks[block], v == value {
        // Don't clear the block which _defines_ `value` as its own phi. Such a block doesn't
        // merely forward `value`, it is its definition. This can happen if `value` is a loop-header
        // phi and the successor walk reaches the header again via a back-edge. Clearing it would
        // drop the genuine phi and cause a duplicate phi to be inserted on the next iteration.
        if value.isPhi(in: block) {
          continue
        }
        computedValuesAtBeginOfBlocks.removeValue(forKey: block)
        worklist.pushIfNotVisited(contentsOf: block.successors)
      }
    }
  }

  /// Rewrites each `br` predecessor of `phi`'s block to also pass the value that is live at the
  /// end of that predecessor, making `phi` an actual SIL phi argument.
  private func addBranchArguments(for phi: Phi) {
    var previousPredVal: Value? = nil
    var diff = false
    for pred in phi.predecessors {
      let oldBranch = pred.terminator as! BranchInst
      let builder = Builder(before: oldBranch, context)
      let predVal = getComputedValue(atEndOf: pred)!
      if predVal != phi.value {
        if let previousPredVal, predVal != previousPredVal {
          diff = true
        }
        previousPredVal = predVal
      }
      builder.createBranch(to: oldBranch.targetBlock, arguments: Array(oldBranch.operands.values) + [predVal])
      context.erase(instruction: oldBranch)
    }
    assert(diff, "inserted a phi argument for identical input values")
  }
}

/// A variant of `SSAUpdater` that tracks available values at instruction granularity
/// rather than just per-block.
///
/// `SSAUpdater` only knows about one available value per basic block. If multiple values
/// are defined in the same block (e.g. because a value is redefined multiple times while
/// iterating over a block), it cannot tell which of them is visible at a given use point
/// within that block. `InstructionBasedSSAUpdater` fixes this by additionally recording
/// the instruction for each block after which a value becomes available, and picking the
/// closest dominating value for an instruction query in the same block. Queries in other
/// blocks fall back to the normal per-block SSA construction of `SSAUpdater`.
public struct InstructionBasedSSAUpdater<Context: MutatingContext> {

  /// The available values within a single basic block, ordered by their position in the block.
  private struct ValuesInBlock {
    var values = SingleInlineArray<(value: Value, atInstruction: Instruction)>()
    var lastInstruction: Instruction? = nil
    var isSorted = false

    /// Records `value` as available after `instruction`.
    ///
    /// Returns true if `instruction` is the (currently) last instruction which makes a value
    /// available in this block, i.e. if `value` needs to be registered with the underlying
    /// `SSAUpdater` as *the* available value for this block.
    mutating func add(value: Value, after instruction: Instruction) -> Bool {
      precondition(!isSorted, "cannot add value after getValue")
      values.append((value, instruction))
      if lastInstruction == nil || lastInstruction!.strictlyDominatesInBlock(instruction) {
        lastInstruction = instruction
        return true
      } else {
        return false
      }
    }

    /// Returns the available value which is defined closest to, but still strictly before,
    /// `instruction` in this block, or nil if no available value precedes `instruction`.
    ///
    /// Lazily sorts `values` by their position in the block on first access (further calls to
    /// `add` are disallowed afterwards) and then does a binary search for the closest preceding
    /// value.
    mutating func getAvailableValue(before instruction: Instruction) -> Value? {
      if values.isEmpty {
        return nil
      }
      if !isSorted {
        values.sort(by: { $0.atInstruction.strictlyDominatesInBlock($1.atInstruction) })
        isSorted = true
      }

      let vals = values
      var low = 0
      var high = vals.endIndex - 1

      while low != high {
        let mid = (low + high + 1) / 2
        if instruction.dominatesInBlock(vals[mid].atInstruction) {
          high = mid - 1
        } else {
          low = mid
        }
      }
      let (value, atInstruction) = vals[low]
      if atInstruction.strictlyDominatesInBlock(instruction) {
        return value
      }
      return nil
    }
  }

  private var ssaUpdater: SSAUpdater<Context>

  /// The available values, per block, added so far via `addAvailableValue`.
  private var availableValues = Dictionary<BasicBlock, ValuesInBlock>()

  public init(type: Type, ownership: Ownership, _ context: Context) {
    self.ssaUpdater = SSAUpdater(type: type, ownership: ownership, context)
  }

  public mutating func deinitialize() {
    ssaUpdater.deinitialize()
  }

  /// Registers `value` as available for uses which are dominated by `instruction` within its block.
  public mutating func addAvailableValue(_ value: Value, after instruction: Instruction) {
    let block = instruction.parentBlock
    if availableValues[block, default: ValuesInBlock()].add(value: value, after: instruction) {
      ssaUpdater.addAvailableValue(value, in: block)
    }
  }

  /// Constructs SSA for a value that is live at `instruction`.
  ///
  /// If an available value was added after some instruction which precedes `instruction` in the
  /// same block, that value is returned directly. Otherwise this falls back to `SSAUpdater`,
  /// which may insert phi arguments to merge values coming from different blocks.
  public mutating func getValue(before instruction: Instruction) -> Value {
    if var valuesInBlock = availableValues[instruction.parentBlock],
       let value = valuesInBlock.getAvailableValue(before: instruction)
    {
      return value
    }
    return ssaUpdater.getValue(atBeginOf: instruction.parentBlock)
  }

  /// The phi arguments which were inserted so far by `getValue`, in the order they were created.
  public var insertedPhis: [Phi] { ssaUpdater.insertedPhis }
}

private extension Value {
  func isPhi(in block: BasicBlock) -> Bool {
    if let arg = self as? Argument, arg.parentBlock == block {
      return true
    }
    return false
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let ssaUpdaterTest = Test("ssa_updater") {
  function, arguments, context in

  // Available values are all `integer_literal` instructions, defined at their use points.
  // Then all `undef` operands are replaced by the value at that point.

  let availableValues = getTestAvailableValues(in: function)

  guard let firstValue = availableValues.first else {
    return
  }

  var ssaUpdater = SSAUpdater(type: firstValue.type, ownership: firstValue.ownership, context)
  defer { ssaUpdater.deinitialize() }

  var availableValuesInBlocks = Dictionary<BasicBlock, IntegerLiteralInst>()

  for v in availableValues {
    ssaUpdater.addAvailableValue(v, in: v.parentBlock)
    availableValuesInBlocks[v.parentBlock] = v
  }

  for block in function.blocks {
    for inst in block.instructions {
      for op in inst.operands where op.value is Undef && op.value.type == firstValue.type {
        let v: Value
        if let available = availableValuesInBlocks[block], available.strictlyDominatesInBlock(inst) {
          v = ssaUpdater.getValue(atEndOf: block)
        } else {
          v = ssaUpdater.getValue(atBeginOf: block)
        }
        op.set(to: v, context)
      }
    }
  }
}

let instructionBasedSSAUpdaterTest = Test("instruction_based_ssa_updater") {
  function, arguments, context in

  // Available values are all `integer_literal` instructions, defined at their use points.
  // Then all `undef` operands are replaced by the value at that point.

  let availableValues = getTestAvailableValues(in: function)

  guard let firstValue = availableValues.first else {
    return
  }

  var ssaUpdater = InstructionBasedSSAUpdater(type: firstValue.type, ownership: firstValue.ownership, context)
  defer { ssaUpdater.deinitialize() }

  for v in availableValues {
    for user in v.users {
      ssaUpdater.addAvailableValue(v, after: user)
    }
  }

  for inst in function.instructions {
    for op in inst.operands where op.value is Undef && op.value.type == firstValue.type {
      let v = ssaUpdater.getValue(before: inst)
      op.set(to: v, context)
    }
  }
}

private func getTestAvailableValues(in function: Function) -> [IntegerLiteralInst] {
  var availableValues = [IntegerLiteralInst]()

  for inst in function.instructions {
    if let il = inst as? IntegerLiteralInst {
      availableValues.append(il)
    }
  }

  availableValues.sort(by: { $0.value! < $1.value! })

  return availableValues
}

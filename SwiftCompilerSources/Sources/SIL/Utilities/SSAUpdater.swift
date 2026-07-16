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

/// Utility for updating SSA for a set of SIL instructions defined in multiple blocks.
public struct SSAUpdater<Context: MutatingContext> {
  let context: Context

  public init(type: Type, ownership: Ownership, _ context: Context) {
    self.context = context
    context._bridged.SSAUpdater_initialize(type.bridged, ownership._bridged)
  }

  public mutating func addAvailableValue(_ value: Value, in block: BasicBlock) {
    context._bridged.SSAUpdater_addAvailableValue(block.bridged, value.bridged)
  }

  /// Construct SSA for a value that is live at the *end* of a basic block.
  public mutating func getValue(atEndOf block: BasicBlock) -> Value {
    context.notifyInstructionsChanged()
    return context._bridged.SSAUpdater_getValueAtEndOfBlock(block.bridged).value
  }

  /// Construct SSA for a value that is live in the *middle* of a block.
  /// This handles the case where the use is before a definition of the value in the same block.
  ///
  ///   bb1:
  ///     %1 = def
  ///     br bb2
  ///   bb2:
  ///       = use(?)
  ///    %2 = def
  ///    cond_br bb2, bb3
  ///
  /// In this case we need to insert a phi argument in bb2, merging %1 and %2.
  public mutating func getValue(inMiddleOf block: BasicBlock) -> Value {
    context.notifyInstructionsChanged()
    return context._bridged.SSAUpdater_getValueInMiddleOfBlock(block.bridged).value
  }

  public var insertedPhis: [Phi] {
    var phis = [Phi]()
    let numPhis = context._bridged.SSAUpdater_getNumInsertedPhis()
    phis.reserveCapacity(numPhis)
    for idx in 0..<numPhis {
      phis.append(Phi(context._bridged.SSAUpdater_getInsertedPhi(idx).value)!)
    }
    return phis
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
    return ssaUpdater.getValue(inMiddleOf: instruction.parentBlock)
  }

  public var insertedPhis: [Phi] { ssaUpdater.insertedPhis }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let instructionBasedSSAUpdaterTest = Test("instruction_based_ssa_updater") {
  function, arguments, context in

  // Available values are all `integer_literal` instructions, defined at their use points.
  // Then all `undef` operands are replaced by the value at that point.

  var availableValues = [IntegerLiteralInst]()

  for inst in function.instructions {
    if let il = inst as? IntegerLiteralInst {
      availableValues.append(il)
    }
  }

  availableValues.sort(by: { $0.value! < $1.value! })

  guard let firstValue = availableValues.first else {
    return
  }

  var ssaUpdater = InstructionBasedSSAUpdater(type: firstValue.type, ownership: firstValue.ownership, context)

  for v in availableValues {
    for user in v.users {
      ssaUpdater.addAvailableValue(v, after: user)
    }
  }

  for inst in function.instructions {
    for op in inst.operands {
      if op.value is Undef {
        let v = ssaUpdater.getValue(before: inst)
        op.set(to: v, context)
      }
    }
  }
}


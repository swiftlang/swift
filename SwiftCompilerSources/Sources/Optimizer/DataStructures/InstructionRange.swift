//===--- InstructionRange.swift - a range of instructions -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// A range of instructions.
///
/// The `InstructionRange` defines a range from a dominating "begin" instruction to one or more "end" instructions.
/// The range is "exclusive", which means that the "end" instructions are not part of the range.
///
/// One or more "potential" end instructions can be inserted.
/// Though, not all inserted instructions end up as "end" instructions.
///
/// `InstructionRange` is useful for calculating the liverange of values.
///
/// The `InstructionRange` is similar to a `BasicBlockRange`, but defines the range
/// in a "finer" granularity, i.e. on instructions instead of blocks.
/// `InstructionRange` uses an underlying `BasicBlockRange` to compute the
/// involved blocks of the instruction range.
///
/// There are several kind of instructions:
/// * begin: it is a single instruction which dominates all instructions of the range
/// * ends: all inserted instruction which are at the end of the range
/// * exits: the first instructions of the exit blocks
/// * interiors: all inserted instructions which are not end instructions.
///
/// See also `BasicBlockRange` for more information.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct InstructionRange : CustomStringConvertible, NoReflectionChildren {
  
  /// The underlying block range.
  private(set) var blockRange: BasicBlockRange

  private var insertedInsts: InstructionSet

  // For efficiency, this set does not include instructions in blocks which are not the begin or any end block.
  private var inExclusiveRange: InstructionSet

  init(begin beginInst: Instruction, _ context: some Context) {
    self = InstructionRange(beginBlock: beginInst.parentBlock, context)
    self.inExclusiveRange.insert(beginInst)
  }

  init<S: Sequence>(begin beginInst: Instruction, ends: S, _ context: some Context) where S.Element: Instruction {
    self = InstructionRange(begin: beginInst, context)
    insert(contentsOf: ends)
  }

  init(for value: Value, _ context: some Context) {
    if let inst = value.definingInstruction {
      self = InstructionRange(begin: inst, context)
    } else if let arg = value as? Argument {
      self = InstructionRange(beginBlock: arg.parentBlock, context)
    } else {
      fatalError("cannot build an instruction range for \(value)")
    }
  }

  private init(beginBlock: BasicBlock, _ context: some Context) {
    self.blockRange = BasicBlockRange(begin: beginBlock, context)
    self.insertedInsts = InstructionSet(context)
    self.inExclusiveRange = InstructionSet(context)
  }

  /// Insert a potential end instruction.
  mutating func insert(_ inst: Instruction) {
    insertedInsts.insert(inst)
    insertIntoRange(instructions: ReverseInstructionList(first: inst.previous))
    blockRange.insert(inst.parentBlock)
    if inst.parentBlock != blockRange.begin {
      // The first time an instruction is inserted in another block than the begin-block we need to insert
      // instructions from the begin instruction to the end of the begin block.
      // For subsequent insertions this is a no-op: `insertIntoRange` will return immediately because those
      // instruction are already inserted.
      insertIntoRange(instructions: blockRange.begin.instructions.reversed())
    }
  }

  /// Insert a sequence of potential end instructions.
  mutating func insert<S: Sequence>(contentsOf other: S) where S.Element: Instruction {
    for inst in other {
      insert(inst)
    }
  }

  /// Returns true if the exclusive range contains `inst`.
  func contains(_ inst: Instruction) -> Bool {
    if inExclusiveRange.contains(inst) {
      return true
    }
    let block = inst.parentBlock
    return block != blockRange.begin && blockRange.contains(block)
  }

  /// Returns true if the inclusive range contains `inst`.
  func inclusiveRangeContains (_ inst: Instruction) -> Bool {
    contains(inst) || insertedInsts.contains(inst)
  }

  /// Returns the end instructions.
  ///
  /// Warning: this returns `begin` if no instructions were inserted.
  var ends: LazyMapSequence<LazyFilterSequence<Stack<BasicBlock>>, Instruction> {
    blockRange.ends.map {
      $0.instructions.reversed().first(where: { insertedInsts.contains($0)})!
    }
  }

  // Returns the exit blocks.
  var exitBlocks: LazySequence<FlattenSequence<
                    LazyMapSequence<LazyFilterSequence<Stack<BasicBlock>>,
                                    LazyFilterSequence<SuccessorArray>>>> {
    blockRange.exits
  }

  /// Returns the exit instructions.
  var exits: LazyMapSequence<LazySequence<FlattenSequence<
                               LazyMapSequence<LazyFilterSequence<Stack<BasicBlock>>,
                                               LazyFilterSequence<SuccessorArray>>>>,
                             Instruction> {
    blockRange.exits.lazy.map { $0.instructions.first! }
  }

  /// Returns the interior instructions.
  var interiors: LazySequence<FlattenSequence<
                   LazyMapSequence<Stack<BasicBlock>,
                                   LazyFilterSequence<ReverseInstructionList>>>> {
    blockRange.inserted.lazy.flatMap {
      var include = blockRange.contains($0)
      return $0.instructions.reversed().lazy.filter {
        if insertedInsts.contains($0) {
          let isInterior = include
          include = true
          return isInterior
        }
        return false
      }
    }
  }

  var begin: Instruction? {
    blockRange.begin.instructions.first(where: inExclusiveRange.contains)
  }

  private mutating func insertIntoRange(instructions: ReverseInstructionList) {
    for inst in instructions {
      if !inExclusiveRange.insert(inst) {
        return
      }
    }
  }

  var description: String {
    return (blockRange.isValid ? "" : "<invalid>\n") +
      """
      begin:    \(begin?.description ?? blockRange.begin.name)
      ends:     \(ends.map { $0.description }.joined(separator: "\n          "))
      exits:    \(exits.map { $0.description }.joined(separator: "\n          "))
      interiors:\(interiors.map { $0.description }.joined(separator: "\n          "))
      """
  }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    inExclusiveRange.deinitialize()
    insertedInsts.deinitialize()
    blockRange.deinitialize()
  }
}

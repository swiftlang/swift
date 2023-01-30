//===--- BasicBlockRange.swift - a range of basic blocks ------------------===//
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

/// A range of basic blocks.
///
/// The `BasicBlockRange` defines a range from a dominating "begin" block to one or more "end" blocks.
/// The range is "exclusive", which means that the "end" blocks are not part of the range.
///
/// The `BasicBlockRange` is in the same spirit as a linear range, but as the control flow is a graph
/// and not a linear list, there can be "exit" blocks from within the range.
///
/// One or more "potential" end blocks can be inserted.
/// Though, not all inserted blocks end up as "end" blocks.
///
/// There are several kind of blocks:
/// * begin: it is a single block which dominates all blocks of the range
/// * range: all blocks from which there is a path from the begin block to any of the end blocks
/// * ends: all inserted blocks which are at the end of the range
/// * exits: all successor blocks of range blocks which are not in the range themselves
/// * interiors: all inserted blocks which are not end blocks.
///
/// In the following example, let's assume `B` is the begin block and `I1`, `I2` and `I3`
/// were inserted as potential end blocks:
///
///     B
///    / \
///   I1 I2
///      / \
///     I3  X
///
/// Then `I1` and `I3` are "end" blocks. `I2` is an interior block and `X` is an exit block.
/// The range consists of `B` and  `I2`. Note that the range does not include `I1` and `I3`
/// because it's an _exclusive_ range.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct BasicBlockRange : CustomStringConvertible, NoReflectionChildren {

  /// The dominating begin block.
  let begin: BasicBlock

  /// The inclusive range, i.e. the exclusive range plus the end blocks.
  private(set) var inclusiveRange: Stack<BasicBlock>
  
  /// The exclusive range, i.e. not containing the end blocks.
  var range: LazyFilterSequence<Stack<BasicBlock>> {
    inclusiveRange.lazy.filter { contains($0) }
  }

  /// All inserted blocks.
  private(set) var inserted: Stack<BasicBlock>

  private var wasInserted: BasicBlockSet
  private var inExclusiveRange: BasicBlockSet
  private var worklist: BasicBlockWorklist
  
  init(begin: BasicBlock, _ context: some Context) {
    self.begin = begin
    self.inclusiveRange = Stack(context)
    self.inserted = Stack(context)
    self.wasInserted = BasicBlockSet(context)
    self.inExclusiveRange = BasicBlockSet(context)
    self.worklist = BasicBlockWorklist(context)
    worklist.pushIfNotVisited(begin)
  }

  /// Insert a potential end block.
  mutating func insert(_ block: BasicBlock) {
    if wasInserted.insert(block) {
      inserted.append(block)
    }
    worklist.pushIfNotVisited(block)
    while let b = worklist.pop() {
      inclusiveRange.append(b)
      if b != begin {
        for pred in b.predecessors {
          worklist.pushIfNotVisited(pred)
          inExclusiveRange.insert(pred)
        }
      }
    }
  }

  /// Insert a sequence of potential end blocks.
  mutating func insert<S: Sequence>(contentsOf other: S) where S.Element == BasicBlock {
    for block in other {
      insert(block)
    }
  }

  /// Returns true if the exclusive range contains `block`.
  func contains(_ block: BasicBlock) -> Bool { inExclusiveRange.contains(block) }
  
  /// Returns true if the inclusive range contains `block`.
  func inclusiveRangeContains (_ block: BasicBlock) -> Bool {
    worklist.hasBeenPushed(block)
  }

  /// Returns true if the range is valid and that's iff the begin block dominates all blocks of the range.
  var isValid: Bool {
    let entry = begin.parentFunction.entryBlock
    return begin == entry ||
      // If any block in the range is not dominated by `begin`, the range propagates back to the entry block.
      !inclusiveRangeContains(entry)
  }

  /// Returns the end blocks.
  var ends: LazyFilterSequence<Stack<BasicBlock>> {
    inserted.lazy.filter { !contains($0) }
  }

  /// Returns the exit blocks.
  var exits: LazySequence<FlattenSequence<
                    LazyMapSequence<LazyFilterSequence<Stack<BasicBlock>>,
                                    LazyFilterSequence<SuccessorArray>>>> {
    range.flatMap {
      $0.successors.lazy.filter {
        !inclusiveRangeContains($0) || $0 == begin
      }
    }
  }

  /// Returns the interior blocks.
  var interiors: LazyFilterSequence<Stack<BasicBlock>> {
    inserted.lazy.filter { contains($0) && $0 != begin }
  }
  
  var description: String {
    return (isValid ? "" : "<invalid>\n") +
      """
      begin:     \(begin.name)
      range:     \(range)
      inclrange: \(inclusiveRange)
      ends:      \(ends)
      exits:     \(exits)
      interiors: \(interiors)
      """
  }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    worklist.deinitialize()
    inExclusiveRange.deinitialize()
    wasInserted.deinitialize()
    inserted.deinitialize()
    inclusiveRange.deinitialize()
  }
}

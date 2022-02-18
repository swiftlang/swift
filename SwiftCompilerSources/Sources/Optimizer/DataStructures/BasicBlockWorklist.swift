//===--- BasicBlockWorklist.swift - a worklist of basic block -------------===//
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

/// A utility for processing basic blocks in a worklist.
///
/// A `BasicBlockWorklist` is basically a combination of a block array and a block set.
/// It can be used for typical worklist-processing algorithms.
///
/// This type should be a move-only type, but unfortunately we don't have move-only
/// types yet. Therefore it's needed to call `deinitialize()` explicitly to
/// destruct this data structure, e.g. in a `defer {}` block.
struct BasicBlockWorklist : CustomStringConvertible, CustomReflectable {
  private var worklist: Stack<BasicBlock>
  private var pushedBlocks: BasicBlockSet

  init(_ context: PassContext) {
    self.worklist = Stack(context)
    self.pushedBlocks = BasicBlockSet(context)
  }
  
  /// Pops the last added block from the worklist or returns nil, if the worklist is empty.
  mutating func pop() -> BasicBlock? { return worklist.pop() }

  /// Pushes \p block onto the worklist if \p block has never been pushed before.
  mutating func pushIfNotVisited(_ block: BasicBlock) {
    if !pushedBlocks.contains(block) {
      pushedBlocks.insert(block);
      worklist.append(block)
    }
  }

  /// Pushes all elements of \p contentsOf which have never been pushed before.
  mutating func pushIfNotVisited<S: Sequence>(contentsOf other: S) where S.Element == BasicBlock {
    for block in other {
      pushIfNotVisited(block)
    }
  }

  /// Returns true if \p block was pushed to the worklist, regardless if it's already popped or not.
  func hasBeenPushed(_ block: BasicBlock) -> Bool { pushedBlocks.contains(block) }
  
  var description: String {
    """
    worklist: \(worklist)
    pushed:   \(pushedBlocks)
    """
  }

  var customMirror: Mirror { Mirror(self, children: []) }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    pushedBlocks.deinitialize()
    worklist.deinitialize()
  }
}

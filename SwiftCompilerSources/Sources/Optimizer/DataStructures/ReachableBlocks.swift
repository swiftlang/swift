//===--- ReachableBlocks.swift --------------------------------------------===//
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

import SIL

/// A utility for finding reachable and unreachable blocks.
///
/// Reachable blocks are all blocks which are reachable from the entry block of
/// the function. All other blocks are dead blocks.
struct ReachableBlocks : CustomStringConvertible, NoReflectionChildren {
  private var worklist: BasicBlockWorklist
  private var function: Function

  init(function: Function, _ context: FunctionPassContext) {
    self.function = function
    self.worklist = BasicBlockWorklist(context)

    worklist.pushIfNotVisited(function.entryBlock)

    // Propagate lifeness down the control flow.
    while let block = worklist.pop() {
      worklist.pushIfNotVisited(contentsOf: block.successors)
    }
  }

  func isReachable(block: BasicBlock) -> Bool {
    return worklist.hasBeenPushed(block)
  }

  var description: String {
    let blockNames = function.blocks.filter(isReachable).map(\.name)
    return "[" + blockNames.joined(separator: ",") + "]"
  }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    worklist.deinitialize()
  }
}

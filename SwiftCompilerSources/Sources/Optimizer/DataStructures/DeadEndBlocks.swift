//===--- DeadEndBlocks.swift ----------------------------------------------===//
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

/// A utility for finding dead-end blocks.
///
/// Dead-end blocks are blocks from which there is no path to the function exit
/// (`return`, `throw` or unwind). These are blocks which end with an unreachable
/// instruction and blocks from which all paths end in "unreachable" blocks.
struct DeadEndBlocks : CustomStringConvertible, NoReflectionChildren {
  private var worklist: BasicBlockWorklist
  private var function: Function
  
  init(function: Function, _ context: FunctionPassContext) {
    self.function = function
    self.worklist = BasicBlockWorklist(context)
    
    // Initialize the worklist with all function-exiting blocks.
    for block in function.blocks where block.terminator.isFunctionExiting {
      worklist.pushIfNotVisited(block)
    }
    
    // Propagate lifeness up the control flow.
    while let block = worklist.pop() {
      worklist.pushIfNotVisited(contentsOf: block.predecessors)
    }
  }
  
  /// Returns true if `block` is a dead-end block.
  func isDeadEnd(block: BasicBlock) -> Bool {
    return !worklist.hasBeenPushed(block)
  }

  var description: String {
    let blockNames = function.blocks.filter(isDeadEnd).map(\.name)
    return "[" + blockNames.joined(separator: ",") + "]"
  }

  /// TODO: once we have move-only types, make this a real deinit.
  mutating func deinitialize() {
    worklist.deinitialize()
  }
}

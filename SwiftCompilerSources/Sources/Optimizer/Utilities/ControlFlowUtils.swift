//===--- ControlFlowUtils.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

/// Breaks infinite loops in the control flow by inserting an "artificial" loop exit to a new
/// dead-end block with an `unreachable`.
///
/// Inserts a `cond_br` with a `builtin "infinite_loop_true_condition"`:
/// ```
/// bb0:
///   br bb1
/// bb1:
///   br bb1              // back-end branch
/// ```
/// ->
/// ```
/// bb0:
///   br bb1
/// bb1:
///   %1 = builtin "infinite_loop_true_condition"() // always true, but the compiler doesn't know
///   cond_br %1, bb2, bb3
/// bb2:                  // new back-end block
///   br bb1
/// bb3:                  // new dead-end block
///   unreachable
/// ```
///
func breakInfiniteLoops(in function: Function, _ context: FunctionPassContext) {
  context.setNeedBreakInfiniteLoops(to: false)

  guard function.hasOwnership else {
    // The algorithm relies on not having critical edges in the CFG. Therefore it only works for OSSA which
    // enforces no critical edges.
    return
  }

  var noInfiniteLoops = findBlocksWhichDontEndInInfiniteLoops(in: function, context)
  defer { noInfiniteLoops.deinitialize() }

  var changed: Bool
  repeat {
    changed = false

    for block in function.blocks where block.isEntryToInfiniteLoopRegion(noInfiniteLoops) {
      let newDeadEndBlock = breakInfiniteLoop(startingAt: block, context)
      noInfiniteLoops.transitivelyAddBlockWithPredecessors(startingAt: newDeadEndBlock)
      changed = true
    }
  } while changed
}

/// Returns a BasicBlockWorklist containing all blocks in `function` from which there is a path
/// to a function exit or to a block with an `unreachable`.
/// All other blocks are inside an infinite loop or on a path to an infinite loop (e.g. an infinite
/// loop's pre-header block).
private func findBlocksWhichDontEndInInfiniteLoops(in function: Function,
                                                   _ context: FunctionPassContext
) -> BasicBlockWorklist {
  var noInfiniteLoops = BasicBlockWorklist(context)

  for block in function.blocks {
    if block.successors.isEmpty {
      noInfiniteLoops.transitivelyAddBlockWithPredecessors(startingAt: block)
    }
  }

  return noInfiniteLoops
}

private func breakInfiniteLoop(startingAt startBlock: BasicBlock, _ context: FunctionPassContext) -> BasicBlock {
  let backEdgeBranch = getInfiniteLoopBackEdgeBranch(reachableFrom: startBlock, context)

  let newBackEdgeBlock = context.createBlock(after: backEdgeBranch.parentBlock)
  Builder(atEndOf: newBackEdgeBlock, location: backEdgeBranch.location, context)
    .createBranch(to: backEdgeBranch.targetBlock, arguments: Array(backEdgeBranch.operands.values))

  let deadEndBlock = context.createBlock(after: newBackEdgeBlock)
  Builder(atEndOf: deadEndBlock, location: backEdgeBranch.location, context)
    .createUnreachable()

  let builder = Builder(before: backEdgeBranch, context)
  let trueValue = builder.createBuiltin(name: "infinite_loop_true_condition",
                                        type: context.getBuiltinIntegerType(bitWidth: 1),
                                        arguments: [])
  builder.createCondBranch(condition: trueValue, trueBlock: newBackEdgeBlock, falseBlock: deadEndBlock)
  context.erase(instruction: backEdgeBranch)
  return deadEndBlock
}

private func getInfiniteLoopBackEdgeBranch(reachableFrom startBlock: BasicBlock,
                                           _ context: FunctionPassContext
) -> BranchInst {
  var visited = BasicBlockSet(context)
  defer { visited.deinitialize() }

  var block = startBlock
  while true {
    guard let succ = block.successors.first else {
      fatalError("all blocks in an inifinite loop region must have at least one successor")
    }
    guard visited.insert(succ) else {
      break
    }
    block = succ
  }

  guard let backEdgeBranch = block.terminator as? BranchInst else {
    fatalError("back-edge of a loop must be a branch instruction")
  }

  return backEdgeBranch
}

private extension BasicBlock {
  func isEntryToInfiniteLoopRegion(_ noInfiniteLoops: BasicBlockWorklist) -> Bool {
    return !noInfiniteLoops.hasBeenPushed(self) &&
           (predecessors.contains{ noInfiniteLoops.hasBeenPushed($0) } ||
            predecessors.isEmpty)
  }
}

func registerControlFlowUtils() {
  BridgedOptimizerUtilities.registerControlFlowUtils(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function;
      breakInfiniteLoops(in: function, context)
    }
  )
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let breakInfiniteLoopsTest = FunctionTest("break_infinite_loops") {
  function, arguments, context in

  breakInfiniteLoops(in: function, context)
}

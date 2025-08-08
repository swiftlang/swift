//===--- LoopTree.swift ---------------------------------------------------===//
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

import SIL
import OptimizerBridging

/// Describes top level loops.
struct LoopTree {
  fileprivate let bridged: BridgedLoopTree
  
  let loops: TopLevelLoopArray
  
  init(bridged: BridgedLoopTree, context: FunctionPassContext) {
    self.bridged = bridged
    self.loops = TopLevelLoopArray(bridged)
  }
}

/// Describes a loop with its children.
struct Loop {
  private let bridged: BridgedLoop
  
  let innerLoops: LoopArray
  let loopBlocks: LoopBlocks
  
  var exitingAndLatchBlocks: [BasicBlock] {
    return header.predecessors
      .filter { predecessor in
        contains(block: predecessor) && !isLoopExiting(loopBlock: predecessor)
      } + exitingBlocks
  }
  
  var exitBlocks: [BasicBlock] {
    return loopBlocks
      .flatMap(\.successors)
      .filter { !contains(block: $0) }
  }
  
  var exitingBlocks: [BasicBlock] {
    return loopBlocks
      .filter { isLoopExiting(loopBlock: $0) }
  }
  
  init(bridged: BridgedLoop) {
    self.bridged = bridged
    self.innerLoops = LoopArray(bridged)
    self.loopBlocks = LoopBlocks(bridged)
  }
  
  var preheader: BasicBlock? {
    bridged.getPreheader().block
  }
  
  var header: BasicBlock {
    bridged.getHeader().block
  }
  
  var isSingleExit: Bool {
    return exitBlocks.singleElement != nil
  }
  
  var hasNoExitBlocks: Bool {
    return exitBlocks.isEmpty
  }
  
  private func isLoopExiting(loopBlock: BasicBlock) -> Bool {
    return loopBlock.successors.contains { !contains(block: $0) }
  }
  
  func getBlocksThatDominateAllExitingAndLatchBlocks(domTree: DominatorTree) -> some Sequence<BasicBlock> {
    return getBlocksThatDominateAllExitingAndLatchBlocksHelper(bb: header, domTree: domTree)
  }
  
  func contains(block: BasicBlock) -> Bool {
    return bridged.contains(block.bridged)
  }

  private func getBlocksThatDominateAllExitingAndLatchBlocksHelper(
    bb: BasicBlock,
    domTree: DominatorTree
  ) -> some Sequence<BasicBlock> {
    guard exitingAndLatchBlocks.allSatisfy({ exitBlock in
      return bb.dominates(exitBlock, domTree)
    }) else {
      return []
    }
    
    return [bb] + domTree.getChildren(of: bb).lazy
      .flatMap { child in
        getBlocksThatDominateAllExitingAndLatchBlocksHelper(
          bb: child,
          domTree: domTree
        )
      }
  }
}

struct TopLevelLoopArray: BridgedRandomAccessCollection {
  private let bridgedLoopTree: BridgedLoopTree
  
  let count: Int
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  
  init(_ bridgedLoopTree: BridgedLoopTree) {
    self.bridgedLoopTree = bridgedLoopTree
    self.count = bridgedLoopTree.getTopLevelLoopCount()
  }
  
  subscript(_ index: Int) -> Loop {
    assert(index >= startIndex && index < endIndex)
    return Loop(bridged: bridgedLoopTree.getLoop(index))
  }
}

struct LoopArray: BridgedRandomAccessCollection {
  private let bridgedLoop: BridgedLoop
  
  let count: Int
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  
  init(_ bridgedLoop: BridgedLoop) {
    self.bridgedLoop = bridgedLoop
    self.count = bridgedLoop.getInnerLoopCount()
  }
  
  subscript(_ index: Int) -> Loop {
    assert(index >= startIndex && index < endIndex)
    return Loop(bridged: bridgedLoop.getInnerLoop(index))
  }
}

struct LoopBlocks: BridgedRandomAccessCollection {
  private let bridgedLoop: BridgedLoop
  
  let count: Int
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  
  init(_ bridgedLoop: BridgedLoop) {
    self.bridgedLoop = bridgedLoop
    self.count = bridgedLoop.getBasicBlockCount()
  }
  
  subscript(_ index: Int) -> BasicBlock {
    assert(index >= startIndex && index < endIndex)
    return bridgedLoop.getBasicBlock(index).block
  }
}

func splitEdge(
  from block: BasicBlock,
  toEdgeIndex: Int,
  dominatorTree: DominatorTree,
  loopTree: LoopTree,
  _ context: some MutatingContext
) -> BasicBlock? {
  guard let result = loopTree.bridged.splitEdge(block.bridged, toEdgeIndex, dominatorTree.bridged).block else {
    return nil
  }
  
  context.notifyBranchesChanged()
  return result
}

/// If the specified edge is critical, the function returns inserted block. Otherwise returns `nil`.
@discardableResult
func splitCriticalEdge(
  from block: BasicBlock,
  toEdgeIndex: Int,
  dominatorTree: DominatorTree,
  loopTree: LoopTree,
  _ context: some MutatingContext
) -> BasicBlock? {
  guard block.isCriticalEdge(edgeIndex: toEdgeIndex) else {
    return nil
  }
  
  return splitEdge(from: block, toEdgeIndex: toEdgeIndex, dominatorTree: dominatorTree, loopTree: loopTree, context)
}

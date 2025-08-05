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
  private let bridged: BridgedLoopTree
  
  let loops: TopLevelLoopArray
  
  init(bridged: BridgedLoopTree, context: FunctionPassContext) {
    self.bridged = bridged
    self.loops = TopLevelLoopArray(bridged)
  }
  
  func splitCriticalEdge(basicBlock: BasicBlock, edgeIndex: Int, domTree: DominatorTree) -> BasicBlock? {
    guard basicBlock.isCriticalEdge(edgeIndex: edgeIndex) else {
      return nil
    }
    
    return splitEdge(basicBlock: basicBlock, edgeIndex: edgeIndex, domTree: domTree)
  }
  
  func splitEdge(basicBlock: BasicBlock, edgeIndex: Int, domTree: DominatorTree) -> BasicBlock? {
    return bridged.splitEdge(basicBlock.bridged, edgeIndex, domTree.bridged).block
  }
}

/// Describes a loop with its children.
struct Loop {
  private let bridged: BridgedLoop
  
  let innerLoops: LoopArray
  let loopBlocks: LoopBlocks
  
  var preheader: BasicBlock? {
    bridged.getPreheader().block
  }
  
  var header: BasicBlock {
    bridged.getHeader().block
  }
  
  var exitingAndLatchBlocks: some Sequence<BasicBlock> {
    return header.predecessors.lazy
      .filter { predecessor in
        loopBlocks.contains(predecessor) && !isLoopExiting(loopBlock: predecessor)
      } + exitingBlocks
  }
  
  var exitBlocks: some Sequence<BasicBlock> {
    return loopBlocks.lazy
      .flatMap(\.successors)
      .filter { succesor in
        !loopBlocks.contains(succesor)
      }
  }
  
  var exitingBlocks: some Sequence<BasicBlock> {
    return loopBlocks.lazy
      .filter { bb in
        isLoopExiting(loopBlock: bb)
      }
  }
  
  var isSingleExit: Bool {
    return exitBlocks.singleElement != nil
  }
  
  var hasNoExitBlocks: Bool {
    return exitBlocks.isEmpty
  }
  
  private func isLoopExiting(loopBlock: BasicBlock) -> Bool {
    return loopBlock.successors.contains { !loopBlocks.contains($0) }
  }
  
  func getBlocksThatDominateAllExitingAndLatchBlocks(domTree: DominatorTree) -> some Sequence<BasicBlock> {
    return getBlocksThatDominateAllExitingAndLatchBlocksHelper(bb: header, domTree: domTree)
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
  
  init(bridged: BridgedLoop) {
    self.bridged = bridged
    self.innerLoops = LoopArray(bridged)
    self.loopBlocks = LoopBlocks(bridged)
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

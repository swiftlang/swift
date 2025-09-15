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
  
  var exitingAndLatchBlocks: some Sequence<BasicBlock> {
    return header.predecessors.lazy
      .filter { predecessor in
        contains(block: predecessor) && !isLoopExiting(loopBlock: predecessor)
      } + exitingBlocks
  }
  
  /// Exit blocks of the loop.
  ///
  /// - Note: Some exit blocks will be duplicated if the loop has critical edges.
  var exitBlocks: some Sequence<BasicBlock> {
    return loopBlocks.lazy
      .flatMap(\.successors)
      .filter { !contains(block: $0) }
  }
  
  var exitingBlocks: some Sequence<BasicBlock> {
    return loopBlocks.lazy
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
  
  /// Returns `true` if the loop has exactly one exit block.
  var hasSingleExitBlock: Bool {
    return exitBlocks.singleElement != nil
  }
  
  var hasNoExitBlocks: Bool {
    return exitBlocks.isEmpty
  }
  
  private func isLoopExiting(loopBlock: BasicBlock) -> Bool {
    return loopBlock.successors.contains { !contains(block: $0) }
  }
  
  func getBlocksThatDominateAllExitingAndLatchBlocks(_ context: FunctionPassContext) -> [BasicBlock] {
    var result: [BasicBlock] = []
    var cachedExitingAndLatchBlocks = Stack<BasicBlock>(context)
    var workList = BasicBlockWorklist(context)
    defer {
      cachedExitingAndLatchBlocks.deinitialize()
      workList.deinitialize()
    }
    
    cachedExitingAndLatchBlocks.append(contentsOf: exitingAndLatchBlocks)
    workList.pushIfNotVisited(header)
    
    while let block = workList.pop() {
      guard cachedExitingAndLatchBlocks.allSatisfy({ exitBlock in
        return block.dominates(exitBlock, context.dominatorTree)
      }) else {
        continue
      }
      
      result.append(block)
      
      workList.pushIfNotVisited(contentsOf: context.dominatorTree.getChildren(of: block))
    }
    
    return result
  }
  
  func contains(block: BasicBlock) -> Bool {
    return bridged.contains(block.bridged)
  }
  
  func splitCriticalExitingAndBackEdges(_ context: FunctionPassContext) {
    for exitingOrLatchBlock in exitingAndLatchBlocks {
      for (index, succesor) in exitingOrLatchBlock.successors.enumerated() where !contains(block: succesor) {
        splitCriticalEdge(
          from: exitingOrLatchBlock.terminator.parentBlock,
          toEdgeIndex: index,
          dominatorTree: context.dominatorTree,
          loopTree: context.loopTree,
          context
        )
      }
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
) -> BasicBlock {
  let result = loopTree.bridged.splitEdge(block.bridged, toEdgeIndex, dominatorTree.bridged).block
  
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

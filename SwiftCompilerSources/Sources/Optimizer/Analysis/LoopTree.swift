//===--- LoopTree.swift ---------------------------------------------------===//
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
import OptimizerBridging

/// Describes top level loops.
struct LoopTree {
  private let context: Context
  private let bridged: BridgedLoopTree
  
  let loops: TopLevelLoopArray
  
  init(bridged: BridgedLoopTree, context: Context) {
    self.context = context
    self.bridged = bridged
    self.loops = TopLevelLoopArray(bridged, context: context)
  }
  
  func splitCriticalEdge(
    basicBlock: BasicBlock,
    edgeIndex: Int,
    domTree: DominatorTree
  ) -> BasicBlock? {
    guard basicBlock.isCriticalEdge(edgeIndex: edgeIndex) else {
      return nil
    }
    
    return splitEdge(
      basicBlock: basicBlock,
      edgeIndex: edgeIndex,
      domTree: domTree
    )
  }
  
  func splitEdge(
    basicBlock: BasicBlock,
    edgeIndex: Int,
    domTree: DominatorTree
  ) -> BasicBlock? {
    return bridged.splitEdge(basicBlock.bridged, edgeIndex, domTree.bridged).block
  }
}

/// Describes a loop with it's children.
struct Loop {
  private let context: Context
  private let bridged: BridgedLoop
  
  let innerLoops: LoopArray
  let basicBlocks: BasicBlockArray
//  var basicBlockSet: BasicBlockSet
  
  var preheader: BasicBlock? {
    bridged.getPreheader().block
  }
  
  var header: BasicBlock {
    bridged.getHeader().block
  }
  
  var exitingAndLatchBlocks: some Sequence<BasicBlock> {
    return header.predecessors.lazy
      .filter { predecessor in
        basicBlocks.contains(predecessor) && !isLoopExiting(bb: predecessor)
//        basicBlockSet.contains(predecessor) && !isLoopExiting(bb: predecessor)
      } + exitingBlocks
  }
  
  var exitBlocks: some Sequence<BasicBlock> {
    return basicBlocks.lazy
      .flatMap(\.successors)
      .filter { succesor in
        !basicBlocks.contains(succesor)
//        !basicBlockSet.contains(succesor)
      }
  }
  
  var exitingBlocks: some Sequence<BasicBlock> {
    return basicBlocks.lazy
      .filter { bb in
        isLoopExiting(bb: bb)
      }
  }
  
  var isSingleExit: Bool {
    return exitBlocks.singleElement != nil
  }
  
  var hasNoExitBlocks: Bool {
    return exitBlocks.isEmpty
  }
  
  private func isLoopExiting(bb: BasicBlock) -> Bool {
    return bb.successors
      .contains { succesor in
        !basicBlocks.contains(succesor)
//        !basicBlockSet.contains(succesor)
      }
  }
  
  init(bridged: BridgedLoop, context: Context) {
    self.context = context
    self.bridged = bridged
    self.innerLoops = LoopArray(bridged, context: context)
    self.basicBlocks = BasicBlockArray(bridged)
//    self.basicBlockSet = BasicBlockSet(insertContentsOf: basicBlocks, context)
  }
}

struct TopLevelLoopArray: BridgedRandomAccessCollection {
  private let context: Context
  private let bridgedLoopTree: BridgedLoopTree
  
  public let count: Int
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  
  public init(_ bridgedLoopTree: BridgedLoopTree, context: Context) {
    self.context = context
    self.bridgedLoopTree = bridgedLoopTree
    self.count = bridgedLoopTree.getTopLevelLoopCount()
  }
  
  public subscript(_ index: Int) -> Loop {
    assert(index >= startIndex && index < endIndex)
    return Loop(bridged: bridgedLoopTree.getLoop(index), context: context)
  }
}

struct LoopArray: BridgedRandomAccessCollection {
  private let context: Context
  private let bridgedLoop: BridgedLoop
  
  public let count: Int
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  
  public init(_ bridgedLoop: BridgedLoop, context: Context) {
    self.context = context
    self.bridgedLoop = bridgedLoop
    self.count = bridgedLoop.getInnerLoopCount()
  }
  
  public subscript(_ index: Int) -> Loop {
    assert(index >= startIndex && index < endIndex)
    return Loop(bridged: bridgedLoop.getInnerLoop(index), context: context)
  }
}

struct BasicBlockArray: BridgedRandomAccessCollection {
  private let bridgedLoop: BridgedLoop
  
  public let count: Int
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  
  public init(_ bridgedLoop: BridgedLoop) {
    self.bridgedLoop = bridgedLoop
    self.count = bridgedLoop.getBasicBlockCount()
  }
  
  public subscript(_ index: Int) -> BasicBlock {
    assert(index >= startIndex && index < endIndex)
    return bridgedLoop.getBasicBlock(index).block
  }
}

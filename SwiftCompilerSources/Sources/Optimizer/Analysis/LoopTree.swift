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
}

/// Describes a loop with it's children.
struct Loop {
  private let context: Context
  private let bridged: BridgedLoop
  
  let innerLoops: LoopArray
  let basicBlocks: BasicBlockArray
  let basicBlockSet: BasicBlockSet
  
  var preheader: BasicBlock? {
    bridged.getPreheader().block
  }
  
  init(bridged: BridgedLoop, context: Context) {
    self.context = context
    self.bridged = bridged
    self.innerLoops = LoopArray(bridged, context: context)
    self.basicBlocks = BasicBlockArray(bridged)
    self.basicBlockSet = BasicBlockSet(insertContentsOf: basicBlocks, context)
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

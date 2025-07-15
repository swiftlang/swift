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
  let bridged: BridgedLoopTree
  
  let loops: TopLevelLoopArray
  
  init(bridged: BridgedLoopTree) {
    self.bridged = bridged
    self.loops = TopLevelLoopArray(bridged)
  }
}

/// Describes a loop with it's children.
struct Loop {
  let bridged: BridgedLoop
  
  let innerLoops: LoopArray
  let basicBlocks: BasicBlockArray
  
  init(bridged: BridgedLoop) {
    self.bridged = bridged
    self.innerLoops = LoopArray(bridged)
    self.basicBlocks = BasicBlockArray(bridged)
  }
}

struct TopLevelLoopArray: BridgedRandomAccessCollection {
  private let bridgedLoopTree: BridgedLoopTree
  
  public let count: Int
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  
  public init(_ bridgedLoopTree: BridgedLoopTree) {
    self.bridgedLoopTree = bridgedLoopTree
    self.count = bridgedLoopTree.getTopLevelLoopCount()
  }
  
  public subscript(_ index: Int) -> Loop {
    assert(index >= startIndex && index < endIndex)
    return Loop(bridged: bridgedLoopTree.getLoop(index))
  }
}

struct LoopArray: BridgedRandomAccessCollection {
  private let bridgedLoop: BridgedLoop
  
  public let count: Int
  
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  
  public init(_ bridgedLoop: BridgedLoop) {
    self.bridgedLoop = bridgedLoop
    self.count = bridgedLoop.getInnerLoopCount()
  }
  
  public subscript(_ index: Int) -> Loop {
    assert(index >= startIndex && index < endIndex)
    return Loop(bridged: bridgedLoop.getInnerLoop(index))
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

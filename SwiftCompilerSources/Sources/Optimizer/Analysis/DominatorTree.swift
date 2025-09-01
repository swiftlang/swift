//===--- DominatorTree.swift - the dominator tree -------------------------===//
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

struct DominatorTree {
  let bridged: BridgedDomTree
  
  func getChildren(of block: BasicBlock) -> DomChildren {
    return DomChildren(bridgedDomTree: bridged, bb: block)
  }
}

struct DomChildren: BridgedRandomAccessCollection {
  private let bridgedDomTree: BridgedDomTree
  private let block: BasicBlock
  
  let count: Int
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  
  init(bridgedDomTree: BridgedDomTree, bb: BasicBlock) {
    self.bridgedDomTree = bridgedDomTree
    self.block = bb
    self.count = bridgedDomTree.getNumberOfChildren(bb.bridged)
  }
  
  subscript(_ index: Int) -> BasicBlock {
    assert(index >= startIndex && index < endIndex)
    return bridgedDomTree.getChildAt(block.bridged, index).block
  }
}

extension BasicBlock {
  func dominates(_ other: BasicBlock, _ domTree: DominatorTree) -> Bool {
    domTree.bridged.dominates(self.bridged, other.bridged)
  }
  
  func strictlyDominates(_ other: BasicBlock, _ domTree: DominatorTree) -> Bool {
    dominates(other, domTree) && self != other
  }
}

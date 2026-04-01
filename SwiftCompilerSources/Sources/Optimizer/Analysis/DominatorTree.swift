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

  /// Returns the immediate dominator of `block`, i.e. the parent of `block` in the tree.
  func getImmediateDominator(of block: BasicBlock) -> BasicBlock? {
    bridged.getImmediateDominator(block.bridged).block
  }

  /// Returns the sub-dominator-tree of `startBlock` in dominance order.
  ///
  /// Blocks - including their sub-tree - are only included if `filter` returns true.
  func getDominanceOrder(startingAt startBlock: BasicBlock, filter: (BasicBlock) -> Bool) -> [BasicBlock] {
    guard filter(startBlock) else {
      return []
    }

    var order = [BasicBlock]()
    order.append(startBlock)

    var idx = 0
    while idx < order.count {
      let block = order[idx]
      idx += 1
      order.append(contentsOf: getChildren(of: block).lazy.filter(filter))
    }
    return order
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

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let domtreeTest = FunctionTest("domtree") {
  function, arguments, context in

  let domtree = context.dominatorTree
  let startBlock = arguments.takeBlock()

  let order = domtree.getDominanceOrder(startingAt: startBlock, filter: { block in
    if let sli = block.instructions.first! as? StringLiteralInst,
       sli.value == "skip"
    {
      return false
    }
    return true
  })

  print("order: \(order.map { $0.index })")
}

//===--- PostDominatorTree.swift - the post dominator tree ----------------===//
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

struct PostDominatorTree {
  let bridged: BridgedPostDomTree
}

extension BasicBlock {
  func postDominates(_ other: BasicBlock, _ pdomTree: PostDominatorTree) -> Bool {
    pdomTree.bridged.postDominates(self.bridged, other.bridged)
  }
  
  func strictlyPostDominates(_ other: BasicBlock, _ pdomTree: PostDominatorTree) -> Bool {
    postDominates(other, pdomTree) && self != other
  }
}

extension Instruction {
  /// Returns true if `otherInst` is in the same block and is post-dominated by this instruction or
  /// the parent block of the instruction post-dominates parent block of `otherInst`.
  func postDominates(_ otherInst: Instruction, _ postDomTree: PostDominatorTree) -> Bool {
    if parentBlock == otherInst.parentBlock {
      return otherInst.dominatesInBlock(self)
    } else {
      return parentBlock.postDominates(otherInst.parentBlock, postDomTree)
    }
  }

  /// Like `Instruction.postDominates`, but also returns false if `otherInst` == `self`.
  func strictlyPostDominates(_ otherInst: Instruction, _ postDomTree: PostDominatorTree) -> Bool {
    if parentBlock == otherInst.parentBlock {
      return otherInst.strictlyDominatesInBlock(self)
    } else {
      return parentBlock.postDominates(otherInst.parentBlock, postDomTree)
    }
  }
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let postDominanceTest = FunctionTest("post_dominance") {
  function, arguments, context in

  let domtree = context.postDominatorTree

  let literals = Array(function.instructions.compactMap { $0 as? IntegerLiteralInst })

  for first in literals {
    for second in literals {
      let dominates = first.postDominates(second, domtree)
      let strictly = first.strictlyPostDominates(second, domtree)
      print("(\(first.value!), \(second.value!)): dominates: \(dominates), strictly: \(strictly)")
    }
  }
}

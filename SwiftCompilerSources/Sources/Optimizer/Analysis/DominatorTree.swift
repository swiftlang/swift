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

public struct DominatorTree {
  let bridged: BridgedDomTree
}

extension BasicBlock {
  func dominates(_ other: BasicBlock, _ domTree: DominatorTree) -> Bool {
    DominatorTree_dominates(domTree.bridged, self.bridged, other.bridged) != 0
  }
  
  func strictlyDominates(_ other: BasicBlock, _ domTree: DominatorTree) -> Bool {
    dominates(other, domTree) && self != other
  }
}

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

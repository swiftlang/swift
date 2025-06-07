//===--- DeadEndBlocksAnalysis.swift - the dead-end blocks analysis -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL

struct DeadEndBlocksAnalysis {
  let bridged: BridgedDeadEndBlocksAnalysis

  func isDeadEnd(_ block: BasicBlock) -> Bool {
    return bridged.isDeadEnd(block.bridged)
  }
}

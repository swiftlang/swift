//===--- ArraySemanticsCall.swift -----------------------------------------===//
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

struct ArraySemanticsCall {
  public static func getArraySemanticsCallKind(inst: Instruction) -> BridgedArrayCallKind {
    return BridgedArraySemanticsCall.getArraySemanticsCallKind(inst.bridged)
  }
  
  public static func canHoist(inst: Instruction, to toInst: Instruction, domTree: DominatorTree) -> Bool {
    return BridgedArraySemanticsCall.canHoist(inst.bridged, toInst.bridged, domTree.bridged)
  }
}

//===--- AliasAnalysis.swift - the alias analysis -------------------------===//
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

struct AliasAnalysis {
  let bridged: BridgedAliasAnalysis
  
  func mayRead(_ inst: Instruction, fromAddress: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, fromAddress.bridged) {
      case MayReadBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  func mayWrite(_ inst: Instruction, toAddress: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, toAddress.bridged) {
      case MayWriteBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  func mayReadOrWrite(_ inst: Instruction, address: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, address.bridged) {
      case MayReadBehavior, MayWriteBehavior, MayReadWriteBehavior,
           MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }
}

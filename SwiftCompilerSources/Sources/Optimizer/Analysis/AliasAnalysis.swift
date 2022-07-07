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

  /// Returns the correct path for address-alias functions.
  static func getPtrOrAddressPath(for value: Value) -> SmallProjectionPath {
    let ty = value.type
    if ty.isAddress {
      // This is the regular case: the path selects any sub-fields of an address.
      return SmallProjectionPath(.anyValueFields)
    }
    // Some optimizations use the address-alias APIs with non-address SIL values.
    // TODO: this is non-intuitive and we should eliminate those API uses.
    if ty.isClass {
    // If the value is a (non-address) reference it means: all addresses within the class instance.
      return SmallProjectionPath(.anyValueFields).push(.anyClassField)
    }
    // Any other non-address value means: all addresses of any referenced class instances within the value.
    return SmallProjectionPath(.anyValueFields).push(.anyClassField).push(.anyValueFields)
  }
}

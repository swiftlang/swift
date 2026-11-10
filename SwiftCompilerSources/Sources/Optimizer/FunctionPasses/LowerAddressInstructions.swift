//===--- LowerAddressInstructions.swift -----------------------------------===//
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


/// Lowers `copy_addr` and `destroy_addr` of loadable types.
///
/// ```
///   copy_addr %1 to %2                   ->     %3 = load [copy] %1
///                                               store %3 to [assign] %2
///
///   copy_addr [take] %1 to [init] %2     ->     %3 = load [take] %1
///                                               store %3 to [init] %2
///
///   destroy_addr %1                      ->     %3 = load [take] %1
///                                               destroy_value %3
/// ```
let lowerAddressInstructions = FunctionPass(name: "lower-address-instructions") { function, context in
  guard function.hasOwnership else {
    return
  }

  for inst in function.instructions {
    tryLower(instruction: inst, context)
  }
}

private func tryLower(instruction: Instruction, _ context: FunctionPassContext) {
  switch instruction {
  case let copyAddr as CopyAddrInst:
    guard copyAddr.source.type.isLoadable(in: copyAddr.parentFunction) else {
      break
    }
    let builder = Builder(before: copyAddr, context)
    let value = builder.createLoad(fromAddress: copyAddr.source, ownership: copyAddr.loadOwnership)
    builder.createStore(source: value, destination: copyAddr.destination, ownership: copyAddr.storeOwnership)
    context.erase(instruction: copyAddr)

  case let destroyAddr as DestroyAddrInst:
    guard destroyAddr.destroyedAddress.type.isLoadable(in: destroyAddr.parentFunction) else {
      break
    }
    if !destroyAddr.destroyedAddress.type.isTrivial(in: destroyAddr.parentFunction) {
      let builder = Builder(before: destroyAddr, context)
      let value = builder.createLoad(fromAddress: destroyAddr.destroyedAddress, ownership: .take)
      builder.createDestroyValue(operand: value)
    }
    context.erase(instruction: destroyAddr)

  default:
    break
  }
}

//===--- SimplifyEndCOWMutationAddr.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

// Simplify end_cow_mutation_addr to end_cow_mutation when it's operand is loadable
extension EndCOWMutationAddrInst: OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    let address = operand.value
    if !address.type.isLoadable(in: parentFunction) {
      return
    }
    if address.type.isTrivial(in: parentFunction) {
      context.erase(instruction: self)
      return
    }
    let builder = Builder(before: self, context)
    let load = builder.createLoad(fromAddress: address, ownership: parentFunction.hasOwnership ? .take : .unqualified)
    let endCOWMutation = builder.createEndCOWMutation(instance: load, keepUnique: false) 
    builder.createStore(source: endCOWMutation, destination: address, ownership: parentFunction.hasOwnership ? .initialize : .unqualified)
    context.erase(instruction: self)
  }
}

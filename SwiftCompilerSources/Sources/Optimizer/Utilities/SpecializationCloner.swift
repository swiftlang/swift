//===--- SpecializationCloner.swift --------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL

/// Utility cloner type that can be used by optimizations that generate new functions or specialized versions of
/// existing functions. 
struct SpecializationCloner {
  private let bridged: BridgedSpecializationCloner
  let context: FunctionPassContext

  init(emptySpecializedFunction: Function, _ context: FunctionPassContext) {
    self.bridged = BridgedSpecializationCloner(emptySpecializedFunction.bridged)
    self.context = context
  }
  
  var cloned: Function {
    bridged.getCloned().function
  }

  var entryBlock: BasicBlock {
    if cloned.blocks.isEmpty {
      return cloned.appendNewBlock(context)
    } else {
      return cloned.entryBlock
    }
  }

  func getClonedBlock(for originalBlock: BasicBlock) -> BasicBlock {
    bridged.getClonedBasicBlock(originalBlock.bridged).block
  }

  func cloneFunctionBody(from originalFunction: Function, entryBlockArguments: [Value]) {
    entryBlockArguments.withBridgedValues { bridgedEntryBlockArgs in
      bridged.cloneFunctionBody(originalFunction.bridged, self.entryBlock.bridged, bridgedEntryBlockArgs)
    }
  }

}
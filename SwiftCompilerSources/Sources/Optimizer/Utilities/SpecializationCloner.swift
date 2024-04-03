//===--- SpecializationCloner.swift --------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
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
  private var _context: FunctionPassContext
  private var _bridged: BridgedSpecializationCloner

  init(emptySpecializedFunction: Function, _ context: FunctionPassContext) {
    self._context = context
    self._bridged = BridgedSpecializationCloner(emptySpecializedFunction.bridged)
  }

  public var context: FunctionPassContext {
    self._context
  }

  public var bridged: BridgedSpecializationCloner {
    self._bridged
  }
  
  public var cloned: Function {
    bridged.getCloned().function
  }

  public var entryBlock: BasicBlock {
    if cloned.blocks.isEmpty {
      cloned.appendNewBlock(context)
    } else {
      cloned.entryBlock
    }
  }

  public func getClonedBlock(for originalBlock: BasicBlock) -> BasicBlock {
    bridged.getClonedBasicBlock(originalBlock.bridged).block
  }

  public func cloneFunctionBody(from originalFunction: Function, entryBlockArgs: [Value]) {
    entryBlockArgs.withBridgedValues { bridgedEntryBlockArgs in
      bridged.cloneFunctionBody(originalFunction.bridged, self.entryBlock.bridged, bridgedEntryBlockArgs)
    }
  }

}
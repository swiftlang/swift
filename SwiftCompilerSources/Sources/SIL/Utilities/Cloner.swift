//===--- Cloner.swift ------------------------------------------------------==//
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

import SILBridging
import AST

/// Clones the initializer value of a GlobalVariable.
///
/// Used to transitively clone "constant" instructions, including their operands,
/// from or to the static initializer value of a GlobalVariable.
///
public struct Cloner<Context: MutatingContext> {
  private var bridged: BridgedCloner
  public let context: Context

  public enum GetClonedResult {
    case defaultValue
    case customValue(Value)
    case stopCloning
  }
  
  public enum Target {
    case function(Function)
    case global(GlobalVariable)
  }
  public let target: Target

  public init(cloneToGlobal: GlobalVariable, _ context: Context) {
    self.bridged = BridgedCloner(cloneToGlobal.bridged, context._bridged)
    self.context = context
    self.target = .global(cloneToGlobal)
  }

  public init(cloneBefore inst: Instruction, _ context: Context) {
    self.bridged = BridgedCloner(inst.bridged, context._bridged)
    self.context = context
    self.target = .function(inst.parentFunction)
  }

  public init(cloneToEmptyFunction: Function, _ context: Context) {
    self.bridged = BridgedCloner(cloneToEmptyFunction.bridged, context._bridged)
    self.context = context
    self.target = .function(cloneToEmptyFunction)
  }

  public mutating func deinitialize() {
    bridged.destroy(context._bridged)
  }

  public var targetFunction: Function {
    guard case .function(let function) = target else {
      fatalError("expected cloning into a function")
    }
    return function
  }

  public func getOrCreateEntryBlock() -> BasicBlock {
    if let entryBlock = targetFunction.blocks.first {
      return entryBlock
    }
    return targetFunction.appendNewBlock(context)
  }

  public func cloneFunctionBody(from originalFunction: Function, entryBlockArguments: [Value]) {
    entryBlockArguments.withBridgedValues { bridgedEntryBlockArgs in
      let entryBlock = getOrCreateEntryBlock()
      bridged.cloneFunctionBody(originalFunction.bridged, entryBlock.bridged, bridgedEntryBlockArgs)
    }
  }

  public func cloneFunctionBody(from originalFunction: Function) {
    bridged.cloneFunctionBody(originalFunction.bridged)
  }

  public mutating func clone(instruction: Instruction) -> Instruction {
    let cloned = bridged.clone(instruction.bridged).instruction
    if case .function = target {
      context.notifyInstructionChanged(cloned)
      context.notifyInstructionsChanged()
    }
    return cloned
  }
  
  public mutating func cloneRecursively(globalInitValue: Value) -> Value {
    guard let cloned = cloneRecursively(value: globalInitValue, customGetCloned: { value, cloner in
      guard let beginAccess = value as? BeginAccessInst else {
        return .defaultValue
      }
      
      // Skip access instructions, which might be generated for UnsafePointer globals which point to other globals.
      let clonedOperand = cloner.cloneRecursively(globalInitValue: beginAccess.address)
      cloner.recordFoldedValue(beginAccess, mappedTo: clonedOperand)
      return .customValue(clonedOperand)
    }) else {
      fatalError("Clone recursively to global shouldn't bail.")
    }
    
    return cloned
  }

  /// Transitively clones `value` including its defining instruction's operands.
  public mutating func cloneRecursively( value: Value) -> Value {
    return cloneRecursively(value: value, customGetCloned: { _, _ in .defaultValue })!
  }

  /// Transitively clones `value` including its defining instruction's operands.
  public mutating func cloneRecursively(value: Value, customGetCloned: (Value, inout Cloner) -> GetClonedResult) -> Value? {
    if isCloned(value: value) {
      return getClonedValue(of: value)
    }
    
    switch customGetCloned(value, &self) {
    case .customValue(let base):
      return base
    case .stopCloning:
      return nil
    case .defaultValue:
      break
    }

    guard let inst = value.definingInstruction else {
      fatalError("expected instruction to clone or already cloned value")
    }

    for op in inst.operands {
      if cloneRecursively(value: op.value, customGetCloned: customGetCloned) == nil {
        return nil
      }
    }

    let cloned = clone(instruction: inst)
    if let svi = cloned as? SingleValueInstruction {
      return svi
    } else if let originalMvi = value as? MultipleValueInstructionResult {
      return cloned.results[originalMvi.index]
    }
    fatalError("unexpected instruction kind")
  }

  public mutating func getClonedValue(of originalValue: Value) -> Value {
    bridged.getClonedValue(originalValue.bridged).value
  }

  public func isCloned(value: Value) -> Bool {
    bridged.isValueCloned(value.bridged)
  }

  public func getClonedBlock(for originalBlock: BasicBlock) -> BasicBlock {
    bridged.getClonedBasicBlock(originalBlock.bridged).block
  }

  public func recordFoldedValue(_ origValue: Value, mappedTo mappedValue: Value) {
    bridged.recordFoldedValue(origValue.bridged, mappedValue.bridged)
  }
}

public struct TypeSubstitutionCloner<Context: MutatingContext> {
  public private(set) var bridged: BridgedTypeSubstCloner
  public let context: Context

  public init(fromFunction: Function,
              toEmptyFunction: Function,
              substitutions: SubstitutionMap, _ context: Context
  ) {
    context.verifyIsTransforming(function: toEmptyFunction)
    self.bridged = BridgedTypeSubstCloner(fromFunction.bridged, toEmptyFunction.bridged,
                                          substitutions.bridged, context._bridged)
    self.context = context
  }

  public mutating func deinitialize() {
    bridged.destroy(context._bridged)
  }

  public mutating func getClonedValue(of originalValue: Value) -> Value {
    bridged.getClonedValue(originalValue.bridged).value
  }

  public func getClonedBlock(for originalBlock: BasicBlock) -> BasicBlock {
    bridged.getClonedBasicBlock(originalBlock.bridged).block
  }

  public func cloneFunctionBody() {
    bridged.cloneFunctionBody()
  }
}

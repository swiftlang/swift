//===--- Builder.swift -  Building and modifying SIL ----------------------===//
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

import SILBridging

/// A utility to create new instructions at a given insertion point.
public struct Builder {
  let insertionPoint: Instruction
  let location: Location
  private let passContext: BridgedPassContext

  private var bridgedInsPoint: swift.SILInstruction { insertionPoint }

  private func notifyInstructionsChanged() {
    PassContext_notifyChanges(passContext, instructionsChanged)
  }

  private func notifyCallsChanged() {
    PassContext_notifyChanges(passContext, callsChanged)
  }

  private func notifyBranchesChanged() {
    PassContext_notifyChanges(passContext, branchesChanged)
  }

  public init(insertionPoint: Instruction, location: Location,
              passContext: BridgedPassContext) {
    self.insertionPoint = insertionPoint
    self.location = location;
    self.passContext = passContext
  }

  public func createBuiltinBinaryFunction(name: String,
                                          operandType: Type, resultType: Type, arguments: [Value]) -> swift.BuiltinInst {
    notifyInstructionsChanged()
    return arguments.withBridgedValues { valuesRef in
      return name.withBridgedStringRef { nameStr in
        let bi = SILBuilder_createBuiltinBinaryFunction(
          bridgedInsPoint, location.bridgedLocation, nameStr,
          operandType, resultType, valuesRef)
        return getAsBuiltinInst(bi)!
      }
    }
  }

  public func createCondFail(condition: Value, message: String) -> swift.CondFailInst {
    notifyInstructionsChanged()
    return message.withBridgedStringRef { messageStr in
      let cf = SILBuilder_createCondFail(
        bridgedInsPoint, location.bridgedLocation, condition, messageStr)
      return getAsCondFailInst(cf)!
    }
  }
}

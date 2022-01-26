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

  private var bridgedInsPoint: BridgedInstruction { insertionPoint.bridged }

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
      operandType: Type, resultType: Type, arguments: [Value]) -> BuiltinInst {
    notifyInstructionsChanged()
    return arguments.withBridgedValues { valuesRef in
      return name.withBridgedStringRef { nameStr in
        let bi = SILBuilder_createBuiltinBinaryFunction(
          bridgedInsPoint, location.bridgedLocation, nameStr,
          operandType.bridged, resultType.bridged, valuesRef)
        return bi.getAs(BuiltinInst.self)
      }
    }
  }

  public func createCondFail(condition: Value, message: String) -> CondFailInst {
    notifyInstructionsChanged()
    return message.withBridgedStringRef { messageStr in
      let cf = SILBuilder_createCondFail(
        bridgedInsPoint, location.bridgedLocation, condition.bridged, messageStr)
      return cf.getAs(CondFailInst.self)
    }
  }

  public func createIntegerLiteral(_ value: Int, type: Type) -> IntegerLiteralInst {
    notifyInstructionsChanged()
    let literal = SILBuilder_createIntegerLiteral(
      bridgedInsPoint, location.bridgedLocation, type.bridged, value)
    return literal.getAs(IntegerLiteralInst.self)
  }
  
  public func createDeallocStackRef(_ operand: Value) -> DeallocStackRefInst {
    notifyInstructionsChanged()
    let dr = SILBuilder_createDeallocStackRef(bridgedInsPoint, location.bridgedLocation, operand.bridged)
    return dr.getAs(DeallocStackRefInst.self)
  }

  public func createUncheckedRefCast(object: Value, type: Type) -> UncheckedRefCastInst {
    notifyInstructionsChanged()
    let object = SILBuilder_createUncheckedRefCast(
      bridgedInsPoint, location.bridgedLocation, object.bridged, type.bridged)
    return object.getAs(UncheckedRefCastInst.self)
  }

  @discardableResult
  public func createSetDeallocating(operand: Value, isAtomic: Bool) -> SetDeallocatingInst {
    notifyInstructionsChanged()
    let setDeallocating = SILBuilder_createSetDeallocating(
      bridgedInsPoint, location.bridgedLocation, operand.bridged, isAtomic)
    return setDeallocating.getAs(SetDeallocatingInst.self)
  }

  public func createFunctionRef(_ function: Function) -> FunctionRefInst {
    notifyInstructionsChanged()
    let functionRef = SILBuilder_createFunctionRef(
      bridgedInsPoint, location.bridgedLocation, function.bridged)
    return functionRef.getAs(FunctionRefInst.self)
  }

  @discardableResult
  public func createApply(
    function: Value,
    _ substitutionMap: SubstitutionMap,
    arguments: [Value]
  ) -> ApplyInst {
    notifyInstructionsChanged()
    notifyCallsChanged()

    let apply = arguments.withBridgedValues { valuesRef in
      SILBuilder_createApply(
        bridgedInsPoint, location.bridgedLocation, function.bridged,
        substitutionMap.bridged, valuesRef
      )
    }
    return apply.getAs(ApplyInst.self)
  }
}

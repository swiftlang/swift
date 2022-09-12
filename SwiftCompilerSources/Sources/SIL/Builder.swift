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

import Basic
import SILBridging

/// A utility to create new instructions at a given insertion point.
public struct Builder {

  public enum InsertionPoint {
    case before(Instruction)
    case atEndOf(BasicBlock)
  }

  let insertAt: InsertionPoint
  let location: Location
  private let passContext: BridgedPassContext

  private var bridged: BridgedBuilder {
    switch insertAt {
    case .before(let inst):
      return BridgedBuilder(insertBefore: inst.bridged.optional,
                            insertAtEnd: OptionalBridgedBasicBlock.none,
                            loc: location.bridged)
    case .atEndOf(let block):
      return BridgedBuilder(insertBefore: OptionalBridgedInstruction.none,
                            insertAtEnd: block.bridged.optional,
                            loc: location.bridged)
    }
  }

  private func notifyInstructionsChanged() {
    PassContext_notifyChanges(passContext, instructionsChanged)
  }

  private func notifyCallsChanged() {
    PassContext_notifyChanges(passContext, callsChanged)
  }

  private func notifyBranchesChanged() {
    PassContext_notifyChanges(passContext, branchesChanged)
  }

  public init(insertAt: InsertionPoint, location: Location,
              passContext: BridgedPassContext) {
    self.insertAt = insertAt
    self.location = location;
    self.passContext = passContext
  }

  public func createBuiltinBinaryFunction(name: String,
      operandType: Type, resultType: Type, arguments: [Value]) -> BuiltinInst {
    notifyInstructionsChanged()
    return arguments.withBridgedValues { valuesRef in
      return name.withStringRef { nameStr in
        let bi = SILBuilder_createBuiltinBinaryFunction(
          bridged, nameStr, operandType.bridged, resultType.bridged, valuesRef)
        return bi.getAs(BuiltinInst.self)
      }
    }
  }

  public func createCondFail(condition: Value, message: String) -> CondFailInst {
    notifyInstructionsChanged()
    return message.withStringRef { messageStr in
      let cf = SILBuilder_createCondFail(bridged, condition.bridged, messageStr)
      return cf.getAs(CondFailInst.self)
    }
  }

  public func createIntegerLiteral(_ value: Int, type: Type) -> IntegerLiteralInst {
    notifyInstructionsChanged()
    let literal = SILBuilder_createIntegerLiteral(bridged, type.bridged, value)
    return literal.getAs(IntegerLiteralInst.self)
  }

  public func createAllocStack(_ type: Type, hasDynamicLifetime: Bool = false,
                               isLexical: Bool = false, wasMoved: Bool = false) -> AllocStackInst {
    notifyInstructionsChanged()
    let dr = SILBuilder_createAllocStack(bridged, type.bridged, hasDynamicLifetime ? 1 : 0,
                                         isLexical ? 1 : 0, wasMoved ? 1 : 0)
    return dr.getAs(AllocStackInst.self)
  }

  @discardableResult
  public func createDeallocStack(_ operand: Value) -> DeallocStackInst {
    notifyInstructionsChanged()
    let dr = SILBuilder_createDeallocStack(bridged, operand.bridged)
    return dr.getAs(DeallocStackInst.self)
  }

  @discardableResult
  public func createDeallocStackRef(_ operand: Value) -> DeallocStackRefInst {
    notifyInstructionsChanged()
    let dr = SILBuilder_createDeallocStackRef(bridged, operand.bridged)
    return dr.getAs(DeallocStackRefInst.self)
  }

  public func createUncheckedRefCast(object: Value, type: Type) -> UncheckedRefCastInst {
    notifyInstructionsChanged()
    let object = SILBuilder_createUncheckedRefCast(bridged, object.bridged, type.bridged)
    return object.getAs(UncheckedRefCastInst.self)
  }

  @discardableResult
  public func createSetDeallocating(operand: Value, isAtomic: Bool) -> SetDeallocatingInst {
    notifyInstructionsChanged()
    let setDeallocating = SILBuilder_createSetDeallocating(bridged, operand.bridged, isAtomic)
    return setDeallocating.getAs(SetDeallocatingInst.self)
  }

  public func createFunctionRef(_ function: Function) -> FunctionRefInst {
    notifyInstructionsChanged()
    let functionRef = SILBuilder_createFunctionRef(bridged, function.bridged)
    return functionRef.getAs(FunctionRefInst.self)
  }

  public func createCopyValue(operand: Value) -> CopyValueInst {
    notifyInstructionsChanged()
    return SILBuilder_createCopyValue(bridged, operand.bridged).getAs(CopyValueInst.self)
  }

  @discardableResult
  public func createCopyAddr(from fromAddr: Value, to toAddr: Value,
                             takeSource: Bool = false, initializeDest: Bool = false) -> CopyAddrInst {
    notifyInstructionsChanged()
    return SILBuilder_createCopyAddr(bridged, fromAddr.bridged, toAddr.bridged,
                                     takeSource ? 1 : 0, initializeDest ? 1 : 0).getAs(CopyAddrInst.self)
  }

  @discardableResult
  public func createDestroyValue(operand: Value) -> DestroyValueInst {
    notifyInstructionsChanged()
    return SILBuilder_createDestroyValue(bridged, operand.bridged).getAs(DestroyValueInst.self)
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
      SILBuilder_createApply(bridged, function.bridged, substitutionMap.bridged, valuesRef)
    }
    return apply.getAs(ApplyInst.self)
  }
  
  public func createUncheckedEnumData(enum enumVal: Value,
                                      caseIndex: Int,
                                      resultType: Type) -> UncheckedEnumDataInst {
    notifyInstructionsChanged()
    let ued = SILBuilder_createUncheckedEnumData(bridged, enumVal.bridged, caseIndex, resultType.bridged)
    return ued.getAs(UncheckedEnumDataInst.self)
  }
  @discardableResult
  public func createSwitchEnum(enum enumVal: Value,
                               cases: [(Int, BasicBlock)],
                               defaultBlock: BasicBlock? = nil) -> SwitchEnumInst {
    notifyInstructionsChanged()
    notifyBranchesChanged()
    let se = cases.withUnsafeBufferPointer { caseBuffer in
      SILBuilder_createSwitchEnumInst(
        bridged, enumVal.bridged, defaultBlock.bridged, caseBuffer.baseAddress, caseBuffer.count)
    }
    return se.getAs(SwitchEnumInst.self)
  }
  
  @discardableResult
  public func createBranch(to destBlock: BasicBlock, arguments: [Value] = []) -> BranchInst {
    notifyInstructionsChanged()
    notifyBranchesChanged()
    return arguments.withBridgedValues { valuesRef in
      let bi = SILBuilder_createBranch(bridged, destBlock.bridged, valuesRef)
      return bi.getAs(BranchInst.self)
    }
  }
}

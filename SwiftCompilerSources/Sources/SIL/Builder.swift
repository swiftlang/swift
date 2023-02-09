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
  private let notifyNewInstruction: (Instruction) -> ()

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

  private func notifyNew<I: Instruction>(_ instruction: I) -> I {
    PassContext_notifyChanges(passContext, instructionsChanged)
    if instruction is FullApplySite {
      PassContext_notifyChanges(passContext, callsChanged)
    }
    if instruction is TermInst {
      PassContext_notifyChanges(passContext, branchesChanged)
    }
    notifyNewInstruction(instruction)
    return instruction
  }

  public init(insertAt: InsertionPoint, location: Location,
              _ notifyNewInstruction: @escaping (Instruction) -> (),
              _ passContext: BridgedPassContext) {
    self.insertAt = insertAt
    self.location = location;
    self.notifyNewInstruction = notifyNewInstruction
    self.passContext = passContext
  }

  public func createBuiltinBinaryFunction(name: String,
      operandType: Type, resultType: Type, arguments: [Value]) -> BuiltinInst {
    return arguments.withBridgedValues { valuesRef in
      return name._withStringRef { nameStr in
        let bi = SILBuilder_createBuiltinBinaryFunction(
          bridged, nameStr, operandType.bridged, resultType.bridged, valuesRef)
        return notifyNew(bi.getAs(BuiltinInst.self))
      }
    }
  }

  public func createCondFail(condition: Value, message: String) -> CondFailInst {
    return message._withStringRef { messageStr in
      let cf = SILBuilder_createCondFail(bridged, condition.bridged, messageStr)
      return notifyNew(cf.getAs(CondFailInst.self))
    }
  }

  public func createIntegerLiteral(_ value: Int, type: Type) -> IntegerLiteralInst {
    let literal = SILBuilder_createIntegerLiteral(bridged, type.bridged, value)
    return notifyNew(literal.getAs(IntegerLiteralInst.self))
  }

  public func createAllocStack(_ type: Type, hasDynamicLifetime: Bool = false,
                               isLexical: Bool = false, wasMoved: Bool = false) -> AllocStackInst {
    let dr = SILBuilder_createAllocStack(bridged, type.bridged, hasDynamicLifetime ? 1 : 0,
                                         isLexical ? 1 : 0, wasMoved ? 1 : 0)
    return notifyNew(dr.getAs(AllocStackInst.self))
  }

  @discardableResult
  public func createDeallocStack(_ operand: Value) -> DeallocStackInst {
    let dr = SILBuilder_createDeallocStack(bridged, operand.bridged)
    return notifyNew(dr.getAs(DeallocStackInst.self))
  }

  @discardableResult
  public func createDeallocStackRef(_ operand: Value) -> DeallocStackRefInst {
    let dr = SILBuilder_createDeallocStackRef(bridged, operand.bridged)
    return notifyNew(dr.getAs(DeallocStackRefInst.self))
  }

  public func createUncheckedRefCast(object: Value, type: Type) -> UncheckedRefCastInst {
    let object = SILBuilder_createUncheckedRefCast(bridged, object.bridged, type.bridged)
    return notifyNew(object.getAs(UncheckedRefCastInst.self))
  }

  @discardableResult
  public func createSetDeallocating(operand: Value, isAtomic: Bool) -> SetDeallocatingInst {
    let setDeallocating = SILBuilder_createSetDeallocating(bridged, operand.bridged, isAtomic)
    return notifyNew(setDeallocating.getAs(SetDeallocatingInst.self))
  }

  public func createFunctionRef(_ function: Function) -> FunctionRefInst {
    let functionRef = SILBuilder_createFunctionRef(bridged, function.bridged)
    return notifyNew(functionRef.getAs(FunctionRefInst.self))
  }

  public func createCopyValue(operand: Value) -> CopyValueInst {
    return notifyNew(SILBuilder_createCopyValue(bridged, operand.bridged).getAs(CopyValueInst.self))
  }

  @discardableResult
  public func createCopyAddr(from fromAddr: Value, to toAddr: Value,
                             takeSource: Bool = false, initializeDest: Bool = false) -> CopyAddrInst {
    return notifyNew(SILBuilder_createCopyAddr(bridged, fromAddr.bridged, toAddr.bridged,
                                     takeSource ? 1 : 0, initializeDest ? 1 : 0).getAs(CopyAddrInst.self))
  }

  @discardableResult
  public func createDestroyValue(operand: Value) -> DestroyValueInst {
    return notifyNew(SILBuilder_createDestroyValue(bridged, operand.bridged).getAs(DestroyValueInst.self))
  }

  @discardableResult
  public func createDebugStep() -> DebugStepInst {
    return notifyNew(SILBuilder_createDebugStep(bridged).getAs(DebugStepInst.self))
  }

  @discardableResult
  public func createApply(
    function: Value,
    _ substitutionMap: SubstitutionMap,
    arguments: [Value],
    isNonThrowing: Bool = false,
    isNonAsync: Bool = false,
    specializationInfo: ApplyInst.SpecializationInfo = nil
  ) -> ApplyInst {
    let apply = arguments.withBridgedValues { valuesRef in
      SILBuilder_createApply(bridged, function.bridged, substitutionMap.bridged, valuesRef,
                             isNonThrowing, isNonAsync, specializationInfo)
    }
    return notifyNew(apply.getAs(ApplyInst.self))
  }
  
  public func createUncheckedEnumData(enum enumVal: Value,
                                      caseIndex: Int,
                                      resultType: Type) -> UncheckedEnumDataInst {
    let ued = SILBuilder_createUncheckedEnumData(bridged, enumVal.bridged, caseIndex, resultType.bridged)
    return notifyNew(ued.getAs(UncheckedEnumDataInst.self))
  }
  @discardableResult
  public func createSwitchEnum(enum enumVal: Value,
                               cases: [(Int, BasicBlock)],
                               defaultBlock: BasicBlock? = nil) -> SwitchEnumInst {
    let se = cases.withUnsafeBufferPointer { caseBuffer in
      SILBuilder_createSwitchEnumInst(
        bridged, enumVal.bridged, defaultBlock.bridged, caseBuffer.baseAddress, caseBuffer.count)
    }
    return notifyNew(se.getAs(SwitchEnumInst.self))
  }
  
  @discardableResult
  public func createBranch(to destBlock: BasicBlock, arguments: [Value] = []) -> BranchInst {
    return arguments.withBridgedValues { valuesRef in
      let bi = SILBuilder_createBranch(bridged, destBlock.bridged, valuesRef)
      return notifyNew(bi.getAs(BranchInst.self))
    }
  }

  @discardableResult
  public func createUnreachable() -> UnreachableInst {
    let ui = SILBuilder_createUnreachable(bridged)
    return notifyNew(ui.getAs(UnreachableInst.self))
  }
}

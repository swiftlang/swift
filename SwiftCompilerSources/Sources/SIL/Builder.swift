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
import AST
import SILBridging

/// A utility to create new instructions at a given insertion point.
public struct Builder {

  public enum InsertionPoint {
    case before(Instruction)
    case atEndOf(BasicBlock)
    case atStartOf(Function)
    case staticInitializer(GlobalVariable)
  }

  public let insertionPoint: InsertionPoint
  let location: Location
  private let notificationHandler: BridgedChangeNotificationHandler
  private let notifyNewInstruction: (Instruction) -> ()

  /// Return 'nil' when inserting at the start of a function or in a global initializer.
  public var insertionBlock: BasicBlock? {
    switch insertionPoint {
    case let .before(inst):
      return inst.parentBlock
    case let .atEndOf(block):
      return block
    case .atStartOf, .staticInitializer:
      return nil
    }
  }

  public var bridged: BridgedBuilder {
    switch insertionPoint {
    case .before(let inst):
      return BridgedBuilder(insertAt: .beforeInst, insertionObj: inst.bridged.obj,
                            loc: location.bridged)
    case .atEndOf(let block):
      return BridgedBuilder(insertAt: .endOfBlock, insertionObj: block.bridged.obj,
                            loc: location.bridged)
    case .atStartOf(let function):
      return BridgedBuilder(insertAt: .startOfFunction, insertionObj: function.bridged.obj,
                            loc: location.bridged)
    case .staticInitializer(let global):
      return BridgedBuilder(insertAt: .intoGlobal, insertionObj: global.bridged.obj,
                            loc: location.bridged)
    }
  }

  private func notifyNew<I: Instruction>(_ instruction: I) -> I {
    notificationHandler.notifyChanges(.instructionsChanged)
    if instruction is FullApplySite {
      notificationHandler.notifyChanges(.callsChanged)
    }
    if instruction is TermInst {
      notificationHandler.notifyChanges(.branchesChanged)
    }
    notifyNewInstruction(instruction)
    return instruction
  }

  public init(insertAt: InsertionPoint, location: Location,
              _ notifyNewInstruction: @escaping (Instruction) -> (),
              _ notificationHandler: BridgedChangeNotificationHandler) {
    self.insertionPoint = insertAt
    self.location = location;
    self.notifyNewInstruction = notifyNewInstruction
    self.notificationHandler = notificationHandler
  }

  public func createBuiltinBinaryFunction(name: String,
      operandType: Type, resultType: Type, arguments: [Value]) -> BuiltinInst {
    return arguments.withBridgedValues { valuesRef in
      return name._withBridgedStringRef { nameStr in
        let bi = bridged.createBuiltinBinaryFunction(
          nameStr, operandType.bridged, resultType.bridged, valuesRef)
        return notifyNew(bi.getAs(BuiltinInst.self))
      }
    }
  }

  @discardableResult
  public func createCondFail(condition: Value, message: String) -> CondFailInst {
    return message._withBridgedStringRef { messageStr in
      let cf = bridged.createCondFail(condition.bridged, messageStr)
      return notifyNew(cf.getAs(CondFailInst.self))
    }
  }

  public func createIntegerLiteral(_ value: Int, type: Type) -> IntegerLiteralInst {
    let literal = bridged.createIntegerLiteral(type.bridged, value)
    return notifyNew(literal.getAs(IntegerLiteralInst.self))
  }
    
  public func createAllocRef(_ type: Type, isObjC: Bool = false, canAllocOnStack: Bool = false, isBare: Bool = false,
                             tailAllocatedTypes: TypeArray, tailAllocatedCounts: [Value]) -> AllocRefInst {
    return tailAllocatedCounts.withBridgedValues { countsRef in
      let dr = bridged.createAllocRef(type.bridged, isObjC, canAllocOnStack, isBare, tailAllocatedTypes.bridged, countsRef)
      return notifyNew(dr.getAs(AllocRefInst.self))
    }
  }

  public func createAllocStack(_ type: Type, hasDynamicLifetime: Bool = false,
                               isLexical: Bool = false, isFromVarDecl: Bool = false,
                               usesMoveableValueDebugInfo: Bool = false) -> AllocStackInst {
    let dr = bridged.createAllocStack(type.bridged, hasDynamicLifetime, isLexical,
                                      isFromVarDecl, usesMoveableValueDebugInfo)
    return notifyNew(dr.getAs(AllocStackInst.self))
  }

  public func createAllocVector(capacity: Value, elementType: Type) -> AllocVectorInst {
    let dr = bridged.createAllocVector(capacity.bridged, elementType.bridged)
    return notifyNew(dr.getAs(AllocVectorInst.self))
  }

  @discardableResult
  public func createDeallocStack(_ operand: Value) -> DeallocStackInst {
    let dr = bridged.createDeallocStack(operand.bridged)
    return notifyNew(dr.getAs(DeallocStackInst.self))
  }

  @discardableResult
  public func createDeallocStackRef(_ operand: Value) -> DeallocStackRefInst {
    let dr = bridged.createDeallocStackRef(operand.bridged)
    return notifyNew(dr.getAs(DeallocStackRefInst.self))
  }

  public func createAddressToPointer(address: Value, pointerType: Type,
                                     needStackProtection: Bool) -> AddressToPointerInst {
    let dr = bridged.createAddressToPointer(address.bridged, pointerType.bridged, needStackProtection)
    return notifyNew(dr.getAs(AddressToPointerInst.self))
  }

  public func createUncheckedRefCast(from value: Value, to type: Type) -> UncheckedRefCastInst {
    let cast = bridged.createUncheckedRefCast(value.bridged, type.bridged)
    return notifyNew(cast.getAs(UncheckedRefCastInst.self))
  }

  public func createUpcast(from value: Value, to type: Type) -> UpcastInst {
    let cast = bridged.createUpcast(value.bridged, type.bridged)
    return notifyNew(cast.getAs(UpcastInst.self))
  }

  public func createLoad(fromAddress: Value, ownership: LoadInst.LoadOwnership) -> LoadInst {
    let load = bridged.createLoad(fromAddress.bridged, ownership.rawValue)
    return notifyNew(load.getAs(LoadInst.self))
  }

  public func createLoadBorrow(fromAddress: Value) -> LoadBorrowInst {
    let load = bridged.createLoadBorrow(fromAddress.bridged)
    return notifyNew(load.getAs(LoadBorrowInst.self))
  }

  public func createBeginDeallocRef(reference: Value, allocation: AllocRefInstBase) -> BeginDeallocRefInst {
    let beginDealloc = bridged.createBeginDeallocRef(reference.bridged, allocation.bridged)
    return notifyNew(beginDealloc.getAs(BeginDeallocRefInst.self))
  }

  public func createEndInitLetRef(operand: Value) -> EndInitLetRefInst {
    let endInit = bridged.createEndInitLetRef(operand.bridged)
    return notifyNew(endInit.getAs(EndInitLetRefInst.self))
  }

  @discardableResult
  public func createRetainValue(operand: Value) -> RetainValueInst {
    let retain = bridged.createRetainValue(operand.bridged)
    return notifyNew(retain.getAs(RetainValueInst.self))
  }

  @discardableResult
  public func createReleaseValue(operand: Value) -> ReleaseValueInst {
    let release = bridged.createReleaseValue(operand.bridged)
    return notifyNew(release.getAs(ReleaseValueInst.self))
  }

  @discardableResult
  public func createStrongRetain(operand: Value) -> StrongRetainInst {
    let retain = bridged.createStrongRetain(operand.bridged)
    return notifyNew(retain.getAs(StrongRetainInst.self))
  }

  @discardableResult
  public func createStrongRelease(operand: Value) -> StrongReleaseInst {
    let release = bridged.createStrongRelease(operand.bridged)
    return notifyNew(release.getAs(StrongReleaseInst.self))
  }

  @discardableResult
  public func createUnownedRetain(operand: Value) -> UnownedRetainInst {
    let retain = bridged.createUnownedRetain(operand.bridged)
    return notifyNew(retain.getAs(UnownedRetainInst.self))
  }

  @discardableResult
  public func createUnownedRelease(operand: Value) -> UnownedReleaseInst {
    let release = bridged.createUnownedRelease(operand.bridged)
    return notifyNew(release.getAs(UnownedReleaseInst.self))
  }

  public func createFunctionRef(_ function: Function) -> FunctionRefInst {
    let functionRef = bridged.createFunctionRef(function.bridged)
    return notifyNew(functionRef.getAs(FunctionRefInst.self))
  }

  public func createCopyValue(operand: Value) -> CopyValueInst {
    return notifyNew(bridged.createCopyValue(operand.bridged).getAs(CopyValueInst.self))
  }

  public func createBeginBorrow(
    of value: Value,
    isLexical: Bool = false,
    hasPointerEscape: Bool = false,
    isFromVarDecl: Bool = false
  ) -> BeginBorrowInst {
    return notifyNew(bridged.createBeginBorrow(value.bridged,
             isLexical, hasPointerEscape, isFromVarDecl).getAs(BeginBorrowInst.self))
  }

  public func createBorrowedFrom(borrowedValue: Value, enclosingValues: [Value]) -> BorrowedFromInst {
    let bfi = enclosingValues.withBridgedValues { valuesRef in
      return bridged.createBorrowedFrom(borrowedValue.bridged, valuesRef)
    }
    return notifyNew(bfi.getAs(BorrowedFromInst.self))
  }

  @discardableResult
  public func createEndBorrow(of beginBorrow: Value) -> EndBorrowInst {
    return notifyNew(bridged.createEndBorrow(beginBorrow.bridged).getAs(EndBorrowInst.self))
  }

  @discardableResult
  public func createCopyAddr(from fromAddr: Value, to toAddr: Value,
                             takeSource: Bool = false, initializeDest: Bool = false) -> CopyAddrInst {
    return notifyNew(bridged.createCopyAddr(fromAddr.bridged, toAddr.bridged,
                                            takeSource, initializeDest).getAs(CopyAddrInst.self))
  }

  @discardableResult
  public func createDestroyValue(operand: Value) -> DestroyValueInst {
    return notifyNew(bridged.createDestroyValue(operand.bridged).getAs(DestroyValueInst.self))
  }

  @discardableResult
  public func createDestroyAddr(address: Value) -> DestroyAddrInst {
    return notifyNew(bridged.createDestroyAddr(address.bridged).getAs(DestroyAddrInst.self))
  }

  @discardableResult
  public func createEndLifetime(of value: Value) -> EndLifetimeInst {
    return notifyNew(bridged.createEndLifetime(value.bridged).getAs(EndLifetimeInst.self))
  }

  @discardableResult
  public func createDebugStep() -> DebugStepInst {
    return notifyNew(bridged.createDebugStep().getAs(DebugStepInst.self))
  }

  @discardableResult
  public func createApply(
    function: Value,
    _ substitutionMap: SubstitutionMap,
    arguments: [Value],
    isNonThrowing: Bool = false,
    isNonAsync: Bool = false,
    specializationInfo: ApplyInst.SpecializationInfo = ApplyInst.SpecializationInfo()
  ) -> ApplyInst {
    let apply = arguments.withBridgedValues { valuesRef in
      bridged.createApply(function.bridged, substitutionMap.bridged, valuesRef,
                          isNonThrowing, isNonAsync, specializationInfo)
    }
    return notifyNew(apply.getAs(ApplyInst.self))
  }
  
  @discardableResult
  public func createTryApply(
    function: Value,
    _ substitutionMap: SubstitutionMap,
    arguments: [Value],
    normalBlock: BasicBlock,
    errorBlock: BasicBlock,
    isNonAsync: Bool = false,
    specializationInfo: ApplyInst.SpecializationInfo = ApplyInst.SpecializationInfo()
  ) -> TryApplyInst {
    let apply = arguments.withBridgedValues { valuesRef in
      bridged.createTryApply(function.bridged, substitutionMap.bridged, valuesRef,
                             normalBlock.bridged, errorBlock.bridged,
                             isNonAsync, specializationInfo)
    }
    return notifyNew(apply.getAs(TryApplyInst.self))
  }
  
  @discardableResult
  public func createReturn(of value: Value) -> ReturnInst {
    return notifyNew(bridged.createReturn(value.bridged).getAs(ReturnInst.self))
  }

  @discardableResult
  public func createThrow(of value: Value) -> ThrowInst {
    return notifyNew(bridged.createThrow(value.bridged).getAs(ThrowInst.self))
  }

  public func createUncheckedEnumData(enum enumVal: Value,
                                      caseIndex: Int,
                                      resultType: Type) -> UncheckedEnumDataInst {
    let ued = bridged.createUncheckedEnumData(enumVal.bridged, caseIndex, resultType.bridged)
    return notifyNew(ued.getAs(UncheckedEnumDataInst.self))
  }

  public func createUncheckedTakeEnumDataAddr(enumAddress: Value,
                                              caseIndex: Int) -> UncheckedTakeEnumDataAddrInst {
    let uteda = bridged.createUncheckedTakeEnumDataAddr(enumAddress.bridged, caseIndex)
    return notifyNew(uteda.getAs(UncheckedTakeEnumDataAddrInst.self))
  }

  public func createEnum(caseIndex: Int, payload: Value?, enumType: Type) -> EnumInst {
    let enumInst = bridged.createEnum(caseIndex, payload.bridged, enumType.bridged)
    return notifyNew(enumInst.getAs(EnumInst.self))
  }

  public func createThinToThickFunction(thinFunction: Value, resultType: Type) -> ThinToThickFunctionInst {
    let tttf = bridged.createThinToThickFunction(thinFunction.bridged, resultType.bridged)
    return notifyNew(tttf.getAs(ThinToThickFunctionInst.self))
  }

  public func createPartialApply(
    function: Value,
    substitutionMap: SubstitutionMap, 
    capturedArguments: [Value], 
    calleeConvention: ArgumentConvention, 
    hasUnknownResultIsolation: Bool, 
    isOnStack: Bool
  ) -> PartialApplyInst {
    return capturedArguments.withBridgedValues { capturedArgsRef in
      let pai = bridged.createPartialApply(function.bridged, capturedArgsRef, calleeConvention.bridged, substitutionMap.bridged, hasUnknownResultIsolation, isOnStack)
      return notifyNew(pai.getAs(PartialApplyInst.self))
    }
  }

  @discardableResult
  public func createSwitchEnum(enum enumVal: Value,
                               cases: [(Int, BasicBlock)],
                               defaultBlock: BasicBlock? = nil) -> SwitchEnumInst {
    let se = cases.withUnsafeBufferPointer { caseBuffer in
      bridged.createSwitchEnumInst(enumVal.bridged, defaultBlock.bridged,
                                   caseBuffer.baseAddress, caseBuffer.count)
    }
    return notifyNew(se.getAs(SwitchEnumInst.self))
  }
  
  @discardableResult
  public func createSwitchEnumAddr(enumAddress: Value,
                                   cases: [(Int, BasicBlock)],
                                   defaultBlock: BasicBlock? = nil) -> SwitchEnumAddrInst {
    let se = cases.withUnsafeBufferPointer { caseBuffer in
      bridged.createSwitchEnumAddrInst(enumAddress.bridged, defaultBlock.bridged,
                                       caseBuffer.baseAddress, caseBuffer.count)
    }
    return notifyNew(se.getAs(SwitchEnumAddrInst.self))
  }

  @discardableResult
  public func createBranch(to destBlock: BasicBlock, arguments: [Value] = []) -> BranchInst {
    return arguments.withBridgedValues { valuesRef in
      let bi = bridged.createBranch(destBlock.bridged, valuesRef)
      return notifyNew(bi.getAs(BranchInst.self))
    }
  }

  @discardableResult
  public func createUnreachable() -> UnreachableInst {
    let ui = bridged.createUnreachable()
    return notifyNew(ui.getAs(UnreachableInst.self))
  }

  @discardableResult
  public func createObject(type: Type, arguments: [Value], numBaseElements: Int) -> ObjectInst {
    let objectInst = arguments.withBridgedValues { valuesRef in
      return bridged.createObject(type.bridged, valuesRef, numBaseElements)
    }
    return notifyNew(objectInst.getAs(ObjectInst.self))
  }

  @discardableResult
  public func createVector(type: Type, arguments: [Value]) -> VectorInst {
    let vectorInst = arguments.withBridgedValues { valuesRef in
      return bridged.createVector(valuesRef)
    }
    return notifyNew(vectorInst.getAs(VectorInst.self))
  }

  public func createGlobalAddr(global: GlobalVariable, dependencyToken: Value?) -> GlobalAddrInst {
    return notifyNew(bridged.createGlobalAddr(global.bridged, dependencyToken.bridged).getAs(GlobalAddrInst.self))
  }

  public func createGlobalValue(global: GlobalVariable, isBare: Bool) -> GlobalValueInst {
    return notifyNew(bridged.createGlobalValue(global.bridged, isBare).getAs(GlobalValueInst.self))
  }

  public func createStruct(type: Type, elements: [Value]) -> StructInst {
    let structInst = elements.withBridgedValues { valuesRef in
      return bridged.createStruct(type.bridged, valuesRef)
    }
    return notifyNew(structInst.getAs(StructInst.self))
  }

  public func createStructExtract(struct: Value, fieldIndex: Int) -> StructExtractInst {
    return notifyNew(bridged.createStructExtract(`struct`.bridged, fieldIndex).getAs(StructExtractInst.self))
  }

  public func createStructElementAddr(structAddress: Value, fieldIndex: Int) -> StructElementAddrInst {
    return notifyNew(bridged.createStructElementAddr(structAddress.bridged, fieldIndex).getAs(StructElementAddrInst.self))
  }

  public func createDestructureStruct(struct: Value) -> DestructureStructInst {
    return notifyNew(bridged.createDestructureStruct(`struct`.bridged).getAs(DestructureStructInst.self))
  }

  public func createTuple(type: Type, elements: [Value]) -> TupleInst {
    let tuple = elements.withBridgedValues { valuesRef in
      return bridged.createTuple(type.bridged, valuesRef)
    }
    return notifyNew(tuple.getAs(TupleInst.self))
  }

  public func createTupleExtract(tuple: Value, elementIndex: Int) -> TupleExtractInst {
    return notifyNew(bridged.createTupleExtract(tuple.bridged, elementIndex).getAs(TupleExtractInst.self))
  }

  public func createTupleElementAddr(tupleAddress: Value, elementIndex: Int) -> TupleElementAddrInst {
    return notifyNew(bridged.createTupleElementAddr(tupleAddress.bridged, elementIndex).getAs(TupleElementAddrInst.self))
  }

  public func createDestructureTuple(tuple: Value) -> DestructureTupleInst {
    return notifyNew(bridged.createDestructureTuple(tuple.bridged).getAs(DestructureTupleInst.self))
  }

  @discardableResult
  public func createStore(source: Value, destination: Value, ownership: StoreInst.StoreOwnership) -> StoreInst {
    let store = bridged.createStore(source.bridged, destination.bridged, ownership.rawValue)
    return notifyNew(store.getAs(StoreInst.self))
  }

  public func createInitExistentialRef(instance: Value,
                                       existentialType: Type,
                                       formalConcreteType: CanonicalType,
                                       conformances: ConformanceArray) -> InitExistentialRefInst {
    let initExistential = bridged.createInitExistentialRef(instance.bridged,
                                                           existentialType.bridged,
                                                           formalConcreteType.bridged,
                                                           conformances.bridged)
    return notifyNew(initExistential.getAs(InitExistentialRefInst.self))
  }

  public func createInitExistentialMetatype(
    metatype: Value,
    existentialType: Type,
    conformances: ConformanceArray) -> InitExistentialMetatypeInst {
    let initExistential = bridged.createInitExistentialMetatype(metatype.bridged,
                                                                existentialType.bridged,
                                                                conformances.bridged)
    return notifyNew(initExistential.getAs(InitExistentialMetatypeInst.self))
  }

  public func createMetatype(of type: Type, representation: Type.MetatypeRepresentation) -> MetatypeInst {
    let metatype = bridged.createMetatype(type.bridged, representation)
    return notifyNew(metatype.getAs(MetatypeInst.self))
  }

  public func createEndCOWMutation(instance: Value, keepUnique: Bool) -> EndCOWMutationInst {
    let endMutation = bridged.createEndCOWMutation(instance.bridged, keepUnique)
    return notifyNew(endMutation.getAs(EndCOWMutationInst.self))
  }

  public func createMarkDependence(value: Value, base: Value, kind: MarkDependenceInst.DependenceKind) -> MarkDependenceInst {
    let markDependence = bridged.createMarkDependence(value.bridged, base.bridged,
                                                      BridgedInstruction.MarkDependenceKind(rawValue: kind.rawValue)!)
    return notifyNew(markDependence.getAs(MarkDependenceInst.self))
  }
    
  @discardableResult
  public func createEndAccess(beginAccess: BeginAccessInst) -> EndAccessInst {
      let endAccess = bridged.createEndAccess(beginAccess.bridged)
      return notifyNew(endAccess.getAs(EndAccessInst.self))
  }

  @discardableResult
  public func createEndApply(beginApply: BeginApplyInst) -> EndApplyInst {
    let endApply = bridged.createEndApply(beginApply.token.bridged)
    return notifyNew(endApply.getAs(EndApplyInst.self))
  }

  @discardableResult
  public func createAbortApply(beginApply: BeginApplyInst) -> AbortApplyInst {
    let endApply = bridged.createAbortApply(beginApply.token.bridged)
    return notifyNew(endApply.getAs(AbortApplyInst.self))
  }

  public func createConvertFunction(originalFunction: Value, resultType: Type, withoutActuallyEscaping: Bool) -> ConvertFunctionInst {
    let convertFunction = bridged.createConvertFunction(originalFunction.bridged, resultType.bridged, withoutActuallyEscaping)
    return notifyNew(convertFunction.getAs(ConvertFunctionInst.self))
  }

  public func createConvertEscapeToNoEscape(originalFunction: Value, resultType: Type, isLifetimeGuaranteed: Bool) -> ConvertEscapeToNoEscapeInst {
    let convertFunction = bridged.createConvertEscapeToNoEscape(originalFunction.bridged, resultType.bridged, isLifetimeGuaranteed)
    return notifyNew(convertFunction.getAs(ConvertEscapeToNoEscapeInst.self))
  }
}

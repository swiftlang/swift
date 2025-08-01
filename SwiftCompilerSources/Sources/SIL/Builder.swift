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
  private let notificationHandler: BridgedContext
  private let notifyNewInstruction: (Instruction) -> ()

  /// Creates a builder which inserts _before_ `insPnt`, using a custom `location`.
  public init(before insPnt: Instruction, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    self.init(insertAt: .before(insPnt), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts before `insPnt`, using `insPnt`'s next
  /// non-meta instruction's location.
  /// This function should be used when moving code to an unknown insert point,
  /// when we want to inherit the location of the closest non-meta instruction.
  /// For replacing an existing meta instruction with another, use
  /// ``Builder.init(replacing:_:)``.
  public init(before insPnt: Instruction, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    self.init(insertAt: .before(insPnt), location: insPnt.location,
              context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _before_ `insPnt`, using the exact location of `insPnt`,
  /// for the purpose of replacing that meta instruction with an equivalent instruction.
  /// This function does not delete `insPnt`.
  public init(replacing insPnt: MetaInstruction, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    self.init(insertAt: .before(insPnt), location: insPnt.location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _after_ `insPnt`, using a custom `location`.
  ///
  /// TODO: this is usually incorrect for terminator instructions. Instead use
  /// `Builder.insert(after:location:_:insertFunc)` from OptUtils.swift. Rename this to afterNonTerminator.
  public init(after insPnt: Instruction, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    guard let nextInst = insPnt.next else {
      fatalError("cannot insert an instruction after a block terminator.")
    }
    self.init(insertAt: .before(nextInst), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _after_ `insPnt`, using `insPnt`'s next
  /// non-meta instruction's location.
  ///
  /// TODO: this is incorrect for terminator instructions. Instead use `Builder.insert(after:location:_:insertFunc)`
  /// from OptUtils.swift. Rename this to afterNonTerminator.
  public init(after insPnt: Instruction, _ context: some MutatingContext) {
    self.init(after: insPnt, location: insPnt.location, context)
  }

  /// Creates a builder which inserts at the end of `block`, using a custom `location`.
  public init(atEndOf block: BasicBlock, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: block.parentFunction)
    self.init(insertAt: .atEndOf(block), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using a custom `location`.
  public init(atBeginOf block: BasicBlock, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: block.parentFunction)
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using the location of the first
  /// non-meta instruction of `block`.
  public init(atBeginOf block: BasicBlock, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: block.parentFunction)
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst),
              location: firstInst.location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts instructions into an empty function, using the location of the function itself.
  public init(atStartOf function: Function, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: function)
    self.init(insertAt: .atStartOf(function), location: function.location,
              context.notifyInstructionChanged, context._bridged)
  }

  public init(staticInitializerOf global: GlobalVariable, _ context: some MutatingContext) {
    self.init(insertAt: .staticInitializer(global),
              location: Location.artificialUnreachableLocation, { _ in }, context._bridged)
  }

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
    notificationHandler.notifyChanges(.Instructions)
    if instruction is FullApplySite {
      notificationHandler.notifyChanges(.Calls)
    }
    if instruction is TermInst {
      notificationHandler.notifyChanges(.Branches)
    }
    notifyNewInstruction(instruction)
    return instruction
  }

  private init(insertAt: InsertionPoint, location: Location,
              _ notifyNewInstruction: @escaping (Instruction) -> (),
              _ notificationHandler: BridgedContext) {
    self.insertionPoint = insertAt
    self.location = location;
    self.notifyNewInstruction = notifyNewInstruction
    self.notificationHandler = notificationHandler
  }

  public func createBuiltin(name: StringRef,
                            type: Type,
                            substitutions: SubstitutionMap = SubstitutionMap(),
                            arguments: [Value]) -> BuiltinInst {
    return arguments.withBridgedValues { valuesRef in
      let bi = bridged.createBuiltin(
        name._bridged, type.bridged, substitutions.bridged, valuesRef)
      return notifyNew(bi.getAs(BuiltinInst.self))
    }
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

  public func createAllocStack(_ type: Type,
                               debugVariable: DebugVariableInstruction.DebugVariable? = nil,
                               hasDynamicLifetime: Bool = false,
                               isLexical: Bool = false, isFromVarDecl: Bool = false,
                               usesMoveableValueDebugInfo: Bool = false) -> AllocStackInst {
    let allocStack: BridgedInstruction
    if let debugVariable = debugVariable {
      allocStack = bridged.createAllocStack(type.bridged, debugVariable, hasDynamicLifetime, isLexical,
                                            isFromVarDecl, usesMoveableValueDebugInfo)
    } else {
      allocStack = bridged.createAllocStack(type.bridged, hasDynamicLifetime, isLexical,
                                            isFromVarDecl, usesMoveableValueDebugInfo)
    }
    return notifyNew(allocStack.getAs(AllocStackInst.self))
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

  public func createPointerToAddress(pointer: Value, addressType: Type,
                                     isStrict: Bool, isInvariant: Bool,
                                     alignment: Int? = nil) -> PointerToAddressInst {
    let dr = bridged.createPointerToAddress(pointer.bridged, addressType.bridged, isStrict, isInvariant,
                                            UInt64(alignment ?? 0))
    return notifyNew(dr.getAs(PointerToAddressInst.self))
  }

  public func createIndexAddr(base: Value, index: Value, needStackProtection: Bool) -> IndexAddrInst {
    let dr = bridged.createIndexAddr(base.bridged, index.bridged, needStackProtection)
    return notifyNew(dr.getAs(IndexAddrInst.self))
  }

  public func createUncheckedRefCast(from value: Value, to type: Type) -> UncheckedRefCastInst {
    let cast = bridged.createUncheckedRefCast(value.bridged, type.bridged)
    return notifyNew(cast.getAs(UncheckedRefCastInst.self))
  }

  public func createUncheckedAddrCast(from value: Value, to type: Type) -> UncheckedAddrCastInst {
    let cast = bridged.createUncheckedAddrCast(value.bridged, type.bridged)
    return notifyNew(cast.getAs(UncheckedAddrCastInst.self))
  }

  public func createUpcast(from value: Value, to type: Type) -> UpcastInst {
    let cast = bridged.createUpcast(value.bridged, type.bridged)
    return notifyNew(cast.getAs(UpcastInst.self))
  }
  
  @discardableResult
  public func createCheckedCastAddrBranch(
    source: Value, sourceFormalType: CanonicalType,
    destination: Value, targetFormalType: CanonicalType,
    options: CheckedCastInstOptions,
    consumptionKind: CheckedCastAddrBranchInst.CastConsumptionKind,
    successBlock: BasicBlock,
    failureBlock: BasicBlock
  ) -> CheckedCastAddrBranchInst {
    
    let bridgedConsumption: BridgedInstruction.CastConsumptionKind
    switch consumptionKind {
      case .TakeAlways:    bridgedConsumption = .TakeAlways
      case .TakeOnSuccess: bridgedConsumption = .TakeOnSuccess
      case .CopyOnSuccess: bridgedConsumption = .CopyOnSuccess    
    }

    let cast = bridged.createCheckedCastAddrBranch(source.bridged, sourceFormalType.bridged,
                                                   destination.bridged, targetFormalType.bridged,
                                                   options.bridged,
                                                   bridgedConsumption,
                                                   successBlock.bridged, failureBlock.bridged)
    return notifyNew(cast.getAs(CheckedCastAddrBranchInst.self))
  }

  @discardableResult
  public func createUnconditionalCheckedCastAddr(
    options: CheckedCastInstOptions,
    source: Value, sourceFormalType: CanonicalType,
    destination: Value, targetFormalType: CanonicalType
  ) -> UnconditionalCheckedCastAddrInst {
    let cast = bridged.createUnconditionalCheckedCastAddr(
        options.bridged, source.bridged, sourceFormalType.bridged,
        destination.bridged, targetFormalType.bridged
    )
    return notifyNew(cast.getAs(UnconditionalCheckedCastAddrInst.self))
  }

  public func createUncheckedOwnershipConversion(
    operand: Value, resultOwnership: Ownership
  ) -> UncheckedOwnershipConversionInst {
    let uoc = bridged.createUncheckedOwnershipConversion(operand.bridged, resultOwnership._bridged)
    return notifyNew(uoc.getAs(UncheckedOwnershipConversionInst.self))
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
  public func createDebugValue(value: Value, debugVariable: DebugVariableInstruction.DebugVariable) -> DebugValueInst {
    return notifyNew(bridged.createDebugValue(value.bridged, debugVariable).getAs(DebugValueInst.self))
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
  
  public func createBeginApply(function: Value,
                               _ substitutionMap: SubstitutionMap,
                               arguments: [Value],
                               isNonThrowing: Bool = false,
                               isNonAsync: Bool = false,
                               specializationInfo: ApplyInst.SpecializationInfo = ApplyInst.SpecializationInfo()
  ) -> BeginApplyInst {
    let apply = arguments.withBridgedValues { valuesRef in
      bridged.createBeginApply(function.bridged, substitutionMap.bridged, valuesRef,
                               isNonThrowing, isNonAsync, specializationInfo)
    }
    return notifyNew(apply.getAs(BeginApplyInst.self))
  }
  
  public func createWitnessMethod(lookupType: CanonicalType,
                                  conformance: Conformance,
                                  member: DeclRef,
                                  methodType: Type) -> WitnessMethodInst {
    return notifyNew(bridged.createWitnessMethod(lookupType.bridged, conformance.bridged,
                                                 member.bridged, methodType.bridged).getAs(WitnessMethodInst.self))    
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

  public func createInitEnumDataAddr(enumAddress: Value, caseIndex: Int, type: Type) -> InitEnumDataAddrInst {
    let uteda = bridged.createInitEnumDataAddr(enumAddress.bridged, caseIndex, type.bridged)
    return notifyNew(uteda.getAs(InitEnumDataAddrInst.self))
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

  public func createVectorBaseAddr(vector: Value) -> VectorBaseAddrInst {
    return notifyNew(bridged.createVectorBaseAddr(vector.bridged).getAs(VectorBaseAddrInst.self))
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

  public func createProjectBox(box: Value, fieldIndex: Int) -> ProjectBoxInst {
    return notifyNew(bridged.createProjectBox(box.bridged, fieldIndex).getAs(ProjectBoxInst.self))
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
    conformances: [Conformance]
  ) -> InitExistentialMetatypeInst {
    let initExistential = conformances.map{ $0.bridged }.withBridgedArrayRef {
      return bridged.createInitExistentialMetatype(metatype.bridged,
                                                   existentialType.bridged,
                                                   BridgedConformanceArray(pcArray: $0))
    }
    return notifyNew(initExistential.getAs(InitExistentialMetatypeInst.self))
  }

  public func createMetatype(
    ofInstanceType instanceType: CanonicalType,
    representation: AST.`Type`.MetatypeRepresentation
  ) -> MetatypeInst {
    let bridgedRep: BridgedASTType.MetatypeRepresentation
    switch representation {
    case .thin:  bridgedRep = .Thin
    case .thick: bridgedRep = .Thick
    case .objC:  bridgedRep = .ObjC
    }
    let metatype = bridged.createMetatype(instanceType.bridged, bridgedRep)
    return notifyNew(metatype.getAs(MetatypeInst.self))
  }

  public func createEndCOWMutation(instance: Value, keepUnique: Bool) -> EndCOWMutationInst {
    let endMutation = bridged.createEndCOWMutation(instance.bridged, keepUnique)
    return notifyNew(endMutation.getAs(EndCOWMutationInst.self))
  }

  @discardableResult
  public func createEndCOWMutationAddr(address: Value) -> EndCOWMutationAddrInst {
    let endMutation = bridged.createEndCOWMutationAddr(address.bridged)
    return notifyNew(endMutation.getAs(EndCOWMutationAddrInst.self))
  }

  public func createMarkDependence(value: Value, base: Value, kind: MarkDependenceKind) -> MarkDependenceInst {
    let markDependence = bridged.createMarkDependence(value.bridged, base.bridged,
                                                      BridgedInstruction.MarkDependenceKind(rawValue: kind.rawValue)!)
    return notifyNew(markDependence.getAs(MarkDependenceInst.self))
  }

  public func createMarkDependenceAddr(value: Value, base: Value, kind: MarkDependenceKind) -> MarkDependenceAddrInst {
    let markDependence = bridged.createMarkDependenceAddr(
      value.bridged, base.bridged, BridgedInstruction.MarkDependenceKind(rawValue: kind.rawValue)!)
    return notifyNew(markDependence.getAs(MarkDependenceAddrInst.self))
  }
    
  public func createMarkUninitialized(value: Value, kind: MarkUninitializedInst.Kind) -> MarkUninitializedInst {
    let mu = bridged.createMarkUninitialized(value.bridged, kind.rawValue)
    return notifyNew(mu.getAs(MarkUninitializedInst.self))
  }

  public func createMarkUnresolvedNonCopyableValue(value: Value,
                                                   checkKind: MarkUnresolvedNonCopyableValueInst.CheckKind,
                                                   isStrict: Bool) -> MarkUnresolvedNonCopyableValueInst {
    let mu = bridged.createMarkUnresolvedNonCopyableValue(value.bridged, checkKind.rawValue, isStrict)
    return notifyNew(mu.getAs(MarkUnresolvedNonCopyableValueInst.self))
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

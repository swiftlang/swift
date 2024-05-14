//===--- AllocVectorLowering.swift -----------------------------------------==//
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

import SIL

/// An experimental pass to lower allocateVector builtins.
///
/// By default the builtin is lowered to an `alloc_vector` with a paired `dealloc_stack`.
/// If the builtin appears in the initializer of a global variable and the vector elements
/// are initialized, a statically initialized global is created where the initializer is a
/// `vector` instruction.
///
/// TODO: liverange computation is done very ad-hoc and should be eventually done by inspecting
///       forwarding instructions.
let allocVectorLowering = FunctionPass(name: "alloc-vector-lowering") {
  (function: Function, context: FunctionPassContext) in

  var needFixStackNesting = false

  for inst in function.instructions {
    if let bi = inst as? BuiltinInst, bi.id == .AllocVector {

      if !context.options.hasFeature(.FixedArrays) {
        if !function.isTransparent {
          context.diagnosticEngine.diagnose(inst.location.sourceLoc, .fixed_arrays_not_available)
        }
        return
      }

      optimizeCopyFromArray(allocVectorBuiltin: bi, context)

      lower(allocVectorBuiltin: bi, context)
      needFixStackNesting = true
    }
  }
  if needFixStackNesting {
    function.fixStackNesting(context)
  }
}

/// Replace the initialization of a vector from a swift Array with a direct initialization of the vector.
///
///   %uninitialized_array = apply "array.uninitialized_intrinsic"
///   store %elements to %uninitialized_array
///   %array = apply "array.finalize_intrinsic"(%uninitialized_array)
///   %vector = builtin allocateVector
///   apply "array.copy_into_vector"(%vector, %array)
///
///  ->
///
///   %vector = builtin allocateVector
///   store %elements to %vector
///
private func optimizeCopyFromArray(allocVectorBuiltin: BuiltinInst, _ context: FunctionPassContext) {
  guard let singleVectorUse = allocVectorBuiltin.uses.ignoreDebugUses.singleUse,
        let unsafePointerStruct = singleVectorUse.instruction as? StructInst,
        let copyVectorCall = unsafePointerStruct.copyVectorUser,
        let finishArrayCall = copyVectorCall.arguments[1].lookThoughOwnershipInstructions as? ApplyInst,
        finishArrayCall.isSemanticCall("array.finalize_intrinsic", withArgumentCount: 1),
        let allocatedArray = finishArrayCall.arguments[0] as? MultipleValueInstructionResult,
        allocatedArray.uses.getSingleUser(notOfType: MarkDependenceInst.self) === finishArrayCall,
        let destructureTuple = allocatedArray.definingInstruction as? DestructureTupleInst,
        let allocArrayCall = destructureTuple.tuple as? ApplyInst,
        allocArrayCall.uses.singleUse?.instruction == destructureTuple else
  {
    return
  }

  let capacityArg: Value
  if allocArrayCall.isSemanticCall("array.uninitialized_intrinsic", withArgumentCount: 1) {
    capacityArg = allocArrayCall.arguments[0]
  } else if allocArrayCall.isSemanticCall("array.uninitialized", withArgumentCount: 2),
            allocArrayCall.arguments[0].isExclusivelyUsedAllocRef,
            let capacityStruct = allocArrayCall.arguments[1] as? StructInst,
            capacityStruct.operands.count == 1
  {
    capacityArg = capacityStruct.operands[0].value
  } else {
    return
  }

  guard let arrayCountLiteral = capacityArg as? IntegerLiteralInst,
        let capacity = arrayCountLiteral.value else
  {
    return
  }

  var getCountCalls: [ApplyInst] = []

  if !checkArrayUses(array: finishArrayCall, getCountCalls: &getCountCalls, ignore: copyVectorCall) {
    return
  }

  allocVectorBuiltin.move(before: destructureTuple, context)
  if allocVectorBuiltin.type != destructureTuple.results[1].type {
    let builder = Builder(after: destructureTuple, context)
    let ptrStruct = builder.createStruct(type: destructureTuple.results[1].type, elements: [allocVectorBuiltin])
    destructureTuple.results[1].uses.replaceAll(with: ptrStruct, context)
  } else {
    destructureTuple.results[1].uses.replaceAll(with: allocVectorBuiltin, context)
  }
  allocVectorBuiltin.operands[1].set(to: arrayCountLiteral, context)

  for getCountCall in getCountCalls {
    let builtinIntType = getCountCall.type.getNominalFields(in: getCountCall.parentFunction)![0]
    let builder = Builder(before: getCountCall, context)
    let countLiteral = builder.createIntegerLiteral(capacity, type: builtinIntType)
    let countIntValue = builder.createStruct(type: getCountCall.type, elements: [countLiteral])
    getCountCall.uses.replaceAll(with: countIntValue, context)
  }

  for use in allocatedArray.uses {
    if let md = use.instruction as? MarkDependenceInst {
      md.uses.replaceAll(with: md.value, context)
    }
  }

  context.erase(instructionIncludingAllUsers: allocArrayCall)

  context.removeTriviallyDeadInstructionsPreservingDebugInfo(in: allocVectorBuiltin.parentFunction)
}

private func checkArrayUses(array: Value, getCountCalls: inout [ApplyInst], ignore: Instruction) -> Bool {
  for arrayUse in array.uses {
    switch arrayUse.instruction {
    case let apply as ApplyInst where apply.isSemanticCall("array.get_count", withArgumentCount: 1):
      getCountCalls.append(apply)
    case ignore,
         is DestroyValueInst,
         is EndBorrowInst:
      break
    case let copy as CopyValueInst:
      if !checkArrayUses(array: copy, getCountCalls: &getCountCalls, ignore: ignore) {
        return false
      }
    case let borrow as BeginBorrowInst:
      if !checkArrayUses(array: borrow, getCountCalls: &getCountCalls, ignore: ignore) {
        return false
      }
    case let str as StructInst where str.operands.count == 1:
      // Needed to handle dead struct constructions of FixedArray._Literal
      if !checkArrayUses(array: str, getCountCalls: &getCountCalls, ignore: ignore) {
        return false
      }
    default:
      return false
    }
  }
  return true
}

private extension Value {
  var isExclusivelyUsedAllocRef: Bool {
    if uses.singleUse == nil {
      return false
    }
    switch self {
    case is AllocRefInst:
      return true
    case let move as MoveValueInst:
      return move.fromValue.isExclusivelyUsedAllocRef
    default:
      return false
    }
  }
}

private extension StructInst {
  var copyVectorUser: ApplyInst? {
    for use in uses {
      if let apply = use.instruction as? ApplyInst,
         apply.isSemanticCall("array.copy_into_vector", withArgumentCount: 2)
      {
        return apply
      }
    }
    return nil
  }
}

/// Lowers the allocateVector builtin either to an stack-allocated vector (`alloc_vector`)
/// or to a statically initialized global.
private func lower(allocVectorBuiltin: BuiltinInst, _ context: FunctionPassContext) {
  let visitor = ComputeNonEscapingLiverange(of: allocVectorBuiltin, context)

  guard let result = allocVectorBuiltin.visit(using: visitor, context) else {
    if allocVectorBuiltin.parentFunction.isTransparent {
      return
    }
    context.diagnosticEngine.diagnose(allocVectorBuiltin.location.sourceLoc, .lifetime_value_outside_scope)
    return
  }
  switch result {
  case .storeToGlobal(let storeInst):
    createOutlinedGlobal(for: allocVectorBuiltin, storeToGlobal: storeInst, context)

  case .liverange(var liverange):
    defer { liverange.deinitialize() }
    createStackAllocatedVector(for: allocVectorBuiltin, liverange: liverange, context)

  case .invalid:
    context.diagnosticEngine.diagnose(allocVectorBuiltin.location.sourceLoc, .lifetime_value_outside_scope)
  }
}

private func createOutlinedGlobal(
  for allocVectorBuiltin: BuiltinInst,
  storeToGlobal: StoreInst,
  _ context: FunctionPassContext
) {

  guard let capacityLiteral = allocVectorBuiltin.operands[1].value as? IntegerLiteralInst,
        let capacity = capacityLiteral.value else {
    context.diagnosticEngine.diagnose(allocVectorBuiltin.location.sourceLoc, .vector_capacity_not_constant)
    return
  }

  let elementType = allocVectorBuiltin.substitutionMap.replacementTypes[0]!
  let outlinedGlobal = context.createGlobalVariable(
        name: context.mangleOutlinedVariable(from: allocVectorBuiltin.parentFunction),
        type: elementType, isPrivate: true)

  let globalBuilder = Builder(staticInitializerOf: outlinedGlobal, context)

  if let initStores = getInitStores(to: allocVectorBuiltin, count: capacity, context) {
    var cloner = StaticInitCloner(cloneTo: outlinedGlobal, context)
    defer { cloner.deinitialize() }
    var elements = [Value]()
    for initStore in initStores {
      elements.append(cloner.clone(initStore.source as! SingleValueInstruction))
      context.erase(instruction: initStore)
    }
    globalBuilder.createVector(type: elementType, arguments: elements)
  } else {
    let capacityInInit = globalBuilder.createIntegerLiteral(capacity, type: capacityLiteral.type)
    _ = globalBuilder.createAllocVector(capacity: capacityInInit, elementType: elementType)
  }

  let builder = Builder(before: allocVectorBuiltin, context)
  let globalAddr = builder.createGlobalAddr(global: outlinedGlobal, dependencyToken: nil)
  let rawVectorPointer = builder.createAddressToPointer(address: globalAddr, pointerType: allocVectorBuiltin.type,
                                                        needStackProtection: false)
  allocVectorBuiltin.uses.replaceAll(with: rawVectorPointer, context)
  context.erase(instruction: allocVectorBuiltin)
}

private func createStackAllocatedVector(
  for allocVectorBuiltin: BuiltinInst,
  liverange: InstructionRange,
  _ context: FunctionPassContext
) {
  let builder = Builder(before: allocVectorBuiltin, context)
  let elementType = allocVectorBuiltin.substitutionMap.replacementTypes[0]!
  let allocVec = builder.createAllocVector(capacity: allocVectorBuiltin.operands[1].value, elementType: elementType)
  let rawVectorPointer = builder.createAddressToPointer(address: allocVec, pointerType: allocVectorBuiltin.type,
                                                        needStackProtection: true)

  allocVectorBuiltin.uses.replaceAll(with: rawVectorPointer, context)
  context.erase(instruction: allocVectorBuiltin)

  for endInst in liverange.ends {
    let builder = Builder(after: endInst, context)
    builder.createDeallocStack(allocVec)
  }
}

private func getInitStores(to allocVectorBuiltin: BuiltinInst, count: Int,
                           _ context: FunctionPassContext) -> [StoreInst]?
{
  var postInitRegion = InstructionSet(context)
  defer { postInitRegion.deinitialize() }

  var stores = Array<StoreInst?>(repeating: nil, count: count)

  for use in allocVectorBuiltin.uses {
    if let ptrToAddr = use.instruction as? PointerToAddressInst {
      if !findInitStores(of: ptrToAddr, atIndex: 0, &stores) {
        return nil
      }
    } else {
      for inst in InstructionList(first: use.instruction) {
        if !postInitRegion.insert(inst) {
          break
        }
      }
    }
  }

  for store in stores {
    if let store = store {
      if postInitRegion.contains(store) || store.parentBlock != allocVectorBuiltin.parentBlock {
        return nil
      }
    } else {
      return nil
    }
  }
  return stores.map { $0! }
}

private func findInitStores(of address: Value, atIndex: Int, _ initStores: inout [StoreInst?]) -> Bool {
  for use in address.uses {
    switch use.instruction {
    case let indexAddr as IndexAddrInst:
      guard let indexLiteral = indexAddr.index as? IntegerLiteralInst,
            let index = indexLiteral.value,
            findInitStores(of: indexAddr, atIndex: atIndex + index, &initStores) else
      {
        return false
      }
    case let store as StoreInst where store.destinationOperand == use:
      if !store.source.isValidGlobalInitValue {
        return false
      }
      if atIndex >= initStores.count ||
         initStores[atIndex] != nil
      {
        return false
      }
      initStores[atIndex] = store
    default:
      return false
    }
  }
  return true
}

// This is very experimental and not ideal at all.
// TODO: replace this with DiagnoseLifetimeDependence from https://github.com/apple/swift/pull/68682
private struct ComputeNonEscapingLiverange : EscapeVisitorWithResult {

  enum Result {
    case liverange(InstructionRange)
    case storeToGlobal(StoreInst)
    case invalid
  }

  var liverange: InstructionRange
  var storeToGlobal: StoreInst? = nil
  let domTree: DominatorTree

  init(of instruction: Instruction, _ context: FunctionPassContext) {
    self.liverange = InstructionRange(begin: instruction, context)
    self.domTree = context.dominatorTree
  }

  var result: Result {
    if let storeToGlobal = storeToGlobal {
      defer {
        var lr = liverange
        lr.deinitialize()
      }
      precondition(liverange.inclusiveRangeContains(storeToGlobal))
      if liverange.inclusiveRangeContains(storeToGlobal.next!) ||
         liverange.ends.contains(where: { $0 != storeToGlobal }) ||
         !storeToGlobal.isInitializingGlobal
      {
        return .invalid
      }
      return .storeToGlobal(storeToGlobal)
    }
    return .liverange(liverange)
  }

  mutating func cleanupOnAbort() {
    liverange.deinitialize()
  }

  mutating func visitDef(def: Value, path: EscapePath) -> DefResult {
    if def.definingInstruction == liverange.begin {
      return .walkDown
    }
    switch def {
    case let arg as FunctionArgument:
      if case .indirectOut = arg.convention {
        return .ignore
      }
      return .continueWalkUp
    default:
      return .continueWalkUp
    }
  }

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    let user = operand.instruction
    let beginBlockOfRange = liverange.blockRange.begin
    let dominates = beginBlockOfRange.dominates(user.parentBlock, domTree)
    switch user {
    case let store as StoreInst:
      if dominates {
        liverange.insert(store)
      }
      let accessPath = store.destination.accessPath
      if case .global = accessPath.base {
        if !accessPath.projectionPath.isEmpty ||
           storeToGlobal != nil
        {
          return .abort
        }
        storeToGlobal = store
        return .ignore
      }
      return .continueWalk
    case let apply as ApplyInst:
      if dominates {
        liverange.insert(user)
      }
      if !apply.isEscapable {
        return .abort
      }
      return .ignore
    case is DeallocStackInst:
      return .continueWalk
    default:
      if dominates {
        liverange.insert(user)
      }
      return .continueWalk
    }
  }
}

private extension StoreInst {
  var isInitializingGlobal: Bool {
    let accessPath = destination.accessPath
    if accessPath.projectionPath.isEmpty,
       case .global(let global) = accessPath.base,
       global == parentFunction.initializedGlobal
    {
      return true
    }
    return false
  }
}

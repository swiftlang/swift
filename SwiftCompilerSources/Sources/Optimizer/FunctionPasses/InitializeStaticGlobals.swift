//===--- InitializeStaticGlobals.swift -------------------------------------==//
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

import AST
import SIL

/// Converts a lazily initialized global to a statically initialized global variable.
///
/// When this pass runs on a global initializer `[global_init_once_fn]` it tries to
/// create a static initializer for the initialized global.
///
/// ```
///   sil [global_init_once_fn] @globalinit {
///     alloc_global @the_global
///     %a = global_addr @the_global
///     %i = some_const_initializer_insts
///     store %i to %a
///   }
/// ```
/// The pass creates a static initializer for the global:
/// ```
///   sil_global @the_global = {
///     %initval = some_const_initializer_insts
///   }
/// ```
/// and removes the allocation and store instructions from the initializer function:
/// ```
///   sil [global_init_once_fn] @globalinit {
///     %a = global_addr @the_global
///     %i = some_const_initializer_insts
///   }
/// ```
/// The initializer then becomes a side-effect free function which let's the builtin-
/// simplification remove the `builtin "once"` which calls the initializer.
///
let initializeStaticGlobalsPass = FunctionPass(name: "initialize-static-globals") {
  (function: Function, context: FunctionPassContext) in

  guard // In case of a preceding error, there is no guarantee that the SIL is valid.
        !context.hadError,
        // Is `function` a global init function?
        let global = function.initializedGlobal,
        // Even if the init value is a constant, the initializer can have side effects, e.g.
        //   let g = { print("hello"); return 27 }()
        !function.hasSideEffects(besideStoringTo: global),
        // Try to get a constant global init value.
        let initValue = GlobalInitValue(of: function, context)
  else {
    return
  }

  initValue.materialize(into: global, from: function, context)

  // The initial value can contain a `begin_access` if it references another global variable by address, e.g.
  //   var p = Point(x: 10, y: 20)
  //   let o = UnsafePointer(&p)
  //
  global.stripAccessInstructionFromInitializer(context)

  function.removeAllInitializationCode(for: global, context)
}

/// A tree, which represents a constant init value of a global variable.
private indirect enum GlobalInitValue {
  case undefined

  // A value which is a SIL "constant", e.g. a literal instruction, a struct/tuple of literals, etc.
  case constant(Value)

  // A struct, tuple or vector, which is _not_ a SIL "constant".
  // For example, a struct or vector which is initialized by storing its elements.
  case aggregate([GlobalInitValue])

  // An enum with a payload which is not a SIL "constant".
  case enumCase(caseIndex: Int, payload: GlobalInitValue)

  init?(of globalInitFunction: Function, _ context: FunctionPassContext) {
    self = .undefined
    var global: GlobalVariable? = nil

    // The init values for temporary stack locations. This is needed if e.g. a vector is initialized in a
    // stack location and the loaded from it and stored in the global.
    var stackValues = Dictionary<AllocStackInst, GlobalInitValue>()

    for inst in globalInitFunction.instructions {
      switch inst {
      case let allocGlobal as AllocGlobalInst:
        guard global == nil, allocGlobal.global.canBeInitializedStatically else {
          return nil
        }
        global = allocGlobal.global
      case let ga as GlobalAddrInst where ga.global == global:
        // SILGen only creates a single global_addr in the init function. So no need to support multiple ones.
        guard case .undefined = self,
              let selfInitVal = GlobalInitValue(of: ga, context)
        else {
          return nil
        }
        self = selfInitVal
      case let allocStack as AllocStackInst:
        guard let stackInitVal = GlobalInitValue(of: allocStack, context) else {
          return nil
        }
        stackValues[allocStack] = stackInitVal
      default:
        break
      }
    }

    // At this point some `.constant` elements can be (or contain) loads from stack locations.
    // Replace those elements with the init value of the corresponding stack location.
    resolveLoads(from: &stackValues, context)

    if !isValid(context) {
      return nil
    }
  }

  private init?(of address: Value, _ context: FunctionPassContext) {
    var builder = InitValueBuilder(originalAddress: address)
    if builder.walkDownUses(ofAddress: address, path: UnusedWalkingPath()) == .abortWalk {
      return nil
    }
    self = builder.initValue
  }

  // Sets an element in the constant tree.
  // Returns true if this was successful. One reason for being not successful is if a certain
  // element is set twice, i.e. does not have a single defined value.
  mutating func setElement(to value: Value, at path: SmallProjectionPath, type: Type) -> Bool {
    let (kind, index, subPath) = path.pop()
    switch kind {
    case .root:
      guard case .undefined = self else {
        // The element was set twice.
        return false
      }
      self = .constant(value)
      return true

    case .structField:
      guard let structFields = type.getNominalFields(in: value.parentFunction) else {
        return false
      }
      return setField(to: value, at: subPath, index: index, type: structFields[index], numFields: structFields.count)

    case .tupleField:
      let tupleElements = type.tupleElements
      return setField(to: value, at: subPath, index: index, type: tupleElements[index], numFields: tupleElements.count)

    case .vectorBase:
      let elementType = type.builtinFixedArrayElementType(in: value.parentFunction)
      guard let vectorSize = type.builtinFixedArraySizeType.valueOfInteger else {
        return false
      }
      let (indexKind, vectorIndex, vectorSubPath) = subPath.pop()
      switch indexKind {
      case .indexedElement:
        return setField(to: value, at: vectorSubPath, index: vectorIndex, type: elementType, numFields: vectorSize)
      case .anyIndexedElement:
        // A non-constant index means: all elements are initialized with the same value.
        // (that's what we checked in `storesToAllVectorElements`)
        for i in 0..<vectorSize {
          if !setField(to: value, at: vectorSubPath, index: i, type: elementType, numFields: vectorSize) {
            return false
          }
        }
        return true
      default:
        // A missing index in the path means: the first element
        return setField(to: value, at: subPath, index: 0, type: elementType, numFields: vectorSize)
      }

    default:
      return false
    }
  }

  private mutating func setField(
    to value: Value, at path: SmallProjectionPath,
    index: Int, type: Type, numFields: Int
  ) -> Bool {
    if case .undefined = self {
      // Initialize the aggregate array if not done, yet.
      self = .aggregate(Array(repeating: GlobalInitValue.undefined, count: numFields))
    }
    if case .aggregate(let fields) = self {
      var newFields = fields
      self = .undefined // avoid copy-on-write
      if !newFields[index].setElement(to: value, at: path, type: type) {
        return false
      }
      self = .aggregate(newFields)
      return true
    }
    return false
  }

  /// Creates SIL for this global init value in the initializer of the `global`.
  func materialize(into global: GlobalVariable, from function: Function, _ context: FunctionPassContext) {
    var cloner = Cloner(cloneToGlobal: global, context)
    defer { cloner.deinitialize() }
    let builder = Builder(staticInitializerOf: global, context)

    _ = materializeRecursively(type: global.type, &cloner, builder, function)
  }

  private func materializeRecursively(
    type: Type,
    _ cloner: inout Cloner<FunctionPassContext>,
    _ builder: Builder,
    _ function: Function
  ) -> Value {
    switch self {
    case .undefined:
      fatalError("cannot materialize undefined init value")

    case .constant(let value):
      return cloner.cloneRecursively(value: value)

    case .aggregate(let fields):
      if type.isStruct {
        let fieldTypes = type.getNominalFields(in: function)!
        let fieldValues = zip(fields, fieldTypes).map {
          $0.0.materializeRecursively(type: $0.1, &cloner, builder, function)
        }
        return builder.createStruct(type: type, elements: fieldValues)
      }
      if type.isTuple {
        let elementValues = zip(fields, type.tupleElements).map {
          $0.0.materializeRecursively(type: $0.1, &cloner, builder, function)
        }
        return builder.createTuple(type: type, elements: elementValues)
      }
      assert(type.isBuiltinFixedArray)
      let elementType = type.builtinFixedArrayElementType(in: function)
      let elementValues = fields.map {
        $0.materializeRecursively(type: elementType, &cloner, builder, function)
      }
      return builder.createVector(type: type, arguments: elementValues)

    case .enumCase(let caseIndex, let payload):
      let payloadType = type.getEnumCases(in: function)!.first(where: { $0.index == caseIndex })!.payload!
      let payloadValue = payload.materializeRecursively(type: payloadType, &cloner, builder, function)
      return builder.createEnum(caseIndex: caseIndex, payload: payloadValue, enumType: type)
    }
  }

  // Replace `.constant` elements, which are (or contain) loads, init value of the corresponding stack location.
  // Some  from stack locations.
  mutating func resolveLoads(
    from stackValues: inout Dictionary<AllocStackInst, GlobalInitValue>,
    _ context: FunctionPassContext
  ) {
    var resolvedAllocStacks = InstructionSet(context)
    defer { resolvedAllocStacks.deinitialize() }
    resolveLoadsRecursively(from: &stackValues, &resolvedAllocStacks, context)
  }

  private mutating func resolveLoadsRecursively(
    from stackValues: inout Dictionary<AllocStackInst, GlobalInitValue>,
    _ resolvedAllocStacks: inout InstructionSet,
    _ context: FunctionPassContext
  ) {
    switch self {
    case .undefined:
      break
    case .constant(let value):
      if value.containsLoad(context) {
        switch value {
        case is StructInst, is TupleInst:
          self = .aggregate((value as! Instruction).operands.lazy.map { .constant($0.value) })
          resolveLoadsRecursively(from: &stackValues, &resolvedAllocStacks, context)
        case let ei as EnumInst:
          self = .enumCase(caseIndex: ei.caseIndex, payload: .constant(ei.payload!))
          resolveLoadsRecursively(from: &stackValues, &resolvedAllocStacks, context)
        case let li as LoadInst:
          guard let allocStack = li.address as? AllocStackInst,
                var stackInit = stackValues[allocStack]
          else {
            self = .undefined
            return
          }
          if resolvedAllocStacks.insert(allocStack) {
            stackInit.resolveLoadsRecursively(from: &stackValues, &resolvedAllocStacks, context)
            stackValues[allocStack] = stackInit
          }
          self = stackInit
        default:
          break
        }
      }
    case .aggregate(let fields):
      var newFields = fields
      self = .undefined // avoid copy-on-write
      for i in 0..<fields.count {
        newFields[i].resolveLoadsRecursively(from: &stackValues, &resolvedAllocStacks, context)
      }
      self = .aggregate(newFields)
    case .enumCase(let caseIndex, let payload):
      var newPayload = payload
      newPayload.resolveLoadsRecursively(from: &stackValues, &resolvedAllocStacks, context)
      self = .enumCase(caseIndex: caseIndex, payload: newPayload)
    }
  }

  func isValid(_ context: FunctionPassContext) -> Bool {
    switch self {
    case .undefined:
      return false
    case .constant(let value):
      return value.isValidGlobalInitValue(context)
    case .aggregate(let fields):
      return fields.allSatisfy { $0.isValid(context) }
    case .enumCase(_, let payload):
      return payload.isValid(context)
    }
  }
}

private extension Value {
  /// Returns true if this value is a `load` or a struct/tuple/enum with a `load` operand.
  func containsLoad(_ context: FunctionPassContext) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(self)
    while let v = worklist.pop() {
      switch v {
      case is LoadInst:
        return true
      case is StructInst, is TupleInst:
        worklist.pushIfNotVisited(contentsOf: (v as! Instruction).operands.lazy.map { $0.value })
      case let ei as EnumInst:
        if let payload = ei.payload {
          worklist.pushIfNotVisited(payload)
        }
      default:
        break
      }
    }
    return false
  }
}

private struct InitValueBuilder: AddressDefUseWalker {
  let originalAddress: Value
  var initValue = GlobalInitValue.undefined

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    switch address.instruction {
    case let store as StoreInst:
      let accessPath = store.destination.constantAccessPath
      switch accessPath.base {
      case .global, .stack:
        if !initValue.setElement(to: store.source, at: accessPath.projectionPath, type: originalAddress.type) {
          return .abortWalk
        }
        return .continueWalk
      case .index(let indexAddr):
        // If we have a non-constant index in the access path, check if this is from a loop which
        // initializes all elements.
        guard store.storesToAllVectorElements(using: indexAddr) else {
          return .abortWalk
        }
        let nonConstAccessPath = store.destination.accessPath
        guard indexAddr.base.constantAccessPath.base == nonConstAccessPath.base else {
          return .abortWalk
        }
        // The `nonConstAccessPath` now contains a single `.anyIndexedElement`.
        if !initValue.setElement(to: store.source, at: nonConstAccessPath.projectionPath, type: originalAddress.type) {
          return .abortWalk
        }
        return .continueWalk
      default:
        fatalError("could not compute access path")
      }
    case is LoadInst, is DeallocStackInst:
      return .continueWalk
    case let bi as BuiltinInst where bi.id == .PrepareInitialization:
      return .continueWalk
    default:
      return .abortWalk
    }
  }
}

private extension StoreInst {
  func storesToAllVectorElements(using indexAddr: IndexAddrInst) -> Bool {
    if let vectorBase = indexAddr.base as? VectorBaseAddrInst,
       let headerBr = vectorBase.parentBlock.terminator as? BranchInst,
       headerBr.targetBlock == parentBlock,
       let vectorSize = vectorBase.vector.type.builtinFixedArraySizeType.valueOfInteger,
       let (start, loopCount, increment) = getLoopInductionInfo(of: indexAddr.index.lookThroughIndexScalarCast),
       start == 0, loopCount == vectorSize, increment == 1
    {
      return true
    }
    return false
  }
}

/// Matches the pattern:
/// ```
///   %startValue = integer_literal <start>
///   %incrementValue = integer_literal <increment>
///   %loopEndValue = integer_literal <loopCount>
///   br loopBlock(%startValue)
/// loopBlock(%inductionVar):
///   %add = builtin "sadd_with_overflow"(%inductionVar, %incrementValue, ...)
///   %incrementedInductionVar = tuple_extract %add, 0
///   %loopBreakCondition = builtin "cmp_eq"(%incrementedInductionVar, %loopEndValue)
///   cond_br %loopBreakCondition, exitBlock, backEdgeBlock
/// backEdgeBlock:
///   br loopBlock(%incrementedInductionVar)
/// exitBlock:
/// ```
private func getLoopInductionInfo(of inductionVar: Value) -> (start: Int, loopCount: Int, increment: Int)? {
  let loopBlock = inductionVar.parentBlock
  if let inductionVarArg = inductionVar as? Argument, inductionVarArg.parentBlock == loopBlock,
     let inductionVarPhi = Phi(inductionVarArg),
     let condBr = loopBlock.terminator as? CondBranchInst,
     let loopBreakCondition = condBr.condition as? BuiltinInst, loopBreakCondition.id == .ICMP_EQ,
     let incrementedInductionVar = loopBreakCondition.arguments[0] as? TupleExtractInst,
     incrementedInductionVar.fieldIndex == 0,
     let add = incrementedInductionVar.tuple as? BuiltinInst, add.id == .SAddOver,
     add.arguments[0] == inductionVarArg,
     let incrementValue = add.arguments[1] as? IntegerLiteralInst,
     let increment = incrementValue.value,
     let loopEndValue = loopBreakCondition.arguments[1] as? IntegerLiteralInst,
     let loopCount = loopEndValue.value,
     let backEdgeBranch = condBr.falseBlock.terminator as? BranchInst,
     backEdgeBranch.targetBlock == loopBlock,
     inductionVarPhi.incomingOperand(inPredecessor: backEdgeBranch.parentBlock).value == incrementedInductionVar,
     let startValue = inductionVarPhi.incomingValue(notInBlock: backEdgeBranch.parentBlock) as? IntegerLiteralInst,
     let start = startValue.value
  {
    return (start, loopCount, increment)
  }
  return nil
}

private extension Phi {
  func incomingValue(notInBlock: BasicBlock) -> Value? {
    if let block = predecessors.lazy.filter({ $0 != notInBlock }).singleElement {
      return incomingOperand(inPredecessor: block).value
    }
    return nil
  }
}

private extension Function {
  func hasSideEffects(besideStoringTo global: GlobalVariable) -> Bool {
    return instructions.contains { inst in
      switch inst {
      case is DeallocStackInst, is DebugStepInst, is DebugValueInst, is BeginAccessInst, is EndAccessInst:
        return false
      case let alloc as AllocGlobalInst where alloc.global == global:
        return false
      case let store as StoreInst:
        switch store.destination.accessBase {
        case .global(let g) where g == global:
          return false
        case .stack:
          return false
        default:
          return true
        }
      case let bi as BuiltinInst where bi.id == .PrepareInitialization:
        return false
      default:
        return inst.hasUnspecifiedSideEffects
      }
    }
  }

  func removeAllInitializationCode(for global: GlobalVariable, _ context: FunctionPassContext) {
    for inst in instructions {
      switch inst {
      case let allocGlobal as AllocGlobalInst where allocGlobal.global == global:
        context.erase(instruction: allocGlobal)
      case let globalAddr as GlobalAddrInst where globalAddr.global == global:
        context.erase(instructionIncludingAllUsers: globalAddr)
      default:
        break
      }
    }
    context.removeTriviallyDeadInstructionsIgnoringDebugUses(in: self)
  }
}

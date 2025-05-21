//===--- ObjectOutliner.swift ----------------------------------------------==//
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

/// Outlines class objects from functions into statically initialized global variables.
/// This is currently done for Arrays and for global let variables.
///
/// If a function constructs an Array literal with constant elements (done by storing
/// the element values into the array buffer), a new global variable is created which
/// contains the constant elements in its static initializer.
/// For example:
/// ```
///   public func arrayLookup(_ i: Int) -> Int {
///     let lookupTable = [10, 11, 12]
///     return lookupTable[i]
///   }
/// ```
/// is turned into
/// ```
///   private let outlinedVariable = [10, 11, 12]  // statically initialized and allocated in the data section
///
///   public func arrayLookup(_ i: Int) -> Int {
///     return outlinedVariable[i]
///   }
/// ```
///
/// Similar with global let variables:
/// ```
///   let c = SomeClass()
/// ```
/// is turned into
/// ```
///   private let outlinedVariable = SomeClass()  // statically initialized and allocated in the data section
///
///   let c = outlinedVariable
/// ```
///
/// As a second optimization, if an array is a string literal which is a parameter to the
/// `_findStringSwitchCase` library function and the array has many elements (> 16), the
/// call is redirected to `_findStringSwitchCaseWithCache`. This function builds a cache
/// (e.g. a Dictionary) and stores it into a global variable.
/// Then subsequent calls to this function can do a fast lookup using the cache.
///
let objectOutliner = FunctionPass(name: "object-outliner") {
  (function: Function, context: FunctionPassContext) in

  if function.hasOwnership && !function.isSwift51RuntimeAvailable {
    // Since Swift 5.1 global objects have immortal ref counts. And that's required for ownership.
    return
  }

  var allocRefs = Stack<AllocRefInstBase>(context)
  defer { allocRefs.deinitialize() }

  allocRefs.append(contentsOf: function.instructions.lazy.compactMap { $0 as? AllocRefInstBase })

  // Try multiple iterations to handle multi-dimensional arrays.
  var changed: Bool
  repeat {
    changed = false
    for ari in allocRefs where !ari.isDeleted {
      if !context.continueWithNextSubpassRun(for: ari) {
        return
      }
      if let globalValue = optimizeObjectAllocation(allocRef: ari, context) {
        optimizeFindStringCall(stringArray: globalValue, context)
        changed = true
      }
    }
  } while changed
}

private func optimizeObjectAllocation(allocRef: AllocRefInstBase, _ context: FunctionPassContext) -> GlobalValueInst? {
  if !allocRef.fieldsKnownStatically {
    return nil
  }

  guard let endOfInitInst = findEndOfInitialization(
    of: allocRef,
    // An object with tail allocated elements is in risk of being passed to malloc_size, which does
    // not work for non-heap allocated objects. Conservatively, disable objects with tail allocations.
    // Note, that this does not affect Array because Array always has an end_cow_mutation at the end of
    // initialization.
    canStoreToGlobal: allocRef.tailAllocatedCounts.count == 0)
  else {
    return nil
  }

  guard let (storesToClassFields, storesToTailElements) = getInitialization(of: allocRef,
                                                                            ignore: endOfInitInst,
                                                                            context) else
  {
    return nil
  }

  let outlinedGlobal = context.createGlobalVariable(
        name: context.mangleOutlinedVariable(from: allocRef.parentFunction),
        type: allocRef.type, linkage: .private,
        // Only if it's a COW object we can be sure that the object allocated in the global is not mutated.
        // If someone wants to mutate it, it has to be copied first.
        isLet: endOfInitInst is EndCOWMutationInst)

  constructObject(of: allocRef, inInitializerOf: outlinedGlobal, storesToClassFields, storesToTailElements, context)
  context.erase(instructions: storesToClassFields)
  context.erase(instructions: storesToTailElements)

  return replace(object: allocRef, with: outlinedGlobal, context)
}

// The end-of-initialization is either an end_cow_mutation, because it guarantees that the originally initialized
// object is not mutated (it must be copied before mutation).
// Or it is the store to a global let variable in the global's initializer function.
private func findEndOfInitialization(of object: Value, canStoreToGlobal: Bool) -> Instruction? {
  for use in object.uses {
    let user = use.instruction
    switch user {
    case is UpcastInst,
         is UncheckedRefCastInst,
         is MoveValueInst,
         is EndInitLetRefInst:
      if let ecm = findEndOfInitialization(of: user as! SingleValueInstruction, canStoreToGlobal: canStoreToGlobal) {
        return ecm
      }
    case let ecm as EndCOWMutationInst:
      if ecm.doKeepUnique {
        return nil
      }
      return ecm
    case let store as StoreInst:
      if canStoreToGlobal,
         let ga = store.destination as? GlobalAddrInst,
         ga.global.isLet,
         ga.parentFunction.initializedGlobal == ga.global
      {
        return store
      }
    default:
      break
    }
  }
  return nil
}

private func getInitialization(of allocRef: AllocRefInstBase, ignore ignoreInst: Instruction,
                               _ context: FunctionPassContext)
  -> (storesToClassFields: [StoreInst], storesToTailElements: [StoreInst])?
{
  guard let numTailElements = allocRef.numTailElements else {
    return nil
  }
  var fieldStores = Array<StoreInst?>(repeating: nil, count: allocRef.numClassFields)

  // If the tail element is a tuple, then its tuple elements are initialized with separate stores.
  // E.g:
  //   %2 = ref_tail_addr
  //   %3 = tuple_element_addr %2, 0
  //   store %0 to %3
  //   %4 = tuple_element_addr %2, 1
  //   store %1 to %4
  let tailCount = numTailElements != 0 ? numTailElements * allocRef.numStoresPerTailElement : 0
  var tailStores = Array<StoreInst?>(repeating: nil, count: tailCount)

  if !findInitStores(of: allocRef, &fieldStores, &tailStores, ignore: ignoreInst, context) {
    return nil
  }

  // Check that all fields and tail elements are initialized.
  if fieldStores.contains(nil) || tailStores.contains(nil) {
    return nil
  }
  return (fieldStores.map { $0! }, tailStores.map { $0! })
}

private func findInitStores(of object: Value,
                            _ fieldStores: inout [StoreInst?],
                            _ tailStores: inout [StoreInst?],
                            ignore ignoreInst: Instruction,
                            _ context: FunctionPassContext) -> Bool
{
  for use in object.uses {
    let user = use.instruction
    switch user {
    case is UpcastInst,
         is UncheckedRefCastInst,
         is MoveValueInst,
         is EndInitLetRefInst,
         is BeginBorrowInst:
      if !findInitStores(of: user as! SingleValueInstruction, &fieldStores, &tailStores, ignore: ignoreInst, context) {
        return false
      }
    case let rea as RefElementAddrInst:
      if !findStores(inUsesOf: rea, index: rea.fieldIndex, stores: &fieldStores, context) {
        return false
      }
    case let rta as RefTailAddrInst:
      if !findStores(toTailAddress: rta, tailElementIndex: 0, stores: &tailStores, context) {
        return false
      }
    case ignoreInst,
         is EndBorrowInst:
      break
    default:
      if !isValidUseOfObject(use) {
        return false
      }
    }
  }
  return true
}

private func findStores(toTailAddress tailAddr: Value, tailElementIndex: Int, stores: inout [StoreInst?],
                        _ context: FunctionPassContext) -> Bool {
  for use in tailAddr.uses {
    switch use.instruction {
    case let indexAddr as IndexAddrInst:
      guard let indexLiteral = indexAddr.index as? IntegerLiteralInst,
            let tailIdx = indexLiteral.value else
      {
        return false
      }
      if !findStores(toTailAddress: indexAddr, tailElementIndex: tailElementIndex + tailIdx, stores: &stores, context) {
        return false
      }
    case let tea as TupleElementAddrInst:
      // The tail elements are tuples. There is a separate store for each tuple element.
      let numTupleElements = tea.tuple.type.tupleElements.count
      let tupleIdx = tea.fieldIndex
      if !findStores(inUsesOf: tea, index: tailElementIndex * numTupleElements + tupleIdx, stores: &stores, context) {
        return false
      }
    case let atp as AddressToPointerInst:
      if !findStores(toTailAddress: atp, tailElementIndex: tailElementIndex, stores: &stores, context) {
        return false
      }
    case let mdi as MarkDependenceInst:
      if !findStores(toTailAddress: mdi, tailElementIndex: tailElementIndex, stores: &stores, context) {
        return false
      }
    case let pta as PointerToAddressInst:
      if !findStores(toTailAddress: pta, tailElementIndex: tailElementIndex, stores: &stores, context) {
        return false
      }
    case let store as StoreInst:
      if store.source.type.isTuple {
        // This kind of SIL is never generated because tuples are stored with separated stores to tuple_element_addr.
        // Just to be on the safe side..
        return false
      }
      if !handleStore(store, index: tailElementIndex, stores: &stores, context) {
        return false
      }
    default:
      if !isValidUseOfObject(use) {
        return false
      }
    }
  }
  return true
}

private func findStores(inUsesOf address: Value, index: Int, stores: inout [StoreInst?],
                        _ context: FunctionPassContext) -> Bool
{
  for use in address.uses {
    if let store = use.instruction as? StoreInst {
      if !handleStore(store, index: index, stores: &stores, context) {
        return false
      }
    } else if !isValidUseOfObject(use) {
      return false
    }
  }
  return true
}

private func handleStore(_ store: StoreInst, index: Int, stores: inout [StoreInst?],
                         _ context: FunctionPassContext) -> Bool
{
  if index >= 0 && index < stores.count,
     store.source.isValidGlobalInitValue(context),
     stores[index] == nil {
    stores[index] = store
    return true
  }
  return false
}

private func isValidUseOfObject(_ use: Operand) -> Bool {
  let inst = use.instruction
  switch inst {
  case is DebugValueInst,
       is LoadInst,
       is DeallocRefInst,
       is DeallocStackRefInst,
       is StrongRetainInst,
       is StrongReleaseInst,
       is FixLifetimeInst,
       is MarkDependenceAddrInst:
    return true

  case let mdi as MarkDependenceInst:
    if (use == mdi.baseOperand) {
      return true;
    }
    for mdiUse in mdi.uses {
      if !isValidUseOfObject(mdiUse) {
        return false
      }
    }
    return true

  case is StructElementAddrInst,
       is AddressToPointerInst,
       is StructInst,
       is TupleInst,
       is TupleExtractInst,
       is EnumInst,
       is StructExtractInst,
       is UncheckedRefCastInst,
       is UpcastInst,
       is BeginDeallocRefInst,
       is RefTailAddrInst,
       is RefElementAddrInst:
    for instUse in (inst as! SingleValueInstruction).uses {
      if !isValidUseOfObject(instUse) {
        return false
      }
    }
    return true

  case let bi as BuiltinInst:
    switch bi.id {
    case .ICMP_EQ, .ICMP_NE:
      // Handle the case for comparing addresses. This occurs when the Array
      // comparison function is inlined.
      return true
    case .DestroyArray:
      // We must not try to delete the tail allocated values. Although this would be a no-op
      // (because we only handle trivial types), it would be semantically wrong to apply this
      // builtin on the outlined object.
      return true
    default:
      return false
    }

  default:
    return false
  }
}

private func constructObject(of allocRef: AllocRefInstBase,
                             inInitializerOf global: GlobalVariable,
                             _ storesToClassFields: [StoreInst], _ storesToTailElements: [StoreInst],
                             _ context: FunctionPassContext) {
  var cloner = StaticInitCloner(cloneTo: global, context)
  defer { cloner.deinitialize() }

  // Create the initializers for the fields
  var objectArgs = [Value]()
  for store in storesToClassFields {
    objectArgs.append(cloner.clone(store.source as! SingleValueInstruction))
  }
  let globalBuilder = Builder(staticInitializerOf: global, context)

  if !storesToTailElements.isEmpty {
    // Create the initializers for the tail elements.
    let numTailTupleElems = allocRef.numStoresPerTailElement
    if numTailTupleElems > 1 {
      // The elements are tuples: combine numTailTupleElems elements to a single tuple instruction.
      for elementIdx in 0..<allocRef.numTailElements! {
        let tupleElems = (0..<numTailTupleElems).map { tupleIdx in
            let store = storesToTailElements[elementIdx * numTailTupleElems + tupleIdx]
            return cloner.clone(store.source as! SingleValueInstruction)
        }
        let tuple = globalBuilder.createTuple(type: allocRef.tailAllocatedTypes[0], elements: tupleElems)
        objectArgs.append(tuple)
      }
    } else {
      // The non-tuple element case.
      for store in storesToTailElements {
        objectArgs.append(cloner.clone(store.source as! SingleValueInstruction))
      }
    }
  }
  globalBuilder.createObject(type: allocRef.type, arguments: objectArgs, numBaseElements: storesToClassFields.count)

  // The initial value can contain a `begin_access` if it references another global variable by address, e.g.
  //   var p = Point(x: 10, y: 20)
  //   let a = [UnsafePointer(&p)]
  //
  global.stripAccessInstructionFromInitializer(context)
}

private func replace(object allocRef: AllocRefInstBase,
                     with global: GlobalVariable,
                     _ context: FunctionPassContext) -> GlobalValueInst {

  // Replace the alloc_ref by global_value + strong_retain instructions.
  let builder = Builder(before: allocRef, context)
  let globalValue = builder.createGlobalValue(global: global, isBare: false)
  if !allocRef.parentFunction.hasOwnership {
    builder.createStrongRetain(operand: globalValue)
  }

  rewriteUses(of: allocRef, context)
  allocRef.replace(with: globalValue, context)
  return globalValue
}

private func rewriteUses(of startValue: Value, _ context: FunctionPassContext) {
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(usersOf: startValue)

  while let inst = worklist.pop() {
    switch inst {
    case let beginDealloc as BeginDeallocRefInst:
      worklist.pushIfNotVisited(usersOf: beginDealloc)
      let builder = Builder(before: beginDealloc, context)
      if !beginDealloc.parentFunction.hasOwnership {
        builder.createStrongRelease(operand: beginDealloc.reference)
      }
      beginDealloc.replace(with: beginDealloc.reference, context)
    case is EndCOWMutationInst, is EndInitLetRefInst, is MoveValueInst:
      let svi = inst as! SingleValueInstruction
      worklist.pushIfNotVisited(usersOf: svi)
      svi.replace(with: svi.operands[0].value, context)
    case let upCast as UpcastInst:
      worklist.pushIfNotVisited(usersOf: upCast)
    case let refCast as UncheckedRefCastInst:
      worklist.pushIfNotVisited(usersOf: refCast)
    case let moveValue as MoveValueInst:
      worklist.pushIfNotVisited(usersOf: moveValue)
    case is DeallocRefInst, is DeallocStackRefInst:
      context.erase(instruction: inst)
    default:
      break
    }
  }
}

private extension InstructionWorklist {
  mutating func pushIfNotVisited(usersOf value: Value) {
    pushIfNotVisited(contentsOf: value.users)
  }
}

private extension AllocRefInstBase {
  var fieldsKnownStatically: Bool {
    if let allocDynamic = self as? AllocRefDynamicInst,
       !allocDynamic.isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType {
      return false
    }
    if isObjC {
      return false
    }
    return true
  }

  var numTailElements: Int? {

    if tailAllocatedCounts.count == 0 {
      return 0
    }

    // We only support a single tail allocated array.
    // Stdlib's tail allocated arrays don't have any side-effects in the constructor if the element type is trivial.
    // TODO: also exclude custom tail allocated arrays which might have side-effects in the destructor.
    if tailAllocatedCounts.count != 1 {
      return nil
    }

    // The number of tail allocated elements must be constant.
    if let tailCountLiteral = tailAllocatedCounts[0].value as? IntegerLiteralInst,
       let count = tailCountLiteral.value
    {
      return count
    }
    return nil
  }

  var numClassFields: Int {
    assert(type.isClass)
    return type.getNominalFields(in: parentFunction)!.count
  }

  var numStoresPerTailElement: Int {
    let tailType = tailAllocatedTypes[0]
    if tailType.isTuple {
      return tailType.tupleElements.count
    }
    return 1
  }
}

private func optimizeFindStringCall(stringArray: GlobalValueInst, _ context: FunctionPassContext) {
  if stringArray.numArrayElements > 16,
     let findStringCall = findFindStringCall(stringArray: stringArray),
     let cachedFindStringFunc = getFindStringSwitchCaseWithCacheFunction(context) {
    replace(findStringCall: findStringCall, with: cachedFindStringFunc, context)
  }
}

/// Finds a call to findStringSwitchCase which takes `stringArray` as parameter.
private func findFindStringCall(stringArray: Value) -> ApplyInst? {
  for use in stringArray.uses {
    switch use.instruction {
    case let apply as ApplyInst:
      // There should only be a single call to findStringSwitchCase. But even
      // if there are multiple calls, it's not problem - we'll just optimize the
      // last one we find.
      if apply.hasSemanticsAttribute("findStringSwitchCase") {
        return apply
      }
    case is StructInst,
         is TupleInst,
         is UncheckedRefCastInst,
         is UpcastInst:
      if let foundCall = findFindStringCall(stringArray: use.instruction as! SingleValueInstruction) {
        return foundCall
      }
    default:
      break
    }
  }
  return nil
}

private func getFindStringSwitchCaseWithCacheFunction(_ context: FunctionPassContext) -> Function? {
  if let f = context.lookupStdlibFunction(name: "_findStringSwitchCaseWithCache"),
     f.argumentTypes.count == 3 {
    return f
  }
  return nil
}

private func replace(findStringCall: ApplyInst,
                     with cachedFindStringFunc: Function,
                     _ context: FunctionPassContext) {
  let cacheType = cachedFindStringFunc.argumentTypes[2].objectType
  let wordTy = cacheType.getNominalFields(in: findStringCall.parentFunction)![0]

  let name = context.mangleOutlinedVariable(from: findStringCall.parentFunction)

  // Create an "opaque" global variable which is passed as inout to
  // _findStringSwitchCaseWithCache and into which the function stores the "cache".
  let cacheVar = context.createGlobalVariable(name: name, type: cacheType, linkage: .private, isLet: false)

  let varBuilder = Builder(staticInitializerOf: cacheVar, context)
  let zero = varBuilder.createIntegerLiteral(0, type: wordTy)
  _ = varBuilder.createStruct(type: cacheType, elements: [zero, zero])

  let builder = Builder(before: findStringCall, context)
  let cacheAddr = builder.createGlobalAddr(global: cacheVar, dependencyToken: nil)
  let findStringRef = builder.createFunctionRef(cachedFindStringFunc)
  let newCall = builder.createApply(function: findStringRef, SubstitutionMap(),
                                    arguments: [findStringCall.arguments[0],
                                                findStringCall.arguments[1],
                                                cacheAddr])

  findStringCall.replace(with: newCall, context)
}

private extension GlobalValueInst {
  /// Assuming the global is an Array, returns the number of elements = tail elements.
  var numArrayElements: Int {
    (global.staticInitValue! as! ObjectInst).tailOperands.count
  }
}

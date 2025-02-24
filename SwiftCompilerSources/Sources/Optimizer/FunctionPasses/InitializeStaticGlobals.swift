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

  if context.hadError {
    // In case of a preceding error, there is no guarantee that the SIL is valid.
    return
  }

  if !function.isGlobalInitOnceFunction {
    return
  }

  // Sometimes structs are not stored in one piece, but as individual elements.
  // Merge such individual stores to a single store of the whole struct.
  mergeStores(in: function, context)

  guard let (allocInst, storeToGlobal, inlineArrays) = getGlobalInitializerInfo(of: function, context) else {
    return
  }

  if !allocInst.global.canBeInitializedStatically {
    return
  }

  /// Replace inline arrays, which are allocated in stack locations with `vector` instructions.
  /// Note that `vector` instructions are only allowed in global initializers. Therefore it's important
  /// that the code in this global initializer is eventually completely removed after copying it to the global.
  for array in inlineArrays {
    lowerInlineArray(array: array, context)
  }
    
  var cloner = StaticInitCloner(cloneTo: allocInst.global, context)
  defer { cloner.deinitialize() }

  _ = cloner.clone(storeToGlobal.source)

  // The initial value can contain a `begin_access` if it references another global variable by address, e.g.
  //   var p = Point(x: 10, y: 20)
  //   let o = UnsafePointer(&p)
  //
  allocInst.global.stripAccessInstructionFromInitializer(context)

  context.erase(instruction: allocInst)
  context.erase(instruction: storeToGlobal)
  context.removeTriviallyDeadInstructionsIgnoringDebugUses(in: function)
}

/// Gets all info about a global initializer function if it can be converted to a statically initialized global.
private func getGlobalInitializerInfo(
  of function: Function,
  _ context: FunctionPassContext
) -> (allocInst: AllocGlobalInst, storeToGlobal: StoreInst, inlineArrays: [InlineArray])? {

  var arrayInitInstructions = InstructionSet(context)
  defer { arrayInitInstructions.deinitialize() }
  
  var inlineArrays = [InlineArray]()
  
  guard let (allocInst, storeToGlobal) = getGlobalInitialization(of: function, context,
    handleUnknownInstruction: { inst in
      if let asi = inst as? AllocStackInst {
        if let array = getInlineArrayInfo(of: asi) {
          inlineArrays.append(array)
          arrayInitInstructions.insertAllAddressUses(of: asi)
          return true
        }
        return false
      }
      // Accept all instructions which are part of inline array initialization, because we'll remove them anyway.
      return arrayInitInstructions.contains(inst)
    })
  else {
    return nil
  }

  return (allocInst, storeToGlobal, inlineArrays)   
}

/// Represents an inline array which is initialized by a literal.
private struct InlineArray {
  let elementType: Type
  
  /// In case the `elementType` is a tuple, the element values are flattened,
  /// i.e. `elements` contains elementcount * tupleelements values.
  let elements: [Value]
  
  /// The final load instruction which loads the initialized array from a temporary stack location.
  let finalArrayLoad: LoadInst
  
  /// The stack location which contains the initialized array.
  var stackLoocation: AllocStackInst { finalArrayLoad.address as! AllocStackInst }
}

/// Replaces an initialized inline array (which is allocated in a temporary stack location) with a
/// `vector` instruction.
/// The stack location of the array is removed.
private func lowerInlineArray(array: InlineArray, _ context: FunctionPassContext) {
  let vector: VectorInst
  let builder = Builder(after: array.finalArrayLoad, context)
  if array.elementType.isTuple {
    let numTupleElements = array.elementType.tupleElements.count
    assert(array.elements.count % numTupleElements == 0)
    var tuples: [TupleInst] = []
    for tupleIdx in 0..<(array.elements.count / numTupleElements) {
      let range = (tupleIdx * numTupleElements) ..< ((tupleIdx + 1) * numTupleElements) 
      let tuple = builder.createTuple(type: array.elementType, elements: Array(array.elements[range]))
      tuples.append(tuple)
    }
    vector = builder.createVector(type: array.elementType, arguments: tuples)
  } else {
    vector = builder.createVector(type: array.elementType, arguments: array.elements)      
  }
  array.finalArrayLoad.uses.replaceAll(with: vector, context)
  context.erase(instructionIncludingAllUsers: array.stackLoocation)
}

/// An alloc_stack could be a temporary object which holds an initialized inline-array literal.
/// It looks like:
///
///     %1 = alloc_stack $InlineArray<Count, ElementType>
///     %2 = unchecked_addr_cast %1 to $*ElementType      // the elementStorage
///     store %firstElement to [trivial] %2
///     %4 = integer_literal $Builtin.Word, 1
///     %5 = index_addr %2, %4
///     store %secondElement to [trivial] %5
///     ...
///     %10 = load [trivial] %1                   // the final arrayLoad
///     dealloc_stack %1
///
/// Returns nil if `allocStack` is not a properly initialized inline array.
///
private func getInlineArrayInfo(of allocStack: AllocStackInst) -> InlineArray? {
  var arrayLoad: LoadInst? = nil
  var elementStorage: UncheckedAddrCastInst? = nil

  for use in allocStack.uses {
    switch use.instruction {
    case let load as LoadInst:
      if arrayLoad != nil {
        return nil
      }
      // It's guaranteed that the array load is located after all element stores.
      // Otherwise it would load uninitialized memory.
      arrayLoad = load
    case is DeallocStackInst:
      break
    case let addrCastToElement as UncheckedAddrCastInst:
      if elementStorage != nil {
        return nil
      }
      elementStorage = addrCastToElement
    default:
      return nil
    }
  }
  guard let arrayLoad, let elementStorage else {
    return nil
  } 
  
  var stores = Array<StoreInst?>()
  if !findArrayElementStores(toElementAddress: elementStorage, elementIndex: 0, stores: &stores) {
    return nil
  }
  if stores.isEmpty {
    // We cannot create an empty `vector` instruction, therefore we don't support empty inline arrays.
    return nil
  }
  // Usually there must be a store for each element. Otherwise the `arrayLoad` would load uninitialized memory.
  // We still check this to not crash in some weird corner cases, like the element type is an empty tuple.
  if stores.contains(nil) {
    return nil
  }

  return InlineArray(elementType: elementStorage.type.objectType,
                     elements: stores.map { $0!.source },
                     finalArrayLoad: arrayLoad)
}

/// Recursively traverses all uses of `elementAddr` and finds all stores to an inline array storage.
/// The element store instructions are put into `stores` - one store for each element.
/// In case the element type is a tuple, the tuples are flattened. See `InlineArray.elements`.  
private func findArrayElementStores(
  toElementAddress elementAddr: Value,
  elementIndex: Int,
  stores: inout [StoreInst?]
) -> Bool {
  for use in elementAddr.uses {
    switch use.instruction {
    case let indexAddr as IndexAddrInst:
      guard let indexLiteral = indexAddr.index as? IntegerLiteralInst,
            let tailIdx = indexLiteral.value else
      {
        return false
      }
      if !findArrayElementStores(toElementAddress: indexAddr, elementIndex: elementIndex + tailIdx, stores: &stores) {
        return false
      }
    case let tea as TupleElementAddrInst:
      // The array elements are tuples. There is a separate store for each tuple element.
      let numTupleElements = tea.tuple.type.tupleElements.count
      let tupleIdx = tea.fieldIndex
      if !findArrayElementStores(toElementAddress: tea,
                                 elementIndex: elementIndex * numTupleElements + tupleIdx,
                                 stores: &stores) {
        return false
      }
    case let store as StoreInst:
      if store.source.type.isTuple {
        // This kind of SIL is never generated because tuples are stored with separated stores to tuple_element_addr.
        // Just to be on the safe side..
        return false
      }
      if elementIndex >= stores.count {
        stores += Array(repeating: nil, count: elementIndex - stores.count + 1)
      }
      if stores[elementIndex] != nil {
        // An element is stored twice.
        return false
      }
      stores[elementIndex] = store
    default:
      return false
    }
  }
  return true
}

/// Merges stores to individual struct fields to a single store of the whole struct.
///
///   store %element1 to %element1Addr
///   store %element2 to %element2Addr
/// ->
///   %s = struct $S (%element1, %element2)
///   store %s to @structAddr
private func mergeStores(in function: Function, _ context: FunctionPassContext) {
  for inst in function.instructions {
    if let store = inst as? StoreInst {
      if let (elementStores, lastStore) = getSequenceOfElementStores(firstStore: store) {
        merge(elementStores: elementStores, lastStore: lastStore, context)
      }
    }
  }
}

/// Returns a sequence of individual stores to elements of a struct.
///
///   %addr1 = struct_element_addr %structAddr, #field1
///   store %element1 to %addr1
///   // ...
///   %addr_n = struct_element_addr %structAddr, #field_n
///   store %element_n to %addr_n
///
private func getSequenceOfElementStores(firstStore: StoreInst) -> ([StoreInst], lastStore: StoreInst)? {
  guard let elementAddr = firstStore.destination as? StructElementAddrInst else {
    return nil
  }
  let structAddr = elementAddr.struct
  let structType = structAddr.type
  if structType.isMoveOnly {
    return nil
  }
  if (structType.nominal as! StructDecl).hasUnreferenceableStorage {
    return nil
  }
  guard let fields = structType.getNominalFields(in: firstStore.parentFunction) else {
    return nil
  }
  let numElements = fields.count
  var elementStores = Array<StoreInst?>(repeating: nil, count: numElements)
  var numStoresFound = 0

  for inst in InstructionList(first: firstStore) {
    switch inst {
    case let store as StoreInst:
      guard store.storeOwnership == .trivial,
            let sea = store.destination as? StructElementAddrInst,
            sea.struct == structAddr,
            // Multiple stores to the same element?
            elementStores[sea.fieldIndex] == nil else {
        return nil
      }

      elementStores[sea.fieldIndex] = store
      numStoresFound += 1
      if numStoresFound == numElements {
        // If we saw  `numElements` distinct stores, it implies that all elements in `elementStores` are not nil.
        return (elementStores.map { $0! }, lastStore: store)
      }
    default:
      if inst.mayReadOrWriteMemory {
        return nil
      }
    }
  }
  return nil
}

private func merge(elementStores: [StoreInst], lastStore: StoreInst, _ context: FunctionPassContext) {
  let builder = Builder(after: lastStore, context)

  let structAddr = (lastStore.destination as! StructElementAddrInst).struct
  let str = builder.createStruct(type: structAddr.type.objectType, elements: elementStores.map { $0.source })
  builder.createStore(source: str, destination: structAddr, ownership: lastStore.storeOwnership)

  for store in elementStores {
    let destAddr = store.destination as! StructElementAddrInst
    context.erase(instruction: store)
    if destAddr.uses.isEmpty {
      context.erase(instruction: destAddr)
    }
  }
}

private extension InstructionSet {
  mutating func insertAllAddressUses(of value: Value) {
    for use in value.uses {
      if insert(use.instruction) {
        for result in use.instruction.results where result.type.isAddress {
          insertAllAddressUses(of: result)
        }
      }
    }
  }
}

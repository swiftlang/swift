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

  // The initializer must not contain a `global_value` because `global_value` needs to
  // initialize the class metadata at runtime.
  guard let (allocInst, storeToGlobal) = getGlobalInitialization(of: function,
                                                                 forStaticInitializer: true,
                                                                 context) else
  {
    return
  }

  if !allocInst.global.canBeInitializedStatically {
    return
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

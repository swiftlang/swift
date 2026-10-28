//===- PackTypeElimination.swift - Eliminate packs in function arguments -===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Eliminate pack element archetype (@pack_element) types.
///
/// If the type that a @pack_element type maps to can be statically determined,
/// cloning its corresponding open_pack_element instruction will register a
/// mapping from the @pack_element type to the concrete type. By recursively
/// cloning all type-dependent users of the open_pack_element, we eliminate the
/// @pack_element type. This enables generic specialization of code that uses
/// pack types.
///
/// There are two ways to determine the corresponding concrete type for a
/// @pack_element type:
/// - If every element of the pack is the same type, it will be that.
/// - If the index operand of open_pack_element is a scalar_pack_index, it will
///   be the type at the index indicated by the scalar_pack_index.
///
/// To enable the second option in as many cases as possible, we rewrite any
/// dynamic_pack_index and pack_pack_index instructions that we can to an
/// equivalent scalar_pack_index. We must rewrite open_pack_element and the
/// dynamic pack indexing instructions in the same pass to prevent verifier
/// errors. For instance, pack_element_get cannot use a scalar_pack_index to get
/// a pack element with a @pack_element type:
///
/// ```
///   %idx = scalar_pack_index 0 of $Pack{Int, Int, Int}
///   %ope = open_pack_element %idx of <each T> at <Pack{Int, Int, Int}>, shape $each T, id 1
///   // verifier error: scalar pack index must match exactly
///   %ptr = pack_element_get %idx of %0 : $*Pack{Int, Int, Int} as $*@pack_element(1) each T
/// ```
///
/// ```
///   %14 = alloc_pack $Pack{Double, String}
///   %15 = integer_literal $Builtin.Word, 0
///   %17 = dynamic_pack_index %15 of $Pack{Double, String}
///   %18 = open_pack_element %17 of <each Value> at <Pack{Double, String}>, shape $each U, id 6
///   %20 = unchecked_addr_cast %9 to $*@pack_element(6) each U
///   %21 = alloc_stack $@pack_element(6) each U
///   pack_element_set %21 : $*@pack_element(6) each U into %17 of %14
///   copy_addr %20 to [init] %21
///   %22 = function_ref @generic_op : $@convention(thin) <T> (@in_guaranteed T) -> ()
///   %23 = apply %22<@pack_element(6) each U>(%21) : $@convention(thin) <T> (@in_guaranteed T) -> ()
///   destroy_addr %21
///   dealloc_stack %21
///   dealloc_pack %14
/// ```
/// ->
/// ```
///   %14 = alloc_pack $Pack{Double, String}
///   %17 = scalar_pack_index 0 of $Pack{Double, String}
///   %20 = unchecked_addr_cast %9 to $*Double
///   %21 = alloc_stack $Double
///   copy_addr %20 to [init] %21
///   pack_element_set %21 : $*Double into %17 of %14
///   %22 = function_ref @generic_op : $@convention(thin) <T> (@in_guaranteed T) -> ()
///   %23 = apply %22<Double>(%21) : $@convention(thin) <T> (@in_guaranteed T) -> ()
///   destroy_addr %21
///   dealloc_stack %21
///   dealloc_pack %14
/// ```
let packTypeElimination = FunctionPass(name: "pack-type-elimination") {
  (function: Function, context: FunctionPassContext) in

  tryEliminateOpenedPackElementTypes(in: function, context)
}

/// If an equivalent structural pack index can be determined for the provided
/// dynamic pack indexing instruction, replace it with an equivalent
/// scalar_pack_index.
private func tryRewriteToScalar(
  dynamicPackIndex inst: AnyPackIndexInst, in function: Function, _ context: FunctionPassContext
) {
  guard let index = inst.structuralIndex
  else {
    return
  }

  let builder = Builder(before: inst, context)
  let scalarPackIndex = builder.createScalarPackIndex(
    componentIndex: index, indexedPackType: inst.indexedPackType)
  inst.replace(with: scalarPackIndex, context)
  return
}

private func tryEliminateOpenedPackElementTypes(
  in function: Function, _ context: FunctionPassContext
) {
  var cloner = Cloner(in: function, context)
  defer { cloner.deinitialize() }

  var mappedOpenInsts = [OpenPackElementInst]()

  // Collect open_pack_element instructions whose element archetypes can be
  // eliminated, and register the SubstitutionMaps that will eliminate them with
  // the cloner.
  //
  // Rewrite dynamic_pack_index and pack_pack_index to scalar_pack_index when
  // possible to allow us to eliminate open_pack_element in more cases.
  for inst in function.instructions {
    switch inst {
    case let opei as OpenPackElementInst:
      if let map = opei.elementArchetypeSubstitutionMap {
        cloner.registerConcreteLocalArchetypeMapping(from: opei.openedGenericEnvironment, to: map)
        mappedOpenInsts.append(opei)
      }
      break
    case let dpi as DynamicPackIndexInst:
      tryRewriteToScalar(dynamicPackIndex: dpi, in: function, context)
      break
    case let ppi as PackPackIndexInst:
      tryRewriteToScalar(dynamicPackIndex: ppi, in: function, context)
      break
    default:
      break
    }
  }

  if mappedOpenInsts.isEmpty {
    return
  }

  for inst in function.instructions {
    if instUsesMappedArchetype(inst: inst, from: mappedOpenInsts) {
      cloner.setInsertionPoint(before: inst)
      let clone = cloner.clone(instruction: inst)
      for (origResult, newResult) in zip(inst.results, clone.results) {
        origResult.uses.replaceAll(with: newResult, context)
      }
      context.erase(instruction: inst)
    }
  }

  // All uses of the mapped open_pack_element instructions have been eliminated.
  // We can now erase them.
  for opei in mappedOpenInsts {
    context.erase(instruction: opei)
  }
}

private func instUsesMappedArchetype(inst: Instruction, from mappedOpenInsts: [OpenPackElementInst])
  -> Bool
{
  for opei in mappedOpenInsts {
    let hasElementArchetype = { (value: Value) in
      value.type.canonicalType.hasLocalArchetype(from: opei.openedGenericEnvironment)
    }
    if inst.definedOperands.map({ $0.value }).contains(where: hasElementArchetype)
      || inst.typeDependentOperands.contains(where: { operand in
        operand.value.definingInstruction == opei
      })
      || inst.results.contains(where: hasElementArchetype)
    {
      return true
    }
  }
  return false
}

//===--- SimplifyDestructure.swift ----------------------------------------===//
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

extension DestructureTupleInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    // If the tuple is trivial, replace
    // ```
    //   (%1, %2) = destructure_tuple %t
    // ```
    // ->
    // ```
    //   %1 = tuple_extract %t, 0
    //   %2 = tuple_extract %t, 1
    // ```
    // This canonicalization helps other optimizations to e.g. CSE tuple_extracts.
    //
    if replaceWithTupleExtract(context) {
      return
    }

    // Eliminate the redundant instruction pair
    // ```
    //   %t = tuple (%0, %1, %2)
    //   (%3, %4, %5) = destructure_tuple %t
    // ```
    // and replace the results %3, %4, %5 with %0, %1, %2, respectively
    //
    if let tuple = self.tuple as? TupleInst {
      tryReplaceConstructDestructPair(construct: tuple, destruct: self, context)
    }
  }

  private func replaceWithTupleExtract(_ context: SimplifyContext) -> Bool {
    guard self.tuple.type.isTrivial(in: parentFunction) else {
      return false
    }
    let builder = Builder(before: self, context)
    for (elementIdx, result) in results.enumerated() {
      let elementValue = builder.createTupleExtract(tuple: self.tuple, elementIndex: elementIdx)
      result.uses.replaceAll(with: elementValue, context)
    }
    context.erase(instruction: self)
    return true
  }
}

extension DestructureStructInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    // If the struct is trivial, replace
    // ```
    //   (%1, %2) = destructure_struct %s
    // ```
    // ->
    // ```
    //   %1 = struct_extract %s, #S.field0
    //   %2 = struct_extract %s, #S.field1
    // ```
    // This canonicalization helps other optimizations to e.g. CSE tuple_extracts.
    //
    if replaceWithStructExtract(context) {
      return
    }

    switch self.struct {
    case let str as StructInst:
      // Eliminate the redundant instruction pair
      // ```
      //   %s = struct (%0, %1, %2)
      //   (%3, %4, %5) = destructure_struct %s
      // ```
      // and replace the results %3, %4, %5 with %0, %1, %2, respectively
      //
      tryReplaceConstructDestructPair(construct: str, destruct: self, context)

    case let copy as CopyValueInst:
      // Similar to the pattern above, but with a copy_value:
      // Replace
      // ```
      //   %s = struct (%0, %1, %2)
      //   %c = copy_value %s           // can also be a chain of multiple copies
      //   (%3, %4, %5) = destructure_struct %c
      // ```
      // with
      // ```
      //   %c0 = copy_value %0
      //   %c1 = copy_value %1
      //   %c2 = copy_value %2
      //   %s = struct (%0, %1, %2)    // keep the original struct
      // ```
      // and replace the results %3, %4, %5 with %c0, %c1, %c2, respectively.
      //
      // This transformation has the advantage that we can do it even if the `struct` instruction
      // has other uses than the `copy_value`.
      //
      if copy.uses.singleUse?.instruction == self,
         let structInst = copy.fromValue.lookThroughCopy as? StructInst,
         structInst.parentBlock == self.parentBlock
      {
        for (result, operand) in zip(self.results, structInst.operands) {
          if operand.value.type.isTrivial(in: parentFunction) {
            result.uses.replaceAll(with: operand.value, context)
          } else {
            let builder = Builder(before: structInst, context)
            let copiedOperand = builder.createCopyValue(operand: operand.value)
            result.uses.replaceAll(with: copiedOperand, context)
          }
        }
        context.erase(instruction: self)
        context.erase(instruction: copy)
      }
    default:
      break
    }
  }

  private func replaceWithStructExtract(_ context: SimplifyContext) -> Bool {
    guard self.struct.type.isTrivial(in: parentFunction) else {
      return false
    }
    let builder = Builder(before: self, context)
    for (fieldIdx, result) in results.enumerated() {
      let fieldValue = builder.createStructExtract(struct: self.struct, fieldIndex: fieldIdx)
      result.uses.replaceAll(with: fieldValue, context)
    }
    context.erase(instruction: self)
    return true
  }
}

private func tryReplaceConstructDestructPair(construct: SingleValueInstruction,
                                             destruct: MultipleValueInstruction,
                                             _ context: SimplifyContext) {
  let singleUse = context.preserveDebugInfo ? construct.uses.singleUse : construct.uses.ignoreDebugUses.singleUse
  let canEraseFirst = singleUse?.instruction == destruct

  if !canEraseFirst && construct.parentFunction.hasOwnership && construct.ownership == .owned {
    // We cannot add more uses to this tuple without inserting a copy.
    return
  }

  for (result, operand) in zip(destruct.results, construct.operands) {
    result.uses.replaceAll(with: operand.value, context)
  }

  if canEraseFirst {
    context.erase(instructionIncludingDebugUses: destruct)
  }
}

//===--- SimplifyTuple.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension TupleInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {

    // Eliminate the redundant instruction pair
    // ```
    //   (%3, %4, %5) = destructure_tuple %input
    //   %output = tuple (%3, %4, %5)
    // ```
    // and replace the result %output with %input
    //
    var destructure: DestructureTupleInst?
    for operand in operands {
      guard let def = operand.value.definingInstruction as? DestructureTupleInst else {
        return
      }
      guard let destructure else {
        destructure = def
        continue
      }
      if destructure != def {
        return
      }
    }
    guard let destructure else {
      return
    }
    guard destructure.operand.value.type == type else {
      return
    }
    // The destructure's operand having the same type as the tuple ensures that
    // the count of results of the destructure is equal to the count of operands
    // of the tuple.
    assert(destructure.results.count == operands.count)
    for (result, operand) in zip(destructure.results, operands) {
      if result != operand.value {
        return
      }
    }
    tryReplaceDestructConstructPair(destruct: destructure, construct: self, context)
  }
}

private func tryReplaceDestructConstructPair(destruct: MultipleValueInstruction & UnaryInstruction,
                                             construct: SingleValueInstruction,
                                             _ context: SimplifyContext) {
  let everyResultUsedOnce = context.preserveDebugInfo
                    ? destruct.results.allSatisfy { $0.uses.singleUse != nil }
                    : destruct.results.allSatisfy { $0.uses.ignoreDebugUses.singleUse != nil }
  let anyOwned = destruct.results.contains { $0.ownership == .owned }

  if !everyResultUsedOnce && construct.parentFunction.hasOwnership && anyOwned {
    // We cannot add more uses to this destructure without inserting a copy.
    return
  }

  construct.uses.replaceAll(with: destruct.operand.value, context)

  if everyResultUsedOnce {
    context.erase(instructionIncludingDebugUses: construct)
  }
}

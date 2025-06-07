//===--- SimplificationPasses.swift ----------------------------------------==//
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

/// Removes redundant `debug_step` instructions.
/// If a `debug_step` has the same debug location as a previous or succeeding instruction
/// it is removed. It's just important that there is at least one instruction for a
/// certain debug location so that single stepping on that location will work.
let cleanupDebugStepsPass = FunctionPass(name: "cleanup-debug-steps") {
  (function: Function, context: FunctionPassContext) in

  for block in function.blocks {
    cleanupDebugSteps(in: block, context)
  }
}

private func cleanupDebugSteps(in block: BasicBlock, _ context: FunctionPassContext) {
  var lastInstWithSameLocation: Instruction?

  for inst in block.instructions {
    if !inst.location.isDebugSteppable {
      if inst is DebugStepInst && !inst.location.isDebugSteppable {
        // First case: the instruction which is replaced by the debug_step didn't have a valid
        // location itself. Then we don't need the debug_step either.
        context.erase(instruction: inst)
      }
      continue
    }

    if let li = lastInstWithSameLocation,
       !inst.location.hasSameSourceLocation(as: li.location) {
      lastInstWithSameLocation = nil
    }

    // Only instructions which are really compiled down to some machine instructions can be
    // single stepped on.
    if !inst.producesMachineCode {
      continue
    }

    if let li = lastInstWithSameLocation {
      if inst is DebugStepInst {

        // Second case:
        //   %li = some_instruction, loc "l"
        //   debug_step, loc "l"                // current inst -> erase
        context.erase(instruction: inst)
        continue
      } else if li is DebugStepInst {

        // Third case:
        //   debug_step, loc "l"                // li -> erase
        //   %inst = some_instruction, loc "l"  // current inst
        context.erase(instruction: li)
      }
    }
    lastInstWithSameLocation = inst
  }
}

private extension Instruction {
  var producesMachineCode: Bool {
    switch self {
    // We could include more instructions here.
    // In worst case a debug_step instruction remains in the code although it's not needed.
    // This is harmless.
    case is DebugStepInst, is ApplySite, is LoadInst, is StoreInst, is TermInst:
      return location.isDebugSteppable
    default:
      return false
    }
  }
}
